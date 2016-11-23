module Parser.MetaFunctionParser where

{-
This module parses metafunctions and then passes them through a type checker.
These have a different parse tree, as we can't parse that in one pass 
-}

import Utils
import Parser.ParsingUtils
import Parser.BNFParser

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (intersperse)

import Debug.Trace


-- S from Simple, before typing
data SClause = SClause [SExpression] SExpression
	deriving (Show, Ord, Eq)

data SExpression = SVar Name 
		| SMELiteral String 
		| SMELiteralInt Int 
		| SMESeq [SExpression] 
		| SMECall Name Builtin [SExpression]
		| SError String
	deriving (Show, Ord, Eq)

data SFunction = SFunction {sf_name :: Name, sf_type :: MetaType, sf_body :: [SClause]}
	deriving (Show, Ord, Eq)


parseMetaFunctions	:: BNFRules -> Parser u MetaFunctions
parseMetaFunctions bnfs
	= do	funcs	<- many $ try (nls >> parseMetaFunction bnfs)
		return $ M.fromList (funcs |> buildTypedFunction)



buildTypedFunction	:: SFunction -> (Name, MetaFunction)
buildTypedFunction (SFunction nm tp body)
	= (nm, MFunction tp ((body |> buildTypedClause) ++ [undefinedClause tp]) )

undefinedClause	:: MetaType -> MetaClause
undefinedClause tp
	= let	flat	= flatten tp
		args	= [0 .. length flat - 2] |> show |> ("t"++) |> MVar
		expr	= MError "Undefined behaviour: no pattern matched"
		in
		MClause args expr
		


buildTypedClause	:: SClause -> MetaClause
buildTypedClause (SClause pats expr)
	= MClause (pats |> buildTypedExpr) (expr & buildTypedExpr)


-- Given function typings, builds what variable is expected to have what type
buildTypedExpr		:: SExpression -> MetaExpression
buildTypedExpr (SVar name)	= MVar name
buildTypedExpr (SMELiteral s)
			= MLiteral s
buildTypedExpr (SMELiteralInt i)
			= MInt i
buildTypedExpr (SMESeq exprs)
			= exprs |> buildTypedExpr & MSeq
buildTypedExpr (SMECall nm builtin args)
			= MCall nm builtin (args |> buildTypedExpr)
buildTypedExpr (SError msg)	
			= MError msg






---------------------------- PARSING OF A SINGLE FUNCTION ------------------------------

parseMetaFunction	:: BNFRules -> Parser u SFunction
parseMetaFunction bnfs	
	= do	(nm, tp)	<- metaSignature bnfs
		let tps		= flatten tp
		let prsClause	= metaClause bnfs nm (init tps) (last tps)
		nls
		clauses		<- many1 $ try (prsClause <* nls)
		return $ SFunction nm tp clauses

metaType	:: [Name] -> Parser u MetaType
metaType bnfTypes	
	= try (do	t1 <- choose bnfTypes |> MType
			ws
			string "->"
			ws
			tr <- metaType bnfTypes
			return $ MTArrow t1 tr)
		<|>	(choose bnfTypes |> MType)
				

metaSignature	:: BNFRules -> Parser u (Name, MetaType)
metaSignature bnfs
	= do	ws
		nm	<- identifier
		ws
		string	":"
		ws
		tp	<- metaType $ bnfNames bnfs
		return (nm, tp)
		

metaClause	:: BNFRules -> Name -> [MetaTypeName] -> MetaTypeName -> Parser u SClause
metaClause bnfs name argsTps returnTp
	= (do	ws
		string name
		ws
		string "("
		ws
		let argParsers	= argsTps |> metaExpr bnfs
		args	<- intersperseM (ws >> char ',' >> ws) argParsers 
		ws
		string ")"
		try (ws >> nl >> ws) <|> ws
		string "="
		ws
		expr	<- (try metaError <|> metaExpr bnfs returnTp)
		return $ SClause args expr)


metaVar	= identifier |> SVar

metaCall
	= do	builtin	<- (try $ char '!' >> return True) <|> return False
		nm	<- identifier
		ws
		char '('
		vars	<- (ws >> (try metaCall <|> metaVar) <* ws) `sepBy` char ','	
		char ')'
		return $ SMECall nm builtin vars



metaExpr	:: BNFRules -> MetaTypeName -> Parser u SExpression
metaExpr bnfs rule
	= trace ("Trying "++rule)
	  (do	let errMsg	= "MetaType (= BNF-rule) "++rule++" not known"
		guidances	<- M.lookup rule bnfs & maybe (fail errMsg) return
		guidances |> metaExprPart' bnfs & first)




metaExprPart'	:: BNFRules -> BNFAST -> Parser u SExpression
metaExprPart' rules guidance
	= trace ("   "++show guidance)
	  (do	ws
		try (metaExprPart rules guidance) <|> try metaCall <|> try metaVar)

metaExprPart	:: BNFRules -> BNFAST -> Parser u SExpression
metaExprPart _ (Literal string)
	= do	Literal val	<- bnfLiteral
		if val /= string then
			fail $ "Expected a literal "++string
		else	
			return $ SMELiteral string
metaExprPart _ Identifier
	= do	char '"'
		id	<- identifier
		char '"'
		return $ SMELiteral id
metaExprPart _ Number
	= do	char '"'
		i	<- number
		char '"'
		return $ SMELiteralInt i
metaExprPart rules (Seq bnfAsts)
	= trace (show bnfAsts) (bnfAsts |+> metaExprPart' rules |> SMESeq)
metaExprPart rules (BNFRuleCall name)
	= metaExpr rules name 
				
					
			
metaError	:: Parser u SExpression
metaError	= do	ws
			string "ERROR"
			ws
			Literal val	<- bnfLiteral
			ws
			return $ SError val


