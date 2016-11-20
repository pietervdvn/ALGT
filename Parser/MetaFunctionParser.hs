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


-- S from Simple, before typing
data SClause = SClause [SExpression] SExpression
	deriving (Show, Ord, Eq)
data SExpression = SVar Name | SMEliteral String | SMESeq [SExpression] | SMECall Name [SExpression]
	deriving (Show, Ord, Eq)

data SFunction = SFunction Name MetaType [SClause]
	deriving (Show, Ord, Eq)


parseMetaFunctions	:: BNFRules -> Parser u MetaFunctions
parseMetaFunctions bnfs
	= do	funcs	<- many (nls >> parseMetaFunction bnfs)
		error $ show funcs
		return M.empty

parseMetaFunction	:: BNFRules -> Parser u SFunction
parseMetaFunction bnfs	
	= do	(nm, tp)	<- metaSignature bnfs
		let tps		= flatten tp
		clauses		<- many1 (nls >> metaClause bnfs nm (init tps) (last tps))
		return $ SFunction nm tp []

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
metaClause bnfs name (argTp:argTps) returnTp
	= do	ws
		string name
		ws
		string "("
		ws
		arg	<- metaExpr bnfs argTp	-- TODO multiple
		ws
		string ")"
		ws
		string "="
		ws
		expr	<- metaExpr bnfs returnTp
		return $ SClause [arg] expr


--- TODO PICKUP
metaExpr	:: BNFRules -> MetaTypeName -> Parser u SExpression
metaExpr bnfs rule
	= do	let errMsg		= "MetaType (= BNF-rule) "++rule++" not known"
		guidances	<- M.lookup rule bnfs & maybe (fail errMsg) return
		let prsrs	= guidances |> metaExpr' bnfs
		first (prsrs ++ [metaVar])


metaVar	= identifier |> SVar

metaExpr'	:: BNFRules -> BNFAST -> Parser u SExpression
metaExpr' _ (Literal string)
		= do	Literal val	<- bnfLiteral
			if (val /= string) then
				fail $ "Expected a literal "++string
			else	
				return $ SMEliteral string
metaExpr' rules (Seq bnfAsts)
		= bnfAsts |+> metaExpr' rules |> SMESeq
metaExpr' rules (BNFRuleCall name)
		= metaExpr rules name
metaExpr' rules pat	= error $ show pat
				
				
			



