module Parser.FunctionParser where

{-
This module parses metafunctions and then passes them through a type checker.
These have a different parse tree, as we can't parse that in one pass 
-}

import Utils
import Parser.ParsingUtils
import Parser.ExpressionParser

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (intersperse)


-- S from Simple, before typing
data SClause = SClause [MEParseTree] MEParseTree
	deriving (Show, Ord, Eq)


data SFunction = SFunction {sf_name :: Name, sf_type :: Type, sf_body :: [SClause]}
	deriving (Show, Ord, Eq)


parseFunctions	:: BNFRules -> Parser u Functions
parseFunctions bnfs
	= do	nls
		funcs	<- many $ try (parseFunction bnfs)
		typeFunctions bnfs funcs & either error return




typeFunctions	:: BNFRules -> [SFunction] -> Either String Functions
typeFunctions bnfs funcs
	= inMsg ("Within the environment\n"++ neatFuncs funcs ) $
	  do	let typings	= funcs |> (sf_name &&& sf_type) & M.fromList
		funcs |+> typeFunction bnfs typings |> M.fromList

neatFuncs	:: [SFunction] -> String
neatFuncs funcs	
	= funcs |> (\f -> sf_name f ++ " : " ++ show (sf_type f))
		|> ("    " ++) & unlines

typeFunction	:: BNFRules -> Map Name Type -> SFunction -> Either String (Name, Function)
typeFunction bnfs typings (SFunction nm tp body)
	= inMsg ("While typing the function "++nm) $ 
	  do 	clauses	<- body |+> typeClause bnfs typings tp
		return (nm, MFunction tp (clauses ++ [undefinedClause tp]))

typeClause	:: BNFRules -> Map Name Type -> Type -> SClause -> Either String Clause
typeClause bnfs funcs tps sc@(SClause patterns expr)
	= inMsg ("In clause "++show sc) $
          do	let argTps	= init tps
		let rType	= last tps
		if length argTps /= length patterns then fail "Number of patterns is incorrect, in comparison with the type" else return ()
		patterns'	<- zip argTps patterns |+> uncurry (typeAs funcs bnfs) 
		expr'		<- typeAs funcs bnfs rType expr

		patternsDeclares	<- patterns' |+> expectedTyping bnfs 
		patternsDeclare		<- inMsg "While checking for conflicting declarations" $ mergeContexts bnfs patternsDeclares
		exprNeed		<- expectedTyping bnfs expr'
		let unknown		= exprNeed `M.difference` patternsDeclare & M.keys
		if not $ null unknown then fail ("Undeclared variable(s): "++show unknown) else return ()
		inMsg "While checking for conflicting usage" $ mergeContext bnfs patternsDeclare exprNeed
		return $ MClause patterns' expr'



undefinedClause	:: Type -> Clause
undefinedClause tp
	= let	args	= zip tp [0 .. length tp - 2] ||>> show ||>> ("t"++) |> (\(tp, nm) -> MVar (tp, -1) nm) 
		expr	= MCall "" "error" True [MLiteral ("", -1) "Undefined behaviour: no pattern matched"]
		in
		MClause args expr
		






---------------------------- PARSING OF A SINGLE FUNCTION ------------------------------

parseFunction	:: BNFRules -> Parser u SFunction
parseFunction bnfs	
	= do	(nm, tps)	<- metaSignature bnfs
		nls
		clauses		<- many1 $ try (parseClause nm (length tps - 1) <* nls)
		return $ SFunction nm tps clauses

parseType	:: [Name] -> Parser u Type
parseType bnfTypes	
	= try (do	t1 <- choose bnfTypes
			ws
			string "->"
			ws
			tr <- parseType bnfTypes
			return (t1:tr))
		<|>	(choose bnfTypes |> (:[]))
				

metaSignature	:: BNFRules -> Parser u (Name, Type)
metaSignature bnfs
	= do	ws
		nm	<- identifier
		ws
		string	":"
		ws
		tp	<- parseType $ bnfNames bnfs
		return (nm, tp)
		

parseClause	:: Name -> Int -> Parser u SClause
parseClause name i
	= (do	ws
		string name
		ws
		string "("
		ws
		args	<- intersperseM (ws >> char ',' >> ws) (replicate i parseExpression)
		ws
		string ")"
		try (ws >> nl >> ws) <|> ws
		string "="
		ws
		expr	<- parseExpression
		return $ SClause args expr)


