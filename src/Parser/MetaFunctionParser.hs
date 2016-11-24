module Parser.MetaFunctionParser where

{-
This module parses metafunctions and then passes them through a type checker.
These have a different parse tree, as we can't parse that in one pass 
-}

import Utils
import Parser.ParsingUtils
import Parser.MetaExpressionParser

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


data SFunction = SFunction {sf_name :: Name, sf_type :: MetaType, sf_body :: [SClause]}
	deriving (Show, Ord, Eq)


parseMetaFunctions	:: BNFRules -> Parser u MetaFunctions
parseMetaFunctions bnfs
	= do	nls
		funcs	<- many $ try (parseMetaFunction bnfs)
		typeFunctions bnfs funcs & either error return



typeFunctions	:: BNFRules -> [SFunction] -> Either String MetaFunctions
typeFunctions bnfs funcs
	= inMsg ("Within the environment\n"++ neatFuncs funcs ) $
	  do	let typings	= funcs |> (sf_name &&& sf_type) & M.fromList
		funcs |+> typeFunction bnfs typings |> M.fromList

neatFuncs	:: [SFunction] -> String
neatFuncs funcs	
	= funcs |> (\f -> sf_name f ++ " : " ++ show (sf_type f))
		|> ("    " ++) & unlines

typeFunction	:: BNFRules -> Map Name MetaType -> SFunction -> Either String (Name, MetaFunction)
typeFunction bnfs typings (SFunction nm tp body)
	= inMsg ("While typing the function "++nm) $ 
	  do 	clauses	<- body |+> typeClause bnfs typings tp
		return (nm, MFunction tp (clauses ++ [undefinedClause tp]))

typeClause	:: BNFRules -> Map Name MetaType -> MetaType -> SClause -> Either String MetaClause
typeClause bnfs funcs tp sc@(SClause patterns expr)
	= inMsg ("In clause "++show sc) $
          do	let tps		= flatten tp
		let argTps	= init tps
		let rType	= last tps
		if length argTps /= length patterns then fail "Number of patterns is incorrect, in comparison with the type" else return ()
		patterns'	<- zip argTps patterns |+> uncurry (typeAs' funcs bnfs) 
		expr'		<- typeAs' funcs bnfs rType expr

		patternsDeclares	<- patterns' |+> expectedTyping bnfs 
		patternsDeclare		<- inMsg "While checking for conflicting declarations" $ mergeContexts bnfs patternsDeclares
		exprNeed		<- expectedTyping bnfs expr'
		let unknown		= exprNeed `M.difference` patternsDeclare & M.keys
		if not $ null unknown then fail ("Undeclared variable(s): "++show unknown) else return ()
		inMsg "While checking for conflicting usage" $ mergeContext bnfs patternsDeclare exprNeed
		return $ MClause patterns' expr'



undefinedClause	:: MetaType -> MetaClause
undefinedClause tp
	= let	flat	= flatten tp
		args	= zip flat [0 .. length flat - 2] ||>> show ||>> ("t"++) |> (\(tp, nm) -> MVar (tp, -1) nm) 
		expr	= MCall "" "error" True [MLiteral ("", -1) "Undefined behaviour: no pattern matched"]
		in
		MClause args expr
		






---------------------------- PARSING OF A SINGLE FUNCTION ------------------------------

parseMetaFunction	:: BNFRules -> Parser u SFunction
parseMetaFunction bnfs	
	= do	(nm, tp)	<- metaSignature bnfs
		let tps		= flatten tp
		nls
		clauses		<- many1 $ try (metaClause nm (length tps - 1) <* nls)
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
		

metaClause	:: Name -> Int -> Parser u SClause
metaClause name i
	= (do	ws
		string name
		ws
		string "("
		ws
		args	<- intersperseM (ws >> char ',' >> ws) (replicate i parseMetaExpression)
		ws
		string ")"
		try (ws >> nl >> ws) <|> ws
		string "="
		ws
		expr	<- parseMetaExpression
		return $ SClause args expr)


