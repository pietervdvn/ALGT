module TypeSystem.Parser.FunctionParser where

{-
This module parses metafunctions and then passes them through a type checker.
These have a different parse tree, as we can't parse that in one pass 
-}

import Utils.Utils
import TypeSystem.Parser.ParsingUtils
import TypeSystem.Parser.ExpressionParser

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (intersperse, intercalate)


-- S from Simple, before typing
data SClause = SClause [MEParseTree] MEParseTree
	deriving (Ord, Eq)

instance Show SClause where
	show (SClause pats e)	= (pats & showComma & inParens)++" = "++show e


data SFunction = SFunction {sfName :: Name, sfType :: Type, sfBody :: [SClause]}
	deriving (Show, Ord, Eq)


parseFunctions	:: Maybe (Map Name Type) -> Syntax -> Parser u Functions
parseFunctions typings bnfs
	= do	nls
		funcs	<- many $ try (parseFunction bnfs)
		typeFunctions typings bnfs funcs |> M.fromList
			& either error return




typeFunctions	:: Maybe (Map Name Type) -> Syntax -> [SFunction] -> Either String [(Name, Function)]
typeFunctions alreadyExistingTyping bnfs funcs
	= inMsg ("Within the environment\n"++ neatFuncs funcs ) $
	  do	let typings'	= funcs |> (sfName &&& sfType) & M.fromList	:: Map Name Type
		let typings	= maybe typings' (M.union typings') alreadyExistingTyping
		typedFuncs	<- funcs |+> typeFunction bnfs typings 
		checkNoDuplicates (typedFuncs |> fst) (\dups -> "The function "++showComma dups++" was declared multiple times")
		return typedFuncs

neatFuncs	:: [SFunction] -> String
neatFuncs funcs	
	= funcs |> (\f -> sfName f ++ " : " ++ intercalate " -> " (sfType f))
		|> ("    " ++) & unlines

-- TODO first detect non-existing function names
typeFunction	:: Syntax -> Map Name Type -> SFunction -> Either String (Name, Function)
typeFunction bnfs typings (SFunction nm tp body)
	= inMsg ("While typing the function "++nm) $ 
	  do 	clauses	<- body |+> typeClause bnfs typings tp
		clauses |+> (\cl -> assert Left (equivalents bnfs (typesOf cl) tp) $ "Clause of type "++show (typesOf cl)++" does not match the expected type of "++show tp++"\n"++show cl)
		return (nm, MFunction tp (clauses ++ [undefinedClause tp nm]))

typeClause	:: Syntax -> Map Name Type -> Type -> SClause -> Either String Clause
typeClause bnfs funcs tps sc@(SClause patterns expr)
	= inMsg ("In clause "++show sc) $
          do	let argTps	= init tps
		let rType	= last tps
		assert Left (length argTps == length patterns) $ "Expected "++show (length argTps)++" patterns, but only got "++show (length patterns)
		patterns'	<- zip argTps patterns |+> uncurry (typeAs funcs bnfs) 
		expr'		<- typeAs funcs bnfs rType expr

		patternsDeclares	<- patterns' |+> expectedTyping bnfs 
		patternsDeclare		<- inMsg "While checking for conflicting declarations" $ mergeContexts bnfs patternsDeclares
		exprNeed		<- expectedTyping bnfs expr'
		let unknown		= exprNeed `M.difference` patternsDeclare & M.keys
		assert Left (null unknown) ("Undeclared variable(s): "++show unknown)
		inMsg "While checking for conflicting typings of variables by using them" $ mergeContext bnfs patternsDeclare exprNeed
		return $ MClause patterns' expr'



undefinedClause	:: Type -> Name -> Clause
undefinedClause tp nm
	= let	args	= zip tp [0 .. length tp - 2] ||>> show ||>> ("t"++) |> uncurry MVar
		expr	= MCall "" "error" True [MParseTree $ MLiteral ("", -1) $ "Undefined behaviour: non exhaustive patterns in function "++ nm]
		in
		MClause args expr
		






---------------------------- PARSING OF A SINGLE FUNCTION ------------------------------

parseFunction	:: Syntax -> Parser u SFunction
parseFunction bnfs	
	= do	(nm, tps)	<- metaSignature bnfs
		nls1
		clauses		<- parseClauses nm tps
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
				

metaSignature	:: Syntax -> Parser u (Name, Type)
metaSignature bnfs
	= do	ws
		nm	<- identifier
		ws
		string	":"
		ws
		tp	<- parseType $ bnfNames bnfs
		return (nm, tp)
		

parseClauses nm tps
		= many1 $ try (parseClause nm (length tps - 1) <* nls1)

parseClause	:: Name -> Int -> Parser u SClause
parseClause name i
	= do	ws
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
		return $ SClause args expr


