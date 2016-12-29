module AbstractInterpreter.AbstractFunctionInterpreter where

{-
This module executes functions on an abstract representation of the set
-}

import TypeSystem
import Utils.Utils
import Utils.ToString


import Prelude hiding (subtract)
import AbstractInterpreter.AbstractParseTree
import AbstractInterpreter.AbstractPatternMatcher

import Data.List
import Data.Maybe
import Data.Map (Map, (!))


type Arguments	= [AbstractSet']


data Analysis	= Analysis 
		{ results	:: [(Int, (Arguments, AbstractSet'))]	-- clausenr, arguments to value
		, leftOvers	:: [Arguments]			-- Unhandled cases
		}

addResult	:: Int -> [(Arguments, AbstractSet')] -> Analysis -> Analysis
addResult i res' (Analysis res lo)
	= Analysis (zip (repeat i) res' ++ res) lo

instance Show Analysis where
	show (Analysis results lo)	
			= results |> showRes & unlines
				++ "\nNON-MATCHING: "++show lo

showRes	:: (Int, (Arguments, AbstractSet')) -> String
showRes	(i, (args, res))
	= padR 40 ' ' (show i ++ "   "++toParsable' " , " args) ++
		"\t--> "++
		 padR 10 ' ' (toParsable res) ++ " : " ++ show (typeOf res)



interpretFunction'	:: Syntax -> Map Name TypeName -> Function -> Analysis
interpretFunction' s fs f
	= let	args 	= f & typesOf & init |> generateAbstractSet' s "_" in
		interpretFunction s fs f args

interpretFunction	::  Syntax -> Map Name TypeName -> Function -> Arguments -> Analysis
interpretFunction syntax f (MFunction _ clauses) args
	= interpretClauses syntax f (mapi $ init clauses) [args]





interpretClauses	:: Syntax -> Map Name TypeName -> [(Int, Clause)] -> [Arguments] -> Analysis
interpretClauses _ _ [] leftOvers
			= Analysis [] leftOvers
interpretClauses syntax f ((i, clause):clauses) argss
		= let 	results		= [ interpretClause syntax f clause args | args <- argss ] & concat
			usedArgs	= results |> fst	:: [Arguments]
			argss'		= [subtractArgs syntax args usedArgs | args <- argss] & concat
			analysis	= interpretClauses syntax f clauses argss'
			in
			addResult i results analysis

subtractArgs	:: Syntax -> Arguments -> [Arguments] -> [Arguments]
subtractArgs s args []
		= [args]
subtractArgs s args (minus:minuses)
		= do	args'	<- subtractArg s args minus
			subtractArgs s args' minuses

subtractArg	:: Syntax -> Arguments -> Arguments -> [Arguments]
subtractArg s args minus
 | length args /= length minus	= error "Length of arguments in minus don't match; this is weird"
 | otherwise	= zip args minus |> (\(arg, min) -> subtract s [arg] min ) & allCombinations	


interpretClause	:: Syntax -> Map Name TypeName -> Clause -> [AbstractSet'] -> [(Arguments, AbstractSet')]
interpretClause syntax functionReturns (MClause patterns expr) args
 | length args /= length patterns	= error $ "Number of arguments does not match, expected "++show (length patterns)++" arguments but only got "++show (length args)
 | otherwise
	= do	let assgns	= zip patterns args |> uncurry (patternMatch syntax)
					& allCombinations |> mergeAssgnss syntax & concat
		assgn	<- assgns
		let filledPats	= patterns |> evalExpr functionReturns assgn
		let filledExpr	= evalExpr functionReturns assgn expr
		return (filledPats, filledExpr)


evalExpr	:: Map Name TypeName -> Assignments -> Expression -> AbstractSet'
evalExpr _ assgns (MParseTree (MLiteral mi token))
		= ConcreteLiteral mi token
evalExpr _ assgns (MVar _ n)
		= fromMaybe (error $ "Unknown variable: "++show n) (findAssignment n assgns) & fst
evalExpr f _ (MCall _ n _ _)
		= let tp = f ! n in
			EveryPossible (tp, -1) "Function call" tp
evalExpr f assgns (MSeq mi exprs)
		= exprs |> evalExpr f assgns & AsSeq mi
evalExpr f assgns (MAscription t e)
		= let	e'	= evalExpr f assgns e in
			if typeOf e' == t then e' else error "Ascription failed"
evalExpr f assgns (MEvalContext _ nm hole)	
		= let	(ctx, Just path)	= fromMaybe (error $ "Unknwown variable"++show nm) $ findAssignment nm assgns
			hole'			= evalExpr f assgns hole
			in
			replaceAS ctx path hole'
