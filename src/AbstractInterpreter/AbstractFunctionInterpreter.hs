module AbstractInterpreter.AbstractFunctionInterpreter where

{-
This module executes functions on an abstract representation of the set
-}

import TypeSystem
import Utils.Utils

import Data.List
import Data.Map ((!))

import Prelude hiding (Subtract)
import AbstractInterpreter.AbstractParseTree
import AbstractInterpreter.AbstractPatternMatcher

import Data.Maybe


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
			= results |> (\(i, (args, res))-> padR 40 ' ' (show i ++ "   "++show args) ++ "\t--> "++ padR 10 ' ' (show res) ++ " : " ++ show (typeOf res)) & unlines
				++ "\nNON-MATCHING: "++show lo

interpretFunction	::  Syntax -> Function -> Arguments -> Analysis
interpretFunction syntax (MFunction _ clauses) args
	= interpretClauses syntax (mapi $ init clauses) [args]


interpretClauses	:: Syntax -> [(Int, Clause)] -> [Arguments] -> Analysis
interpretClauses _ [] leftOvers
			= Analysis [] leftOvers
interpretClauses syntax ((i, clause):clauses) argss
		= let 	results		= [ interpretClause syntax clause args | args <- argss ] & concat
			usedArgs	= results >>= fst
			analysis	= interpretClauses syntax clauses argss
			in
			addResult i results analysis
			


interpretClause	:: Syntax -> Clause -> [AbstractSet'] -> [(Arguments, AbstractSet')]
interpretClause syntax (MClause patterns expr) args
 | length args /= length patterns	= error $ "Number of arguments does not match, expected "++show (length patterns)++" arguments but only got "++show (length args)
 | otherwise
	= do	let assgns	= zip patterns args |> uncurry (patternMatch syntax)
					& allCombinations |> mergeAssgnss syntax & concat
		assgn	<- assgns
		let filledPats	= patterns |> evalExpr assgn
		let filledExpr	= evalExpr assgn expr
		return (filledPats, filledExpr)


evalExpr	:: Assignments -> Expression -> AbstractSet'
evalExpr assgns (MParseTree (MLiteral mi token))
		= ConcreteLiteral mi token
evalExpr assgns (MVar _ n)
		= fromMaybe (error $ "Unknown variable: "++show n) (findAssignment n assgns) & fst
evalExpr _ (MCall tp _ _ _)
		= EveryPossible (tp, -1) "Function call" tp
evalExpr assgns (MSeq mi exprs)
		= exprs |> evalExpr assgns & AsSeq mi
evalExpr assgns (MAscription t e)
		= let	e'	= evalExpr assgns e in
			if typeOf e' == t then e' else error "Ascription failed"
evalExpr assgns (MEvalContext _ nm hole)	
		= let	(ctx, Just path)	= fromMaybe (error $ "Unknwown variable"++show nm) $ findAssignment nm assgns
			hole'			= evalExpr assgns hole
			in
			replaceAS ctx path hole'
