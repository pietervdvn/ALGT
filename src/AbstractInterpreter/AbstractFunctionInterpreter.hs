module AbstractInterpreter.AbstractFunctionInterpreter where

{-
This module executes functions on an abstract representation of the set
-}

import TypeSystem
import Utils.Utils

import Data.List
import Data.Map ((!))

import AbstractInterpreter.AbstractParseTree
import AbstractInterpreter.AbstractPatternMatcher

import Data.Maybe

interpretClause	:: Syntax -> Clause -> [AbstractSet'] -> [([AbstractSet'], AbstractSet')]
interpretClause syntax (MClause patterns expr) args
 | length args /= length patterns	= error $ "Number of arguments does not match, expected "++show (length patterns)++" arguments but only got "++show (length args)
 | otherwise
	= do	let assgns	= zip patterns args |> uncurry (patternMatch syntax)
					& allCombinations |> mergeAssgnss syntax & concat
		-- assgns |> fst3 |> (! "e") |> show & unlines & error 
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
		= EveryPossible ("", -1) "Function call" tp
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
