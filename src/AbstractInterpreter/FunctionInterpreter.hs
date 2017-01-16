{-# LANGUAGE FlexibleContexts #-}
module AbstractInterpreter.FunctionInterpreter where

{-
This module executes functions on an abstract representation of the set
-}

import TypeSystem
import Utils.Utils
import Utils.ToString


import Prelude hiding (subtract)
import AbstractInterpreter.Data
import AbstractInterpreter.AbstractSet
import AbstractInterpreter.PatternMatcher


import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M



interpretFunction'	:: Syntax -> Map Name TypeName -> Function -> Analysis
interpretFunction' s fs f
	= let	args	= generateArgs s (f & typesOf & init) in
		interpretFunction s fs f args

interpretFunction	::  Syntax -> Map Name TypeName -> Function -> Arguments -> Analysis
interpretFunction syntax f (MFunction _ clauses) args
	= interpretClauses syntax f (mapi $ init clauses) [args]





interpretClauses	:: Syntax -> Map Name TypeName -> [(Int, Clause)] -> [Arguments] -> Analysis
interpretClauses _ _ [] leftOvers
			= addLeftover (-1) False leftOvers emptyAnalysis
interpretClauses syntax f ((i, clause):clauses) argss
		= let 	results		= [ interpretClause syntax f clause args | args <- argss ] & concat
			usedArgs	= results |> fst
			-- checks if the patterns don't use something as 'T -> T', thus a part of the argument that should be the same
			-- this implies that the pattern might not match in some cases and falls through
			noEquality	= (mecPatterns clause >>= usedVariables) & dubbles & null
			argss'		= if noEquality then [subtractArgs syntax args usedArgs | args <- argss] & concat
						else argss
			analysis	= interpretClauses syntax f clauses argss'
			in
			addLeftover i noEquality argss $ addResult i results analysis


interpretClause	:: Syntax -> Map Name TypeName -> Clause -> [AbstractSet] -> [(Arguments, AbstractSet)]
interpretClause syntax functionReturns (MClause patterns expr) args
 | length args /= length patterns	= error $ "Number of arguments does not match, expected "++show (length patterns)++" arguments but only got "++show (length args)
 | otherwise
	= do	let assgns	= zip patterns args |> uncurry (patternMatch syntax)
					& allCombinations |> mergeAssgnss syntax & concat
		assgn	<- assgns
		let filledPats	= patterns |> evalExpr functionReturns assgn
		let filledExpr	= evalExpr functionReturns assgn expr
		return (filledPats, filledExpr)


