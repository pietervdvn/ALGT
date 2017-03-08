{-# LANGUAGE FlexibleContexts, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module AbstractInterpreter.FunctionAnalysis where

{-
This module executes functions on an abstract representation of the set
-}

import TypeSystem
import Utils.Utils
import Utils.ToString


import Prelude hiding (subtract)
import AbstractInterpreter.Assignment
import AbstractInterpreter.AbstractSet
import AbstractInterpreter.ASSubtract

import AbstractInterpreter.PatternMatcher


import Data.List
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Lens.Micro hiding ((&))
import Lens.Micro.TH

data ClauseAnalysis
		= ClauseAnalysis
		{ _clauseIndex	:: Int
		, _hasEquality	:: Bool			-- Some inputs might have the right form, but might not match due to two parts not being equal
		, _hasFunction	:: Bool			-- Some inputs might not match as the function call doesn't equal the argument
		, _inputs	:: Set Arguments	-- Possible inputs at this stage. Not every input might have a result!
		, _results	:: Map Arguments AbstractSet	-- arguments --> value
		} deriving (Show, Eq)

data FunctionAnalysis	
		= FunctionAnalysis 
		{ _clauseAnalysises	:: [ClauseAnalysis]
		, _functionLeftOvers	:: Set Arguments
		} deriving (Show, Eq)

makeLenses ''ClauseAnalysis
makeLenses ''FunctionAnalysis

_genFuncSign	:: TypeSystem -> Map Name TypeName
_genFuncSign ts	= get tsFunctions ts |> typesOf |> last


assumptionsFor	:: TypeSystem -> a -> b -> Maybe AbstractSet
assumptionsFor ts _ _	= Nothing

analyzeFunction'	:: TypeSystem -> Function -> FunctionAnalysis
analyzeFunction' ts f
	= analyzeFunctionWith' ts (_genFuncSign ts, assumptionsFor ts) f

analyzeFunctionWith'	:: TypeSystem -> (Map Name TypeName, Name -> Arguments -> Maybe AbstractSet) -> Function -> FunctionAnalysis
analyzeFunctionWith' ts fs f
	= let	syntax	= get tsSyntax ts
		args	= generateArgs syntax (f & typesOf & init) in
		analyzeFunctionWith ts fs f args


analyzeFunction	::  TypeSystem -> Function -> Arguments -> FunctionAnalysis
analyzeFunction ts
	= analyzeFunctionWith ts (_genFuncSign ts, assumptionsFor ts)



{-
(f: when you want to 'lie' about the function types
moreKnowledge: sometimes, you can make assumptions about what a function might return. That is expressed here
-}
analyzeFunctionWith	::  TypeSystem -> (Map Name TypeName, Name -> Arguments -> Maybe AbstractSet) -> Function -> Arguments -> FunctionAnalysis
analyzeFunctionWith ts (f, extraKnowledge) (MFunction _ clauses) args
	= analyzeClauses (get tsSyntax ts) f (mapi clauses) [args]





analyzeClauses	:: Syntax -> Map Name TypeName -> [(Int, Clause)] -> [Arguments] -> FunctionAnalysis
analyzeClauses _ _ [] leftOvers
			= FunctionAnalysis [] $ S.fromList leftOvers
analyzeClauses syntax f ((i, clause):clauses) argss
		= let 	clauseAn	= analyzeClause syntax f (i, clause) argss
			usedArgs	= get results clauseAn & M.keys
			-- checks if the patterns don't use something as 'T -> T', thus a part of the argument that should be the same
			-- this implies that the pattern might not match in some cases and falls through
			argss'		= if get hasEquality clauseAn || get hasFunction clauseAn then argss
						else argss >>= (\args -> subtractArgs syntax args usedArgs)
			restAnalysis	= analyzeClauses syntax f clauses argss'
			in
			restAnalysis & over clauseAnalysises (clauseAn:)



analyzeClause	:: Syntax -> Map Name TypeName -> (Int, Clause) -> [Arguments] -> ClauseAnalysis
analyzeClause  syntax functionReturns (i, clause) possibleInputs
	= let	usesEquality	= (mecPatterns clause >>= usedVariables) |> fst & dubbles & null & not
		usesFunctions	= (mecPatterns clause >>= usedFunctions) & null & not
		results		= possibleInputs |> analyzeClauseWith syntax functionReturns (i, clause) & M.unions
		in
		ClauseAnalysis i usesEquality usesFunctions (S.fromList possibleInputs) results

analyzeClauseWith	:: Syntax -> Map Name TypeName -> (Int, Clause) -> [AbstractSet] -> Map Arguments AbstractSet
analyzeClauseWith syntax functionReturns (i, MClause patterns expr) args
 | length args /= length patterns	
	= error $ "Number of arguments does not match, expected "++show (length patterns)++" arguments but only got "++show (length args)
 | otherwise
	= M.fromList $ 
	  do	assgn	<- zip patterns args |> uncurry (patternMatch syntax)
					& allCombinations |> mergeAssgnss syntax & concat
		let filledPats	= patterns |> evalExpr functionReturns assgn & allRight & either error id	:: Arguments
		let filledExpr	= evalExpr functionReturns assgn expr & either error id	:: AbstractSet
		return (filledPats, filledExpr)


----------------------------- UTILS --------------------------------------------------


instance ToString' (Name, Int, Function) FunctionAnalysis where
	toParsable' 	= _toStringFunctionAnalysis toParsable' toParsable'
	toCoParsable' 	= _toStringFunctionAnalysis toCoParsable' toCoParsable'
	debug' 		= _toStringFunctionAnalysis debug' debug'
	show'	 	= const show


_toStringFunctionAnalysis cats argsts (funcName, width, MFunction t clauses) (FunctionAnalysis analysises fallthroughs)	
		= inHeader "" ("Analysis of "++funcName++" : "++intercalate " -> " t) '=' $ indent $ unlines
			[zip clauses analysises |> (\(c, ca) -> cats (funcName, width, c) ca) & unlines & indent
			, inHeader "" "Falthrough" '-' $
				if S.null fallthroughs then "No fallthrough is possible"
					else fallthroughs & S.toList |> argsts ", " |> inParens & unlines]


instance ToString' (Name, Int, Clause) ClauseAnalysis where
	toParsable' 	= _toStringClauseAnalysis toParsable' toParsable'
	toCoParsable'	= _toStringClauseAnalysis toCoParsable' toCoParsable'
	debug'		= _toStringClauseAnalysis debug' debug'
	show'		= const show
	

_toStringClauseAnalysis cts argsts (fn, w, clause) (ClauseAnalysis clauseI equality functions inputs results)
	= inHeader "" ("Analysis of clause "++show clauseI) '.' $ unlines
		[ "Clause: "
		, cts (fn, w) clause & indent
		, ""
		, "Possible inputs at this point: "
		, inputs & S.toList |> argsts ", " |> inParens |> indent |> ("#"++) & unlines
		, ""
		, "Possible results: "
		, results & M.toList |> showRes clauseI & unlines
		, if equality || functions then
			"This clause uses "++ (if functions then "equality and functions" else "equality")
			++ " in the patterns and might not match. No arguments are thus used in this abstract interpretation.\n"
			else ""
		]


showRes	:: Int -> (Arguments, AbstractSet) -> String
showRes	i (args, res)
	= padR 30 ' ' (show i ++ "   "++toCoParsable' " , " args) ++
		"\t--> "++
		 padR 15 ' ' (toCoParsable res) ++ " : " ++ show (typeOf res)




