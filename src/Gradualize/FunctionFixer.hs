module Gradualize.FunctionFixer where

{-  Tries to figure out what f(?) should be -}

import Utils.Utils
import Utils.ToString

import Changer.Changes

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.FunctionAnalysis

import TypeSystem

import qualified Data.Map as M
import Data.List as L

import AbstractInterpreter.AbstractFunctionInterpreter
import ParseTreeInterpreter.FunctionInterpreter as PTI

import Debug.Trace

{-
Introduces a dynamic type "?" into the typeRule at syntax; extends the concretization function with "concretization("?") = "?"
-}
fixSyntax	:: TypeSystem -> String -> TypeName -> Either String (TypeSystem, ParseTree, Changes)
fixSyntax ts dynName typeType
	= do	(bnfs, wsMode, grouped)	<- checkSyntaxExists ts typeType
		let syntaxPatch	= Edit typeType ([Literal dynName], wsMode, grouped)

		let dynIndex		= length bnfs
		-- dynamic type parsetree representation
		let dyn			= MLiteral (typeType, dynIndex) dynName

		
		let changes	= Changes "Gradualized" [syntaxPatch]
					 [] [] []
		ts'	<- applyChanges changes ts
		return (ts', dyn, changes)


{-

Gradualizes a function

- The typesystem it lives in; assumed to be patched with "?" existing
- The set (!) of abstract values that "concretization(?)" should give; this will often be the entire type set
- Name of the concretization function
- Name of the abstraction function;
- Name of the function that should be gradualized




-}

gradualizeFunc	:: TypeSystem -> TypeName -> String -> Name -> Name -> Name -> Either String Clause
gradualizeFunc ts dynSet dyn  absN funcN
	= do	error "hi"



possibleResults	:: TypeSystem -> Name -> Arguments -> Either String [AbstractSet]
possibleResults ts funcN args
	= do	function	<- checkFunctionExists ts funcN
		let fanalysis	= analyzeFunction ts function args
		let analysises	= fanalysis & get clauseAnalysises |> get results
		let results	= analysises >>= M.elems 
		--analyzeFunction	::  TypeSystem -> Function -> Arguments -> FunctionAnalysis
		trace ("INPUT: "++toCoParsable' ", " args ++ toParsable' (funcN, 24::Int, function) fanalysis) $ 
			return $ nub results


{- Given an abstract set as input, gives possible output as result from concretization; -}
concretization	:: (ParseTree, Int -> [AbstractSet]) -> ParseTree -> [AbstractSet]
concretization (dyn, dynSet) pt
	= do	let dynAS	= dyn & fromParseTree "dyn"
		let baseAS	= pt & fromParseTree "base"
		let dynPaths	= baseAS & searchPathAS (== dynAS)
		foldl (\ass (i, p) -> [ replaceAS as p option | option <- dynSet i, as <- ass ]) [baseAS] (mapi dynPaths)
	
