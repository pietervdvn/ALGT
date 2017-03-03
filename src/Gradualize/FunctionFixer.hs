module Gradualize.FunctionFixer where

{-  Tries to figure out what f(?) should be -}

import Utils.Utils

import Changer.Changes

import AbstractInterpreter.AbstractSet

import TypeSystem

import AbstractInterpreter.AbstractFunctionInterpreter
import ParseTreeInterpreter.FunctionInterpreter as PTI

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
gradualizeFunc ts dynSet dyn concrN absN funcN
	= do	concr	<- checkFunctionExists ts concrN
		abs	<- checkFunctionExists ts absN
		func	<- checkFunctionExists ts funcN
		error "hi"



{- Given an abstract set as input, gives possible output as result from concretization; -}
concretization	:: (ParseTree, [AbstractSet]) -> ParseTree -> [AbstractSet]
concretization (dyn, dynSet) pt
	= do	let dynAS	= dyn & fromParseTree "dyn"
		let baseAS	= pt & fromParseTree "base"
		let dynPaths	= baseAS & searchPathAS (== dynAS)
		foldl (\ass p -> [ replaceAS as p option | option <- dynSet, as <- ass ]) [baseAS] dynPaths		
