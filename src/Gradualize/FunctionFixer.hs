module Gradualize.FunctionFixer where

{-  Tries to figure out what f(?) should be -}

import Utils.Utils

import TypeSystem

import AbstractInterpreter.AbstractFunctionInterpreter

{-

Gradualizes a function

- The typesystem it lives in
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




