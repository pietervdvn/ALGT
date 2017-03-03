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
fixSyntax	:: TypeSystem -> String -> TypeName -> (Name, TypeName) -> Either String (TypeSystem, ParseTree, Changes)
fixSyntax ts dynName typeType (concrFunc, concrRet)
	= do	(bnfs, wsMode, grouped)	<- checkSyntaxExists ts typeType
		let syntaxPatch	= Edit typeType ([Literal dynName], wsMode, grouped)

		let dynIndex		= length bnfs
		-- dynamic type parsetree representation
		let dyn			= MLiteral (typeType, dynIndex) dynName

		(Literal typeListEnd)	<- checkSyntaxExists ts concrRet |> fst3 |> last
		let extraClauseConcr	= MClause [MParseTree dyn] $ MParseTree $ PtSeq (concrRet, 0) [dyn, MLiteral (concrRet, 1) typeListEnd]
		let concrPatch		= Edit concrFunc $ MFunction [typeType, concrRet] [extraClauseConcr]

		let changes	= Changes "Gradualized" [syntaxPatch]
					 [concrPatch] [] []
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



{- Given an abstract set as input, gives possible output as result from concretization; e.g. what is the concretization of "Bool" should give [ConcreteLiteral "Bool], whereas concretization (ConcreteLiteral "?") should give [ConcreteLiteral "type"]. Note that this last behaviour is 'hacked on', as we can't effeciently represent the entire set in the language itself

This is done by simple function evaluation and a bit of hacks

First hack:
- Concretization("?") = "?"; this is done by adding a clause to concretization (done in fixSyntax)
- Second hack: the "list" in ALGT is flattened to a list in haskell
- These concrete types are returned as parsetrees and converted to abstract sets
- Third hack: in the abstract set, every "?" is replaced by the dynset

-}
concretization	:: TypeSystem -> TypeName -> Name -> ParseTree -> [AbstractSet] -> ParseTree -> Either String [AbstractSet]
concretization ts typeType concrFuncName dyn dynSet pt
	= do	(MFunction tp clauses)	<- checkFunctionExists ts concrFuncName

		output	<- PTI.evalFunc ts concrFuncName [pt]
		let types	= flattenList output
		types |> fromParseTree typeType & return



flattenList	:: ParseTree -> [ParseTree]
flattenList (PtSeq _ [tp, rest])
		= tp : flattenList rest
flattenList _	= []

