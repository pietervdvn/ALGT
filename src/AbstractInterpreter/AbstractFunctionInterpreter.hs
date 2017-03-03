module AbstractInterpreter.AbstractFunctionInterpreter where

{- Runs a function on a single abstract set.

	Note that evalFunc uses open recursion, to be able to inject other evalFuncs that have been tampered with


-}

import Utils.Utils

import TypeSystem
import AbstractInterpreter.AbstractSet
import AbstractInterpreter.PatternMatcher
import AbstractInterpreter.Assignment


type EvalFunc		= Function -> [AbstractSet] -> AbstractSet

evalFunc		:: TypeSystem -> (Function -> [AbstractSet] -> AbstractSet) -> Function -> [AbstractSet] -> Either String AbstractSet
evalFunc ts evalFunc' (MFunction _ clauses) args
	= clauses |> evalClause ts evalFunc' args & firstRight


evalClause		:: TypeSystem -> EvalFunc -> Arguments -> Clause -> Either String AbstractSet
evalClause ts evalFunc' args (MClause pats expr)
 | length args /= length pats
	= error "Evalclause in abstract function interpreter: number of args doesn't match"
 | otherwise
	= do	let syntax	= get tsSyntax ts
		-- let vars	= zip pats args |> uncurry (patternMatch syntax) & mergeAssgn syntax
		Left "hi"
		

