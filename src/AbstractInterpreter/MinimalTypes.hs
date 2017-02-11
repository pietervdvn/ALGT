module AbstractInterpreter.MinimalTypes where

{-
This module calculates what the minimal typings for the functions are (thus the most strict types they possible return) 
-}

import TypeSystem
import Utils.Utils

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.Assignment
import AbstractInterpreter.FunctionAnalysis

import qualified Data.Map as M
import qualified Data.List as L
import Data.Map (Map, (!), mapWithKey, differenceWith)
import Data.Maybe

-- same as strictestTypes, but only returns the types that can be stricter
stricterTypes	:: TypeSystem -> Functions -> Map Name TypeName
stricterTypes ts f
	= let	orig		= f |> typesOf |> last
		strictest	= strictestTypes ts f
		in
		differenceWith (\orig strictest -> if orig == strictest then Nothing else Just strictest) orig strictest


strictestTypes	:: TypeSystem -> Functions -> Map Name TypeName
strictestTypes ts functions
	= let	startTypes	= functions |> const bottomSymbol
		origTypes	= functions |> typesOf |> last
		in
		searchStrictest ts origTypes functions startTypes


searchStrictest		:: TypeSystem -> Map Name TypeName -> Functions -> Map Name TypeName -> Map Name TypeName
searchStrictest ts backup f state
	= let	state'	= step ts backup f state in
		if state == state' then state else searchStrictest ts backup f state'


step		:: TypeSystem -> Map Name TypeName -> Functions -> Map Name TypeName -> Map Name TypeName
step ts backup functions state
	= let	minimalTypes	= functions |> minimalTypeOf ts state	:: Map Name (Maybe TypeName)
		minimalTypes'	= minimalTypes & mapWithKey (\k v -> fromMaybe (backup ! k) v)
		in
		minimalTypes'


minimalTypeOf	:: TypeSystem -> Map Name TypeName -> Function -> Maybe TypeName
minimalTypeOf ts funcSigns f
	= let	s		= get tsSyntax ts
		args		= typesOf f & init |> generateAbstractSet s "_"
		analysis	= analyzeFunctionWith ts funcSigns f args
		tps		= analysis & get clauseAnalysises
					|> get results
					|> M.elems & concat
					|> typeOf
					& L.nub
		in
		biggestCommonType' s tps


