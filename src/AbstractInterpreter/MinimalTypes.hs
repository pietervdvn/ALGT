module AbstractInterpreter.MinimalTypes where

{-
This module calculates what the minimal typings for the functions are (thus the most strict types they possible return) 
-}

import TypeSystem
import Utils.Utils

import AbstractInterpreter.AbstractFunctionInterpreter
import AbstractInterpreter.AbstractParseTree

import qualified Data.Map as M
import Data.Map (Map, (!), mapWithKey, differenceWith)
import Data.Maybe


-- same as strictestTypes, but only returns the types that can be stricter
stricterTypes	:: Syntax -> Functions -> Map Name TypeName
stricterTypes s f
	= let	orig		= f |> typesOf |> last
		strictest	= strictestTypes s f
		in
		differenceWith (\orig strictest -> if orig == strictest then Nothing else Just strictest) orig strictest


strictestTypes	:: Syntax -> Functions -> Map Name TypeName
strictestTypes syntax functions
	= let	startTypes	= functions |> const ""
		origTypes	= functions |> typesOf |> last
		in
		searchStrictest syntax origTypes functions startTypes


searchStrictest		:: Syntax -> Map Name TypeName -> Functions -> Map Name TypeName -> Map Name TypeName
searchStrictest s backup f state
	= let	state'	= step s backup f state in
		if state == state' then state else searchStrictest s backup f state'


step		:: Syntax -> Map Name TypeName -> Functions -> Map Name TypeName -> Map Name TypeName
step s backup functions state
	= let	minimalTypes	= functions |> minimalTypeOf s state	:: Map Name (Maybe TypeName)
		minimalTypes'	= minimalTypes & mapWithKey (\k v -> fromMaybe (backup ! k) v)
		in
		minimalTypes'


minimalTypeOf	:: Syntax -> Map Name TypeName -> Function -> Maybe TypeName
minimalTypeOf s funcSigns f
	= let	args		= typesOf f & init |> generateAbstractSet' s "_"
		analysis	= interpretFunction s funcSigns f args
		tps		= analysis & results |> snd |> snd |> typeOf & filter (/= "")
		in
		smallestCommonType' s tps


