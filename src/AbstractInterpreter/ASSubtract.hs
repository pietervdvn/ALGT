module AbstractInterpreter.ASSubtract (subtract, subtractWith, subtractAll, subtractAllWith, subtractArg, subtractArgs) where

{- This module defines `subset of` and `subtract` for abstract set, + examples against STFL-}

import Prelude hiding (subtract)
import Utils.Utils

import AbstractInterpreter.AbstractSet

import TypeSystem

import Data.List as L
import Data.Map (Map)
import qualified Data.Map as M

subtract	:: Syntax -> [AbstractSet] -> AbstractSet -> [AbstractSet]
subtract s	= subtractWith s M.empty

subtractWith	:: Syntax -> Map (TypeName, TypeName) TypeName -> [AbstractSet] -> AbstractSet -> [AbstractSet]
subtractWith syntax known ass minus
	= nub $ do	as	<- ass
			_subtract' syntax known as minus

subtractAll	:: Syntax -> [AbstractSet] -> [AbstractSet] -> [AbstractSet]
subtractAll syntax 
		= subtractAllWith syntax M.empty

subtractAllWith	:: Syntax -> Map (TypeName, TypeName) TypeName -> [AbstractSet] -> [AbstractSet] -> [AbstractSet]
subtractAllWith syntax known ass minuses
		= nub $ L.foldl (subtractWith syntax known) ass minuses


subtractArgs	:: Syntax -> Arguments -> [Arguments] -> [Arguments]
subtractArgs s args []
		= [args]
subtractArgs s args (minus:minuses)
		= do	args'	<- subtractArg s args minus
			subtractArgs s args' minuses


subtractArg	:: Syntax -> Arguments -> Arguments -> [Arguments]
subtractArg s args minus
 | length args /= length minus	= error "Length of arguments in minus don't match; this is weird"
 | otherwise	= let	pointWise	= zip args minus |> (\(e, emin) -> subtract s [e] emin)
			in	
			replacePointwise args pointWise


-------------------------------------------------------- Actual subtraction algorithm  ----------------------------------------------------



_subtract'	:: Syntax -> Map (TypeName, TypeName) TypeName -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract' s known as asMinus
 | as == asMinus	= []
 | otherwise		= error "TODO: subtraction"
