module AbstractInterpreter.ASSubtract (subtract, subtractWith, subtractAll, subtractAllWith) where

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


_subtract'	:: Syntax -> Map (TypeName, TypeName) TypeName -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract' s known as asMinus
 | as == asMinus	= []
 | otherwise		= error "TODO: subtraction"
