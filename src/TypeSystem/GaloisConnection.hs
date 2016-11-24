module TypeSystem.GaloisConnection where

import Data.Set as Set

data Set' t	= Set' (Set t) | Universe


fromList'	:: (Ord a) => [a] -> Set' a
fromList'	= Set' . fromList

data GradualType t
	= StaticType t | UnkownT

instance (Show t) => Show (GradualType t) where
	show (StaticType st)	= show st
	show UnkownT	= "?"

class GaloisConnection t where
	concretization	:: GradualType t -> Set' t
	abstraction	:: Set' t -> Maybe (GradualType t)
