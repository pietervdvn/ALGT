module TypeSystem.MetaFunctionLift where

{-
This module rewrites metafunctions to collecting and gradual counterparts
-}

import TypeSystem.GaloisConnection

import Data.Set as Set

collectingPredLifting	:: GaloisConnection t => (t -> t -> Bool) -> (Set t -> Set t -> Bool)
collectingPredLifting f t1s t2s
	= or [f t1 t2 | t1 <- toList t1s, t2 <- toList t2s]

gradualPredLifting	:: GaloisConnection t => (t -> t -> Bool) -> (t -> t -> Bool)
gradualPredLifting f gt1 gt2
	= collectingPredLifting f (concretization gt1) (concretization gt2)
