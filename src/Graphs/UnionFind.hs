module Graphs.UnionFind where

{--
This module implements the union find algo
--}
import State
import Data.Map
import Prelude hiding (lookup)

{-
Calculates out the union find algorithm.
The lowest numbered node will always be the representative of the union.

-}
unionFind	:: (Ord n) => [(n, n)] -> Map n n
unionFind tuples
	= snd $ runstate (mapM_ add tuples) empty

{- the map is a map of pointers. A name will always try to point to its representative, being the smallest element of the set.

State invariant: if the name appears as value in the map, it will also appear as key.
-}
add	:: (Ord n) => (n, n) -> State (Map n n) ()
add (n1,n2)	= do	repres1	<- representative n1
			repres2	<- representative n2
			let r	= min repres1 repres2
			modify $ insert n1 r
			modify $ insert n2 r
			modify $ insert repres1 r
			modify $ insert repres2 r


{-
Searches the representative for the given name.
When it is found, the path gets collapsed.
If it is not found, n is returned (and not inserted)
-}
representative	:: (Ord n) => n -> State (Map n n) n
representative nm
		= do 	dict	<- get
			let nmv	= lookup nm dict
			case nmv of
				(Just found)	-> _collapse nm found
				(Nothing)	-> return nm

-- Collapses the path.
_collapse	:: (Ord n) => n -> n -> State (Map n n) n
_collapse nm found
	| nm == found	= return nm
	| otherwise	= do	repres	<- representative found
				modify	$ insert found repres
				return repres
