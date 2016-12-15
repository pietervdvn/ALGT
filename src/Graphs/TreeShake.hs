module Graphs.TreeShake where

{--
This module implements treeshake.
--}

import StdDef
import Graphs.DirectedGraph
import Data.Set as S
import Data.Map as M

{-
treeshake removes all nodes from the graph that can not be reached from the given startpoints
-}
treeshake	:: (Ord n) => Set n -> DirectedGraph n -> DirectedGraph n
treeshake starts dg
	= let	reachable	= growUntil dg S.empty starts in
		dg & M.filterWithKey (\k v -> k `S.member` reachable)

-- grows the set to the maximum
growUntil	:: (Ord n) => DirectedGraph n -> Set n -> Set n -> Set n
growUntil dg known start
		= let 	known'		= S.union known start
			nextStep	= grow dg known start in
			if S.null nextStep then known'
				else growUntil dg known' nextStep

-- Gives a set of all nodes that can be reached from the given set (excluding the set itself). Gives back none visited nodes
grow			:: (Ord n) => DirectedGraph n -> Set n -> Set n -> Set n
grow dg	known starts	=  starts & S.toList |> (\n -> findWithDefault S.empty n dg) & S.unions & (`S.difference` starts) & (`S.difference` known)
