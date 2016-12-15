module Graphs.DirectedGraph (DG,DirectedGraph, invert, addLink, nodesFrom, leafs, dropNodes, isEmpty, empty, addLinks, fromLinks, addNode, addNodes, makeComplete) where

import StdDef ((||>>),(|>))

import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Tuple (swap)



type DirectedGraph n	= Map n (Set n)
type DG n	= DirectedGraph n

type Inverted n	= DG n

-- Inverts a directed graph
invert	:: (Ord n, Eq n) => DG n -> DG n
invert 	= fmap S.fromList . M.fromList . merge . fmap swap . unmerge . M.toList . fmap S.toList


addLink	:: (Ord n, Eq n) => (n,n) -> DG n -> DG n
addLink (from, to) graph
	= let newLinks	= S.insert to $ nodesFrom from graph in
		M.insert from newLinks graph


addLinks	:: (Ord n, Eq n) => [(n,n)] -> DG n -> DG n
addLinks links graph
	= L.foldr addLink graph links

fromLinks	:: (Ord n, Eq n) => [(n,n)] -> DG n
fromLinks links	= addLinks links M.empty

addNode	:: (Ord n, Eq n) => n -> DG n -> DG n
addNode n
	= M.insert n S.empty

addNodes	:: (Ord n, Eq n) => [n] -> DG n -> DG n
addNodes ns graph
	= L.foldr addNode graph ns

-- Adds all nodes that occur. E.g. {a --> {b}} -> {a --> {b}, b --> {}}
makeComplete	:: (Ord n, Eq n) => DG n -> DG n
makeComplete graph
	= let 	known	= S.toList $ S.unions $ M.elems graph
		knownNodes	= M.keys graph
		toAdd	= known L.\\ knownNodes in
		addNodes toAdd graph

-- Nodes reachable from n with one hop
nodesFrom	:: (Ord n, Eq n) => n -> DG n -> Set n
nodesFrom	= findWithDefault S.empty


-- Gives all the leafs of the graph (nodes with no outgoing links)
leafs	:: (Ord n, Eq n) => DG n -> Set n
leafs graph
	= S.fromList . fmap fst . L.filter (S.null . snd) $ M.toList graph

dropNodes	:: (Ord n, Eq n) => Set n -> DG n -> DG n
dropNodes ns graph
	= S.foldr M.delete graph ns |> S.filter (`S.notMember` ns)

merge		:: Eq a => [(a,b)] -> [(a,[b])]
merge []	= []
merge ((a,b):ls)
		= let bs	= L.filter ((==) a . fst) ls |> snd in
			(a,b:bs): merge (L.filter ((/=) a . fst) ls)

unmerge		:: [(a,[b])] -> [(a,b)]
unmerge 	=  concatMap (\(a,bs) -> [(a,b) | b <- bs])

isEmpty	:: DG n -> Bool
isEmpty	= M.null

empty	= M.empty
