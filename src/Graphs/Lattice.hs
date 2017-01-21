{-# LANGUAGE TemplateHaskell #-}
module Graphs.Lattice (Lattice, makeLattice, addElement, addRelation, asSVG, subsetsOf, supersetsOf, allSubsetsOf, allSupersetsOf, infimum, infimums, supremum, supremums, removeTransitive') where

{-
This module defines a finite lattice structure.

It allows easy lookup of direct suprema/infima and a chaining

-}

import Utils.Utils
import Utils.LatticeImage

import Lens.Micro hiding ((&))
import Lens.Micro.TH

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Data.Maybe

import Control.Monad
import Control.Monad.State (State, execState)
import qualified Control.Monad.State as State

import Debug.Trace


data Lattice a	= Lattice
	{ _bottom	:: a
	, _top		:: a
	, _isSubsetOfEvery	:: Map a (Set a)	-- every 'a' is also every element of the corresponding set	
	, _isSupersetOfEvery	:: Map a (Set a)
	} deriving (Show)

makeLenses ''Lattice


makeLattice	:: (Ord a) => a -> a -> Map a (Set a) -> (Lattice a, [(a, a)])
makeLattice bottom top isSubsetOf
	= let	(lattice, unneeded)	= removeTransitive $ Lattice bottom top isSubsetOf (error "Lattices: isSupersetOfEvery used in intialization. This is a bug") 
		lattice'		= set isSupersetOfEvery (invertDict $ get isSubsetOfEvery lattice) lattice
		in
		(lattice', unneeded)

addElement	:: (Ord a) => a -> [a] -> [a] -> Lattice a -> (Lattice a, [(a, a)])
addElement newEl subsOfNew supersOfNew lattice
	= let	cleanse ls symb	= if null ls then [symb] else ls
		subsOfNew'	= cleanse subsOfNew (get bottom lattice)
		supersOfNew'	= cleanse supersOfNew (get top lattice)

		isSubsetOfEvery'= foldr (M.adjust (S.insert newEl)) (get isSubsetOfEvery lattice) subsOfNew'
					& M.insert newEl (S.fromList supersOfNew')
		isSupersetOfEvery'
				= foldr (M.adjust (S.insert newEl)) (get isSupersetOfEvery lattice) supersOfNew'
					& M.insert newEl (S.fromList subsOfNew')
		lattice'	= lattice	& set isSubsetOfEvery isSubsetOfEvery' 
						& set isSupersetOfEvery isSupersetOfEvery'
		in
		removeTransitive lattice'


addRelation	:: (Ord a, Eq a) => a -> a -> Lattice a -> Lattice a
addRelation sub super l
 | sub == super	= l
 | otherwise
	= l	& over isSubsetOfEvery (M.adjust (S.insert super) sub)
		& over isSupersetOfEvery (M.adjust (S.insert sub) super)

{-
 Consider lattice ["bottom" {"a","b"}] ["a", {"top", "b"} ] ["b", {"top"}]
"bottom" can reach "b" via a, so the direct link "bottom" "b" can be removed

Returns removed links
-}
removeTransitive	:: (Ord a) => Lattice a -> (Lattice a, [(a, a)])
removeTransitive lattice
	= foldl removeUnneedDirectLinksFor (lattice, []) (M.keys $ get isSubsetOfEvery lattice)


removeTransitive'	:: (Ord a) => Lattice a -> Lattice a
removeTransitive'	= fst . removeTransitive

removeUnneedDirectLinksFor	:: (Ord a) => (Lattice a, [(a, a)]) -> a -> (Lattice a, [(a, a)])
removeUnneedDirectLinksFor (lattice, removed) a
	= let	-- direct links
		direct		= supersetsOf lattice a
		-- All subsets which can not be reached in one step.
		indirect	= direct & S.toList |> allSupersetsOf lattice & S.unions
		-- In a proper lattice, a related value can be reached in one step or inderectly, but not both at the same time
		-- we take the overlap...
		unneeded	= S.intersection direct indirect
		-- ... and remove it
		subsets'	= foldl (\subsets ch -> M.adjust (S.delete ch) a subsets) (get isSubsetOfEvery lattice) unneeded
		removed'	= zip (S.toList unneeded) (repeat a)
		lattice'	= set isSubsetOfEvery subsets' lattice
		in
		(lattice', removed ++ removed')

		
-- Gives all supersets of a, thus all sets including 'a'
allSupersetsOf	:: (Ord a) => Lattice a -> a -> Set a
allSupersetsOf lattice a
	= let	dirSubs	= supersetsOf lattice a
		subs	= dirSubs & S.toList |> allSupersetsOf lattice & S.unions
		in
		S.union dirSubs subs

-- Gives all subsets of a, thus all (named) sets which are a part of set 'a'
allSubsetsOf	:: (Ord a) => Lattice a -> a -> Set a
allSubsetsOf lattice a
	= let	dirSups	= subsetsOf lattice a
		sups	= dirSups & S.toList |> allSubsetsOf lattice & S.unions
		in
		S.union dirSups sups

-- Direct subsets, << , 'bedekkingsgraad', thus all (named) sets which are a part of set 'a' 
subsetsOf		:: (Ord a) => Lattice a -> a -> Set a
subsetsOf lattice a
	= get isSupersetOfEvery lattice & M.findWithDefault S.empty a

allElems	:: (Ord a) => Lattice a -> [a]
allElems l	= get top l:get bottom l:(get isSubsetOfEvery l & M.keys)


-- Direct supersets, tus all sets of which set 'a' is a part
supersetsOf		:: (Ord a) => Lattice a -> a -> Set a
supersetsOf lattice a
	= get isSubsetOfEvery lattice & M.findWithDefault S.empty a



infimum		:: (Eq a, Ord a, Show a) => Lattice a -> a -> a -> a
infimum l a b
 | a == b				= a
 | a `S.member` allSubsetsOf l b	= a
 | b `S.member` allSubsetsOf l a	= b
 | otherwise	= supremums l $ S.intersection (allSubsetsOf l a) (allSubsetsOf l b)

infimums	:: (Eq a, Ord a, Foldable t, Show a) => Lattice a -> t a -> a
infimums l as
 | length as == 1	= minimum as
 | otherwise	= foldl (infimum l) (get top l) as 


supremum	:: (Eq a, Ord a, Show a) => Lattice a -> a -> a -> a
supremum l a b
 | a == b				= a
 | a `S.member` allSupersetsOf l b	= a
 | b `S.member` allSupersetsOf l a	= b
 | otherwise			= infimums l $ S.intersection (allSupersetsOf l a) (allSupersetsOf l b)

supremums	:: (Eq a, Ord a, Foldable t, Show a) => Lattice a -> t a -> a
supremums l as	
 | length as == 1	= maximum as
 | otherwise = foldl (supremum l) (get top l) as 



------------------- CODE TO SHOW A FANCY SVG ---------------------


asSVG			:: (Ord a, Show a) => (a -> String) -> (a -> Bool) -> Int -> ColorScheme -> Lattice a -> String
asSVG shw doDash pxW cs lattice
	= let	(groups, conn, dashed)	= flatRepresentation lattice doDash
		shwTpl (a, b)		= (shw a, shw b)
		in latticeSVG  pxW cs (groups ||>> shw, conn |> shwTpl, dashed |> shwTpl)
		

-- calculates how many steps might be taken from the top
longestPathToTop	:: (Ord a) => Lattice a -> [(a, Int)]
longestPathToTop l	
		= let	startState	= M.singleton (get top l) (0::Int) in
			execState (untilStable $ stepLongest l) startState & M.toList

untilStable	:: (Eq s) => State s () -> State s ()
untilStable st	= do	st0	<- State.get
			st
			st1	<- State.get
			unless (st0 == st1) $ untilStable st

stepLongest	:: (Ord a) => Lattice a -> State (Map a Int) ()
stepLongest l	= allElems l |+> stepElem l & void
				


stepElem	:: (Ord a) => Lattice a -> a -> State (Map a Int) ()
stepElem l a	= do	let supersOfA	= a & allSupersetsOf l & S.toList
			distances	<- State.get
			let distances'	= supersOfA |> (`M.lookup` distances) & catMaybes
			unless (null distances') (State.modify (M.insert a $ 1 + maximum distances'))



flatRepresentation	:: (Ord a, Show a) => Lattice a -> (a -> Bool) -> ([[a]], [(a, a)], [(a, a)])
flatRepresentation lattice dashedLines
	= let	-- Connections between elements
		relations	= get isSubsetOfEvery lattice |> S.toList & M.toList & unmerge
		(dashed, conn)	= L.partition (\(a, b) -> dashedLines a || dashedLines b) relations
		levels		= longestPathToTop lattice |> swap & merge & L.sortOn fst
		in
		(levels |> snd & reverse, conn, dashed)

nextQueue	:: (Ord a) => Lattice a -> (Set a, [a]) -> (Set a, [a])
nextQueue lattice (alreadySeen, queue)
	= let	queue'	= queue |> (\a -> M.findWithDefault S.empty a (get isSubsetOfEvery lattice & invertDict))
				& S.unions 
				& S.filter (`S.notMember` alreadySeen)
		in
		(S.union alreadySeen queue', S.toList queue')



