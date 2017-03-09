 {-# LANGUAGE TemplateHaskell #-}
module Graphs.MinDistance (minDistance) where

{-  

Given connections

x	::= y | z
w	::= x

and start values:

y	-> 0
z	-> ?
x	-> ?
w	-> ?

We calculate the minimum distance to a start value (where each hop has a cost one)

-}

import Utils.Utils

import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Set as S
import Graphs.DirectedGraph
import Data.Maybe
import Data.Set
import Data.List (nub)
import qualified Data.List as L

import Control.Monad.State hiding (get)

import Lens.Micro hiding ((&))
import Lens.Micro.TH



data S n	= S 
	{ _initial	:: Map n Int
	, _calculated	:: Map n Int
	, _dg		:: Map n (Set [n])
	, _dgi		:: DG n
	, _queue	:: [n]
	} deriving (Show)
makeLenses ''S

type St	n
	= State (S n) ()


minDistance	:: (Show n, Ord n, Eq n) => Map n (Set [n]) -> Map n (Maybe Int) -> Map n Int
minDistance dg' initialCosts
	= let	initial	= initialCosts & M.toList 
				|> sndEffect & catMaybes & M.fromList
		dg	= dg' |> (\sn -> S.toList sn & concat & S.fromList)  & makeComplete
		dgi	= dg & invert
		calculatd	= initial
		queue	= (M.keys initial |> (dgi !)) >>= S.toList
		initialState	= S initial calculatd dg' dgi queue
		in execState (update' (+1)) initialState
			& get calculated


test	= [("a",[["b","c"]]), ("c", [["a"], ["a","b"]])] & M.fromList |> S.fromList
test'	= [("b", Just 0)] & M.fromList
t	= minDistance test test'



update'	:: (Ord n, Eq n) => (Int -> Int) -> St n
update'	f
	= do	q	<- gets (get queue)
		unless (L.null q) $ do
			let n	= head q
			modify (over queue tail)
			update f n
			update' f

update	:: (Ord n, Eq n) => (Int -> Int) -> n -> St n
update f node
	= do	initialV	<- gets (get initial) |> M.lookup node
		when (isNothing initialV) $ do
		known		<- gets (get calculated)
		let currV	= known & M.lookup node 
		buildsOn	<- gets (get dg) |> (M.! node)
		let newV'	= buildsOn & S.toList
					||>> (`M.lookup` known) 
					|> catMaybes & L.filter (not . L.null)
					|> maximum 
		let newV	= if L.null newV' then Nothing else newV' & minimum & f & Just
		unless (newV == currV) $ do
			-- smaller path found!
			modify (over calculated $ M.insert node $ fromJust newV)
			supports	<- gets (get dgi) |> (M.! node)
			modify (over queue (nub . (++S.toList supports)))



