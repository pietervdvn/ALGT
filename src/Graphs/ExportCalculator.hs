module Graphs.ExportCalculator where

{--
This module implements calculateExports and calculateImports
--}
import StdDef ((|>), (||>>))
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (lookup)
import Data.Map (Map, lookup, insert, findWithDefault, mapWithKey)
import Data.Set (Set, unions, union)
import Data.Maybe
import State
import Data.Tuple
import Control.Arrow
import Control.Monad

import Graphs.DirectedGraph


{- calculates what properties each node exports, even by reexporting.
Params:
- import graph: node n imports all things from the given nodes
- export graph: inverse relation, node n is imported by given set
- local exports: function which gives, for a node, what properties it exports. These are not passed through the secondary filter
- filter: for a given property, does this node re-export this property?
	current node -> (property, imported out of node) -> current node -> current node re-exports?
Returns:
- What properties each node exports + via which node it got imported (this is **not** ~~the home node~~ where prop originally came from).
-}
calculateExports	:: (Eq prop, Ord n, Ord prop) => Map n (Set n) -> Map n (Set n) -> (n -> Set prop) -> (n -> (n,prop) -> Bool) -> Map n (Set (prop,n))
calculateExports importGraph exportGraph exps impFilt
			= exported $ snd $ runstate _ce $ ExpS importGraph exportGraph exps impFilt M.empty (S.fromList $ M.keys importGraph)

{- calculateImports gives all things which are visible within n (thus local -private- props and imported props)

-}
calculateImports	:: (Eq prop, Ord n, Ord prop) => Map n (Set n) -> (n -> Set prop) -> Map n (Set (prop,n)) -> Map n (Set (prop,n))
calculateImports importGraph local exports
			= let	local' n	= S.map (\p -> (p,n)) $ local n	in
				mapWithKey (\n impsFrom -> calculateImportsFor impsFrom local' exports n) importGraph

calculateImportsFor	:: (Eq prop, Ord n, Ord prop) => Set n -> (n -> Set prop) -> Map n (Set prop) -> n -> Set prop
calculateImportsFor importsFrom local exports n
			=  let	imported	= S.toList importsFrom |> (`lookup'` exports)
				localDecls	= local n in
				union localDecls $ unions imported




data ExpS n prop
		= ExpS 	{ importGraph :: Map n (Set n)	-- node --> imports from
			, exportGraph :: Map n (Set n)  -- node --> exports to
			, exps :: n -> Set prop		-- base exports
			, expFilter :: n -> (n,prop) -> Bool	-- (property, imported via n) -> current n -> current n re-exports prop?
			, exported :: Map n (Set (prop,n))	-- currently, node n --> exports these props. the extra n is the path where it got imported from.
			, worklist :: Set n}

type St n prop a	= State (ExpS n prop) a

_ce		:: (Eq prop, Ord n, Ord prop) => St n prop ()
_ce		=  do 	done	<- fmap not stillWork
			unless done $
			    do	(Just n)	<- pop
				possiblyChanged	<- rework n
				mapM_ push $ S.toList possiblyChanged
				_ce

-- reworks node n. Recalculates what node n would export. If the exports of node n have changed -> update it -> say which nodes should be reworked later
rework		:: (Ord n, Eq prop, Ord prop) => n -> St n prop (Set n)
rework n	=  do	-- The nodes which n imports
			let injectSet n	= S.map (\p -> (p,n))
			let changeSet n	= S.map(\(p,_) -> (p,n))
			imps	<- get' importGraph |> lookup' n
			-- what each node exports for the moment : Map n (Set prop)
			curExps	<- get' exported
			-- what each node exports for the moment and is visible in n	:: Set (n, Set prop)
			let impProps	= S.map (\n -> (n, lookup' n curExps)) imps
			let impProps'	= setUnions $ S.map (uncurry changeSet) impProps	-- :: Set  (prop, n)
			-- filter function. Decides wether a imported property passes or not
			filtr		<- get' expFilter |> (\filtr  -> filtr n .swap)	-- :: (n,prop) -> Bool
			-- All currently reexported stuff!
			let reexported	= S.filter filtr impProps'	-- :: Set (prop,n)
			let reexported'	= S.map fst reexported		-- properties only, used for 'are we done' comparison
			localExported	<- get' exps |> (\f -> f n)		-- Set prop
			addExports n $ union reexported $ injectSet n localExported	-- add all the stuff!
			let newExports	= localExported `union` reexported'
			let oldExports	= S.map fst $ lookup' n curExps
			if newExports == oldExports then return S.empty	-- no changes! hooray
				else get' exportGraph |> nodesFrom n	-- Some new properties are reexported. We have to rework these nodes later

pop	:: Ord n => St n prop (Maybe n)
pop	=  do	todo	<- get' worklist
		if S.null todo then return Nothing
			else do	let n	= S.findMin todo
				modify $ modWorkList $ S.delete n
				return $ Just n

push	:: Ord n => n -> St n prop ()
push n	=  modify $ modWorkList (S.insert n)

-- False: we're done!
stillWork	:: St n prop Bool
stillWork	=  do	todo	<- get' worklist
			return $ not $ S.null todo

setWorkList	:: Set n -> ExpS n prop -> ExpS n prop
setWorkList wl (ExpS importGraph expGraph exps reExpFilter exported _)
		=  ExpS importGraph expGraph exps reExpFilter exported wl

modWorkList	:: (Set n -> Set n) -> ExpS n prop -> ExpS n prop
modWorkList f s	=  setWorkList (f $ worklist s) s

setExported	:: Map n (Set (prop,n)) -> ExpS n prop -> ExpS n prop
setExported exported (ExpS importGraph expGraph exps reExpFilter _ wl)
		=  ExpS importGraph expGraph exps reExpFilter exported wl

getf		:: (s -> a -> b) -> a -> State s b
getf sf	a	=  do	f	<- get' sf
			return $ f a

addExports	:: (Ord n, Ord prop) => n -> Set (prop,n) -> St n prop ()
addExports n s
		= do 	dict		<- get' exported
			let merged 	= S.foldl (\acc pn -> M.insert n (S.insert pn $ lookup' n acc) acc) dict s
			modify $ setExported merged

setUnions	:: (Ord a) => Set (Set a) -> Set a
setUnions	=  S.foldr S.union S.empty

(??)		:: Maybe (Set a) -> Set a
(??)		=  fromMaybe S.empty

lookup'		:: (Ord k) => k -> Map k (Set v) -> Set v
lookup' k dict	=  (??) $ lookup k dict
