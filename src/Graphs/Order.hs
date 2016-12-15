module Graphs.Order (buildOrdering) where

import Graphs.DirectedGraph
import Graphs.SearchCycles

import qualified Data.Set as S
import qualified Data.Map as M

import Control.Applicative
import Control.Arrow (first)


{-

Given a "bigger then"-relationship, builds a absolute ordering.

e.g.
{a --> {b,c}, b --> {c}, c --> {}}
This means that a > b, a > c, b > c.
You will get a list [a, b, c].

Note that {a --> {}, b --> {}} has no specific ordering, thus a random list might be returned.

Some inputs are unsolvable because of loops, e.g. {a --> {b}, b --> {a}}.
In that case, Left [a,b] is returned: a path that causes a loop.
Loops are searched in advanced and are all returned.

Ambigueties are returned. {a --> {b}, c } means we can both choose quite a lot of combinations.
Some of the choices are returned

-}
buildOrdering	:: (Ord n, Eq n) => DG n -> Either [[n]] ([n], [[n]])
buildOrdering graph
	= let	cycles	= cleanCycles graph in
		if null cycles then Right $ _bo $ makeComplete graph
			else Left cycles

-- build ordering. Ambigueties are given
_bo		:: (Ord n, Eq n) => DG n -> ([n], [[n]])
_bo graph
 | isEmpty graph	= ([],[])
 | otherwise
	= let	leaves	= leafs graph	-- all leafs are free to number
		leafs'	= S.toList leaves
		addAmbigueLeafs	= if length leafs' > 1 then (leafs':) else id
		(ordering, ambigueties)	= _bo (dropNodes leaves graph) in
		(ordering ++ leafs', addAmbigueLeafs ambigueties)

testGraph	=
	S.fromList <$> M.fromList [("a",["b","c"]),("b",["c"]), ("c",[])]

testGraph'	=
	S.fromList <$> M.fromList [("a",["b"]),("b",[]), ("c",[])]
