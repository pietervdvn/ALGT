module Utils.UnificationTest where

{-
This module defines 
-}
import Utils.Unification
import Utils.Utils
import Data.Map (Map, (!), member)
import qualified Data.Map as M
import Data.Set (Set, insert, findMin, deleteMin)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.Bifunctor
import Control.Arrow ((&&&))


data Tree	= Node String [Tree] | Var String
	deriving (Show, Ord, Eq)


instance Node Tree where
	
	hasChildren (Node _ ch)	= not $ L.null ch
	hasChildren _	 	= False

	getChildren (Node _ ch)	= ch
	getChildren _		= []

	newChildren (Node f _) ch
				= Node f ch

	
	isVar (Var _)		= True
	isVar _			= False
	
	getName (Var s)		= s

	sameSymbol (Node f _) (Node g _)
				= f == g


varX		= Var "x"
varY		= Var "y"

tree0		= Node "f" [varX, Node "a"[]]
tree1		= Node "f" [Node "b" [], varY]

unif0_1		= Right (M.fromList [("x", Node "b" []), ("y", Node "a" [])], [])
unifx_y		= Right (M.fromList [("x", varY)], [])

unifx_0		= Right (M.fromList [("x",Node "f" [Var "x",Node "a" []])] , ["x"])
	
test		:: Node a => ((a, a), Either String (Substitution a, [Name])) -> Bool
test ((a, b), exp)
	= let	eSubs	= unify a b
		cycles	= eSubs |> (id &&& occursCheck) in
		cycles == exp

-- Should return true
tests		= [((tree0, tree1), unif0_1), ((varX, varY), unifx_y), ((varX, tree0), unifx_0)] |> test & and


