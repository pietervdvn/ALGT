module AbstractInterpreter.QuickCheck where

{-  -}

import Utils.Utils
import Utils.ToString

import TypeSystem
import AbstractInterpreter.AbstractSet

import Data.Maybe

import System.Random
import Control.Monad.State as State

import AssetsHelper


randomParseTrees		:: (RandomGen random) => Int -> Syntax -> TypeName -> Int -> random -> ([ParseTree], random)
randomParseTrees neededPts s tn depth
	= runState (replicateM neededPts $ _generateRandomPT s tn depth)

randomParseTree		:: (RandomGen random) => Syntax -> TypeName -> Int -> random -> (ParseTree, random)
randomParseTree s tn depth
	= runState (_generateRandomPT s tn depth)


_generateRandomPT	:: (RandomGen random) => Syntax -> TypeName -> Int -> State random ParseTree
_generateRandomPT s tn depth
	= do	rInt	<- getR
		intChoice	<- selectRand ([3,4,5,6,7,8,9,10,32,64,100,128,256,512]	:: [Int])
		let ints	= [0,1,2, intChoice, rInt]
		-- selects given builtin ++ choices
		rands		<- builtinSyntax |> fst |> dropSnd3 |+> selectRand'
		let bi nm	= rands & lookup nm
					& maybe (error $ "No such builtin: "++nm) (:[])
		
		let ass		= generateAbstractSet s "" tn & unfoldDepth s 3
		as		<- selectRand ass
		let pts		= toParsetree s (ints, bi) as
		selectRand pts			


getR		:: (RandomGen random) => State random Int
getR	= do	r	<- State.get
		let (i, r') = next r
		put r'
		return i

selectRand'	:: (RandomGen random) => (x, [a]) -> State random (x, a)
selectRand' (x, as)
	= do	r	<- getR
		return (x, as !! (r `mod` length as))


selectRand	:: (RandomGen random) => [a] -> State random a
selectRand as	= selectRand' ((), as) |> snd
