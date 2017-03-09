module AbstractSet.QuickCheck where

{-  -}

import Utils.Utils

import TypeSystem
import AbstractInterpreter.AbstractSet

import System.Random
import Control.Monad.State as State



generateRandomPT	:: (Random random) => Syntax -> TypeName -> State random ParseTree
generateRandomPT s tn r
	= do	r1	<- getR
		r2	<- getR
		let select r choices
				= choices !! (r `mod` length choices)
		let intChoices	= [3,4,5,6,7,8,9,10,32,64,100,128,256,512]
		let ints	= [0,1,2] ++  select r intChoices ++ [r2]
		-- selects given builtin ++ choices
		let builtInNms	= builtinSyntax |> fst |> fst3
		rands		<- builtInNms |+> const getR
		let bi nm	= builtinSyntax |> fst |> dropSnd3 & lookup nm
					& fromMaybe (error $ "No such builtin: "++nm)
					& select 
					


getR		:: State random Int
getR	= do	r	<- State.get
		let (i, r') = next r
		put r
		return i
