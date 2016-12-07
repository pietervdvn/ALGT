module AbstractInterpreter.Test where

import Utils
import TypeSystem
import AbstractInterpreter.AbstractParseTree

testAS	:: BNFRules -> IO ()	
testAS r	
	= do	let t	= generateAbstractSet r "e"
		print $ snd t
		print $ snd $ unfold t
		print $ snd $ unfold $ unfold t



