module AbstractInterpreter.Test where

import Utils.Utils
import TypeSystem
import AbstractInterpreter.AbstractParseTree
import AbstractInterpreter.AbstractPatternMatcher

tp	= "type"
mi	= (tp, -1)

testPattern	:: Expression
testPattern	= MSeq mi [MVar tp "T1"
			, MParseTree $ MLiteral mi "->"
			, MVar tp "T1"]  

testAS	:: Syntax -> IO ()	
testAS r	
	= do	let t	= generateAbstractSet r "_" "type"
		print $ snd t
		print (unfold t |> snd)
		let pt = patternMatch r testPattern (t & snd)
		print pt
		putStrLn $ "\n\n >>"++show pt



