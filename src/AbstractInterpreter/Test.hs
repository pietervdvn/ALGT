module AbstractInterpreter.Test where

import Utils
import TypeSystem
import AbstractInterpreter.AbstractParseTree
import AbstractInterpreter.AbstractPatternMatcher

tp	= "type"
mi	= (tp, -1)

testPattern	:: Expression
testPattern	= MSeq mi [MVar tp "T"
			, MParseTree $ MLiteral mi "->"
			, MVar tp "T"]  

testAS	:: BNFRules -> IO ()	
testAS r	
	= do	let t	= generateAbstractSet r "_" "type"
		let pt = patternMatch r testPattern (t & snd)
		print pt
		putStrLn $ "\n\n >>"++show pt



