module AbstractInterpreter.Test where

import Utils.Utils
import Assets
import Data.Map ((!))
import TypeSystem
import Parser.TypeSystemParser

import AbstractInterpreter.AbstractParseTree
import AbstractInterpreter.AbstractPatternMatcher
import AbstractInterpreter.AbstractFunctionInterpreter
import Data.List


import Utils.Unification

tp	= "type"
mi	= (tp, -1)

testPattern	:: Expression
testPattern	= MSeq mi [MVar tp "T1"
			, MParseTree $ MLiteral mi "->"
			, MVar tp "T1"]  

testAS	:: IO ()	
testAS	= do	let stfl	= parseTypeSystem Assets._Test_STFL_typesystem (Just "Test_STFL")
		let stfl'	= stfl & either (error . show) id
		let syntax	= tsSyntax stfl'
		let t		= generateAbstractSet syntax "_" "type" & snd
		let e		= generateAbstractSet syntax "_" "e" & snd

		testF stfl' "detectAscr" [e]
		testF stfl' "dom" [t]
		testF stfl' "cod" [t]
		testF stfl' "equality" [t, t]



testF	:: TypeSystem -> Name -> [AbstractSet'] -> IO ()
testF ts n args
	= do	putStrLn $ "Testing (first clause of) function "++show n
		let (MFunction _ clauses)	= tsFunctions ts ! n
		let results	 = interpretClause (tsSyntax ts) (head clauses) args
		results |> (\(pats, res) -> (pats |> show & intercalate "\t") ++ "\t--> "++show res ) & unlines & putStrLn
		



