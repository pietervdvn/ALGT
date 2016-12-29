module AbstractInterpreter.Test where

import Prelude hiding (subtract)
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
import Control.Monad

tp	= "type"
mi	= (tp, -1)

testPattern	:: Expression
testPattern	= MSeq mi [MVar tp "T1"
			, MParseTree $ MLiteral mi "->"
			, MVar tp "T1"]  
stfl	= parseTypeSystem Assets._Test_STFL_typesystem (Just "Test_STFL")
stfl'	= stfl & either (error . show) id
syntax	= tsSyntax stfl'

typ		= generateAbstractSet syntax "_" "type" & snd
e		= generateAbstractSet syntax "_" "e" & snd
eL		= generateAbstractSet syntax "_" "eL" & snd


diffT0		= (e, AsSeq ("e", -1) [eL, e])
diffT1		= (e, generateAbstractSet syntax "_" "value" & snd)
diffT2		= (e, ConcreteLiteral ("bool", -1) "True" )
diffT3		= (e, e)
diffTs		= [diffT0, diffT1, diffT2, diffT3]

tDiff (e, minus)
		= subtract syntax [e] minus
	
tDiff' v@(e, minus)
	= do	let diff	= tDiff v
		putStrLn $ "Difference of "++show e++"   - "++show minus
		print $ length $ show diff
		print diff
		putStrLn "\n\n"



testAS	:: IO ()	
testAS	= diffTs |+> tDiff' & void



testFS	= do	testF stfl' "eval" [e]
		testF stfl' "dom" [typ]
		testF stfl' "cod" [typ]
		testF stfl' "equality" [typ, typ]



testF	:: TypeSystem -> Name -> [AbstractSet'] -> IO ()
testF ts n args
	= do	putStrLn $ "Testing (first clause of) function "++show n
		let function	= tsFunctions ts ! n
		let results	 = interpretFunction (tsSyntax ts) function args
		print results
		putStrLn "\n\n\n"
		



