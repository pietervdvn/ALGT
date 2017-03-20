module Utils.Test (runTest, runTests, recreateTest, testAll, testFast, recreateAllTests, recreateTests, recreateTestsFast) where

import Prelude hiding (exp)
import Utils.Utils
import Utils.ToString
import Utils.ArgumentParser
import PureMain
import Utils.PureIO hiding (writeFile, readFile, putStrLn, putDocLn)
import qualified Utils.PureIO as PureIO

import Text.PrettyPrint.ANSI.Leijen

import Assets (allAssets)
import Utils.CreateAssets

import Data.Map as M
import Data.List as L
import Data.List (nub)

import Control.Monad

import System.Random

import AssetsHelper

import Utils.Tests

import Lens.Micro hiding ((&))



-- Expectations
noExp args	= (args, "")
exp str args	= (args, str)


testArgs	:: [([String], String)]
testArgs      = [ exp "No action specified" ["Test/STFL.language"]
		, exp (stfl & toParsable' (24::Int)) ["Test/STFL.language", "--dlf"]
		, noExp ["Test/STFL.language", "--lsvg", "Syntax.svg"]
		, noExp ["Test/STFL.language", "--ia"]
		, exp " Analysis of dom : type -> typeL "  ["Test/STFL.language", "--ifa", "dom"]
{-5-}		, noExp ["Test/STFL.language", "--ifa", "cod"]
		, noExp ["Test/STFL.language", "--ifa", "equate"]
		, noExp ["Test/STFL.language", "--ifa", "eval"]

		, exp "Analysis for rules about (→)"
			["Test/STFL.language", "--ir", "→"]
		, noExp["Test/STFL.language", "--ir", "→*"]
{-10-}		, noExp["Test/STFL.language", "--ir", "::"]

		, exp "# \"True\" was parsed as:" 
			["Test/STFL.language", "Test/examples.stfl", "e", "-l" ]
		, exp "If True Then False Else True → False"
			["Test/STFL.language", "Test/examples.stfl", "e", "-l", "-r", "→" ]
		, exp "{} ⊢ ( \\ f : Int -> Int . f 5 ) ( \\ i : Int . i + 1 ), Int"
			["Test/STFL.language", "Test/examples.stfl", "e", "-l", "-r", "::" ]
		, noExp ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--ptsvg", "Parsetrees"]
		, exp "# \"True\" was parsed as:" ["Test/STFL.language", "Test/examples.stfl", "e", "-l" ]
{-15-}		, exp "Dynamized STFL"["Test/STFL.language", "-c", "Test/DynamizeSTFL.language-changes", "--dlf"]
		, noExp["Test/STFL.language", "-c", "Test/DynamizeSTFL.language-changes", "-c", "Test/GradualizeSTFL.language-changes", "--dlf"]
		-- , ["Test/STFL.language", "--ira"]
		-- , ["Test/STFL.language", "--irasvg", "SyntaxIRA.svg"]
		, exp " Analysis for rules about (→) " ["Test/STFL.language", "--ir", "→"]
		, exp "Property Progress holds for given examples " ["Test/STFL.language", "Test/examples.stfl", "e", "-l",  "--tp", "Progress" ]
		, exp (   "Property Preservation holds for given examples                                  \n"
			++"Property Progress holds for given examples                                      \n"
			++"Property Termination holds for given examples                                   ")
			["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--tpa" ]
{-20-}		, exp "# Property Progress proven by failing predicate with assignment {e0 --> ( \\ x : Int . x + 1 ) True}:"
			["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--tp", "Progress", "--ppp" ]
		, exp "# Property Preservation proven by failing predicate with assignment {e0 --> True}:" 
			["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--tpa", "--ppp" ]
		, exp "expr ⊂ expr"
			["Test/Recursive.language", "--dlf"]
		, exp "Common choices are: a | b" ["Test/CommonSubset.language", "--dlf"]
		, exp "T1 is typed as both \"typeL\" and \"bool\", which don't have a common supertype"
			["Test/FuncTypeErr.language", "--dlf"]
{-25-}		, exp "<html>" ["Style.language", "Terminal.style", "styleFile", "--html"]
		, exp "Could not parse expression of the form expr" ["Test/STFLBool.language", "Test/STFLBoolMismatch.stflbool", "expr", "-l"]
		] |> over _1 (++["--plain"]) & nub


slow		= []

testArgs'	= mapi testArgs

defaultInput	= allAssets & M.fromList


directoryFor	:: [String] -> String
directoryFor args
	= let	nm	= concat args & ("/"++) & escNames & name ""
		in
		"src/Assets/IntegrationTests/"++nm

escNames	:: String -> String
escNames []	= []
escNames ('→':str)	= "arrow"++escNames str
escNames (':':str)	= "double_point" ++ escNames str
escNames ('*':str)	= "start" ++ escNames str
escNames (c:str)	= c:escNames str
		

runTestWith		:: [String] -> IO Output
runTestWith args
	= do	(_, Just parsedArgs)
				<- parseArgs ([-1::Int], "Integration tests") args
		gen	<- getStdGen
		let output
			= mainPure parsedArgs & void & isolateFailure' PureIO.putStrLn
				& runPureOutput defaultConfig gen defaultInput
		removeCarriageReturns output & removeUnchanged & return

recreateAllTests	
	= do	autoCreateAssets
		testArgs'|+> createTestResult & void

recreateTest i
	= (i, testArgs !! i) & createTestResult	-- unSafe !! ; but not a problem

recreateTests is
	= is |+> recreateTest & void

recreateTestsFast
	= [0..length testArgs - 1] & L.filter (`notElem` slow)
		|+> recreateTest

checkAssumedString output exp
	= do	let isContained	= exp `isInfixOf` (output & get stdOut & unlines)
		unless isContained $ do
			putStrLn ""
			putDoc $ ondullred $ green $ text ("Missing: "++exp)
		return isContained
		

createTestResult	:: (Int, ([String], String)) -> IO ()
createTestResult (i, (args, exp))
	= do	putStrLn $ "Creating output for test "++show i++": "++unwords args 
		output	<- runTestWith args
		met	<- checkAssumedString output exp
		when met $ do
			let log	= get stdOut output & unlines
			writeFile (directoryFor args) (show output)
			changedFiles output & M.toList |+> (\(fp, contents) -> writeFile ("src/Assets/IntegrationTests/"++fp) contents)
			writeFile (directoryFor $ show i:"log___":args) ("Testcase "++show i ++"\n" ++ unwords args ++"\n\n"++ log)

getTestResult		:: [String] -> IO Output
getTestResult args
	= do	cont		<- readFile (directoryFor args)
		let output	= read cont	:: Output
		return output

test		:: Bool -> (Int, ([String], String)) -> IO Bool
test skipSlow (i, (args, exp))	
	= do	let msg	=  "Test "++show i++": "++unwords args 
		putStr $ "[       ] " ++ msg
		if skipSlow && (i `elem` slow) then do
			putStrLn "\r[   S   ]"
			return True
		else do
			expected	<- getTestResult args
			actual		<- runTestWith args
			let log		= get stdOut actual & unlines	:: String
			meetsExtra	<- checkAssumedString actual exp
			let success	= expected == actual && meetsExtra 
			let errMsg	= if not meetsExtra then "\r[ASSUMPT]" else "\r[ FAILS ]"
			if success then putStrLn "\r[   ✓   ]"
				else do	putStrLn errMsg
					writeFile (directoryFor ("log___":args) ++ ".FAILED") 
						(unwords args ++ "\n\n" ++ log)
					let prepStdOut output	= output & get stdOut & unlines & lines
					let exp	= expected & prepStdOut
					let act	= actual & prepStdOut
					unless (exp == act) $ printDiffs exp act

					let prepF out	= out & get files & M.keys |> ("File: "++) 
					let expF	= expected & prepF
					let actF	= actual & prepF
					unless (expF == actF) $ printDiffs expF actF
					let diff	= M.intersectionWith (/=) (expected & get files) (actual & get files)
								& M.keys
					unless (L.null diff) $ putDocLn $ ondullred $ text 
						$ "Differing files: "++show diff

			return success
			     
{--}
minDiff		:: [String] -> [String] -> Int
minDiff	a b
 | a == b	= 0
 | otherwise
	= let 	l	= min (length a) (length b) in
		[(diffBetween offset a b, offset ) |	offset	<- [-l .. l]] & sortOn (abs . snd) & sortOn fst & head & snd


a		= ["e", "a","b","c"]
b		= tail a

diffBetween	:: Int -> [String] -> [String] -> Int
diffBetween offset a b
	= let	(a', b')	= runOffset offset (a, b)
		in
		zip a' b' & L.filter (uncurry (/=)) & length


runOffset	:: Int -> ([String], [String]) -> ([String], [String])
runOffset i (a, b)
 | i >= 0	= (replicate i "" ++ a, b ++ replicate i "")
 | otherwise	= swap $ runOffset (negate i) (b, a)


printDiffs	:: [String] -> [String] -> IO ()
printDiffs exp act
	= do	let minOffset	= minDiff exp act
		let (exp', act')	= runOffset minOffset (exp, act)
		printDiff 0 exp' act'

printDiff	:: Int -> [String] -> [String] -> IO ()
printDiff idLines [] []
	= 	putDocLn $ onyellow $ black $ text (show idLines ++ " identical lines at end of file\n")
printDiff idLines [] a
	= do	putDocLn $ onyellow $ black $ text $ "Actual overshoot after "++show idLines++" identical lines:"
		putDocLn $ red $ text $ unlines a
printDiff idLines exp []
	= do	putDocLn $ onyellow $ black $ text $ "To little output after "++show idLines++" identical lines:"
		putDocLn $ blue $ text $ unlines exp
printDiff idLines (e:exp) (a:act)
 | e == a	= printDiff (idLines + 1) exp act
 | otherwise
		= do	unless (idLines == 0) $ putDocLn $ onyellow $ black $ text (show idLines ++ " identical lines")
			putDocLn $ blue $ text e 
			putDocLn $ red $ text a
			printDiff 0 exp act


putDocLn doc	= putDoc doc >> putStrLn ""

testAll'	:: Bool -> IO [Bool]
testAll' skip	= do	putStrLn $ "Unit tests passed: "++show unitTestsOK
			putStrLn $ "Running "++show (length testArgs)++" tests"
			t	<- testArgs' |+> test skip
			return (unitTestsOK:t)

runTest i	= test False (i, testArgs !! i)	-- unSafe !! ; but not a problem
runTests is	= is |+> runTest

testAll		:: IO [Bool]
testAll		= testAll' False
testFast	= testAll' True & void


