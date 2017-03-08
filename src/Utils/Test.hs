module Utils.Test (runTest, recreateTest, testAll, testFast, recreateAllTests, recreateTests, recreateTestsFast) where

import Utils.Utils
import Utils.ArgumentParser
import PureMain
import Utils.PureIO hiding (writeFile, readFile, putStrLn, putDocLn)

import Text.PrettyPrint.ANSI.Leijen

import Assets (allAssets)
import Utils.CreateAssets

import Data.Map as M
import Data.List as L
import Data.List (nub)

import Control.Monad



import Utils.Tests







testArgs      = [ ["Test/STFL.language"]
		, ["Test/STFL.language", "--dlf"]
		, ["Test/STFL.language", "--lsvg", "Syntax.svg"]
		, ["Test/STFL.language", "--ia"]
		, ["Test/STFL.language", "--ifa", "dom"]
		, ["Test/STFL.language", "--ifa", "cod"]
		, ["Test/STFL.language", "--ifa", "equate"]
		, ["Test/STFL.language", "--ifa", "eval"]

		, ["Test/STFL.language", "--ir", "→"]
		, ["Test/STFL.language", "--ir", "→*"]
		, ["Test/STFL.language", "--ir", "::"]

		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "-r", "→" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "-r", "::" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--ptsvg", "Parsetrees"]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l" ]
		, ["Test/STFL.language", "-c", "Test/DynamizeSTFL.language-changes", "--dlf"]
		, ["Test/STFL.language", "-c", "Test/DynamizeSTFL.language-changes", "-c", "Test/GradualizeSTFL.language-changes", "--dlf"]
		-- , ["Test/STFL.language", "--ira"]
		-- , ["Test/STFL.language", "--irasvg", "SyntaxIRA.svg"]
		, ["Test/STFL.language", "--ir", "EvalCtx"]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l",  "--tp", "Progress" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--tpa" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--tp", "Progress", "--ppp" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--tpa", "--ppp" ]
		, ["Test/Recursive.language", "--dlf"]
		, ["Test/CommonSubset.language", "--dlf"]
		, ["Test/FuncTypeErr.language", "--dlf"]
		, ["Style.language", "Terminal.style", "styleFile", "--html"]
		] |> (++["--plain"]) & nub


slow		= []

testArgs'	= mapi testArgs

defaultInput	= allAssets & M.fromList	:: Input


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
		let output
			= mainPure parsedArgs
				& runPureOutput defaultConfig defaultInput
		return $ removeCarriageReturns output

recreateAllTests	
	= do	autoCreateAssets
		testArgs' |+> createTestResult & void

recreateTest i
	= (i, testArgs !! i) & createTestResult

recreateTests is
	= is |+> recreateTest & void

recreateTestsFast
	= [0..length testArgs - 1] & L.filter (`notElem` slow)
		|+> recreateTest


createTestResult	:: (Int, [String]) -> IO ()
createTestResult (i, args)
	= do	putStrLn $ "Creating output for test "++show i++": "++unwords args 
		output	<- runTestWith args
		let log	= get stdOut output & unlines
		writeFile (directoryFor args) (show output)
		get files output & M.toList |+> (\(fp, contents) -> writeFile ("src/Assets/IntegrationTests/"++fp) contents)
		writeFile (directoryFor $ "log___":args) (unwords args ++"\n\n"++ log)

getTestResult		:: [String] -> IO Output
getTestResult args
	= do	cont		<- readFile (directoryFor args)
		let output	= read cont	:: Output
		return output

test		:: Bool -> (Int, [String]) -> IO Bool
test skipSlow (i, args)	
	= do	let msg	=  "Test "++show i++": "++unwords args 
		putStr $ "[       ] " ++ msg
		if skipSlow && (i `elem` slow) then do
			putStrLn "\r[   S   ]"
			return True
		else do
			expected	<- getTestResult args
			actual		<- runTestWith args
			let log		= get stdOut actual & unlines	:: String
			let errMsg	= "\r[ FAILS ]"
			let success	= expected == actual
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
	= do	putDocLn $ onyellow $ black $ text $ (show idLines ++ " identical lines at end of file\n")
printDiff idLines [] a
	= do	putDocLn $ onyellow $ black $ text $ "Actual overshoot after "++show idLines++" identical lines:"
		putDocLn $ red $ text $ unlines a
printDiff idLines exp []
	= do	putDocLn $ onyellow $ black $ text $ "To little output after "++show idLines++" identical lines:"
		putDocLn $ blue $ text $ unlines exp
printDiff idLines (e:exp) (a:act)
 | e == a	= printDiff (idLines + 1) exp act
 | otherwise
		= do	unless (idLines == 0) $ putDocLn $ onyellow $ black $ text $ (show idLines ++ " identical lines")
			putDocLn $ blue $ text e 
			putDocLn $ red $ text a
			printDiff 0 exp act


putDocLn doc	= putDoc doc >> putStrLn ""


testAll' skip	= do	putStrLn $ "Unit tests passed: "++show unitTestsOK
			putStrLn $ "Running "++show (length testArgs)++" tests"
			t	<- testArgs' |+> test skip
			return $ unitTestsOK

runTest i	= test False (i, testArgs !! i)

testAll		= testAll' False
testFast	= testAll' True & void


