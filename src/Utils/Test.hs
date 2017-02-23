module Utils.Test (runTest, recreateTest, testAll, testFast, recreateAllTests, recreateTests, recreateTestsFast) where

import Utils.Utils
import Utils.ArgumentParser
import PureMain
import Utils.PureIO hiding (writeFile, readFile, putStrLn)

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

		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "-r", "→" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "-r", "::" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--ptsvg", "Parsetrees"]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l" ]
		, ["Test/STFL.language", "-c", "Test/DynamizeSTFL.language-changes", "--dlf"]
		, ["Test/STFL.language", "-c", "Test/DynamizeSTFL.language-changes", "-c", "Test/GradualizeSTFL.language-changes", "--dlf"]
		, ["Test/STFL.language", "--ira"]
		, ["Test/STFL.language", "--irasvg", "SyntaxIRA.svg"]
		, ["Test/STFL.language", "--ir", "EvalCtx"]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l",  "--tp", "Progress" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--tpa" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--tp", "Progress", "--ppp" ]
		, ["Test/STFL.language", "Test/examples.stfl", "e", "-l", "--tpa", "--ppp" ]
		, ["Test/Recursive.language", "--dlf"]
		, ["Test/CommonSubset.language", "--dlf"]
		] & nub


slow		= [10]

testArgs'	= mapi testArgs

defaultInput	= allAssets & M.fromList	:: Input


directoryFor	:: [String] -> String
directoryFor args
	= let	nm	= concat args & ("/"++) & name ""
		in
		"src/Assets/IntegrationTests/"++nm
		

runTest		:: [String] -> IO Output
runTest args
	= do	(_, Just parsedArgs)
				<- parseArgs ([-1::Int], "Integration tests") args
		let output
			= mainPure parsedArgs
				& runPureOutput defaultInput
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
		output	<- runTest args
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
			actual		<- runTest args
			let log		= get stdOut actual & unlines
			let errMsg	= "\r[ FAILS ]"
			let success	= expected == actual
			if success then putStr "\r[   ✓   ]"
				else do	putStr errMsg
					writeFile (directoryFor ("log___":args) ++ ".FAILED") 
						(unwords args ++ "\n\n" ++ log)
			putStrLn ""
			return success
			     

testAll' skip	= do	putStrLn $ "Unit tests passed: "++show unitTestsOK
			putStrLn $ "Running "++show (length testArgs)++" tests"
			t	<- testArgs' |+> test skip
			return $ unitTestsOK:t

testAll		= testAll' False
testFast	= testAll' True & void


