module Utils.Test (recreateTest, testAll, testFast, recreateAllTests, recreateTests, recreateTestsFast) where

import Utils.Utils
import Utils.ArgumentParser
import PureMain

import Assets (allAssets)
import Utils.CreateAssets

import Data.Map as M
import Data.List as L
import Data.List (nub)

import Control.Monad



testArgs      = [ ["Test/STFL.typesystem"]
		, ["Test/STFL.typesystem", "--dts"]
		, ["Test/STFL.typesystem", "--lsvg", "Syntax.svg"]

		, ["Test/STFL.typesystem", "Test/examples.stfl", "e", "-l" ]
		, ["Test/STFL.typesystem", "Test/examples.stfl", "e", "-l", "-r", "→" ]
		, ["Test/STFL.typesystem", "Test/examples.stfl", "e", "-l", "-r", "::" ]
		, ["Test/STFL.typesystem", "Test/examples.stfl", "e", "-l", "--ptsvg", "Parsetrees"]
		, ["Test/STFL.typesystem", "Test/examples.stfl", "e", "-l" ]
		, ["Test/STFL.typesystem", "-c", "Test/DynamizeSTFL.typesystem-changes", "--dts"]
		, ["Test/STFL.typesystem", "-c", "Test/DynamizeSTFL.typesystem-changes", "-c", "Test/GradualizeSTFL.typesystem-changes", "--dts"]
		, ["Test/STFL.typesystem", "--ira"]
		, ["Test/STFL.typesystem", "--irasvg", "SyntaxIRA.svg"]
		, ["Test/STFL.typesystem", "--ir", "EvalCtx"]
		, ["Test/STFL.typesystem", "Test/examples.stfl", "e", "--tp", "Progress" ]
		, ["Test/STFL.typesystem", "Test/examples.stfl", "e", "--tpa" ]
		, ["Test/STFL.typesystem", "Test/examples.stfl", "e", "--tp", "Progress", "--ppp" ]
		, ["Test/STFL.typesystem", "Test/examples.stfl", "e", "--tpa", "--ppp" ]
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
		let output	= mainArgs parsedArgs defaultInput |> snd & isolateFailure
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
		get files output |+> (\(fp, contents) -> writeFile ("src/Assets/IntegrationTests/"++fp) contents)
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
			     

testAll' skip	= do	putStrLn $ "Running "++show (length testArgs)++" tests"
			testArgs' |+> test skip

testAll		= testAll' False
testFast	= testAll' True & void


