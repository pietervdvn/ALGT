module Utils.Test where

import Utils.Utils
import Utils.ArgumentParser
import PureMain

import Assets (allAssets)
import Utils.CreateAssets

import Data.Map as M
import Data.List (nub)

import Control.Monad


testArgs	= [	["Test/STFL.typesystem"]
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
			-- , ["Test/STFL.typesystem", "--irasvg", "SyntaxIRA.svg"]
			] & nub

testArgs'	= mapi testArgs

defaultInput	= allAssets & M.fromList	:: Input


directoryFor	:: [String] -> String
directoryFor args
	= let	nm	= concat args & ("/"++) & name ""
		in
		"src/Assets/IntegrationTests/"++nm
		

runTest		:: [String] -> IO Output
runTest args
	= do	parsedArgs	<- parseArgs ([-1::Int], "Integration tests") args
		let output	= mainArgs parsedArgs defaultInput |> snd & isolateFailure
		return output

createAll	
	= do	autoCreateAssets
		testArgs' |+> createTestResult & void


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

test		:: (Int, [String]) -> IO ()
test (i, args)	= do	putStrLn $ "Running test "++show i++": "++unwords args 
			expected	<- getTestResult args
			actual		<- runTest args
			let log		= get stdOut actual & unlines
			let errMsg	=  [ "Integration test failed", "Arguments: "++unwords args, ""] 
						& unlines
			unless (expected == actual) $
				do	putStrLn errMsg
					writeFile (directoryFor ("log___":args) ++ ".FAILED") 
						(unwords args ++ "\n\n" ++ log)
			     

testAll		= do	putStrLn $ "Running "++show (length testArgs)++" tests"
			testArgs' |+> test & void

