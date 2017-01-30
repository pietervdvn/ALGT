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
			, ["Test/STFL.typesystem", "--irasvg", "SyntaxIRA.svg"]
			
			
			] & nub

defaultInput	= allAssets & M.fromList	:: Input


directoryFor	:: [String] -> String
directoryFor args
	= let	nm	= concat args & ("/"++) & name ""
		in
		"src/Assets/IntegrationTests/"++nm
		

runTest		:: [String] -> IO Output
runTest args
	= do	parsedArgs	<- parseArgs version args
		let output	= mainArgs parsedArgs defaultInput |> snd & isolateFailure
		return output

createAll	
	= do	autoCreateAssets
		testArgs |+> createTestResult & void


createTestResult	:: [String] -> IO ()
createTestResult args
	= do	output	<- runTest args
		let log	= get stdOut output & unlines
		writeFile (directoryFor args) (show output)
		get files output |+> (\(fp, contents) -> writeFile ("src/Assets/IntegrationTests/"++fp) contents)
		writeFile (directoryFor $ "log___":args) (unwords args ++"\n\n"++ log)

getTestResult		:: [String] -> IO Output
getTestResult args
	= do	cont		<- readFile (directoryFor args)
		let output	= read cont	:: Output
		return output

test		:: [String] -> IO ()
test args	= do	expected	<- getTestResult args
			actual		<- runTest args
			unless (expected == actual) $ do
			      [ "Integration test failed"
				, "Arguments: "++unwords args
				, ""
				] & unlines & putStrLn

testAll		= testArgs |+> test & void

