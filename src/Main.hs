module Main (main) where

import PureMain
import Utils.Test
import Utils.Tests
import Utils.CreateAssets as M
import Utils.ArgumentParser
import Utils.PureIO hiding (writeFile, putStrLn)

import Utils.Utils
import Utils.Version (version)

import Control.Monad

import System.Environment
import System.Exit

import TypeSystem


{- Imports to test -}
import Dynamize.Test
import AbstractInterpreter.RelationAnalysis
import AbstractInterpreter.AbstractSet
import AbstractInterpreter.RuleAnalysis

import Data.Maybe

import Data.Map as M

import ParseTreeInterpreter.PropertyTester
import Utils.ManualPreprocessor




main	:: IO ()
main	= do	args	<- getArgs
		main' args


main'	:: [String] -> IO ()
main' args 
	= do	(runTests, parsedArgs)	<- parseArgs version args
		if runTests then do
			successFull	<- testAll |> and
			if successFull then
				putStrLn "All clear!"
			else
				exitFailure 
		else do
			when (isNothing parsedArgs) $
				error  "No typesystem file given. See -h"
			let (Just parsedArgs')	= parsedArgs
			runIO' parsedArgs' (mainPure parsedArgs')




