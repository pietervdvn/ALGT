module Main (module M, main) where

import PureMain as M
import Utils.Test as M
import Utils.CreateAssets as M
import Utils.ArgumentParser

import Utils.Utils

import Control.Monad

import System.Environment
import System.Exit

import TypeSystem


{- Imports to test -}
import Dynamize.Test
import AbstractInterpreter.RelationAnalysis
import AbstractInterpreter.AbstractSet
import AbstractInterpreter.RuleInterpreter

import Data.Maybe

import Data.Map as M

import ParseTreeInterpreter.PropertyProver

version	= ([0,1,12,0], "Unwinding Rules")



main	:: IO ()
main	= do	args	<- getArgs
		main' args
		return ()


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
			runIO' parsedArgs' (mainArgs parsedArgs')
			pass

