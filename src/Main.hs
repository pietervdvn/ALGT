module Main (module M, main) where

import PureMain as M
import Utils.Test as M
import Utils.CreateAssets as M
import Utils.ArgumentParser

import Utils.Utils

import Control.Monad

import System.Environment

import TypeSystem


{- Imports to test -}
import Dynamize.Test
import AbstractInterpreter.RelationAnalysis
import AbstractInterpreter.AbstractSet
import AbstractInterpreter.RuleInterpreter

import Data.Maybe

import Data.Map as M

version	= ([0,1,11], "Total Language Test")



main	:: IO ()
main	= do	args	<- getArgs
		main' args
		return ()


main'	:: [String] -> IO (TypeSystem, [(String, ParseTree)])
main' args 
	= do	parsedArgs	<- parseArgs version args
		when (runTests parsedArgs) testAll
		runIO' parsedArgs (mainArgs parsedArgs)

