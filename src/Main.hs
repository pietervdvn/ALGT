module Main (main) where

import PureMain
import Utils.ToString
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
import System.IO

import Text.PrettyPrint.ANSI.Leijen
import Text.Parsec

import TypeSystem

import TypeSystem.Parser.TargetLanguageParser
import ParseTreeInterpreter.RuleInterpreter

{- Imports to test -}
import SyntaxHighlighting.Coloring
import SyntaxHighlighting.AnsiPT as Ansi
import SyntaxHighlighting.AsHTML as HTML



import AbstractInterpreter.RelationAnalysis
import AbstractInterpreter.AbstractSet
import AbstractInterpreter.RuleAnalysis

import Data.Maybe

import Data.Map as M

import ParseTreeInterpreter.PropertyTester
import Utils.ManualPreprocessor

import Gradualize.Test


main	:: IO ()
main	= do	args	<- getArgs
		main' args & void


main'	:: [String] -> IO TypeSystem
main' args 
	= do	(runTests, parsedArgs)	<- parseArgs version args
		if runTests then do
			successFull	<- testAll |> and
			if successFull then do
				putStrLn "All clear!"
				exitSuccess
			else
				exitFailure 
		else do
			when (isNothing parsedArgs) $
				error  "No typesystem file given. See -h"
			let (Just parsedArgs')	= parsedArgs
			let needsInput	= parsedArgs' & exampleFiles |> fileName |> (== ".") & or	-- we give each exampleFile config the same input
			extraFile 	<- if needsInput then readLines else return []
			let extraInput	= M.singleton "." $ unlines extraFile
			
			(fc, ts)	<- runIOWith defaultConfig extraInput parsedArgs' (mainPure parsedArgs')
			interactiveArg parsedArgs' |> interactive ts fc & fromMaybe pass
			return ts

readLines	:: IO [String]
readLines 	= do	end	<- isEOF
			if end then return [] else do
				inp	<- getLine
				rest	<- readLines
				return (inp:rest)


interactive	:: TypeSystem -> FullColoring -> Symbol -> IO ()
interactive ts fc symbol
	= do	rel	<- checkRelationExists ts symbol & either error return
		let inTypes	= relTypesWith In rel
		unless (length inTypes == 1) $ print "expected exactly one input type for interactive mode"
		let [inType]	= inTypes
		interact (repl ts fc inType rel)
		
repl		:: TypeSystem -> FullColoring -> TypeName -> Relation -> String -> String
repl _ _ _ _ ""	= ""
repl ts fc tn rel input
	= either id id $
	  do	parsed	<- parseTargetLang (get tsSyntax ts) tn "Interactive" input
		proof	<- proofThat ts rel [parsed]
		return $ printProof ts fc rel proof
			

printProof	:: TypeSystem -> FullColoring -> Relation -> Proof -> String
printProof ts fc rel proof
	= let 	[pt]	= _proofConcl proof & get conclusionArgs
					& filterMode Out rel	:: [ParseTree]
		ptDoc	= Ansi.renderPT fc (get tsStyle ts) pt
		in show ptDoc
				
