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
import qualified Data.Bifunctor as BF

import ParseTreeInterpreter.PropertyTester
import Utils.ManualPreprocessor

import Gradualize.Test

import AssetsHelper -- TODO Remove

main	:: IO ()
main	= do	args	<- getArgs
		main' args


main'	:: [String] -> IO ()
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
			(fc, ts)	<- runIO defaultConfig parsedArgs' (mainPure parsedArgs')
			interactiveArg parsedArgs' |> interactive ts fc & fromMaybe pass
			interactiveStyling parsedArgs' |> interactiveParse ts fc & fromMaybe pass


interactiveParse	:: TypeSystem -> FullColoring -> Name -> IO ()
interactiveParse ts fc parseRule
	= do	putStrLn "# Interactive styling active. Please give filename + input"
		-- fileName	<- getLine
		input		<- getLine |> unEscape
		let fileName	= "interactive" -- TODO
		generateHTML ts fc parseRule (fileName, input)
			& either (\msg -> putStrLn $ "# NO PARSE: "++msg) putStrLn
		interactiveParse ts fc parseRule		

generateHTML	:: TypeSystem -> FullColoring -> Name -> (FilePath, String) -> Either String String
generateHTML ts fc parseRule (fileName, input)
	= do	let syntax	= get tsSyntax ts
		parsed		<- (runParserT (parseSyntax syntax parseRule) () fileName input
					|> BF.first show)
		pt		<- parsed
		return $  "# Parsed "++ (get ptAnnot pt & toParsable)++"\n"
			++ HTML.renderPT fc (get tsStyle ts) (deAnnot pt)

thtml	= do	let html	= generateHTML AssetsHelper.stfl terminalStyle "e" ("test", "1 + 1 + (\\x : Int . x + 1) True") & either error id
		putStrLn html
		writeFile "test.html" (html & lines & tail & unlines)

unEscape	:: String -> String
unEscape []	= []
unEscape ('\\':'\\':str)
		= '\\':str
unEscape ('\\':'n':str)
		= '\n':str
unEscape (c:str)
		= c:unEscape str





interactive	:: TypeSystem -> FullColoring -> Symbol -> IO ()
interactive ts fc symbol
	= do	rel	<- checkRelationExists ts symbol & either error return
		let inTypes	= relTypesWith In rel
		unless (length inTypes == 1) $ print "expected exactly one input type for interactive mode"
		let [inType]	= inTypes
		repl ts fc inType rel
		
repl		:: TypeSystem -> FullColoring -> TypeName -> Relation -> IO ()
repl ts fc tn rel
	= do	input	<- getLine
		unless (Prelude.null input || input == "\EOT") $ do
			let parsed	= parseTargetLang (get tsSyntax ts) tn "Interactive" input
			case parsed of
				Left err	-> putStrLn err
				Right pt	-> proofThat ts rel [pt]
							& either putStrLn (printProof ts fc rel)
			repl ts fc tn rel

printProof	:: TypeSystem -> FullColoring -> Relation -> Proof -> IO ()
printProof ts fc rel proof
	= do	let  [pt]	= _proofConcl proof & get conclusionArgs
					& filterMode Out rel	:: [ParseTree]
		let ptDoc	= Ansi.renderPT fc (get tsStyle ts) pt
		print ptDoc
				
