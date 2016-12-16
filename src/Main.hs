module Main where

import System.Environment

import Utils

import Control.Monad

import TypeSystem

import Parser.TargetLanguageParser
import Parser.TypeSystemParser (parseTypeSystemFile)
import ParseTreeInterpreter.FunctionInterpreter
import ParseTreeInterpreter.RuleInterpreter


import Text.Parsec

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList)
import Data.List (intercalate)

import Options.Applicative
import Data.Monoid ((<>))
import Data.Maybe (fromJust, isJust)

import ArgumentParser

version	= [0,0,3]


main	:: IO ()
main	= do	args	<- getArgs
		main' args
		return ()


main'	:: [String] -> IO (TypeSystem, [(String, ParseTree)])
main' args 
	= do	parsedArgs	<- parseArgs version args
		mainArgs parsedArgs


mainArgs	:: Args -> IO (TypeSystem, [(String, ParseTree)])
mainArgs args	
	= do	ts'	<- parseTypeSystemFile (ts_file args)
		ts	<- either (error . show) return ts'
		checkTypeSystem ts & either error return

		let noRules	= not ( dumbTS args) && all isNothing [symbol args, function args, stepByStep args]

		when (dumbTS args) $ print ts

		let targetFile	= example_file args
		targetContents'	<- readFile targetFile
		let targets	= (if line_by_line args then lines else (:[])) targetContents'

		parseTrees		<- (targets |+> parseWith targetFile ts (parser args)) |> zip targets

		parseTrees |+> ifJust (runRule ts) (symbol args)
		parseTrees |+> ifJust (runFunc ts) (function args)
		parseTrees |+> ifJust (runStepByStep ts) (stepByStep args)

		when noRules $ putStrLn "You didn't specify an action to perform. See -h"
	
		return (ts, parseTrees)


parseWith	:: FilePath -> TypeSystem -> Name -> String -> IO ParseTree
parseWith file ts bnfRuleName str
	= do	let parser	= parse $ parseRule (tsSyntax ts) bnfRuleName
		let parsed	= parser file str
		either (error . show) return parsed


ifJust		:: (a -> b -> IO ()) -> Maybe a -> b -> IO ()
ifJust f Nothing b	= return ()
ifJust f (Just a) b	= f a b

runRule		:: TypeSystem -> Symbol -> (String, ParseTree) -> IO ()
runRule ts symbol (input, pt)
	= do	let proof	= [pt] & proofThat' ts symbol	:: Either String Proof
		let shown	= proof & showProofWithDepth input symbol
		putStrLn shown

showProofWithDepth		:: String -> Symbol -> Either String Proof -> String
showProofWithDepth input relation (Left str)	
	= "# Could not apply relation "++relation++" to relation the input "++input++", because: \n"++str
showProofWithDepth input relation (Right proof)
	= "# "++input++" applied to "++relation++
		"\n# Proof weight: "++show (weight proof)++", proof depth: "++ show (depth proof) ++"\n\n"++show proof++"\n\n\n"



		
runFunc		:: TypeSystem -> Name -> (String, ParseTree) -> IO ()
runFunc ts func (inp, pt)
 	= do	let pt'	= evalFunc ts func [pt]
		putStrLn $ "# "++show inp++" applied to "++func
		print pt'

runStepByStep	:: TypeSystem -> Name -> (String, ParseTree) -> IO ()
runStepByStep ts func (inp, pt)
	= do	putStrLn $ "# "++show inp++" applied repeatedly to "++func
		evalStar ts func pt 

evalStar	:: TypeSystem -> Name -> ParseTree -> IO ()
evalStar ts funcName pt	
	= do	putStrLn $ "\n " ++ show pt
		let pt'	= evalFunc ts funcName [pt]
		if pt' /= pt then
			evalStar ts funcName pt'
		else
			return ()

