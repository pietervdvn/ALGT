module Main where

import TypeSystem
import Utils.TypeSystemToString
import Utils.ToString
import Utils.Utils
import Utils.ArgumentParser

import Parser.TargetLanguageParser
import Parser.TypeSystemParser (parseTypeSystemFile)
import ParseTreeInterpreter.FunctionInterpreter
import ParseTreeInterpreter.RuleInterpreter
import Changer.ChangesParser

import System.Environment

import Control.Monad

import Text.Parsec

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList)
import Data.List (intercalate)
import Data.Monoid ((<>))

import Options.Applicative


version	= ([0,1,0], "Language (Re)Factory")


main	:: IO ()
main	= do	args	<- getArgs
		main' args
		return ()


main'	:: [String] -> IO (TypeSystem, [(String, ParseTree)])
main' args 
	= do	parsedArgs	<- parseArgs version args
		mainArgs parsedArgs


mainArgs	:: Args -> IO (TypeSystem, [(String, ParseTree)])
mainArgs (Args tsFile exampleFiles changeFiles dumbTS)
	= do	ts'	<- parseTypeSystemFile tsFile
		ts	<- either (error . show) return ts'
		check ts & either error return

		changedTs	
			<- changeFiles |> mainChanges & foldM (&) ts	:: IO TypeSystem

		when dumbTS $ putStrLn $ toParsable changedTs

		
		
		parseTrees <- exampleFiles |+> (`mainExFile` changedTs)
		return (changedTs , concat parseTrees)



mainChanges	:: String -> TypeSystem -> IO TypeSystem
mainChanges filepath ts
	= do	changes'	<- parseChangesFile ts filepath
		(changes,ts')	<- changes' & either (error . show) return
		return ts'


mainExFile	:: ExampleFile -> TypeSystem -> IO [(String, ParseTree)]
mainExFile args ts 
	= do	let noRules	= all isNothing [symbol args, function args, stepByStep args]


		let targetFile	= fileName args
		targetContents'	<- readFile targetFile
		let targets	= (if lineByLine args then lines else (:[])) targetContents'

		parseTrees		<- (targets |+> parseWith targetFile ts (parser args)) |> zip targets

		parseTrees |+> ifJust (runRule ts) (symbol args)
		parseTrees |+> ifJust (runFunc ts) (function args)
		parseTrees |+> ifJust (runStepByStep ts) (stepByStep args)

		when noRules $ putStrLn "You didn't specify an action to perform. See -h"
		return parseTrees




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
		"\n# Proof weight: "++show (weight proof)++", proof depth: "++ show (depth proof) ++"\n\n"++toParsable proof++"\n\n\n"



		
runFunc		:: TypeSystem -> Name -> (String, ParseTree) -> IO ()
runFunc ts func (inp, pt)
 	= do	let pt'	= evalFunc ts func [pt]
		putStrLn $ "# "++show inp++" applied to "++func
		putStrLn $ toParsable pt'

runStepByStep	:: TypeSystem -> Name -> (String, ParseTree) -> IO ()
runStepByStep ts func (inp, pt)
	= do	putStrLn $ "# "++show inp++" applied repeatedly to "++func
		evalStar ts func pt 

evalStar	:: TypeSystem -> Name -> ParseTree -> IO ()
evalStar ts funcName pt	
	= do	putStrLn $ "\n " ++ toParsable pt
		let pt'	= evalFunc ts funcName [pt]
		when (pt' /= pt) $ evalStar ts funcName pt'
