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
import SyntaxHighlighting.Highlighting

import System.Environment

import Control.Monad

import Text.Parsec

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList)
import Data.List (intercalate, nub)
import Data.Monoid ((<>))
import Data.Hashable
import Options.Applicative



t	= main' ["../Examples/STFL.typesystem", "--ch", "e", "--ash", "/home/pietervdvn/.local/share/gtksourceview-3.0/language-specs"] >> return ()


version	= ([0,1,2], "Spaced Language (Re)Factory")


main	:: IO ()
main	= void $ do	args	<- getArgs
			main' args


main'	:: [String] -> IO (TypeSystem, [(String, ParseTree)])
main' args 
	= do	parsedArgs	<- parseArgs version args
		when (rmConfig parsedArgs) (removeConfig >> putStrLn "# Config file removed")
		mainArgs parsedArgs
			


mainArgs	:: Args -> IO (TypeSystem, [(String, ParseTree)])
mainArgs (Args tsFile exampleFiles changeFiles dumbTS createHighlighting autoSaveTo _)
	= do	config	<- getConfig
		ts'	<- parseTypeSystemFile tsFile
		ts	<- either (error . show) return ts'
		check ts & either error return

		config'		<- mainSyntaxHighl config ts createHighlighting autoSaveTo
		config''	<- updateHighlightings config' ts
		when (config /= config'') $ writeConfig config''
	

		changedTs	
			<- changeFiles |> mainChanges & foldM (&) ts	:: IO TypeSystem

		when dumbTS $ putStrLn $ toParsable changedTs
		
		parseTrees <- exampleFiles |+> (`mainExFile` changedTs)
		return (changedTs , concat parseTrees)

mainSyntaxHighl	:: Config -> TypeSystem -> Maybe String -> Maybe String -> IO Config
mainSyntaxHighl config ts (Just parserRule) (Just targetSave)
	= do	let newConf	= ASH (tsName ts) parserRule 0 targetSave
		let config'	= config{autoSyntaxes = autoSyntaxes config ++ [newConf]}
		putStrLn "# Auto syntax highlighting added"
		return config'
mainSyntaxHighl _ _ Nothing (Just targetSave)
	= error "You want to add a new syntax highlighting rule; but no '--create-highlighting PARSER-RULE' flag was specified."
mainSyntaxHighl c ts (Just parserRule) _
	= do	putStrLn $ toParsable $ createStyleForTypeSystem ts parserRule
		return c
mainSyntaxHighl c _ _ _
	= return c


updateHighlightings 	:: Config -> TypeSystem -> IO Config
updateHighlightings config ts
	= do	let currentState	= hash ((tsSyntax ts & show) ++ (tsStyle ts & show))
		let editState ash	= (tsName ts == ashTsName ash) && (currentState /= ashTsHash ash)
		autoSyntaxes' <- autoSyntaxes config |+> (\ash -> 
			if not $ editState ash then return ash else do
				let fp	= ashSaveTo ash ++ "/" ++ ashTsName ash ++ ".lang"
				let contents	= toParsable $ createStyleForTypeSystem ts (ashRuleName ash)
				putStrLn $ "# Updated syntax highlighting. You might want to restart your editor for changes to apply. Updated path: "++fp
				writeFile fp contents
				return ash{ashTsHash = currentState}
			)
		
		return config{autoSyntaxes = nub autoSyntaxes'}
		


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

		parseTrees |+> ifJust' (runRule ts) (symbol args)
		parseTrees |+> ifJust' (runFunc ts) (function args)
		parseTrees |+> ifJust' (runStepByStep ts) (stepByStep args)

		when noRules $ putStrLn "You didn't specify an action to perform. See -h"
		return parseTrees




parseWith	:: FilePath -> TypeSystem -> Name -> String -> IO ParseTree
parseWith file ts bnfRuleName str
	= do	let parser	= parse $ parseRule (tsSyntax ts) bnfRuleName
		let parsed	= parser file str
		either (error . show) return parsed




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
