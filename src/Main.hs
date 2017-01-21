module Main where

import TypeSystem
import Utils.ToString
import Utils.Utils
import Utils.ArgumentParser
import Graphs.Lattice 
import Utils.LatticeImage (terminalCS, whiteCS)
import Utils.ParseTreeImage

import TypeSystem.Parser.TargetLanguageParser
import TypeSystem.Parser.TypeSystemParser (parseTypeSystemFile)
import ParseTreeInterpreter.FunctionInterpreter
import ParseTreeInterpreter.RuleInterpreter
import Changer.ChangesParser
import SyntaxHighlighting.Highlighting

import System.Environment

import Control.Monad
import Control.Arrow ((&&&))

import Text.Parsec

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList, keys, toList)
import Data.List (intercalate, nub)
import Data.Monoid ((<>))
import Data.Hashable
import Options.Applicative

import AbstractInterpreter.Tools
import AbstractInterpreter.FullAnalysis
import AbstractInterpreter.RuleInterpreter
import AbstractInterpreter.Data

import Dynamize.Test

version	= ([0,1,9], "Total Language Analysis: Syntax (and fancy graphs!)")


main	:: IO ()
main	= void $ do	args	<- getArgs
			main' args


main'	:: [String] -> IO (TypeSystem, [(String, ParseTree)])
main' args 
	= do	parsedArgs	<- parseArgs version args
		when (rmConfig parsedArgs) (removeConfig >> putStrLn "# Config file removed")
		mainArgs parsedArgs
			


mainArgs	:: Args -> IO (TypeSystem, [(String, ParseTree)])
mainArgs (Args tsFile exampleFiles changeFiles dumbTS interpretAbstract interpretRulesAbstract iraSVG createSVG createHighlighting autoSaveTo _)
	= do	config	<- getConfig
		ts'	<- parseTypeSystemFile tsFile
		ts	<- either (error . show) return ts'
		check ts & either error return
		
		checkTS ts & either putStrLn return

		config'		<- mainSyntaxHighl config ts createHighlighting autoSaveTo
		config''	<- updateHighlightings config' ts
		when (config /= config'') $ writeConfig config''
	

		changedTs	
			<- changeFiles |> mainChanges & foldM (&) ts	:: IO TypeSystem

		when dumbTS $ putStrLn $ toParsable' (24::Int) changedTs

		when interpretAbstract $ void $
			get tsFunctions changedTs & keys |+> runFuncAbstract changedTs


		when interpretRulesAbstract $ void $ do
			get tsRules' changedTs & get rules & toList |+> runRuleAbstract changedTs
			abstractRuleSyntax changedTs



		iraSVG & ifJust (\path ->  do
			let l	= createRuleSyntax changedTs & fst & latticeAsSVG terminalCS
			writeFile path l)

		createSVG & ifJust (\pth -> do
			let l	= changedTs & get tsSyntax & latticeAsSVG terminalCS
			writeFile pth l)
		
		parseTrees <- exampleFiles |+> (`mainExFile` changedTs)
		return (changedTs , concat parseTrees)




runRuleAbstract	:: TypeSystem -> (Symbol, [Rule]) -> IO ()
runRuleAbstract ts (s, rules)
	= do	let analysises	= rules |> (get ruleName &&& interpretRule' ts)
		let text	= analysises ||>> toParsable 
					|> (\(nm, analysis) -> inHeader "" ("Analysis of rule "++ show nm) '-' analysis)
					& unlines
		let full	= analysises |> snd & concat & toParsable

		let relation	= findRelation ts s & fromJust & toParsable & (++"\n")
		let ruleNms	= rules |> get ruleName |> indent
		let help	= (relation : "Known rules are" : ruleNms) |> ("# "++) & unlines
	
		putStrLn $ inHeader "" ("Analysis for rules about "++inParens s) '=' help ++ full ++ text



abstractRuleSyntax	:: TypeSystem -> IO ()
abstractRuleSyntax ts	
	= let	(syntax', trivial)	= createRuleSyntax ts
		omitted		= "Following types were ommitted, as they turned out to be exactly their input type: \n"++
					(trivial & unlines & indent)
		omitted'	= omitted & lines |> ("# "++) & unlines
		helpMsg		= "# Use --irasvg [PATH] to create a subtyping of this syntax \n\n"
		 in
		putStrLn $ inHeader "" "Resulting syntax" '=' $  helpMsg ++ omitted' ++ "\n\n"
				 ++ toParsable syntax'

runFuncAbstract	:: TypeSystem -> Name -> IO ()
runFuncAbstract ts name
	= do	putStrLn $ inHeader "" ("Abstract interpretation of "++show name) '-' ""
		putStrLn $ toParsable $ interpretFunction ts name
		









mainSyntaxHighl	:: Config -> TypeSystem -> Maybe String -> Maybe String -> IO Config
mainSyntaxHighl config ts (Just parserRule) (Just targetSave)
	= do	let newConf	= ASH (get tsName ts) parserRule 0 targetSave
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
	= do	let currentState	= hash ((get tsSyntax ts & show) ++ (get tsStyle ts & show))
		let editState ash	= (get tsName ts == ashTsName ash) && (currentState /= ashTsHash ash)
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
	= do	let noRules	= all isNothing ([symbol, function, stepByStep, ptSvg] |> (\f -> f args))


		let targetFile	= fileName args
		targetContents'	<- readFile targetFile
		let targets	= (if lineByLine args then lines else (:[])) targetContents'

		parseTrees		<- (targets |+> parseWith targetFile ts (parser args)) |> zip targets

		parseTrees |+> ifJust' (runRule ts) (symbol args)
		parseTrees |+> ifJust' (runFunc ts) (function args)
		parseTrees |+> ifJust' (runStepByStep ts) (stepByStep args)
		
		parseTrees |> snd & mapi |+> ifJust' renderParseTree (ptSvg args)

		when noRules $ do
			putStrLn "# You didn't specify an action to perform, we'll just dump the parsetrees. See -h how to run functions"
			parseTrees |+> printDebug
			pass
		return parseTrees






parseWith	:: FilePath -> TypeSystem -> Name -> String -> IO ParseTree
parseWith file ts bnfRuleName str
	= do	let parser	= parse $ parseRule (get tsSyntax ts) bnfRuleName
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





printDebug	:: (String, ParseTree) -> IO ()
printDebug (inp, pt)
	= do	putStrLn $ "# "++show inp++" was parsed as:"
		putStrLn $ debug pt


renderParseTree	:: Name -> (Int, ParseTree) -> IO ()
renderParseTree nm (i, pt)
	= do	let fileName	= nm ++ "." ++ show i ++ ".svg"
		let conts	=  parseTreeSVG 1 terminalCS pt
		writeFile fileName conts

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
