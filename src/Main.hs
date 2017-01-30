{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.Map as M
import Data.List (intercalate, nub)
import Data.Monoid ((<>))
import Data.Hashable
import Data.Bifunctor (first)
import Options.Applicative

import AbstractInterpreter.Tools
import AbstractInterpreter.RelationAnalysis
import AbstractInterpreter.RuleInterpreter
import AbstractInterpreter.Data
import AbstractInterpreter.AbstractSet as AS

import Lens.Micro hiding ((&))
import Lens.Micro.TH


version	= ([0,1,10], "Total Language Analysis: Syntax (and fancy graphs!)")



data Output = Output 
		{ _files	:: [(String, String)]
		, _stdOut	:: [String]
		} deriving (Show, Ord, Eq, Read)

makeLenses ''Output

runOutput	:: Output -> IO ()
runOutput (Output files stdOut)
	= do	files |+> uncurry writeFile
		putStrLn $ unlines stdOut


main	:: IO ()
main	= void $ do	args	<- getArgs
			main' args


main'	:: [String] -> IO (TypeSystem, [(String, ParseTree)])
main' args 
	= do	parsedArgs	<- parseArgs version args
		mainArgs parsedArgs
			


mainArgs	:: Args -> IO (TypeSystem, [(String, ParseTree)])
mainArgs (Args tsFile exampleFiles changeFiles dumbTS interpretAbstract interpretRulesAbstract iraSVG createSVG)
	= do	ts'	<- parseTypeSystemFile tsFile
		ts	<- either (error . show) return ts'
		check ts & either error return
		
		checkTS ts & either putStrLn return

	

		changedTs	
			<- changeFiles |> mainChanges & foldM (&) ts	:: IO TypeSystem

		when dumbTS $ putStrLn $ toParsable' (24::Int) changedTs

		when interpretAbstract $ void $
			get tsFunctions changedTs & keys |+> runFuncAbstract changedTs


		when interpretRulesAbstract $ void $ do
			get tsRules' changedTs & get rules & toList |+> runRuleAbstract changedTs
			abstractRuleSyntax changedTs



		iraSVG & ifJust (\path ->  do
			let l	= analyzeRelations changedTs & get raSyntax & latticeAsSVG terminalCS
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
	= do	let ra	= analyzeRelations ts
		putStrLn $ toParsable' ts ra
		putStrLn "# Run --irasvg PATH.svg to generate a nice svg about the subtyping relationsships"

runFuncAbstract	:: TypeSystem -> Name -> IO ()
runFuncAbstract ts name
	= do	putStrLn $ inHeader "" ("Abstract interpretation of "++show name) '-' ""
		putStrLn $ toParsable $ interpretFunction ts name
		



mainChanges	:: String -> TypeSystem -> IO TypeSystem
mainChanges filepath ts
	= do	changes'	<- parseChangesFile ts filepath
		(changes,ts')	<- changes' & either (error . show) return
		return ts'






mainExFile	:: ExampleFile -> TypeSystem -> IO [(String, ParseTree)]
mainExFile args ts
		= do	contents	<- readFile $ fileName args
			case mainExFilePure args ts contents of
				Left errMsg	-> do	putStrLn $ "ERROR (no files written for )"++ fileName args ++": "++errMsg
							return []
				Right (pts, output)
						-> do	runOutput output
							return pts
		

mainExFilePure	:: ExampleFile -> TypeSystem -> String -> Either String ([(String, ParseTree)], Output)
mainExFilePure args ts targetContents
	= do	let noRules	= all isNothing ([symbol, function, stepByStep, ptSvg] |> (\f -> f args))
		let targetFile	= fileName args
		let targets	= (if lineByLine args then lines else (:[])) targetContents

		parseTrees	<- (targets |+> parseWith targetFile ts (parser args)) |> zip targets

		let files	= maybe [] (\fileName -> parseTrees |> snd & mapi |> renderParseTree fileName) (ptSvg args)	

		let rr		= parseTrees >>= ifJustS' [] (runRule ts) (symbol args)
		let rf		= parseTrees >>= ifJustS' [] (runFunc ts) (function args) 
		let sbs		= parseTrees >>= ifJustS' [] (runStepByStep ts) (stepByStep args) 
		let noRulesMsg	= if noRules then
					("# You didn't specify an action to perform, we'll just dump the parsetrees. See -h how to run functions":
					(parseTrees >>= printDebug))
					else []

		let output	= rr ++ rf ++ sbs ++ noRulesMsg
			
		return (parseTrees, Output files output)



ifJustS'			:: c -> (a -> b -> c) -> Maybe a -> b -> c
ifJustS' _ f (Just a) b		= f a b
ifJustS' def _ Nothing _	= def

ifJustS		= ifJustS' ""

parseWith	:: FilePath -> TypeSystem -> Name -> String -> Either String ParseTree
parseWith file ts bnfRuleName str
	= let 	parser	= parse $ parseRule (get tsSyntax ts) bnfRuleName
		parsed	= parser file str
		in
		first show $ parsed




runRule		:: TypeSystem -> Symbol -> (String, ParseTree) -> [String]
runRule ts symbol (input, pt)
	= let	proof		= proofThat' ts symbol [pt]	:: Either String Proof
		in
		proof & showProofWithDepth input symbol

showProofWithDepth		:: String -> Symbol -> Either String Proof -> [String]
showProofWithDepth input relation (Left str)	
	= ["# Could not apply relation "++relation++" to relation the input "++input++", because:",str]
showProofWithDepth input relation (Right proof)
	= ["# "++input++" applied to "++relation
		,"# Proof weight: "++show (weight proof)++", proof depth: "++ show (depth proof) 
		, ""
		, ""
		, toParsable proof, "", "", ""]





printDebug	:: (String, ParseTree) -> [String]
printDebug (inp, pt)
	=	["# "++show inp++" was parsed as:"
		, debug pt]


renderParseTree	:: Name -> (Int, ParseTree) -> (String, String)
renderParseTree nm (i, pt)
	= let 	fileName	= nm ++ "." ++ show i ++ ".svg"
		conts	=  parseTreeSVG 1 terminalCS pt
		in (fileName, conts)

runFunc		:: TypeSystem -> Name -> (String, ParseTree) -> [String]
runFunc ts func (inp, pt)
 	= let	pt'	= evalFunc ts func [pt]
		msg	= "# "++show inp++" applied to "++func
		in
		[msg, toParsable pt']

runStepByStep	:: TypeSystem -> Name -> (String, ParseTree) -> [String]
runStepByStep ts func (inp, pt)
	= let	msg	= "# "++show inp++" applied repeatedly to "++func
		in
		msg:evalStar ts func pt

evalStar	:: TypeSystem -> Name -> ParseTree -> [String]
evalStar ts funcName pt	
	= let	msg	= toParsable pt
		pt'	= evalFunc ts funcName [pt]
		msgs	= if (pt' /= pt) then evalStar ts funcName pt' else []
		in
		msg:msgs
