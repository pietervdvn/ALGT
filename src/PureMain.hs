{-# LANGUAGE TemplateHaskell #-}
module PureMain where

import TypeSystem
import Utils.ToString
import Utils.Utils
import Utils.ArgumentParser
import Graphs.Lattice 
import Utils.LatticeImage (terminalCS, whiteCS)
import Utils.ParseTreeImage

import TypeSystem.Parser.TargetLanguageParser
import TypeSystem.Parser.TypeSystemParser (parseTypeSystem)
import ParseTreeInterpreter.FunctionInterpreter
import ParseTreeInterpreter.RuleInterpreter
import Changer.ChangesParser
import SyntaxHighlighting.Highlighting

import Control.Monad
import Control.Arrow ((&&&))

import Text.Parsec

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList, keys, toList)
import qualified Data.Map as M
import Data.List (intercalate, nub, (\\))
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


version	= ([0,1,11], "Total Language Test")



data Output = Output 
		{ _files	:: [(String, String)]
		, _stdOut	:: [String]
		} deriving (Show, Ord, Eq, Read)
makeLenses ''Output
instance Monoid Output where
	mempty	= emptyOutput
	mappend	(Output f1 o1) (Output f2 o2)
		= Output (f1 ++ f2) (o1 ++ o2) 

emptyOutput	= Output [] []

runOutput	:: Output -> IO ()
runOutput (Output files stdOut)
	= do	files |+> uncurry writeFile
		putStrLn $ unlines stdOut

type Input	= Map FilePath String

runInput	:: (NeedsFiles a) => a -> IO Input
runInput fn	=  filesNeeded fn |> (id &&& readFile) |+> sndEffect |> M.fromList


checkInput	:: (NeedsFiles a) => a -> Input -> Either [String] ()
checkInput fn inp
		=  let	files	= filesNeeded fn
			found	= inp & M.keys
			missing	= files \\ found
			in
			if null missing then return ()
				else Left missing

runIO'		:: (NeedsFiles a) => a -> (Input -> Either String (x, Output)) -> IO x
runIO' needsFiles f
	= do	inp	<- runInput needsFiles
		case f inp of
			Left msg	-> error msg
			Right (x, output)
					-> do	runOutput output
						return x

runIO		:: (NeedsFiles a) => a -> (Input -> Either String Output) -> IO ()
runIO nf f	= let	f' inp	= f inp |> (const () &&& id) in
			runIO' nf f'

runIOMute	::  (NeedsFiles a) => a -> (Input -> Either String x) -> IO x
runIOMute nf f	= let	f' inp	= f inp |> (id &&& const emptyOutput) in
			runIO' nf f'


isolateFailure'	:: x -> Either String (x, Output) -> (x, Output)
isolateFailure' x (Left msg)
		= (x, Output [] [msg])
isolateFailure' _ (Right res)
		= res




isolateFailure	:: Either String Output -> Output
isolateFailure v
		= isolateFailure' () (v |> (const () &&& id)) & snd

isolateCheck	:: Either String () -> Output
isolateCheck (Left msg)
		= Output [] [msg]
isolateCheck _	= emptyOutput




main'	:: IO () -> [String] -> IO (TypeSystem, [(String, ParseTree)])
main' testAll args 
	= do	parsedArgs	<- parseArgs version args
		when (runTests parsedArgs) testAll
		runIO' parsedArgs (mainArgs parsedArgs)
			


mainArgs	:: Args -> Input -> Either String ((TypeSystem, [(String, ParseTree)]), Output)
mainArgs args@(Args tsFile exampleFiles changeFiles dumpTS interpretAbstract interpretRulesAbstract iraSVG createSVG _) input
	= do	checkInput args input & first (\missing -> error $ "MISSING FILES FOR TESTCASE "++showComma missing)
		let tsContents	= input M.! tsFile
		ts		<- parseTypeSystem tsContents (Just tsFile)
					& first show
		check ts
		let warnings	= checkTS ts & isolateCheck

		changedTs	<- mainChanges ts changeFiles input	:: Either String TypeSystem

		let funcAnalysis
				= get tsFunctions changedTs & keys |> runFuncAbstract changedTs

		let ruleAnalysis
				= (get tsRules' changedTs & get rules & toList |> runRuleAbstract changedTs)
				++ [abstractRuleSyntax changedTs]

		let output    =	[ whenL dumpTS [toParsable' (24::Int) changedTs]
				, whenL interpretAbstract funcAnalysis
				, whenL interpretRulesAbstract ruleAnalysis] & concat


		let iraSVGFile	= (iraSVG, analyzeRelations changedTs & get raSyntax & latticeAsSVG terminalCS)
		let createSVGFile
				= (createSVG, changedTs & get tsSyntax & latticeAsSVG terminalCS)
		let files	= [iraSVGFile, createSVGFile] |> fstEffect & catMaybes
		
		let (parseTrees, outputs)
			 	= exampleFiles	|> (\exF -> mainExFilePure changedTs exF input)
						|> isolateFailure' []
						& unzip
		let out		= warnings:Output files output : outputs
		return ((changedTs , concat parseTrees), mconcat out)




runRuleAbstract	:: TypeSystem -> (Symbol, [Rule]) -> String
runRuleAbstract ts (s, rules)
	= let	analysises	= rules |> (get ruleName &&& interpretRule' ts)
		text		= analysises ||>> toParsable 
					|> (\(nm, analysis) -> inHeader "" ("Analysis of rule "++ show nm) '-' analysis)
					& unlines
		full		= analysises |> snd & concat & toParsable

		relation	= findRelation ts s & fromJust & toParsable & (++"\n")
		ruleNms		= rules |> get ruleName |> indent
		help		= (relation : "Known rules are" : ruleNms) |> ("# "++) & unlines
		in
		inHeader "" ("Analysis for rules about "++inParens s) '=' help ++ full ++ text



abstractRuleSyntax	:: TypeSystem -> String
abstractRuleSyntax ts	
	= let ra	= analyzeRelations ts in
		toParsable' ts ra ++ "\n# Run --irasvg PATH.svg to generate a nice svg about the subtyping relationsships"

runFuncAbstract	:: TypeSystem -> Name -> String
runFuncAbstract ts name
	= inHeader "" ("Abstract interpretation of "++show name) '-' $ toParsable $ interpretFunction ts name
		


mainChanges	:: TypeSystem -> [String] -> Input -> Either String TypeSystem
mainChanges ts [] _	= return ts
mainChanges ts (file:files) inp
	= do	ts'	<- mainChange file ts inp
		mainChanges ts' files inp 


mainChange	:: String -> TypeSystem -> Input -> Either String TypeSystem
mainChange filepath ts input
	= do	let 	contents	= input M.! filepath
		(changes, ts')		<- parseChanges ts contents (Just filepath)
						& first show
		return ts'



mainExFilePure	:: TypeSystem -> ExampleFile -> Input -> Either String ([(String, ParseTree)], Output)
mainExFilePure ts args input
	= do	let targetFile	= fileName args
		let fileContent	= input M.! fileName args
		let noRules	= all isNothing ([symbol, function, stepByStep, ptSvg] |> (\f -> f args))
		let targets	= (if lineByLine args then lines else (:[])) fileContent

		parseTrees	<- (targets |+> parseWith targetFile ts (parser args)) |> zip targets

		let files	= maybe [] (\fileName -> parseTrees |> snd & mapi |> renderParseTree fileName) (ptSvg args)	

		let rr		= parseTrees >>= ifJustS' [] (runRule ts) (symbol args)		:: [String]
		let rf		= parseTrees |> ifJustS' (Right []) (runFunc ts) (function args)	:: [Either String [String]]
		let sbs		= parseTrees |> ifJustS' (Right []) (runStepByStep ts) (stepByStep args) :: [Either String [String]]
		let noRulesMsg	= if noRules then
					("# You didn't specify an action to perform, we'll just dump the parsetrees. See -h how to run functions":
					(parseTrees >>= printDebug))
					else []

		let output'	= (rf ++ sbs) >>= either (:[]) id	:: [String]
		let output	= rr ++ noRulesMsg
			
		return (parseTrees, Output files output)



ifJustS'			:: c -> (a -> b -> c) -> Maybe a -> b -> c
ifJustS' _ f (Just a) b		= f a b
ifJustS' def _ Nothing _	= def

ifJustS		= ifJustS' ""


whenL		:: Bool -> [a] -> [a]
whenL False _	= []
whenL True as	= as



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

runFunc		:: TypeSystem -> Name -> (String, ParseTree) -> Either String [String]
runFunc ts func (inp, pt)
 	= do	pt'	<- evalFunc ts func [pt]
		let msg	= "# "++show inp++" applied to "++func
		return [msg, toParsable pt']

runStepByStep	:: TypeSystem -> Name -> (String, ParseTree) -> Either String [String]
runStepByStep ts func (inp, pt)
	= do	let	msg	= "# "++show inp++" applied repeatedly to "++func
		msgs	<- evalStar ts func pt
		return $ msg:msgs

evalStar	:: TypeSystem -> Name -> ParseTree -> Either String [String]
evalStar ts funcName pt	
	= do	let msg	= toParsable pt
		pt'	<- evalFunc ts funcName [pt]
		msgs	<- if (pt' /= pt) then evalStar ts funcName pt' else return []
		return $ msg:msgs
