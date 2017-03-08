 {-# LANGUAGE TemplateHaskell #-}
module PureMain where

import TypeSystem
import Prelude hiding (writeFile, putStrLn, readFile)

import Utils.ToString
import Utils.Utils
import Utils.ArgumentParser
import Graphs.Lattice 
import Utils.LatticeImage (terminalCS, whiteCS)
import SyntaxHighlighting.ParseTreeImage
import Utils.PureIO hiding (PureIO)

import AssetsHelper as Assets

import TypeSystem.Parser.TargetLanguageParser
import TypeSystem.Parser.TypeSystemParser (parseTypeSystem)
import TypeSystem.Parser.ParsingUtils (ws)

import ParseTreeInterpreter.FunctionInterpreter
import ParseTreeInterpreter.RuleInterpreter
import ParseTreeInterpreter.PropertyTester

import Changer.ChangesParser
import SyntaxHighlighting.AnsiPT
import qualified SyntaxHighlighting.AsHTML as HTML

import SyntaxHighlighting.Coloring

import AbstractInterpreter.AbstractInterpreter
import Dynamize.Dynamize

import Control.Monad
import Control.Arrow ((&&&))

import Text.Parsec
import Text.PrettyPrint.ANSI.Leijen (Doc, plain)

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList, keys, toList)
import qualified Data.Map as M
import Data.List (intercalate, nub, (\\))
import qualified Data.List as L
import Data.Monoid ((<>))
import Data.Hashable
import Data.Bifunctor (first)
import Options.Applicative


import Lens.Micro hiding ((&))
import Lens.Micro.TH






data RunConfig	= RunConfig
	{ _colorScheme	:: FullColoring
	, _rcTs		:: TypeSystem
	, _noMakeup	:: Bool}
makeLenses ''RunConfig

getTS		= getConfig' $ get rcTs
getFC		= getConfig' $ get colorScheme

defaultConfig	= RunConfig (error "No style set") (error "No typesystem loaded") False

type PureIO a	= PureIO' RunConfig a


mainPure	:: Args -> PureIO (FullColoring, TypeSystem)
mainPure args
	= do	checkInput args
		style		<- args & styleName & Assets.fetchStyle & liftEith
		let ascii	= noMakeupArg args
		withConfig' (set colorScheme style) $ withConfig' (set noMakeup ascii) $ do
			tsContents	<- readFile (tsFile args)
			ts		<- parseTypeSystem tsContents (Just $ tsFile args)
						& liftEith
			changedTs	<- foldM mainChange ts (changeFiles args)
			check   changedTs & inMsg "Error" & liftEith
			unless (noCheck args) (checkTS changedTs & isolateCheck)

			withConfig' (set rcTs changedTs) $ mainPureOn args changedTs
			fc	<- getConfig' $ get colorScheme
			return (fc, changedTs)


mainPureOn	:: Args -> TypeSystem -> PureIO ()
mainPureOn args ts
      = [ ioIf' dumpTS			$ putStrLn $ toParsable' (24::Int) ts
	, \args -> 			  exampleFiles args 	     |+> mainExFilePure   & void
	, ioIf' interpretAbstract	  (get tsFunctions ts & keys |+> runFuncAbstract  ts & void)
	, \args ->			  interpretFunctionAbs args  |+> runFuncAbstract  ts & void
	, ioIf' interpretRulesAbstract	$ abstractRuleSyntax (isJust $ iraSVG args) ts
	, \args -> 			  interpretRules args	     |+> runRuleAbstract' ts & void
	, ioIfJust' subtypingSVG	$ saveSubtypingSVG (get tsSyntax ts)
	, ioIfJust' iraSVG		$ saveSubtypingSVG (analyzeRelations ts & get raSyntax)
	, ioIf' (not . actionSpecified)	$ putStrLn " # Language file parsed. No action specified, see -h or --manual to specify other options"
	, ioIfJust' dynamizeArgs	  dynamizeTS
	] |+> (args &) & void




-------------------------------- ABSTRACT INTERPRETERS ----------------------------------------

runRuleAbstract'	:: TypeSystem -> Symbol -> PureIO ()
runRuleAbstract' ts symb
	= do	rules	<- liftEith $
				checkExists symb (get tsRules ts) $ 
					"No relation "++symb++" found"
		runRuleAbstract ts (symb, rules)
		


runRuleAbstract	:: TypeSystem -> (Symbol, [Rule]) -> PureIO ()
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
		putStrLn $ inHeader "" ("Analysis for rules about "++inParens s) '=' help ++ full ++ text



abstractRuleSyntax	:: Bool -> TypeSystem -> PureIO ()
abstractRuleSyntax irasvg ts	
	= do	putStrLn $ toParsable' ts $ analyzeRelations ts
		unless irasvg $ putStrLn "# Run --irasvg PATH.svg to generate a nice svg about the subtyping relationsships"

runFuncAbstract	:: TypeSystem -> Name -> PureIO ()
runFuncAbstract ts name
	= do	func		<- liftEith $ checkExists name (get tsFunctions ts) $ "No such function: "++name
		let analysis	= analyzeFunction' ts func
		putStrLn $ toParsable' (name, 24::Int, func) analysis
		

saveSubtypingSVG:: Syntax -> Name -> PureIO ()
saveSubtypingSVG s fp
	= do	let fp'	= if ".svg" `L.isSuffixOf` fp then fp else fp ++ ".svg"
		fc	<- getConfig' $ get colorScheme
		s & latticeAsSVG (toSVGColorScheme Nothing fc) & writeFile fp'



-------------------------------------- CHANGER ------------------------------------------------


mainChange	:: TypeSystem -> FilePath -> PureIO TypeSystem
mainChange ts filepath
	= do	contents	<- readFile filepath
		(changes, ts')	<- parseChanges ts contents (Just filepath)
						& liftEith
		return ts'






dynamizeTS	:: DynamizeArgs -> PureIO ()
dynamizeTS (DynamizeArgs rule error relsToAnalyze relsToAdd)
	= do	ts		<- getTS
		changes	<- dynamize' ts rule error relsToAnalyze relsToAdd
				& liftEith
		putStrLn $ toParsable' (16::Int) changes







------------------------------------------- EXAMPLE FILE HANDLER -------------------------------------------------



mainExFilePure	:: ExampleFile -> PureIO [(String, ParseTree)]
mainExFilePure args
	= isolateFailure [] $ 
	  do	ts		<- getTS
		let path	= fileName args
		contents	<- readFile path
		let inputs	= (if lineByLine args then filter (/= "") . lines else (:[])) contents
		parsed		<- mapi inputs |> parseWith path ts (parser args) |+> liftEith
		let parsed'	= zip inputs parsed
		handleExampleFile (parser args) args parsed'

		when (renderHTML args) $ do
			pts'	<- parseTargetLang' (get tsSyntax ts) (parser args) (True, False) (fileName args) (head inputs)
					& liftEith
			printPtHTML pts'

		return parsed'

parseWith	:: FilePath -> TypeSystem -> Name -> (Int, String) -> Either String ParseTree
parseWith file ts bnfRuleName (i, str)
	= parseTargetLang (get tsSyntax ts) bnfRuleName (file++" (line "++show i++")") str







handleExampleFile	:: TypeName -> ExampleFile -> [(String, ParseTree)] -> PureIO ()
handleExampleFile parsedWith exFile pts
	= 	[ ioIfJust' ruleSymbol	$ runRule `onAll'` pts
		, ioIfJust' function 	$ runFunc `onAll'` pts
		, ioIfJust' stepByStep	$ evalStar `onAll'` (pts |> snd)
		, ioIfJust' testProp 	$ testPropertyOn' (verbose exFile) parsedWith pts
		, ioIf' testAllProps 	$ testAllProperties (verbose exFile) parsedWith pts
		, ioIfJust' ptSvg	$ renderParseTree `onAll'` (	pts |> snd & mapi)
		, ioIf' (not . actionSpecified) 
					(pts |+> printPTDebug & void)
	
		] |+> (exFile &) & void


printPtHTML	:: ParseTreeA LocationInfo -> PureIO ()
printPtHTML pt
	= do	ts	<- getTS
		fc	<- getFC
		let rendered	= HTML.renderPT fc (get tsStyle ts) $ deAnnot pt
		putStrLn $ " # Parsed and rendered: "++(pt & get ptAnnot & toParsable)
		putStrLn rendered


unEscape	:: String -> String
unEscape []	= []
unEscape ('\\':'\\':str)
		= '\\':str
unEscape ('\\':'n':str)
		= '\n':str
unEscape (c:str)
		= c:unEscape str




testAllProperties	:: Bool -> TypeName -> [(String, ParseTree)] -> PureIO ()
testAllProperties verbose tp pts
	= do	ts	<- getTS
		get tsProps ts |+> testPropertyOn verbose tp pts & void



testPropertyOn'	:: Bool -> TypeName -> [(String, ParseTree)] -> Name -> PureIO ()
testPropertyOn' verbose tp pts nm
	= do	ts	<- getTS
		let propDict	= ts & get tsProps |> (get propName &&& id) & M.fromList
		property	<- checkExists nm propDict ("No property "++show nm++" found") & liftEith
		testPropertyOn verbose tp pts property
		



testPropertyOn	:: Bool -> TypeName -> [(String, ParseTree)] -> Property -> PureIO ()
testPropertyOn verbose tp pts property
	= do	ts	<- getTS
		let needed	= neededVars property
		let nm		= get propName property
		let errMsg	= "Properties tested against examples should have exactly one input, of type "++ tp ++" (as this is the used parser)"
					++", however, the property "++nm++" needs inputs "
					++"{"++ (needed |> (\(n, t) -> n ++ " :" ++ t) & intercalate ", ")  ++"}"
		unless (length needed == 1) $ fail errMsg
		let [(n, t)]	= needed
		unless (t == tp) $ fail errMsg


		let proofs	= pts |> testProperty ts property n
		let max		= show $ length pts
		let prepMsg inp
				= "Testing '"++nm++"' on input "++inp++" "
		let results	= proofs & if verbose then id else filter (not . snd)	-- if not verbose: hide success by default
		
		let results'	= results >>= fst
		let allClear	= results |> snd & and
		let msg		= if allClear then "Property "++nm++" holds for given examples"
					else "Property "++nm++" broken"
		-- the progress bar		
		putStrLn $ if length pts <= 1 then msg else fancyString' True msg proofs (pts |> fst |> prepMsg)

		results' |+> putStrLn
		pass


testProperty	:: TypeSystem -> Property -> Name -> (String, ParseTree) -> ([String], Bool)
testProperty ts property exprName (input, pt)
	= let	eithStrProp	= testPropOn ts property (M.singleton exprName (removeEmptyTokens pt, Nothing))
		in either
			(\fail	-> (["Property failed!", fail], False))
			(\proof -> (["Property successfull", toParsable' property proof], True))
			eithStrProp







runRule		:: Symbol -> (String, ParseTree) -> PureIO ()
runRule symbol (input, pt)
	= do	ts	<- getTS		
		let	proof	 = proofThat' ts symbol [removeEmptyTokens pt]	:: Either String Proof
		proof & showProofWithDepth input symbol & putStrLn




renderParseTree	:: Name -> (Int, ParseTree) -> PureIO ()
renderParseTree nm (i, pt)
	= do 	let nm'		= if ".svg" `L.isSuffixOf` nm then nm & reverse & drop 4 & reverse else nm
		let fileName	= nm' ++"_"++ show i ++ ".svg"
		fc		<- getConfig' $ get colorScheme 
		ts		<- getTS
		let conts	=  parseTreeSVG ts 1 fc pt
		writeFile fileName conts

runFunc		:: Name -> (String, ParseTree) -> PureIO ()
runFunc func (inp, pt)
 	= do	ts	<- getTS
		putStrLn $ "\n# "++show inp++" applied to "++func
		catch putStrLn $ do
			pt'	<- evalFunc ts func [removeEmptyTokens pt] & liftEith
			printPT pt'

runStepByStep	:: Name -> (String, ParseTree) -> PureIO ()
runStepByStep func (inp, pt)
	= do	putStrLn $ "# "++show inp++" applied repeatedly to "++func
		evalStar func $ removeEmptyTokens pt


evalStar	:: Name -> ParseTree -> PureIO ()
evalStar funcName pt	
	= do	printPT pt
		ts	<- getTS
		pt'	<- evalFunc ts funcName [pt] & liftEith
		if pt' /= pt then evalStar funcName pt' else pass


printPT		:: ParseTree -> PureIO ()
printPT pt
	= do	ts	<- getTS
		fc	<- getConfig' $ get colorScheme
		let ptDoc	= renderPT fc (get tsStyle ts) pt
		putDocLn' ptDoc

printPTDebug	:: (String, ParseTree) -> PureIO ()
printPTDebug (inp, pt)
	= do	ts	<- getTS
		putStrLn $ "# "++show inp++" was parsed as:"
		fc	<- getConfig' $ get colorScheme
		let ptDoc	= renderPTDebug fc (get tsStyle ts) pt
		putDocLn' ptDoc


putDocLn'	:: Doc -> PureIO ()
putDocLn' d
	= do	ascii	<- getConfig' $ get noMakeup
		let d'	= if ascii then plain d else d
		putDocLn d'
		
