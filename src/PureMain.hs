module PureMain where

import TypeSystem
import Prelude hiding (writeFile, putStrLn, readFile)

import Utils.ToString
import Utils.Utils
import Utils.ArgumentParser
import Graphs.Lattice 
import Utils.LatticeImage (terminalCS, whiteCS)
import Utils.ParseTreeImage
import Utils.PureIO

import TypeSystem.Parser.TargetLanguageParser
import TypeSystem.Parser.TypeSystemParser (parseTypeSystem)
import TypeSystem.Parser.ParsingUtils (ws)

import ParseTreeInterpreter.FunctionInterpreter
import ParseTreeInterpreter.RuleInterpreter
import ParseTreeInterpreter.PropertyTester

import Changer.ChangesParser
import SyntaxHighlighting.Highlighting

import AbstractInterpreter.AbstractInterpreter
import Dynamize.Dynamize

import Control.Monad
import Control.Arrow ((&&&))

import Text.Parsec

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






svgColors	= whiteCS


mainPure	:: Args -> PureIO TypeSystem
mainPure args
	= do	checkInput args
		tsContents	<- readFile (tsFile args)
		ts		<- parseTypeSystem tsContents (Just $ tsFile args)
					& first show & liftEith
		changedTs	<- foldM mainChange ts (changeFiles args)
		check   changedTs & inMsg "Error" & liftEith
		checkTS changedTs & isolateCheck

		mainPureOn args changedTs
		return changedTs


mainPureOn	:: Args -> TypeSystem -> PureIO ()
mainPureOn args ts
      = [ ioIf' dumpTS			$ putStrLn $ toParsable' (24::Int) ts
	, \args -> 			  exampleFiles args 	     |+> mainExFilePure ts   & void
	, ioIf' interpretAbstract	  (get tsFunctions ts & keys |+> runFuncAbstract  ts & void)
	, \args ->			  interpretFunctionAbs args  |+> runFuncAbstract  ts & void
	, ioIf' interpretRulesAbstract	$ abstractRuleSyntax (isJust $ iraSVG args) ts
	, \args -> 			  interpretRules args	     |+> runRuleAbstract' ts & void
	, ioIfJust' subtypingSVG	$ saveSubtypingSVG (get tsSyntax ts)
	, ioIfJust' iraSVG		$ saveSubtypingSVG (analyzeRelations ts & get raSyntax)
	, ioIf' (not . actionSpecified)	$ putStrLn " # Language file parsed. No action specified, see -h or --manual to specify other options"
	, ioIfJust' dynamizeArgs	$ dynamizeTS ts
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
	= let	fp'	= if ".svg" `L.isSuffixOf` fp then fp else fp ++ ".svg"
		in
		s & latticeAsSVG svgColors & writeFile fp'



-------------------------------------- CHANGER ------------------------------------------------



mainChange	:: TypeSystem -> FilePath -> PureIO TypeSystem
mainChange ts filepath
	= do	contents	<- readFile filepath
		(changes, ts')	<- parseChanges ts contents (Just filepath)
						& first show & liftEith
		return ts'






dynamizeTS	:: TypeSystem -> DynamizeArgs -> PureIO ()
dynamizeTS ts (DynamizeArgs rule error relsToAnalyze relsToAdd)
	= do	changes	<- dynamize' ts rule error relsToAnalyze relsToAdd
				& liftEith
		putStrLn $ toParsable' (16::Int) changes







------------------------------------------- EXAMPLE FILE HANDLER -------------------------------------------------



mainExFilePure	:: TypeSystem -> ExampleFile -> PureIO [(String, ParseTree)]
mainExFilePure ts args
	= isolateFailure [] $ 
	  do	let path	= fileName args
		contents	<- readFile path
		let inputs	= (if lineByLine args then filter (/= "") . lines else (:[])) contents
		parsed		<- inputs |> parseWith path ts (parser args) |+> liftEith
		let parsed'	= zip inputs parsed
		handleExampleFile ts (parser args) args parsed'
		return parsed'

parseWith	:: FilePath -> TypeSystem -> Name -> String -> Either String ParseTree
parseWith file ts bnfRuleName str
	= let 	parser	= parse (parseSyntax (get tsSyntax ts) bnfRuleName <* ws <* eof)
		parsed	= parser file str
		in
		first show parsed







handleExampleFile	:: TypeSystem -> TypeName -> ExampleFile -> [(String, ParseTree)] -> PureIO ()
handleExampleFile ts parsedWith exFile pts
	= 	[ ioIfJust' ruleSymbol	$ runRule ts `onAll'` pts
		, ioIfJust' function 	$ runFunc ts `onAll'` pts
		, ioIfJust' stepByStep	$ evalStar ts `onAll'` (pts |> snd)
		, ioIfJust' testProp 	$ testPropertyOn' (verbose exFile) ts parsedWith pts
		, ioIf' testAllProps 	$ testAllProperties (verbose exFile) ts parsedWith pts
		, ioIfJust' ptSvg	$ renderParseTree `onAll'` (	pts |> snd & mapi)
		, ioIf' (not . actionSpecified) 
					(pts |+> printDebug & void)
		] |+> (exFile &) & void




testAllProperties	:: Bool -> TypeSystem -> TypeName -> [(String, ParseTree)] -> PureIO ()
testAllProperties verbose ts tp pts
	= get tsProps ts |+> testPropertyOn verbose ts tp pts & void



testPropertyOn'	:: Bool -> TypeSystem -> TypeName -> [(String, ParseTree)] -> Name -> PureIO ()
testPropertyOn' verbose ts tp pts nm
	= do	let propDict	= ts & get tsProps |> (get propName &&& id) & M.fromList
		property	<- checkExists nm propDict ("No property "++show nm++" found") & liftEith
		testPropertyOn verbose ts tp pts property
		



testPropertyOn	:: Bool -> TypeSystem -> TypeName -> [(String, ParseTree)] -> Property -> PureIO ()
testPropertyOn verbose ts tp pts property
	= do	let needed	= neededVars property
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
	= let 	eithStrProp	= testPropOn ts property (M.singleton exprName (pt, Nothing))
		in
		either
			(\fail	-> (["Property failed!", fail], False))
			(\proof -> (["Property successfull", toParsable' property proof], True))
			eithStrProp







runRule		:: TypeSystem -> Symbol -> (String, ParseTree) -> PureIO ()
runRule ts symbol (input, pt)
	= let	proof		= proofThat' ts symbol [pt]	:: Either String Proof
		in
		proof & showProofWithDepth input symbol & putStrLn


printDebug	:: (String, ParseTree) -> PureIO ()
printDebug (inp, pt)
	= do	putStrLn $ "# "++show inp++" was parsed as:"
		putStrLn $ debug pt


renderParseTree	:: Name -> (Int, ParseTree) -> PureIO ()
renderParseTree nm (i, pt)
	= do 	let nm'		= if ".svg" `L.isSuffixOf` nm then nm & reverse & drop 4 & reverse else nm
		let fileName	= nm' ++"_"++ show i ++ ".svg"
		let conts	=  parseTreeSVG 1 svgColors pt
		writeFile fileName conts

runFunc		:: TypeSystem -> Name -> (String, ParseTree) -> PureIO ()
runFunc ts func (inp, pt)
 	= do	pt'	<- evalFunc ts func [pt] & liftEith
		putStrLn $ "\n# "++show inp++" applied to "++func
		putStrLn $ toParsable pt'

runStepByStep	:: TypeSystem -> Name -> (String, ParseTree) -> PureIO ()
runStepByStep ts func (inp, pt)
	= do	putStrLn $ "# "++show inp++" applied repeatedly to "++func
		evalStar ts func pt


evalStar	:: TypeSystem -> Name -> ParseTree -> PureIO ()
evalStar ts funcName pt	
	= do	putStrLn $ toParsable pt
		pt'	<- evalFunc ts funcName [pt] & liftEith
		if pt' /= pt then evalStar ts funcName pt' else pass



