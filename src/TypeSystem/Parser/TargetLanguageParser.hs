module TypeSystem.Parser.TargetLanguageParser where

{-
This module interprets a BNF-AST datastructure to parse the target language to a Parsetree
-}

import Utils.Utils
import TypeSystem.Parser.ParsingUtils

import Control.Arrow ((&&&))

import TypeSystem.TypeSystemData

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Bifunctor as BF
import qualified Data.Map as M


data WSMode'	= Capture WSMode | NoCapture WSMode

wsModeCons (Capture _)	= Capture
wsModeCons (NoCapture _)	= NoCapture

wsModeActual (Capture wsmode)	= wsmode
wsModeActual (NoCapture wsmode)	= wsmode

------------------------ Syntax: Actually parsed stuff -------------------------

parseTargetLang	:: Syntax -> TypeName -> FilePath -> String -> Either String ParseTree
parseTargetLang s tp fp inp
	= parseTargetLang' s tp (False, True) fp inp |> deAnnot

parseTargetLang' :: Syntax -> TypeName -> (Bool, Bool) -> FilePath -> String -> Either String (ParseTreeA LocationInfo)
parseTargetLang' s tp (capture, requireEOF) fp inp
	= do	parsed	<- runParserT (parseSyntax s capture tp <* (if requireEOF then ws >> eof else pass)) () fp inp
		parsed & BF.first show

parseSyntax	:: Syntax -> Bool -> Name -> Parser u ParseTreeLi
parseSyntax syntax capture nm
	= let 	wsmode'	= if capture then Capture IgnoreWS else NoCapture IgnoreWS  in
		parseSyntax' syntax nm wsmode'


parseSyntax'	:: Syntax -> Name -> WSMode' -> Parser u ParseTreeLi
parseSyntax' bnf@(BNFRules rules wsModes group _ _) nm wsModeParent
 | nm `M.notMember` rules	
		= fail $ "The BNF-syntax-rule "++nm++" is not defined in the syntax of your typesystem. Try one of "++show (bnfNames bnf)
 | otherwise	= do	let choices	= zip (rules M.! nm) [0..]
			let wsMode	= wsModes M.! nm
			let doGroup 	= group M.! nm
			let captureMode	= wsModeCons wsModeParent
			let wsModeParent'
					= wsModeActual wsModeParent
			let newWSMode	= captureMode $ strictest wsMode (enterRule wsModeParent')

			pt	<- parseChoice bnf nm newWSMode choices 
			let (a, minf, flat)	= flatten pt
			return $ if doGroup then
				MLiteral a minf flat
				else pt
			

parseChoice	:: Syntax -> Name -> WSMode' -> [(BNF, Int)] -> Parser u ParseTreeLi
parseChoice _ name _ []
	= fail $ "Could not parse expression of the form "++name
parseChoice rules name wsMode ((bnf,i): rest)
	= try (parsePart' rules (name, i) wsMode bnf)
	   <|>  parseChoice rules name wsMode rest



parsePart'	:: Syntax -> (TypeName, Int) -> WSMode' -> BNF -> Parser u ParseTreeLi
parsePart' rules pt wsMode bnf
		= do	start		<- sourcePos
			(wsPt', li)	<- (parseWS' wsMode ||>> MLiteral () ("ws", 0) ) & locInfoFor
			pt	<- parsePart rules pt wsMode bnf
			end	<- sourcePos
			let wsPt	= wsPt' |> annot li 
						|> (\wsPt' -> PtSeq (locationInfo start end) ("ws", 1) [wsPt', pt])
			return $ fromMaybe pt wsPt

parsePart	:: Syntax -> (TypeName, Int) -> WSMode' -> BNF -> Parser u ParseTreeLi
parsePart rules tp wsMode (BNFSeq [bnf])
		= parsePart rules tp wsMode bnf
parsePart rules tp wsMode (BNFSeq (bnf:bnfs))
		= do	start	<- sourcePos
			head	<- parsePart rules tp wsMode bnf
			tail	<- bnfs |+> parsePart' rules tp wsMode 
			end	<- sourcePos
			return $ PtSeq (locationInfo start end) tp $ head:tail
parsePart _ tp _ (BNFRuleCall "Number") -- TODO dehardcode this
		= annotLi (number |> MInt () ("Number", 0))
parsePart rules _ wsMode bnf@(BNFRuleCall nm)
 | isBuiltin bnf
		= do	let parser	= getParserForBuiltin bnf
			annotLi (parser |> MLiteral () (nm, 0))
 | otherwise
		= parseSyntax' rules nm wsMode
parsePart _ tp _ (Literal str)
		= annotLi (string str |> MLiteral () tp)


annotLi		:: Parser u ParseTree -> Parser u ParseTreeLi
annotLi p = do	(pt, li)	<- locInfoFor p
		pt & annot li & return 

locInfoFor	:: Parser u a -> Parser u (a, LocationInfo)
locInfoFor p 
	= do	start	<- sourcePos
		a	<- p
		end	<- sourcePos
		let li	= locationInfo start end
		return (a, li)

locationInfo	:: SourcePos -> SourcePos -> LocationInfo
locationInfo start end
		= LocationInfo (sourceLine start) (sourceColumn start)
				(sourceLine end) (sourceColumn end)

parseWS'	:: WSMode' -> Parser u (Maybe String)
parseWS' (Capture mode)
		= parseWS mode
parseWS' (NoCapture mode)
		= parseWS mode >> return Nothing

parseWS		:: WSMode -> Parser u (Maybe String)
parseWS IgnoreWS	= ws |> Just
parseWS	_		= return Nothing


