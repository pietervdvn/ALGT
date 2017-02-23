module TypeSystem.Parser.TargetLanguageParser where

{-
This module interprets a BNF-AST datastructure to parse the target language to a Parsetree
-}

import Utils.Utils
import TypeSystem.Parser.ParsingUtils
import TypeSystem.Parser.BNFParser (bnfLiteral)

import Control.Arrow ((&&&))

import TypeSystem

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Bifunctor as BF
import qualified Data.Map as M


------------------------ Syntax: Actually parsed stuff -------------------------

parseTargetLang	:: Syntax -> TypeName -> FilePath -> String -> Either String ParseTree
parseTargetLang s tp fp inp
	= do	parsed	<- runParserT (parseSyntax s tp <* ws <* eof) () fp inp
		parsed & BF.first show

parseSyntax	:: Syntax -> Name -> Parser u ParseTree
parseSyntax syntax nm
	= parseSyntax' syntax nm IgnoreWS


parseSyntax'	:: Syntax -> Name -> WSMode -> Parser u ParseTree
parseSyntax' bnf@(BNFRules rules wsModes _) nm wsModeParent
 | nm `M.notMember` rules	
		= fail $ "The BNF-syntax-rule "++nm++" is not defined in the syntax of your typesystem. Try one of "++show (bnfNames bnf)
 | otherwise	= do	let choices	= zip (rules M.! nm) [0..]
			let wsMode	= wsModes M.! nm
			parseChoice bnf nm (strictest wsMode (enterRule wsModeParent)) choices
			



parseChoice	:: Syntax -> Name -> WSMode -> [(BNF, Int)] -> Parser u ParseTree
parseChoice _ name _ []
	= fail $ "Could not parse expression of the form "++name
parseChoice rules name wsMode ((bnf,i): rest)
	= try (parsePart rules (name, i) wsMode bnf)
	   <|>  parseChoice rules name wsMode rest



parsePart'	:: Syntax -> (TypeName, Int) -> WSMode -> BNF -> Parser u ParseTree
parsePart' rules pt wsMode bnf
		= parseWS wsMode >> parsePart rules pt wsMode bnf

parsePart	:: Syntax -> (TypeName, Int) -> WSMode -> BNF -> Parser u ParseTree
parsePart _ tp _ (Literal str)
		= string str |> MLiteral tp
parsePart _ tp _ Identifier
		= identifier |> MIdentifier tp
parsePart _ tp _ Number
		= number |> MInt tp
parsePart _ tp _ Lower
		= oneOf lowers |> (:[]) |> MLiteral tp
parsePart _ tp _ Upper
		= oneOf uppers |> (:[]) |> MLiteral tp
parsePart _ tp _ Digit
		= oneOf digits |> (:[]) |> MLiteral tp
parsePart _ tp _ String
		= bnfLiteral |> MLiteral tp
parsePart rules tp wsMode (BNFSeq [bnf])
		= parsePart rules tp wsMode bnf
parsePart rules tp wsMode (BNFSeq (bnf:bnfs))
		= do	head	<- parsePart rules tp wsMode bnf
			tail	<- bnfs |+> parsePart' rules tp wsMode 
			return $ PtSeq tp $ head:tail
parsePart rules _ wsMode (BNFRuleCall nm)
		= parseSyntax rules nm




parseWS		:: WSMode -> Parser u String
parseWS IgnoreWS	= ws
parseWS	_		= return ""


