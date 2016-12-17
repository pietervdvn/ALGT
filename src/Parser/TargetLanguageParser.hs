module Parser.TargetLanguageParser where

{-
This module interprets a BNF-AST datastructure to parse the target language to a Parsetree
-}

import Utils.Utils
import Parser.ParsingUtils

import Control.Arrow ((&&&))

import TypeSystem

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Map as M


------------------------ Syntax: Actually parsed stuff -------------------------

parseRule	:: Syntax -> Name -> Parser u ParseTree
parseRule bnf@(BNFRules rules) nm
 | nm `M.notMember` rules	
		= fail $ "The BNF-syntax-rule "++nm++" is not defined in the syntax of your typesystem. Try one of "++show (bnfNames bnf)
 | otherwise	= do	let choices	= zip (rules M.! nm) [0..]
			parseChoice bnf nm choices
			



parseChoice	:: Syntax -> Name -> [(BNF, Int)] -> Parser u ParseTree
parseChoice _ name []
	= fail $ "Could not parse expression of the form "++name
parseChoice rules name ((bnf,i): rest)
	= try (parsePart' rules (name, i) bnf)
	   <|>  parseChoice rules name rest



parsePart'	:: Syntax -> (TypeName, Int) -> BNF -> Parser u ParseTree
parsePart' rules pt bnf
		= ws >> parsePart rules pt bnf

parsePart	:: Syntax -> (TypeName, Int) -> BNF -> Parser u ParseTree
parsePart _ tp (Literal str)
		= string str |> MLiteral tp
parsePart _ tp Identifier
		= identifier |> MIdentifier tp
parsePart _ tp Number
		= number |> MInt tp
parsePart rules tp (BNFSeq bnfs)
		= bnfs |+> parsePart' rules tp |> PtSeq tp
parsePart rules _ (BNFRuleCall nm)
		= parseRule rules nm

