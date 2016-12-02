module Parser.TargetLanguageParser where

{-
This module interprets a BNF-AST datastructure to parse the target language to a Parsetree
-}

import Utils
import Parser.ParsingUtils

import Control.Arrow ((&&&))

import TypeSystem

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Map as M


------------------------ Syntax: Actually parsed stuff -------------------------

data ParseTree	= Token String	-- Contents
		| PtNumber Int
		| PtIdent Name
		| PtSeq [ParseTree]
		| RuleParse Name Int ParseTree -- causing rule and choice index (thus which option caused) + actual contents
	deriving (Eq, Ord, Show)



ptToExpr	:: (TypeName, Int) -> ParseTree -> Expression
ptToExpr mt (Token s)
		= MLiteral mt s
ptToExpr mt (PtNumber i)
		= MInt mt i
ptToExpr mt (PtIdent nm)
		= MIdentifier mt nm
ptToExpr mt (PtSeq pts)
		= pts |> ptToExpr mt & MSeq mt
ptToExpr _ (RuleParse mt i pt)
		= ptToExpr (mt, i) pt

parseRule	:: BNFRules -> Name -> Parser u Expression
parseRule rules nm
		= parseRule' rules nm |> ptToExpr (error "Should not be used")


parseRule'	:: BNFRules -> Name -> Parser u ParseTree
parseRule' rules nm
 | nm `M.notMember` rules	= fail $ "The rule "++nm++" is not defined in the syntax of your typesystem"
 | otherwise	= do	let choices	= zip (rules M.! nm) [0..]
			(i, pt)	<- parseChoice rules nm choices
			return $ RuleParse nm i pt
			



parseChoice	:: BNFRules -> Name -> [(BNFAST, a)] -> Parser u (a, ParseTree)
parseChoice _ name []
	= fail $ "Could not parse expression of the form "++name
parseChoice rules name ((bnf,a): rest)
	= try (do	parsed	<- parsePart' rules bnf
			return (a, parsed))
	   <|>  parseChoice rules name rest



parsePart'	:: BNFRules -> BNFAST -> Parser u ParseTree
parsePart' rules bnf
		= ws >> parsePart rules bnf

parsePart	:: BNFRules -> BNFAST -> Parser u ParseTree
parsePart _ (Literal str)
		= string str |> Token
parsePart _ Identifier
		= identifier |> PtIdent
parsePart _ Number
		= number |> PtNumber
parsePart rules (Seq bnfs)
		= bnfs |+> parsePart' rules |> PtSeq 
parsePart rules (BNFRuleCall nm)
		= parseRule' rules nm

