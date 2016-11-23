module Parser.MetaParser where

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
		| PtSeq [ParseTree]
		| RuleParse Name Int ParseTree -- causing rule and choice index (thus which option caused) + actual contents
	deriving (Eq, Ord, Show)



ptToMetaExpr	:: (MetaType, Int) -> ParseTree -> MetaExpression
ptToMetaExpr mt (Token s)
		= MLiteral s
ptToMetaExpr mt (PtNumber i)
		= MInt i
ptToMetaExpr mt (PtSeq pts)
		= pts |> ptToMetaExpr mt & MSeq mt
ptToMetaExpr _ (RuleParse mt i pt)
		= ptToMetaExpr (MType mt, i) pt

parseRule	:: BNFRules -> Name -> Parser u MetaExpression
parseRule rules nm
		= parseRule' rules nm |> ptToMetaExpr (error "Should not be used")


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
		= identifier |> Token
parsePart _ Number
		= number |> PtNumber
parsePart rules (Seq bnfs)
		= bnfs |+> parsePart' rules |> PtSeq 
parsePart rules (BNFRuleCall nm)
		= parseRule' rules nm

