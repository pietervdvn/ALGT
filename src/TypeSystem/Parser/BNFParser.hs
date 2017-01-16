module TypeSystem.Parser.BNFParser where

{-
This module parses the syntax part of a typesystem to a BNF-AST
-}

import Utils.Utils
import TypeSystem.Parser.ParsingUtils

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Map as M

bnfLiteral	:: Parser s String
bnfLiteral	
	= do	char '"'
		str <- many1 (noneOf "\\\"" <|> 
					(char '\\' >> (char '\\' <|> char '\"' <|> (char 'n' >> return '\n'))))
		char '"'
		return str 

builtins	= ["identifier", "number"]

bnfIdentifier
	= string "Identifier" >> return Identifier

bnfNumber
	= string "Number" >> return Number


bnfRuleCall
	= identifier |> BNFRuleCall

bnfExpr	= bnfIdentifier <|> (bnfLiteral |> Literal) <|> bnfNumber <|> bnfRuleCall

bnfExpr'	
	= do	e	<- many $ inWs bnfExpr
		return $ case e of
			[expr]	-> expr
			_	-> BNFSeq e


bnfLine	= bnfExpr' `sepBy` try (nls *> ws *>  string "|" <* ws)

parseWSMode	=  prs "::=" IgnoreWS <|> prs "~~" StrictWS <|> prs "//=" StrictWSRecursive

bnfRule
	= do	ws
		name	<- identifier
		ws
		wsMode	<- parseWSMode
		bnfs 	<- bnfLine
		return (name, (bnfs, wsMode))
