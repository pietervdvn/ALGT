module Parser.BNFParser where

{-
This module parses the syntax part of a typesystem to a BNF-AST
-}

import Utils
import Parser.ParsingUtils

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
					(char '\\' >> (char '\\' <|> char '\"')))
		char '"'
		return str 

bnfIdentifier
	= string "identifier" >> return Identifier

bnfNumber
	= string "0.." >> return Number


bnfRuleCall
	= identifier |> BNFRuleCall

bnfExpr	= ws >> (bnfIdentifier <|> (bnfLiteral |> Literal) <|> bnfNumber <|> bnfRuleCall) <* ws

bnfExpr'	
	= do	e	<- many bnfExpr
		return $ case e of
			[expr]	-> expr
			_	-> Seq e


bnfLine	= bnfExpr' `sepBy` string "|" <* ws

bnfRule
	= do	name	<- identifier
		ws
		string "::="
		bnfs 	<- bnfLine
		return (name,bnfs)

