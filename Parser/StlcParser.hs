{-# LANGUAGE FlexibleContexts #-}
module Parser.StlcParser where

{-
This module parses a simple typed languate
-}

import Utils
import Parser.ParsingUtils

import Parser.StlcAST

import Text.Parsec
import Data.Maybe
import Data.Char


-- Entrance point

parseExpr	:: String -> Maybe String ->  Either ParseError Expr
parseExpr input sourceFile = parse expr (fromMaybe "unknown source" sourceFile) input

parseType	:: String -> Maybe String -> Either ParseError Type
parseType input sourceFile = parse typ (fromMaybe "unknown source" sourceFile) input


-- Type Parsing --

typ	=     prs "Bool" BoolT 
	  <|> prs "Nat" NatT
	  <|> parens (do
		t1 <- typ
		ws
		string "->"
		ws
		t2 <- typ
		return $ ArrowT t1 t2)
	

-- Expression Parsing --	

expr  	=      try app
	   <|> try ifte
	   <|> try plus
	   <|> try (number |> NatE)
	   <|> try bool 
	   <|> try (identifier |> VarE) 
	   <|> try lambda
	   <|> try (parens expr)


bool	=  do   bl	<- prs "True" True <|> prs "False" False
		return $ BoolE bl

plus	= parens $ 
	  do	expr1	<- expr
		ws
		char '+'
		ws
		expr2	<- expr
		return $ PlusE expr1 expr2
		

ifte	= do	string "If"
		ws1
		cond <- expr
		ws
		string "Then"
		ws1
		expr1 <- expr
		ws
		string "Else"
		ws1
		expr2 <- expr
		return $ IfThenElse cond expr1 expr2

app	= parens $ 
	  do	f	<- expr
		ws1
		var	<- expr
		return $ AppE f var

lambda	= do	char '\\'
		ws
		var	<- identifier
		ws
		char ':'
		ws
		tp	<- typ
		ws
		string "->"
		ws
		e	<- expr
		return $ LambdaE var tp e
		
	

