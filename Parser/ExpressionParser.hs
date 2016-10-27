{-# LANGUAGE FlexibleContexts #-}
module Parser where

{-
This module parses a simple typed languate
-}

import AST

import Text.Parsec
import Data.Maybe
import Data.Char

(|>)	:: Functor f => f a -> (a -> b) -> f b
(|>) 	= flip fmap

typ	= prs "Bool" BoolT <|> prs "Nat" NatT
		

expr  	=      try app
	   <|> try ifte
	   <|> try number
	   <|> try bool 
	   <|> try suc
	   <|> try (identifier |> VarE) 
	   <|> try lambda
	   <|> try (parens expr)

digits	= ['0'..'9']
lowers	= ['a'..'z']
uppers	= ['A'..'Z']
whitespace = [' ','\t']


prs str val	= try (string str) >> return val


identifier  = many1 (oneOf lowers)

number 	= many1 (oneOf digits) |> read |> NatE

bool	=  do   bl	<- prs "True" True <|> prs "False" False
		return $ BoolE bl

suc	= do	string "Succ"
		ws1
		expr |> SuccE

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

ws	= many (oneOf whitespace)

ws1	= many1 (oneOf whitespace)

app	= do	char '('
		ws
		f	<- expr
		ws1
		var	<- expr
		ws
		char ')'
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
		
parens p = do	char '('
		ws
		val <- p
		ws
		char ')'
		return val		


parseGTFL	:: String -> Maybe String -> Either ParseError Expr
parseGTFL input sourceFile = parse expr (fromMaybe "unknown source" sourceFile) input

t str	= parseGTFL str Nothing
