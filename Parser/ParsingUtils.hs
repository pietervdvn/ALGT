{-# LANGUAGE FlexibleContexts #-}
module ParsingUtils where

import Utils
import Text.Parsec

import Data.Functor.Identity
-- Constants --

digits	= ['0'..'9']
lowers	= ['a'..'z']
uppers	= ['A'..'Z']
whitespace = [' ','\t']

-- Parsing tools --

type Parser u r	= ParsecT String u Identity r


prs		:: String -> r -> Parser u r
prs str val	= try (string str) >> return val

parens		:: Parser u r -> Parser u r
parens p = do	char '('
		ws
		val <- p
		ws
		char ')'
		return val	

identifier	:: Parser u String
identifier  = many1 (oneOf lowers)
number	:: Parser u Int
number 	= many1 (oneOf digits) |> read

ws	:: Parser u String
ws	= many (oneOf whitespace)
ws1	:: Parser u String
ws1	= many1 (oneOf whitespace)

