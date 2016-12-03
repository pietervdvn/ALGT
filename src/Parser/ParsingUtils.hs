{-# LANGUAGE FlexibleContexts #-}
module Parser.ParsingUtils where

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

-- starts with lower case
identifier	:: Parser u String
identifier
	= do	head <- oneOf lowers
		tail <- many $ oneOf $ lowers ++ uppers ++ digits
		return $ head:tail

-- starts with upper case
iDentifier	:: Parser u String
iDentifier
	= do	head <- oneOf uppers
		tail <- many $ oneOf $ lowers ++ uppers ++ digits
		return $ head:tail

-- either upper or lower case
identifier'	:: Parser u String
identifier'	= identifier <|> identifier'

number	:: Parser u Int
number 	= many1 (oneOf digits) |> read

ws	:: Parser u String
ws	= many (oneOf whitespace)
ws1	:: Parser u String
ws1	= many1 (oneOf whitespace)
ws'	:: Parser u String
ws'	= many (char ' ')


choose' [] msgs	= fail ("Expected one of the following strings: "++ unwords msgs)
choose' (s:strs) msgs
		= try (string s) <|> choose strs 

choose		:: [String] -> Parser u String
choose strs	= choose' strs strs


first		:: [Parser u a] -> Parser u a
first		= foldr ((<|>) . try) (fail "None of the parsers in first matched")

commentLine	= ws >> char '#' >> many (noneOf "\n") 

nl		= char '\n' <|> (try commentLine >> ws >> char '\n')
nls1		= many1 nl
nls		= many nl


intersperseM	:: Monad m => m b -> [m a] -> m [a]
intersperseM sep []
		= return []
intersperseM sep [ma]
		= ma |> (:[])
intersperseM sep (ma:mas)
		= do	a	<- ma
			sep
			as	<- intersperseM sep mas
			return (a:as)
