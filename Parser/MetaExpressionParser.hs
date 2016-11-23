module Parser.MetaExpressionParser where

{-
This module defines a parser for meta expressions, take 2

In this approach, we tokenize first to a tree, and then try to match a rule with it

-}

import Parser.ParsingUtils
import Parser.BNFParser
import TypeSystem
import Utils

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Map as M

data MEParseTree	= MePtToken String | MePtSeq [MEParseTree] | MePtVar Name | MePtCall Name Builtin [MEParseTree]
	deriving (Show, Ord, Eq)



parseMetaExpression	:: Parser u MEParseTree
parseMetaExpression	= mePt

mePt	= many1 (ws *> mePtPart <* ws) |> MePtSeq

mePtPart	= try mePtToken <|> try mePtVar <|> mePtCall 

mePtToken	= bnfLiteral |> MePtToken
mePtVar		= identifier |> MePtVar
mePtCall	= do	builtin	<- try (char '!' >> return True) <|> return False
			nm	<- identifier
			char '('
			args	<- (ws *> mePt <* ws) `sepBy` char ','
			char ')'
			return $ MePtCall nm builtin args
