module AbstractInterpreter.AbstractSetFromString where

{- Parses an abstract set from a string (command line argument)  -}

import Utils.Utils
import Utils.ToString

import Text.Parsec

import TypeSystem
import AbstractInterpreter.AbstractSet
import Assets
import AssetsHelper
import TypeSystem.Parser.ExpressionParser 

import TypeSystem.Parser.ParsingUtils
import Data.List.Split (splitOn)

import Data.Map as M


import qualified Data.Bifunctor as BF

t	= parseIAArgs stflSyntax ["type","type"] "{ typeL \"->\" type}, {type}"
		& either error id
		& printPars' ","


parseIAArgs	:: Syntax -> [TypeName] -> String -> Either String Arguments
parseIAArgs s tp str
	= do	parsed	<- runParserT (_parseArgs s tp 0) () "<No source>" (str++",")
		parsed & BF.first show


_parseArgs	:: Syntax -> [TypeName] -> Int -> Parser u Arguments
_parseArgs s (tn:tns) i 
	= do	arg	<- parseAS s tn i
		inWs $ char ','
		args	<- _parseArgs s tns (i + 1)
		return $ arg:args
_parseArgs _ [] _
	= do	eof <?> "To many arguments given"
		return []

parseAS	:: Syntax -> TypeName -> Int -> Parser u AbstractSet
parseAS syntax tp i
	= inWs $ do
		expr	<- parseExpression
		expr' 	<- expr & typeAs M.empty syntax tp
				& either error return
		expr' & fromExpression syntax ("arg"++show i) & return





