module Test where

{-
-}

import Utils

import Control.Monad

import TypeSystem

import Parser.TypeSystemParser
import Parser.TargetLanguageParser
import Parser.ExpressionParser
import Parser.FunctionParser
import ParseTreeInterpreter.FunctionInterpreter
import ParseTreeInterpreter.RuleInterpreter

import AbstractInterpreter.Test

import Text.Parsec

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList, (!))

import Main



-- Tests go here

t	= do	(ts, _)	<- main' ["../Examples/STFL.typesystem", "../Examples/STFL.example", "e"]
		testAS (tsSyntax ts)
