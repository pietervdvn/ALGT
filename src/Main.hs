module Main where

import System.Environment

import Utils

import Control.Monad

import TypeSystem

import Parser.TargetLanguageParser
import Parser.TypeSystemParser
import FunctionInterpreter


import Text.Parsec

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList)

main	:: IO ()
main	= do	putStrLn welcome
		args	<- getArgs
		if length args < 3 then
			putStrLn usage
		else main' args & void



main'	:: [String] -> IO (TypeSystem, [Expression])
main' args 
	= do	let (tsFile:exampleFile:bnfRuleName:evalFunc:options) = args
		let lineByLine	= "--line-by-line" `elem` options
		let stepByStep	= "--step" `elem` options
		ts'	<- parseTypeSystemFile tsFile
		ts	<- either (error . show) return ts'
		exampleContents	<- readFile exampleFile
		let he	= handleExample exampleFile ts stepByStep bnfRuleName evalFunc	:: String -> IO Expression
		exprs	<- if lineByLine then do
		 		let examples	= exampleContents & lines 
							& filter (/= "") 
							& filter ((/= '#') . head)
				forM examples (\e -> putStrLn "\n\n" >> he e)
			else he exampleContents |> (:[])
		return (ts, exprs)


handleExample	:: Name -> TypeSystem -> Bool -> Name -> Name -> String -> IO Expression
handleExample fileNm ts stepByStep bnfRuleName evalName str
	= do	putStrLn ("> Input\t"++show str)
		let parser	= parse $ parseRule (tsSyntax ts) bnfRuleName
		let parsed	= parser fileNm str
		parseTree	<- either (error . show) return parsed
		putStrLn $ "> Parse\t"++show parseTree
		if stepByStep then evalStar ts evalName parseTree
			else print $ evalFunc ts evalName [parseTree]
		return parseTree

evalStar	:: TypeSystem -> Name -> Expression -> IO ()
evalStar ts funcName me	
	= do	putStrLn $ "\n " ++ show' me
		let me'	= evalFunc ts funcName [me]
		if me' /= me then
			evalStar ts funcName me'
		else
			return ()



welcome	= 	"  Automated Language Generation Tool \n"++
		" ====================================\n"++
		"             by Pieter Vander Vennet\n"++
		"               Christophe Scholliers\n\n"


usage	=	"USAGE:\n"++
		"AGLT typesystem-file example-file bnf-rule-name evaluator-function-name [--line-by-line] [--step]\n"++
		"Builtin for BNF-syntax: Number, Identifier\n"++
		"Builtin functions: !error, !plus, !neg (negates a number)"


