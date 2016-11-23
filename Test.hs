module Test where

{-
This module defines testing of STFL.example, used to dev against
-}

import Utils

import Control.Monad

import TypeSystem
import Parser.TsParser
import Parser.MetaParser
import Parser.MetaFunctionParser

import Text.Parsec
import Data.Maybe


import MetaFunctionInterpreter

handleExample	:: TypeSystem -> String -> IO ()
handleExample ts str
	= do	putStrLn ("INPUT: "++show str)
		let parser	= parse $ parseRule (tsSyntax ts) "t"
		let parsed	= parser "examples" str
		parseTree	<- either (\str -> putStrLn (show str) >> return (PtNumber 0)) return parsed
		evalStar ts (ptToMetaExpr parseTree)
		putStrLn "\n\n"


evalStar	:: TypeSystem -> MetaExpression -> IO ()
evalStar ts me	
	= do	putStrLn $ "| " ++ show me
		let me'	= evalFunc' ts "eval" [me]
		if me' /= me then
			evalStar ts me'
		else
			return ()
		
		

t 	= do	ts'	<- parseTypeSystemFile "Examples/STFL.typesystem"
		ts	<- either (error . show) return ts'
		print ts
		putStrLn "\n\n\nEXAMPLES\n========\n\n"
		examples	<- readFile "Examples/STFL.example" |> lines |> filter (/= "") |> filter ((/= '#') . head)
		-- forM_ examples $ handleExample ts
		let tst	= "x \"+\" y"
		print (parse (metaExpr (tsSyntax ts) "t") "<interactive>" tst)
		putStrLn "\n"
		print (parse (metaExpr (tsSyntax ts) "x") "<interactive>" tst)
		

		

