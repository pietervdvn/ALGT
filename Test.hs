module Test where

{-
This module defines testing of STFL.example, used to dev against
-}

import Utils

import Control.Monad

import TypeSystem
import Parser.TsParser
import Parser.MetaParser

import Text.Parsec
import Data.Maybe


import MetaFunctionInterpreter

handleExample	:: TypeSystem -> String -> IO ()
handleExample ts str
	= do	putStrLn ("INPUT: "++show str)
		let parser	= parse $ parseRule (tsSyntax ts) "t"
		let parsed	= parser "examples" str
		parseTree	<- either (\str -> putStrLn (show str) >> return (PtNumber 0)) return parsed
		putStrLn ("PARSED" ++ show (ptToMetaExpr parseTree))
		let evaled	= evalFunc ts "eval" [parseTree]
		putStrLn $ "EVALUATED: "++show evaled
		putStrLn "\n\n"
		

t 	= do	ts'	<- parseTypeSystemFile "Examples/STFL.typesystem"
		ts	<- either (error . show) return ts'
		print ts
		putStrLn "\n\n\nEXAMPLES\n========\n\n"
		examples	<- readFile "Examples/STFL.example" |> lines |> filter (/= "") |> filter ((/= '#') . head)
		forM_ examples $ handleExample ts
