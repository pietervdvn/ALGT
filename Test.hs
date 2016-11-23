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

import Data.Map (Map, fromList)

import Parser.MetaExpressionParser

{-
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
--}

testExpr expr rule
	= do	let pt	= parse mePt "<>" expr
		pt'	<- either (error . show) return pt
		print pt'
		let tpd	= typeAs exFunctionTypings stflSyntax rule pt'
		print tpd
		print $ expectedTyping stflSyntax $ fromJust tpd		
		

t 	= do	ts'	<- parseTypeSystemFile "Examples/STFL.typesystem"
		ts	<- either (error . show) return ts'
		print ts
		putStrLn "\n\n\nEXAMPLES\n========\n\n"
		examples	<- readFile "Examples/STFL.example" |> lines |> filter (/= "") |> filter ((/= '#') . head)
		-- forM_ examples $ handleExample ts
		forM_ [(" \"If\" cond \"Then\" t1 \"Else\" t2 ", "t"), ("x \"::\" y", "t"), ("\"5\" \"+\" \"5\"","t")
			, ("\"If\" eval(cond) \"Then\" t1 \"Else\" t2", "t"), ("x \"::\" x", "t")] (uncurry testExpr)


exFunctionTypings	= fromList [("eval", MTArrow (MType "t") (MType "t")) , ("cond", MType "t")]

		
stflSyntax	= fromList [("t",[Seq [BNFRuleCall "tL",Literal "+",BNFRuleCall "t"],Seq [BNFRuleCall "tL",Literal "::",BNFRuleCall "type"],Seq [BNFRuleCall "tL",BNFRuleCall "t"],BNFRuleCall "tL"]),("tL",[Number,Literal "True",Literal "False",BNFRuleCall "var",Seq [Literal "(",Literal "\\",BNFRuleCall "var",Literal ":",BNFRuleCall "type",Literal ".",BNFRuleCall "t",Literal ")"],Seq [Literal "If",BNFRuleCall "t",Literal "Then",BNFRuleCall "t",Literal "Else",BNFRuleCall "t"]]),("type",[Seq [BNFRuleCall "typeL",Literal "->",BNFRuleCall "type"],BNFRuleCall "typeL"]),("typeL",[Literal "Int",Literal "Bool"]),("var",[Identifier])]
