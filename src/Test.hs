module Test where

{-
This module defines testing of STFL.example, used to dev against
-}

import Utils

import Control.Monad

import TypeSystem

import Parser.TypeSystemParser
import Parser.TargetLanguageParser
import Parser.ExpressionParser
import Parser.FunctionParser
import FunctionInterpreter

import Text.Parsec

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList)

import Main


fromRight	= either (error . show) id

testExpr (expr, rule)
	= do	putStrLn "\n-----------------\n"
		let pt	= parse mePt "<>" expr & fromRight
		print pt
		let tpd	= typeAs exFunctionTypings stflSyntax rule pt
		case tpd of
			Left msg	-> putStrLn msg
			Right val	-> print val
		let tpd' = tpd & either (error . show) id
		let typing	= expectedTyping stflSyntax tpd' & either (error . show) id
		print typing
		-- return typing	 --}
	
tf	= main' ["../Examples/STFL.typesystem","../Examples/STFL.example", "t","eval","--step"]

t	= tf


extraTests	:: [(String, Name)]
extraTests = 	[ (" \"If\" cond \"Then\" t1 \"Else\" t2 ", "t")
		, ("x \"::\" y", "t")
		, ("\"5\" \"+\" \"5\"","t")
		, ("\"If\" eval(cond) \"Then\" t1 \"Else\" t2", "t")
		, ("!error(\"undefined behaviour\")", "t")
		, ("(\"(\" \"\\\\\" x \":\" type \".\"  e \")\") arg", "t")
		, ("eval(z)", "t")
		, ("subs(x,y,z)","t")
		, ("eval(x) \"+\" x", "t")
		, ("(\"x\" : var)", "t")
		]


failing	= 	[ ("x \"::\" x", "t")
		, ("(\"5\" : var)", "t")
		, ("x : var", "t")
		]

te	= extraTests |+> testExpr
		
 
tl	= last extraTests & testExpr

exFunctionTypings	= fromList [("eval", Arrow (Type "t") (Type "t")) , ("subs", unflatten ["var","t","t","t"])]

		
stflSyntax	= fromList [("t",[Seq [BNFRuleCall "tL",Literal "+",BNFRuleCall "t"],Seq [BNFRuleCall "tL",Literal "::",BNFRuleCall "type"],Seq [BNFRuleCall "tL",BNFRuleCall "t"],BNFRuleCall "tL"]),("tL",[Number,Literal "True",Literal "False",BNFRuleCall "var",Seq [Literal "(",Literal "\\",BNFRuleCall "var",Literal ":",BNFRuleCall "type",Literal ".",BNFRuleCall "t",Literal ")"],Seq [Literal "If",BNFRuleCall "t",Literal "Then",BNFRuleCall "t",Literal "Else",BNFRuleCall "t"]]),("type",[Seq [BNFRuleCall "typeL",Literal "->",BNFRuleCall "type"],BNFRuleCall "typeL"]),("typeL",[Literal "Int",Literal "Bool"]),("var",[Identifier])]
