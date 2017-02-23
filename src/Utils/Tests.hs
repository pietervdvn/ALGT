module Utils.Tests where

{-
This module defines some tests
-}

import TypeSystem
import Utils.Utils

import Utils.ToString

import Data.Map hiding (null)
import qualified Data.Map as M
import Data.Either
import Data.List

import TypeSystem.Parser.BNFParser
import TypeSystem.Parser.TargetLanguageParser
import Control.Monad

import Text.Parsec
import TypeSystem.Parser.ParsingUtils

import AssetsHelper

import qualified Utils.UnificationTest as UnificationTest

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.ASSubtract as AS


subTestSynt   = [ "bool ::= \"True\" | \"False\""
		, "number ::= \"0\""
		, "val ::= bool | number"
		, "type ::= \"Bool\" | \"Int\""
		, "ascr ::= val \"::\" type"
		, "expr ::= \"If\" expr \"Then\" expr | val"
		] |> runParserUnsafe parseBnfRule & makeSyntax & either error id

runParserUnsafe	:: Parser () a -> String -> a
runParserUnsafe parser bnf
	= runParserT parser () "integration tests" bnf
		& either error id
		& either (error . show) id




parseAS		:: TypeName -> String -> AbstractSet
parseAS tn str	= runParserUnsafe (parseSyntax subTestSynt tn) str
			& fromParseTree "pt"

genAS		= generateAbstractSet subTestSynt ""

sub a		= AS.subtract subTestSynt [a] 

ascr		= genAS "ascr"
val		= genAS "val"
bool		= genAS "bool"
expr		= genAS "expr"

boolT		= parseAS "type" "Bool"
boolAscr	= AsSeq "ascr" 0 [bool, ConcreteLiteral "ascr" "::", boolT]
validIf		= AsSeq "expr" 0 [ConcreteLiteral "expr" "If", bool, ConcreteLiteral "expr" "Then", expr]
validIf'	= AsSeq "expr" 0 [ConcreteLiteral "expr" "If", bool, ConcreteLiteral "expr" "Then", bool]

nestedIf	= AsSeq "expr" 0 [ConcreteLiteral "expr" "If", validIf , ConcreteLiteral "expr" "Then", expr]


s0		= val `sub` bool
s0e		= ["number"]
s1		= ascr `sub` bool
s1e		= ["ascr"]
s2		= ascr `sub` boolAscr
s2e		= ["(number \"::\" type)","(val \"::\" \"Int\")"]
s3		= expr `sub`  validIf
s3e		= ["(\"If\" (\"If\" expr \"Then\" expr) \"Then\" expr)",
			"(\"If\" number \"Then\" expr)",
			"val"]
s4		= AS.subtractWith subTestSynt (M.singleton ("expr", "val") [ascr]) [expr] val 
s4e		= ["ascr"]
s5		= expr `sub` nestedIf
s5e		= ["(\"If\" (\"If\" (\"If\" expr \"Then\" expr) \"Then\" expr) \"Then\" expr)","(\"If\" (\"If\" number \"Then\" expr) \"Then\" expr)","(\"If\" val \"Then\" expr)","val"]
s6		= expr `sub` validIf'
s6e		= ["(\"If\" (\"If\" expr \"Then\" expr) \"Then\" expr)","(\"If\" number \"Then\" expr)","(\"If\" expr \"Then\" (\"If\" expr \"Then\" expr))","(\"If\" expr \"Then\" number)","val"]


s7		= bool `sub` parseAS "bool" "True"
s7e		= ["\"False\""]

s8		= expr `sub` parseAS "bool" "True"
s8e		= ["(\"If\" expr \"Then\" expr)","\"False\"","number"]

s9		= expr `sub` parseAS "expr" "If True Then True"
s9e		= ["(\"If\" (\"If\" expr \"Then\" expr) \"Then\" expr)","(\"If\" \"False\" \"Then\" expr)","(\"If\" number \"Then\" expr)","(\"If\" expr \"Then\" (\"If\" expr \"Then\" expr))","(\"If\" expr \"Then\" \"False\")","(\"If\" expr \"Then\" number)","val"]


stflE		= generateAbstractSet stflSyntax "" "e"
stflEl		= generateAbstractSet stflSyntax "" "eL"
stflPar		= AsSeq "eL" 4 [ConcreteLiteral "eL" "(", stflE, ConcreteLiteral "eL" ")"]
stflApp		= AsSeq "e" 2 [stflEl, stflE]

s10		= AS._subtractWith True stflSyntax M.empty [stflPar] stflApp

stests	= [(s0, s0e), (s1, s1e), (s2, s2e), (s3, s3e), (s4, s4e), (s5, s5e), (s6, s6e), (s7, s7e), (s8, s8e)]

testSubs	= stests |> uncurry testS

testS s sexp	= s |> toParsable & sort == sort sexp



testSyntax = makeSyntax	[ ("a", ([BNFRuleCall "b"], IgnoreWS))
			, ("b", ([Literal "b"] , IgnoreWS))
			] & either error id



t0	= guard (not $ alwaysIsA testSyntax "a" "b")
t1	= guard (alwaysIsA testSyntax "b" "a")
t2	= guard (alwaysIsA stflSyntax "typeL" "type")
t3	= guard (not $ alwaysIsA stflSyntax "type" "typeL")
t4	= UnificationTest.tests |> inMsg "Unificationtests" & allRight_

f0	= Left ()


f	= [f0] & rights & null & flip (assert Left) "Some failer went wrong"

unitTest= [f, t0,t1, t2, t3, t4, guard (and testSubs)] & allRight_

unitTestsOK
	= isRight unitTest


