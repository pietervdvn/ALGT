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
import Control.Monad

import Text.Parsec

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
		] |> parse parseBnfRule "integration tests" |> either (error . show) id
		& makeSyntax
		& either error id


ascr		= generateAbstractSet subTestSynt "" "ascr"
val		= generateAbstractSet subTestSynt "" "val"
bool		= generateAbstractSet subTestSynt "" "bool"
expr		= generateAbstractSet subTestSynt "" "expr"

boolT		= ConcreteLiteral "type" "Bool"
boolAscr	= AsSeq "ascr" 0 [bool, ConcreteLiteral "ascr" "::", boolT]
validIf		= AsSeq "expr" 0 [ConcreteLiteral "expr" "If", bool, ConcreteLiteral "expr" "Then", expr]

s0		= AS.subtract subTestSynt [val] bool
s0e		= ["number"]
s1		= AS.subtract subTestSynt [ascr] bool
s1e		= ["ascr"]
s2		= AS.subtract subTestSynt [ascr] boolAscr
s2e		= ["(number \"::\" type)","(val \"::\" \"Int\")"]
s3		= AS.subtract subTestSynt [expr]  validIf
s3e		= ["(\"If\" (\"If\" expr \"Then\" expr) \"Then\" expr)","(\"If\" number \"Then\" expr)","val"]
s4		= AS.subtractWith subTestSynt (M.singleton ("expr", "val") "ascr") [expr] val 
s4e		= ["ascr"]



stests	= [(s0, s0e), (s1, s1e), (s2, s2e), (s3, s3e), (s4, s4e)]

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


