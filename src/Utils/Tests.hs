module Utils.Tests where

{-
This module defines some tests
-}

import TypeSystem
import Utils.Utils
import Utils.TypeSystemToString
import Utils.ToString
import Utils.Test

import Data.Map hiding (null)
import Data.Either

import Control.Monad

import Utils.CreateAssets

import qualified Utils.UnificationTest as UnificationTest


stflSyntax = BNFRules $ fromList [("bool",[Literal "True",Literal "False"]),("e",[BNFSeq [BNFRuleCall "eL",Literal "+",BNFRuleCall "e"],BNFSeq [BNFRuleCall "eL",Literal "::",BNFRuleCall "type"],BNFSeq [BNFRuleCall "eL",BNFRuleCall "e"],BNFRuleCall "eL"]),("eL",[BNFRuleCall "number",BNFRuleCall "bool",BNFRuleCall "var",BNFSeq [Literal "(",Literal "\\",BNFRuleCall "var",Literal ":",BNFRuleCall "type",Literal ".",BNFRuleCall "e",Literal ")"],BNFSeq [Literal "If",BNFRuleCall "e",Literal "Then",BNFRuleCall "e",Literal "Else",BNFRuleCall "e"],BNFSeq [Literal "(",BNFRuleCall "e",Literal ")"]]),("number",[Number]),("type",[BNFSeq [BNFRuleCall "typeL",Literal "->",BNFRuleCall "type"],BNFRuleCall "typeL"]),("typeL",[Literal "Int",Literal "Bool",BNFSeq [Literal "(",BNFRuleCall "type",Literal ")"]]),("typing",[BNFSeq [BNFRuleCall "var",Literal "::",BNFRuleCall "type"]]),("typingContext",[BNFSeq [BNFRuleCall "typing",Literal ",",BNFRuleCall "typingContext"],Literal ";"]),("var",[Identifier])]



testSyntax = BNFRules $ fromList [("a", [BNFRuleCall "b"])
			, ("b", [Literal "b"])]





t0	= guard (alwaysIsA testSyntax "a" "b")
t1	= guard (alwaysIsA testSyntax "b" "a")
t2	= guard (alwaysIsA stflSyntax "typeL" "type")
t3	= guard (not $ alwaysIsA stflSyntax "type" "typeL")
t4	= unless UnificationTest.tests $ Left "Unification tests failed"

f0	= Left ()


f	= [f0] & rights & null & flip (assert Left) "Some failer went wrong"

tests	= [f, t0,t1, t2, t3, t4] & allRight_

ta	= tests
