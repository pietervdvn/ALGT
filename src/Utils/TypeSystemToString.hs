module Utils.TypeSystemToString where

import TypeSystem

import Utils.Utils
import Utils.ToString

import Data.List (intercalate)

{-
This module defines multiple 'ToString's for all type-system data structures
-}

instance ToString BNFAST where
	toParsable	= toStr toParsable

	debug (BNFSeq asts)
			= asts |> toStr debug & intercalate " " & inParens
	debug ast	= toStr debug ast


toStr			:: (BNFAST -> String) -> BNFAST -> String
toStr _ (Literal str)	= show str
toStr _ Identifier		= "Identifier"
toStr _ Number		= "Number"
toStr _ (BNFRuleCall n)	= n
toStr f (BNFSeq asts)	= asts |> f & intercalate " "

