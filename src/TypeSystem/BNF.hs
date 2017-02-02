 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module TypeSystem.BNF where

{-
This module defines BNF-expressions and rules
-}

import Utils.Utils
import Utils.ToString

import TypeSystem.Types

import Data.Maybe


{- Syntax is described in a Backus-Naur format, a simple naive parser is constructed from it. -}


data BNF 	= Literal String	-- Literally parse 'String'
		| Identifier		-- Parse an identifier
		| Number		-- Parse a number
		| BNFRuleCall Name	-- Parse the rule with the given name
		| BNFSeq [BNF]	-- Sequence of parts
	deriving (Show, Eq, Ord)


normalize	:: BNF -> BNF
normalize (BNFSeq seq)
	= let	seq'	= (seq |> normalize) >>= (\bnf -> fromMaybe [bnf] $ fromSeq bnf) in
		case seq' of
			[bnf] 	-> bnf
			_	-> BNFSeq seq'
normalize bnf	= bnf

data WSMode	= IgnoreWS | StrictWS | StrictWSRecursive
	deriving (Show, Eq)




fromSingle	:: BNF -> Maybe BNF
fromSingle (BNFSeq [bnf])	= Just bnf
fromSingle (BNFSeq _)		= Nothing
fromSingle bnf			= Just bnf


fromRuleCall	:: BNF -> Maybe Name
fromRuleCall (BNFRuleCall nm)	= Just nm
fromRuleCall _			= Nothing

isRuleCall	:: BNF -> Bool
isRuleCall BNFRuleCall{}	= True
isRuleCall _			= False

isSeq		:: BNF -> Bool
isSeq BNFSeq{}	= True
isSeq _		= False

fromSeq		:: BNF -> Maybe [BNF]
fromSeq (BNFSeq seq)	= Just seq
fromSeq _	= Nothing

fromSeq'	:: BNF -> [BNF]
fromSeq' (BNFSeq seq)	= seq
fromSeq' bnf	= [bnf]

calledRules	:: BNF -> [TypeName]
calledRules (BNFRuleCall nm)	= [nm]
calledRules (BNFSeq bnfs)	= bnfs >>= calledRules
calledRules _			= []

containsRule	:: TypeName ->  BNF -> Bool
containsRule tn bnf
		= tn `elem` calledRules bnf


-- First call, without consumption of a character
firstCall	:: BNF -> Maybe TypeName
firstCall (BNFRuleCall nm)	= Just nm
firstCall (BNFSeq (ast:_))	= firstCall ast
firstCall _			= Nothing








enterRule	:: WSMode -> WSMode
enterRule StrictWS	= IgnoreWS
enterRule wsMode	= wsMode


strictest		:: WSMode -> WSMode -> WSMode
strictest StrictWSRecursive _	= StrictWSRecursive
strictest StrictWS IgnoreWS	= StrictWS
strictest IgnoreWS IgnoreWS	= IgnoreWS
strictest a b			= strictest b a


instance ToString BNF where
	toParsable	= toStr toParsable
	toCoParsable	= toParsable

	debug (BNFSeq asts)
			= asts |> toStr debug & unwords & inParens
	debug ast	= toStr debug ast

toStr			:: (BNF -> String) -> BNF -> String
toStr _ (Literal str)	= show str
toStr _ Identifier	= "Identifier"
toStr _ Number		= "Number"
toStr _ (BNFRuleCall n)	= n
toStr f (BNFSeq asts)	= asts |> f & unwords


instance ToString WSMode where
	toParsable IgnoreWS	= "::="
	toParsable StrictWS	= "~~="
	toParsable StrictWSRecursive	= "//="


instance ToString (Name, Int, WSMode, String, [BNF]) where
	toParsable (n, i, ws, extra, [])
		= padR i ' ' n ++ toParsable ws ++ " " ++ extra ++ "< no bnfs declared >"
	toParsable (n, i, ws, extra, bnfs)
		= padR i ' ' n ++ toParsable ws ++ " " ++ extra ++ toParsable' ("\n" ++ replicate i ' ' ++  "| ") bnfs


instance Refactorable TypeName BNF where
	refactor ftn (BNFRuleCall nm)	= BNFRuleCall $ ftn nm
	refactor ftn (BNFSeq seq)	= seq |> refactor ftn & BNFSeq
	refactor _   bnf		= bnf



