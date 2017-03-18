 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module TypeSystem.BNF where

{-
This module defines BNF-expressions and rules
-}

import Utils.Utils
import Utils.ToString

import TypeSystem.Types

import Text.Parsec
import TypeSystem.Parser.ParsingUtils

import Data.Maybe
import Data.Either
import Data.Char (toLower)

import Lens.Micro hiding ((&))
import Control.Arrow ((&&&))

{- Syntax is described in a Backus-Naur format, a simple naive parser is constructed from it. -}


data BNF 	= Literal String	-- Literally parse 'String'
		| BNFRuleCall Name	-- Parse the rule with the given name.
		| BNFSeq [BNF]		-- Sequence of parts
	deriving (Show, Eq, Ord)




wsModeInfo
      = [ (IgnoreWS, "Totally ignore whitespace")
	, (StrictWS, "Parse whitespace for this rule only")
	, (StrictWSRecursive, "Parse whitespace for this rule and all recursively called rules")
	] |> over _1 (toParsable &&& id)

-- Also update: matchTyping in the expressionParser; TargetLanguageParser
-- ((BNF-Name; targetLang-parser (is wrapped in a MLiteral afterwards); example Values) (documentation, documentation regex)
builtinSyntax	= 
	[ (("Identifier", identifier, ["x", "y", "z"]), 
		("Matches an identifier", "[a-z][a-zA-Z0-9]*"))
	, (("Number", number |> show, ["0", "1", "2"]), 
		("Matches an (negative) integer. Integers parsed by this might be passed into the builtin arithmetic functions.", "-?[0-9]*"))
	, (("Any", anyChar |> (:[]), ["a", "X", "\n", "→", " ", "\t", "\\"]),
		 ("Matches a single character, whatever it is, including newline characters", "."))
	, (("Lower", oneOf lowers |> (:[]), ["a","b","x","y","z"]), 
		("Matches a lowercase letter", "[a-z]"))
	, (("Upper", oneOf uppers |> (:[]),["A","B","C"]),
		("Matches an uppercase letter", "[A-Z]"))
	, (("Digit", oneOf digits |> (:[]), [0..9] |> show),
		("Matches an digit", "[0-9]"))
	, (("Hex", oneOf hex |> (:[]), ([0..9] |> show) ++ ['a'..'f'] |> (:[]))
		, ("Matches a hexadecimal digit", "[0-9a-fA-F]"))
	, (("String", dqString', ["\"abc\"","\"\\\" \\\\ \""]),
		("Matches a double quote delimted string, returns the value including the double quotes", "\"([^\"\\]|\\\"|\\\\)*\""))
	, (("StringUnesc", dqString, ["\"abc\"","\"\\\" \\\\ \""]),
		("Matches a double quote delimeted string, returns the value without the double quotes",  "\"([^\"\\]|\\\"|\\\\)*\""))
	, (("LineChar", noneOf "\n" |> (:[]), ["a", "A", "x","y","z", "\r"]),
		("Matches a single character that is not a newline. This includes \\r.", "[^\\n]"))
	, (("ParO", char '(' >> return "", []),
		("Matches a '(', which will dissapear in the parsetree", "("))
	, (("ParC", char ')' >> return "", []),
		("Matches a ')', which will dissapear in the parsetree", ")"))
	]

dissappearingSyntax
	= ["ParO", "ParC"]


isDissapearing	:: BNF -> Bool
isDissapearing (BNFRuleCall r)
 	= r `elem` dissappearingSyntax
isDissapearing (BNFSeq seq)
	= seq |> isDissapearing & and
isDissapearing _
	= False

isValidBuiltin	:: BNF -> String -> Bool
isValidBuiltin bnf s
	= let	result	= runParserT (getParserForBuiltin bnf) () ("Pattern "++show s++" against bnf "++show bnf) s
		in
		result |> isRight & either (const False) id

isBuiltin	:: BNF -> Bool
isBuiltin (BNFRuleCall x)
		= isBuiltinName x
isBuiltin _	= False

isBuiltinName	:: Name -> Bool
isBuiltinName x
		= x `elem` (builtinSyntax |> fst |> fst3)




getParserForBuiltin	:: BNF -> Parser u String
getParserForBuiltin (BNFRuleCall bnf)
	= builtinSyntax |> fst |> dropTrd3 & lookup bnf & fromMaybe (error $ "No builtin for parser defined for "++show bnf++", this is a bug")



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


calledRules'	:: BNF -> [TypeName]
calledRules' bnf
	= bnf & calledRules & filter (not . isBuiltinName)

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
toStr _ (Literal str)	= "\"" ++ esc str ++ "\""
toStr f (BNFSeq asts)	= asts |> f & unwords
toStr _ (BNFRuleCall n)	= n


esc		:: String -> String
esc []		= []
esc ('"':str)	= "\"" ++ esc str
esc ('\\':str)	= "\\\\" ++ esc str
esc (c:str)	= c:esc str

instance ToString WSMode where
	toParsable IgnoreWS	= "::="
	toParsable StrictWS	= "~~="
	toParsable StrictWSRecursive	= "//="


instance ToString (Name, Int, WSMode, Bool, String, [BNF]) where
	toParsable (n, i, ws, group, extra, bnfs)
		= padR i ' ' n ++ toParsable ws ++ _showGroup group ++ " " ++ extra ++ 
			if null bnfs then "< no bnfs declared >"
				else toParsable' ("\n" ++ replicate i ' ' ++  "| ") bnfs

_showGroup True		= " $"
_showGroup False	= ""


instance Refactorable TypeName BNF where
	refactor ftn (BNFRuleCall nm)
					= BNFRuleCall $ ftn nm
	refactor ftn (BNFSeq seq)	= seq |> refactor ftn & BNFSeq
	refactor _   bnf		= bnf




