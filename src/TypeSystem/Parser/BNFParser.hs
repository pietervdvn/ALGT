module TypeSystem.Parser.BNFParser where

{-
This module parses the syntax part of a typesystem to a BNF-AST
-}

import Utils.Utils
import Utils.ToString
import TypeSystem.Parser.ParsingUtils

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem

import Text.Parsec
import Data.Maybe
import Data.Char
import Data.List as L
import qualified Data.Map as M

import Lens.Micro hiding ((&))

builtinEscapes	:: [((Char, Char), String)]
builtinEscapes
      =	[ (('n', '\n'), "newline")
	, (('t', '\t'), "tab")
	, (('"', '"'), "double quote")
	, (('\\', '\\'), "backslash")
	]
builtinEscapes'
	= builtinEscapes |> fst


wsModeInfo
      = [ (IgnoreWS, "Totally ignore whitespace")
	, (StrictWS, "Parse whitespace for this rule only")
	, (StrictWSRecursive, "Parse whitespace for this rule and all recursively called rules")
	] |> over _1 (toParsable &&& id)


builtinSyntax	= 
	[ (("Identifier", Identifier), ("Matches an identifier", "[a-z][a-zA-Z0-9]*"))
	, (("Number", Number), ("Matches an (negative) integer. Integers parsed by this might be passed into the builtin arithmetic functions.", "-?[0-9]*"))
	, (("Lower", Lower), ("Matches a lowercase letter", "[a-z]"))
	, (("Upper", Upper), ("Matches an uppercase letter", "[A-Z]"))
	, (("Digit", Digit), ("Matches an digit", "[0-9]"))
	, (("String", String), ("Matches a double quote delimted string", "\"([^\"]|\\\"|\\\\)*\""))
	]


parseEscape	:: Parser s Char
parseEscape
	= builtinEscapes' |> (\(inp, result) -> char inp >> return result)
		& L.foldl1 (<|>)


bnfLiteral	:: Parser s String
bnfLiteral	
	= do	char '"'
		str <- many1 (noneOf "\\\"" <|> 
					(char '\\' >> parseEscape))
		char '"'
		return str 



builtins	= builtinSyntax |> fst


bnfBuiltin
	= builtins |> (\(str, val) -> string str >> return val)
		& L.foldl1 (<|>)


bnfRuleCall
	= identifier |> BNFRuleCall

bnfExpr	= bnfBuiltin <|> (bnfLiteral |> Literal) <|> bnfRuleCall

bnfExpr'	
	= do	e	<- many $ inWs bnfExpr
		return $ case e of
			[expr]	-> expr
			_	-> BNFSeq e


bnfLine	= bnfExpr' `sepBy` try (nls *> ws *>  string "|" <* ws)

parseWSMode	= wsModeInfo |> fst |> uncurry prs & foldl1 (<|>)

parseBnfRule
	= do	ws
		name	<- identifier
		ws
		wsMode	<- parseWSMode
		bnfs 	<- bnfLine
		return (name, (bnfs, wsMode))

