module TypeSystem.Parser.BNFParser where

{-
This module parses the syntax part of a typesystem to a BNF-AST
-}

import Utils.Utils
import TypeSystem.Parser.ParsingUtils

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem

import Text.Parsec
import Data.Maybe
import Data.Char
import Data.List as L
import qualified Data.Map as M

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
      = [ (("::=", IgnoreWS), "Totally ignore whitespace")
	, (("~~=", StrictWS), "Parse whitespace for this rule only")
	, (("//=", StrictWSRecursive), "Parse whitespace for this rule and all recursively called rules")
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


builtinSyntax	= 
	[ (("Identifier", Identifier), ("Matches an identifier", "[a-z][a-zA-Z0-9]*"))
	, (("Number", Number), ("Matches an (negative) integer. Integers parsed by this might be passed into the builtin arithmetic functions.", "-?[0-9]*"))
	]
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

bnfRule
	= do	ws
		name	<- identifier
		ws
		wsMode	<- parseWSMode
		bnfs 	<- bnfLine
		return (name, (bnfs, wsMode))

