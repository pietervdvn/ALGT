module TypeSystem.Parser.BNFParser where

{-
This module parses the syntax part of a typesystem to a BNF-AST
-}

import Utils.Utils
import Utils.ToString
import TypeSystem.Parser.ParsingUtils

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem.TypeSystemData

import Text.Parsec
import Data.Maybe
import Data.Char
import Data.List as L
import qualified Data.Map as M

import Lens.Micro hiding ((&))





bnfLiteral	= dqString


builtins	= builtinSyntax |> fst

bnfBuiltin	:: Parser u BNF
bnfBuiltin
	= builtins
		|> fst
		|> (\str -> string str |> BNFRuleCall)
		|> try
		& L.foldl1 (<|>)


bnfRuleCall	:: Parser u BNF
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
		ws
		doGroup	<- (char '$' >> return True) <|> return False
		bnfs 	<- bnfLine
		return (name, (bnfs, wsMode, doGroup))

