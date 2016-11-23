module Parser.TsParser where

{-
This module parses typesystem-files
-}

import Utils
import Parser.ParsingUtils

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem
import Parser.BNFParser
import Parser.MetaParser
-- import Parser.MetaFunctionParser

import Text.Parsec
import Data.Maybe
import Data.Char
import qualified Data.Map as M



------------------------ entry point ---------------------------

parseTypeSystemFile	:: String -> IO (Either ParseError TypeSystem)
parseTypeSystemFile fp
	= do	input	<- readFile fp
		return $ parseTypeSystem input (Just fp)

parseTypeSystem	:: String -> Maybe String -> Either ParseError TypeSystem
parseTypeSystem input file
	= let 	nme	= fromMaybe "unknown source" file in
	  	parse (typeSystemFile nme) nme input





{-
------------------------ Rules ---------------------------------

typing		= do	var	<- identifier -- TODO this should become bnf matching
			ws
			char ':'
			ws
			t	<- metaExpr
			return $ Typing var t

typingIn ctxS	= do	typingClause	<- typing
			ws
			string "in"
			ws
			string ctxS
			return $ TypingInContext typingClause

equivalence	= do	me1	<- metaExpr
			ws
			string "=="
			ws
			me2	<- metaExpr
			return $ EqualExprs me1 me2


ruleLine	= do	nme	<- parens (identifier <|> iDentifier)
			ws
			string "---"
			many (char '-')
			ws
			return nme

contextEntails ctxS
		= do	string ctxS
			ws
			string "|-"
			ws
			tping	<- typing
			return $ ContextEntails tping


ruleTop	ctxS	=     try (typingIn ctxS)
		  <|> try (contextEntails ctxS)
		  <|> try equivalence

rule ctxS	= do	ws
			pr	<- try (ruleTop ctxS `sepBy` ws <* nl)
				   <|> (return [])
			nme	<- ruleLine
			nl
			ws
			ctxE	<- contextEntails ctxS `sepBy` ws
			return $ Rule nme pr ctxE
-}


------------------------ full file -----------------------------



contextSymbol
	= do	ws
		string "Contextsymbol is "
		ws
		many1 (noneOf " \n\r\t")

header hdr
	= do	ws
		string hdr
		ws
		char '\n'
		many1 $ char '='
		char '\n'

typeSystemFile name
	= do	nls
		ctxS	<- contextSymbol
		nls1
		header "Syntax"
		bnfs	<- (many $ try (nls >> bnfRule)) |> M.fromList

		nls1
		header "Functions"
		let metaFuncs	= M.empty
 		--metaFuncs 	<- parseMetaFunctions bnfs
		{-
		nls
		header "Rules"
		rules	<- many $ try (nls1 >> rule ctxS)
		nls
		eof
-}
		return $ TypeSystem name ctxS bnfs metaFuncs []

