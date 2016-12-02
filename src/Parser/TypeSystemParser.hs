module Parser.TypeSystemParser where

{-
This module parses typesystem-files
-}

import Utils
import Parser.ParsingUtils

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem
import Parser.BNFParser
import Parser.TargetLanguageParser
import Parser.FunctionParser
import Parser.RuleParser

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




---------------------- Relation ---------------

typeMode	:: BNFRules -> Parser u (TypeName, Mode)
typeMode rules	= do	ws
			t	<- choose $ bnfNames rules
			ws
			mode	<- prs "(in)" In <|> prs "(out)" Out
			ws
			return (t, mode)
			


relationDecl	:: BNFRules -> Parser u Relation
relationDecl r	= do	char '('
			symbol	<- many $ noneOf ")"
			char ')'
			if (symbol == ":") then fail ("Invalid relation symbol: "++symbol++", conflicts with builtin symbol") else return ()
			ws
			char ':'
			ws
			types <- typeMode r `sepBy` char ','
			ws
			pronounciation	<- try (string "Pronounced as" >> ws >> bnfLiteral |> Just) <|> return Nothing
			ws
			return $ Relation symbol types pronounciation
	
		
------------------------ full file -----------------------------



header hdr
	= do	ws
		string hdr
		ws
		char '\n'
		many1 $ char '='
		char '\n'

typeSystemFile	:: String -> Parser u TypeSystem
typeSystemFile name
	= do	nls
		header "Syntax"
		bnfs	<- (many $ try (nls >> bnfRule)) |> M.fromList

		nls1
		header "Functions"
 		metaFuncs 	<- parseFunctions bnfs

		nls
		header "Relations"
		nls1
		rels	<- many $ try (nls *> relationDecl bnfs <* nls)
		
		header "Rules"
		nls1
		rules	<- parseRules (bnfs, rels, metaFuncs)


		return $ TypeSystem name bnfs metaFuncs rels rules

