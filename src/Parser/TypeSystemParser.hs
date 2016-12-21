module Parser.TypeSystemParser where

{-
This module parses typesystem-files
-}

import Utils.Utils
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
import Data.List (intercalate)
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

typeMode	:: Syntax -> Parser u (TypeName, Mode)
typeMode rules	= do	ws
			t	<- choose $ bnfNames rules
			ws
			mode	<- prs "(in)" In <|> prs "(out)" Out
			ws
			return (t, mode)
	
-- parses a relation symbol (between parens)
relationSymb	:: Syntax -> Parser u Symbol
relationSymb r	= do	char '('
			symbol	<- many $ noneOf ")"
			char ')'
			case lookup symbol builtinRelations of
				Just expl	-> fail ("Invalid relation symbol: "++symbol++", conflicts with builtin symbol for "++ expl)
				Nothing		-> return symbol

relationDecl	:: Syntax -> Parser u Relation
relationDecl r	= do	symbol	<- relationSymb r
			ws
			char ':'
			ws
			types <- typeMode r `sepBy` char ','
			ws
			pronounciation	<- try (string "Pronounced as" >> ws >> bnfLiteral |> Just) <|> return Nothing
			ws
			return $ Relation symbol types pronounciation
	
		
------------------------ full file -----------------------------

typeSystemFile	:: String -> Parser u TypeSystem
typeSystemFile name
	= do	bnfs	<- option [] $ try $ do 
				nls
				header "Syntax"
				nls1
				many (try (nls >> bnfRule)) 

		syntax	<- makeSyntax bnfs & either error return


 		funcs	<- option M.empty $ try $ do
				nls
				header "Functions"
				nls1
				parseFunctions Nothing syntax

		
		rels	<- option [] $ try $ do
				nls
				header "Relations"
				nls1
				many $ try (nls *> relationDecl syntax <* nls)

		rules	<- option [] $ try $ do
				nls
				header "Rules"
				nls1
				parseRules (syntax, rels, funcs)
		
		rules'	<- makeRules syntax rules & either error return
		nls
		eof

		return $ TypeSystem name syntax funcs rels rules'

