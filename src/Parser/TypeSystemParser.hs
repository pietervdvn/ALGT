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
	


relationDecl	:: Syntax -> Parser u Relation
relationDecl r	= do	char '('
			symbol	<- many $ noneOf ")"
			char ')'
			case lookup symbol builtinRelations of
				Just expl	-> fail ("Invalid relation symbol: "++symbol++", conflicts with builtin symbol for "++ expl)
				Nothing		-> return ()
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
	= do	nls
		header "Syntax"
		bnfs	<- many (try (nls >> bnfRule)) 

		syntax	<- makeSyntax bnfs & either error return

		nls1
		header "Functions"
 		metaFuncs 	<- parseFunctions Nothing syntax

		nls
		header "Relations"
		nls1
		rels	<- many $ try (nls *> relationDecl syntax <* nls)

		header "Rules"
		nls1
		rules	<- parseRules (syntax, rels, metaFuncs)
		rules'	<- makeRules syntax rules & either error return

		eof

		return $ TypeSystem name syntax metaFuncs rels rules'

