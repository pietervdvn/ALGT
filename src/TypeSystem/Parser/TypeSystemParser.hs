module TypeSystem.Parser.TypeSystemParser where

{-
This module parses typesystem-files
-}

import Utils.Utils
import TypeSystem.Parser.ParsingUtils

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem
import TypeSystem.Parser.BNFParser
import TypeSystem.Parser.TargetLanguageParser
import TypeSystem.Parser.FunctionParser
import TypeSystem.Parser.RuleParser

import Text.Parsec
import Data.Maybe
import Data.Char
import Data.List (intercalate, partition)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Either


------------------------ entry point ---------------------------

parseTypeSystemFile	:: String -> IO (Either ParseError TypeSystem)
parseTypeSystemFile fp
	= do	input	<- readFile fp
		return $ parseTypeSystem input (Just fp)

parseTypeSystem	:: String -> Maybe String -> Either ParseError TypeSystem
parseTypeSystem input file
	= let 	nme	= fromMaybe "unknown source" file in
	  	parse (typeSystemFile nme) nme input



---------------------- Syntax Style -----------

arrow	= string "->" <|> string "\x2192"
		

styleRule	:: Syntax -> Parser u (TypeName, Maybe Int, String)
styleRule syntax
	= do	ws
		t	<- choose $ bnfNames syntax
		i	<- optionMaybe $ try (char '.' >> number)
		inWs arrow
		val	<- bnfLiteral
		ws
		return (t, i, val)
	

styleRemap	:: Parser u (String, String)
styleRemap	= do	ws
			k	<- bnfLiteral
			inWs arrow
			v	<- bnfLiteral
			ws
			return (k, v)

syntaxStyle	:: Syntax -> Parser u SyntaxStyle
syntaxStyle syntax
		= do	baseRules		<- many $ try (nls >> parseEither (try $ styleRule syntax) styleRemap)
			let (base, extra)	= partition (isNothing . snd3) $ lefts baseRules
			let base'		= base |> dropSnd3 & M.fromList
			let extra'		= extra |> (\(n, Just i, v) -> ((n, i), v))
							& M.fromList
			let remaps		= rights baseRules & M.fromList
			return $ SyntaxStyle base' extra' remaps



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
typeSystemFile fp
	= do	name	<- option fp $ try $ do
				nls 
				n	<- many $ inWs (try identifier <|> try iDentifier)
				nl
				inWs $ many1 $ char '*'
				return $ intercalate " " n

		bnfs	<- option [] $ try $ do 
				nls
				header "Syntax"
				nls1
				many (try (nls >> parseBnfRule)) 

		syntax	<- makeSyntax bnfs & either error return

		syntaxStyle
			<- option (SyntaxStyle M.empty M.empty M.empty) $ try $ do
				nls
				header "Syntax Style"
				nls1
				syntaxStyle syntax


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

		props	<- option [] $ try $ do
				nls
				header "Properties"
				nls1
				parseProperties (syntax, rels, funcs)
		props'	<- makeProperties syntax props & either error return
				
		nls
		eof

		return $ TypeSystem name syntax syntaxStyle funcs rels rules' props'

