module TypeSystem.Parser.TypeSystemParser where

{-
This module parses typesystem-files
-}

import Utils.Utils
import Utils.ToString
import TypeSystem.Parser.ParsingUtils

import Control.Arrow ((&&&))
import Control.Monad

import TypeSystem.TypeSystemData
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
import qualified Data.Bifunctor as BF
import Control.Monad.Trans


------------------------ entry point ---------------------------

parseTypeSystemFile	:: String -> IO (Either String TypeSystem)
parseTypeSystemFile fp
	= do	input	<- readFile fp
		return $ parseTypeSystem input (Just fp)

parseTypeSystem	:: String -> Maybe String -> Either String TypeSystem
parseTypeSystem input file
	= do 	let nme	= fromMaybe "unknown source" file
	  	parsed	<- runParserT (typeSystemFile nme) () nme input
		parsed & BF.first show



---------------------- Syntax Style -----------

arrow	= string "->" <|> string "\x2192"
		

stylePath	:: Syntax -> Parser u [(TypeName, Maybe Int)]
stylePath syntax
	= do	ws
		t	<- choose $ bnfNames syntax
		i	<- optionMaybe $ try (char '.' >> number)
		tail	<- try (char '.' >> stylePath syntax) <|> return []
		return ((t, i):tail)

styleRule	:: Syntax -> Parser u ([(TypeName, Maybe Int)], String)
styleRule syntax
	= do	ws
		path	<- stylePath syntax
		inWs arrow
		val	<- dqString
		ws
		return (path, val)
	


syntaxStyle	:: Syntax -> Parser u SyntaxStyle
syntaxStyle syntax
		= do	styles		<- many $ try (nls >> styleRule syntax)
			styles & M.fromList & SyntaxStyle & return



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
			pronounciation	<- try (string "Pronounced as" >> ws >> dqString |> Just) <|> return Nothing
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
				return $ unwords n
		bnfs	<- option [] $ try $ do 
				nls
				header "Syntax"
				nls1
				many (try (nls >> parseBnfRule)) 

		syntax	<- lift $ makeSyntax bnfs

		syntaxStyle
			<- option (SyntaxStyle M.empty) $ try $ do
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
		rules'	<- lift $ makeRules syntax rules

		props	<- option [] $ try $ do
				nls
				header "Properties"
				nls1
				parseProperties (syntax, rels, funcs)
		props'	<- lift $ makeProperties syntax props
				
		nls
		eof

		return $ TypeSystem name syntax syntaxStyle funcs rels rules' props'

