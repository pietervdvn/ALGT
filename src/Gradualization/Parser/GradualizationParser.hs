module Gradualization.Parser.GradualizationParser where

{-
Yeah, what would this module implement?
-}
import TypeSystem
import Gradualization.Gradualization
import Utils.Utils
import Parser.ParsingUtils
import Parser.BNFParser
import Parser.FunctionParser

import Data.Maybe
import qualified Data.Map as M

import Control.Arrow ((&&&))
import Control.Monad

import Text.Parsec


relationRename	:: Parser u (Symbol, (Symbol, Maybe String))
relationRename 
	= do	ws
		string "Rename relation"
		ws
		oldSymb	<- bnfLiteral
		ws
		string "to"
		ws
		newSymb <- bnfLiteral
		ws
		newPron	<- optionMaybe (string ", pronounced as" >> ws >> bnfLiteral)
		return (oldSymb, (newSymb, newPron))

gradualizationFile	:: TypeSystem -> Name -> Parser u Gradualization
gradualizationFile ts name
	= do	nls
		ws
		string "Gradualize rule"
		ws
		ruleName	<- bnfLiteral
		ws
		string ", add"
		ws
		symbol		<- bnfLiteral


		syntax'	<- tsSyntax ts & rewriteSyntax ruleName symbol
				& either error return
		let alreadyExistingTyping	= tsFunctions ts |> typesOf

		nls
 		newFuncs 	<- option M.empty (do
					header "New Functions"
					parseFunctions (Just alreadyExistingTyping) syntax')
		

		let alreadyExistingTyping'	= M.union alreadyExistingTyping (newFuncs |> typesOf)

		
 		overridenFuncs 	<- option M.empty (do
					header "Overridden Functions"
					parseFunctions (Just $ alreadyExistingTyping') syntax')


		header "New relations"
		nls
		let newRelations	= []

		header "Rename Relations"
		nls
		renames	<- many $ try (relationRename <* nls)

		

		eof

		return $ Gradualization name ruleName symbol newFuncs overridenFuncs newRelations renames





parseGradualizationFile	:: TypeSystem -> String -> IO (Either ParseError Gradualization)
parseGradualizationFile ts fp
	= do	input	<- readFile fp
		return $ parseGradualization ts input (Just fp)

parseGradualization	:: TypeSystem -> String -> Maybe String -> Either ParseError Gradualization
parseGradualization ts input file
	= let 	nme	= fromMaybe "unknown source" file in
	  	parse (gradualizationFile ts nme) nme input

