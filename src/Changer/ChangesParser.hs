module Changer.ChangesParser where

{-
Yeah, what would this module implement?
-}
import TypeSystem
import Changer.Changes
import Utils.Utils
import Utils.ToString
import Parser.ParsingUtils
import Parser.BNFParser
import Parser.FunctionParser
import Parser.TypeSystemParser
import Parser.RuleParser

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import Control.Arrow ((&&&))
import Control.Monad

import Text.Parsec


defaultChange	:: Parser u a -> Parser u (DefaultChange a)
defaultChange pa
	= try	(do	string "Delete"
			ws
			a	<- pa
			ws
			return $ Delete a)
	  <|> try (do	string "Copy"
			ws
			a0	<- pa
			ws
			string "as" <|> string "to"
			ws
			a1	<- pa
			ws
			return $ Copy a0 a1)
	  <|> try (do	string "Rename"
			ws
			a0	<- pa
			ws
			string "to" <|> string "as"
			ws
			a1	<- pa
			ws
			return $ Rename a0 a1)


change'		:: Parser u a -> Parser u b -> Parser u (Either (DefaultChange a) b)
change' pa pb	= try (pb |> Right) <|> (defaultChange pa |> Left)


changes' pa pb	= many $ try (nls *> ws *> change' pa pb <* ws)


syntaxChange	:: Parser u SyntaxChange
syntaxChange   = try (bnfRule |> uncurry OverrideBNFRule) 
		<|> (do	name	<- identifier
			ws
			string "::="
			ws
			string "..."
			ws
			string "|"
			ws
			bnfs	<- bnfLine
			return $ AddOption name bnfs)



functionChange	:: Map Name Type -> Syntax -> Parser u FunctionChange
functionChange types syntax
	=  do	cons		<- try (char '+' >> return AddClauses) <|> return OverrideFunc
		untypedFunc	<- parseFunction syntax
		(nm, function)	<- typeFunction syntax types untypedFunc
					& either error return
		return $ cons nm function





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


relationOption	:: Syntax -> Parser u RelationChange
relationOption syntax
 = try (do	ws
		string "Delete"
		ws
		symb	<- relationSymb syntax
		return $ RDelete symb
		)
   <|> try (do	ws
		cons	<- (string "Rename" >> return RRename) 
				<|> (string "Copy" >> return RCopy)
		ws
		symb	<- relationSymb syntax
		ws
		string "to" <|> string "as"
		ws
		nsymb	<- relationSymb syntax
		
		pronounce <- optionMaybe $ do
			ws
			char ','
			ws
			string "pronounced as"
			ws
			bnfLiteral

		prefixRename <- optionMaybe $ do
			ws
			char ','
			ws
			string "prefix"
			ws
			pref	<- bnfLiteral
			ws
			string "becomes"
			ws
			repl	<- bnfLiteral
			return (pref, repl)
			

		return $ cons symb (RCI nsymb pronounce prefixRename)
		)


changesFile	:: TypeSystem -> Name -> Parser u (Changes, TypeSystem)
changesFile ts0 name
	= do	

		-- SYNTAX --
		------------

		bnfs	<- option [] $ try $ do 
				nls
				header "New Syntax"
				nls1
				many (try (nls >> bnfRule)) 

		newSyntax	<- makeSyntax bnfs & either error return

		bnfCh	<- option [] $ try $ do
				nls
				header "Syntax Changes"
				nls1
				changes' identifier syntaxChange

		syntax'	<- (tsSyntax ts0 & addSyntax newSyntax 
				>>= rewriteSyntax bnfCh)
				& either error return
		let ts1	 = ts0{tsSyntax = syntax'}

		-- FUNCTIONS --
		---------------

 		newFuncs <- option M.empty $ try$ do
				nls
				header "New Functions"
				nls1
				parseFunctions (tsFunctions ts1 |> typesOf & Just) syntax'

		functions'	<- tsFunctions ts1 & addFunctions newFuncs
					& either error return

		let ts2		 = ts1{tsFunctions = functions'}

		funcCh	<- option [] $ try $ do
				nls
				header "Function Changes"
				nls1
				changes' identifier $ functionChange (tsFunctions ts2 |> typesOf) syntax' 
		
		functions''	<- rewriteFunctions funcCh (tsFunctions ts2)
					& either error return

		let ts3		= ts2{tsFunctions = functions''}


		-- RELATIONS --
		---------------


		
		newRels	<- option [] $ try $ do
				nls
				header "New Relations"
				nls1
				try (string "Love, for example" >>
					 error "Yeah, get a lover. And while you're out there, get me one too") <|> return ()
				many $ try (nls *> relationDecl syntax' <* nls)
		
		relations'	<- addRelations newRels (tsRelations ts3)
					& either error return
		let ts4	 = ts3{tsRelations = relations'}


		relCh	<- option [] $ try $ do
				nls
				header "Relation Changes"
				nls1
				try (string "Please!" >> 
					error "Hey! You are a polite chap! You deserve a cookie") <|> return ()
				
				many $ try (nls >> relationOption syntax')
		
		ts5	<- rewriteRelations relCh ts4
				& either error return


		-- Rules --
		---------------


		newRules <- option [] $ try $ do
				nls
				header "New Rules"
				nls1
				parseRules (tsSyntax ts5, tsRelations ts5, tsFunctions ts5)
		
		rules'	<- addRules (tsSyntax ts5) newRules (tsRules' ts5)
				& either error return
		let ts6		= ts5{tsRules' = rules'}

		
		ruleCh	<- option [] $ try $ do
				nls
				header "Rule Changes"
				nls1
				many $ try (nls >> defaultChange lineName)	
			
		rewritten	<- rewriteRules (tsSyntax ts6) ruleCh (tsRules' ts6)
					& either error return

		let tsFinal	 = ts6{tsRules' = rewritten}
		nls
		eof


		checkTypeSystem tsFinal & either error return
		return (Changes name newSyntax bnfCh newFuncs funcCh newRels relCh newRules ruleCh, tsFinal)





parseChangesFile	:: TypeSystem -> String -> IO (Either ParseError (Changes, TypeSystem))
parseChangesFile ts fp
	= do	input	<- readFile fp
		return $ parseChanges ts input (Just fp)

parseChanges	:: TypeSystem -> String -> Maybe String -> Either ParseError (Changes, TypeSystem)
parseChanges ts input file
	= let 	nme	= fromMaybe "unknown source" file in
	  	parse (changesFile ts nme) nme input

