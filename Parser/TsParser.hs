module TsParser where

{-
This module parses typesystem-files
-}

import Utils
import ParsingUtils

import StlcAST
import TsAST

import Text.Parsec
import Data.Maybe
import Data.Char

-- entry point --

parseTypeSystem	:: String -> Maybe String -> Either ParseError MetaFunction
parseTypeSystem input file
	= parse metaFunc (fromMaybe "unknown source" file) input

t input = parseTypeSystem input (Just "<interactive>")
-- metafunctions --

metaType
  = try (do	string "Type"
		ws
		string "->"
		ws
		tail	<- metaType
		return $ MTArrow MType tail)
	  <|> prs "Type" MType

metaIdentifier	
	= do	head	<- oneOf uppers
		rest	<- many $ oneOf (uppers ++ digits)
 		return (head:rest)

metaExpr	= metaIdentifier |> MFVariable

metaPat
   = try (do	expr1	<- metaIdentifier |> MPAssign
		ws
		string "->"
		ws
		rest	<- metaPat
		return $ MPDestructArrow expr1 rest)
	<|>	metaIdentifier |> MPAssign
		

metaClause expectedName
	= do	ws
		string expectedName
		ws
		pats	<- parens (metaPat `sepBy` (ws >> char ',' >> ws))
		ws
		char '='
		ws
		expr	<- metaExpr
		ws
		return $ MFC pats expr

metaSignature
	= do	name	<- identifier
		ws
		char ':'
		ws
		tp	<- metaType
		ws
		return (name, tp)

metaFunc	
	= do	(name, mtype)	<- metaSignature
		ws
		clauses	<- many1 $ try (char '\n' >> metaClause name)
		return $ MF name mtype clauses

			
