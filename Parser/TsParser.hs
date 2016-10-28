module Parser.TsParser where

{-
This module parses typesystem-files
-}

import Utils
import Parser.ParsingUtils

import Parser.StlcAST
import Parser.TsAST

import Text.Parsec
import Data.Maybe
import Data.Char

-- entry point --

parseTypeSystemFile	:: String -> IO (Either ParseError [MetaFunction])
parseTypeSystemFile fp
	= do	input	<- readFile fp
		return $ parseTypeSystem input (Just fp)

parseTypeSystem	:: String -> Maybe String -> Either ParseError [MetaFunction]
parseTypeSystem input file
	= parse typeSystemFile (fromMaybe "unknown source" file) input

t 	= parseTypeSystemFile "Parser/STFL.typesystem"





--------------------------- metafunctions ----------------------------------------

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
		clauses	<- many1 $ try (nl >> metaClause name)
		return $ MF name mtype clauses




------------------------ full file -----------------------------


commentLine	= ws >> char '#' >> many (noneOf "\n") 

nl		= char '\n' <|> (try commentLine >> ws >> char '\n')
nls1		= many1 nl
nls		= many nl


contextSymbol
	= do	ws
		string "Contextsymbol is "
		ws
		name	<- many1 (noneOf " \n\r\t")
		return name

header hdr
	= do	ws
		string hdr
		ws
		char '\n'
		many1 $ char '='
		char '\n'

typeSystemFile
	= do	nls
		ctxS	<- contextSymbol
		nls1
		header "Functions"
		funcs 	<- many $ try (nls1 >> metaFunc)
		nls
		eof
		return funcs
