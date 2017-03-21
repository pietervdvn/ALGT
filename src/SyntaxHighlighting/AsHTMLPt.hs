module SyntaxHighlighting.AsHTMLPt where

{- Renders a parsetree as HTML -}

import Utils.Utils

import TypeSystem
import SyntaxHighlighting.Coloring

import Data.Maybe
import Data.List

import Utils.XML hiding (indent)



renderPT	:: FullColoring -> SyntaxStyle -> ParseTree -> String
renderPT fc style pt
	= let	ptannot		= determineStyle' style pt
		ptannot'	= ptannot ||>> styleElem & cascadeAnnot ("", [])
		head		= cssFor fc & comment & inLT "style"
		body		= _renderPT ptannot' & inLT "body"
		in
		(head ++ body) & inLT "html"
		
		


_renderPT	:: ParseTreeA (Name, [Attr]) -> String
_renderPT (MLiteral ("dirty-hack", attrs) _ token)
		= inTag "span" [SA "style" ("color:"++token++";background:"++contrastColor token)] $ insertBR token
_renderPT (MLiteral ("dirtier-hack", attrs) _ token)
		= inTag "span" [SA "class" token] $ insertBR token
_renderPT (MLiteral (tag, attrs) _ token)
		= inTag tag attrs $ insertBR token
_renderPT (MInt t m token)
		= _renderPT (MLiteral t m $ show token)
_renderPT (PtSeq (tag, attrs) _ pts)
		= let	rendered	= pts |> _renderPT & concat in
			rendered & if null attrs then id else inTag tag attrs

inTag		:: Name -> [Attr] -> String -> String
inTag _ _ ""	= ""
inTag _ [] c	= c
inTag n a c	= inLT' n a c

contrastColor	:: String -> String
contrastColor color
		= if highestComponent color < 128 then "#ffffff" else "#000000"



insertBR	:: String -> String
insertBR []	= []
insertBR ('\n':str)
		= "<br />"++insertBR str
insertBR (' ':str)
		= "&nbsp;"++insertBR str
insertBR('<':str)
		= "&lt;"++insertBR str
insertBR('>':str)
		= "&gt;"++insertBR str
insertBR('\t':str)
		= "&#9;"++insertBR str
insertBR('\'':str)
		= "&apos;"++insertBR str
insertBR('"':str)
		= "&quot;"++insertBR str

insertBR(c:str)	= c:insertBR str


styleElem	:: String -> (Name, [Attr])
styleElem ""
		= ("span", [])
styleElem "dirty-hack"
		= ("dirty-hack", [])
styleElem "dirtier-hack"
		= ("dirtier-hack", [])
styleElem name
		= ("span", [SA "class" name])


cssFor		:: FullColoring -> String
cssFor fc
	= let	body	= "body" ++ cssElemFor' fc ""
		rest	= definedStyles fc |> cssElemFor fc & concat
		in
		body ++ rest


cssElemFor	:: FullColoring -> Name -> String
cssElemFor fc n
		= "\n." ++ n ++ cssElemFor' fc n

cssElemFor'	:: FullColoring -> Name -> String
cssElemFor' fc styleName
		= " {"++ cssPropsFor fc styleName ++ "}"

cssPropsFor	:: FullColoring -> Name -> String
cssPropsFor fc style
	= cssProperties |> cssPropsFor' fc style
			|> sndEffect & catMaybes
			|> (\(cssKey, value) -> cssKey ++ ":" ++ value ++ ";") & concat

cssPropsFor'	:: FullColoring -> Name -> (String, String, Either Int String -> String) -> (String, Maybe String)
cssPropsFor' fc style (styleKey, cssKey, editValue)
	= (cssKey, getProperty fc style styleKey |> editValue)

cssProperties	:: [(String, String, Either Int String -> String)]
cssProperties
      = [ ("background-color", "background-color", strEl)
	, ("foreground-color", "color", strEl)
	, ("font-size", "font-size", (++"px") . intEl)
	, ("font-family", "font-family", strEl)
	, ("font-decoration", "text-decoration", strEl)
	]

strEl	= either show id
intEl	= either show id 
