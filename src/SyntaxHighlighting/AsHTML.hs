module SyntaxHighlighting.AsHTML where

{- Renders a parsetree as HTML -}

import Utils.Utils

import TypeSystem
import SyntaxHighlighting.Coloring

import Data.Maybe

import Utils.XML hiding (indent)



renderPT	:: FullColoring -> SyntaxStyle -> ParseTree -> String
renderPT fc style pt
	= let	ptannot		= determineStyle' style pt
		ptannot'	= ptannot |> styleElem
		head		= cssFor fc & comment & inT "style"
		body		= _renderPT ptannot' & inT "body"
		in
		(head ++ body) & inT "html"
		
		


_renderPT	:: ParseTreeA ((Name, [Attr])) -> String
_renderPT (MLiteralA (tag, attrs) _ token)
		= inLT' tag attrs $ insertBR token
_renderPT (MIntA t m token)
		= _renderPT (MLiteralA t m $ show token)
_renderPT (PtSeqA (tag, attrs) _ pts)
		= pts |> _renderPT & unlines & inT' tag attrs


insertBR	:: String -> String
insertBR []	= []
insertBR ('\n':str)
		= "<br />"++str
insertBR(c:str)	= c:insertBR str


styleElem	:: Maybe String -> (Name, [Attr])
styleElem Nothing
		= ("span", [])
styleElem (Just name)
		= ("span", [SA "class" name])


cssFor		:: FullColoring -> String
cssFor fc
	= let	body	= "body" ++ cssElemFor' fc Nothing
		rest	= definedStyles fc |> cssElemFor fc & concat
		in
		body ++ rest


cssElemFor	:: FullColoring -> Name -> String
cssElemFor fc n
		= "\n." ++ n ++ cssElemFor' fc (Just n)

cssElemFor'	:: FullColoring -> Maybe Name -> String
cssElemFor' fc styleName
		= " {"++ cssPropsFor fc styleName ++ "}"

cssPropsFor	:: FullColoring -> Maybe Name -> String
cssPropsFor fc style
	= cssProperties |> (\(styleKey, cssKey, editValue) -> (cssKey, getProperty fc style styleKey |> editValue))
			|> sndEffect & catMaybes
			|> (\(cssKey, value) -> cssKey ++ ":" ++ value ++ ";") & concat
				

cssProperties	:: [(String, String, Either Int String -> String)]
cssProperties
      = [ ("background-color", "background", strEl)
	, ("foreground-color", "color", strEl)
	]

strEl	= either show id
