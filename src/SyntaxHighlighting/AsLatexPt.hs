module SyntaxHighlighting.AsLatexPt where

import Utils.Utils

import Data.Char

import TypeSystem

import SyntaxHighlighting.Coloring


renderPT	:: FullColoring -> SyntaxStyle ->  ParseTree -> String
renderPT fc style pt	
	= determineStyle' style pt
		||>> renderWithStyle fc & cascadeAnnot (renderWithStyle fc "")
		& _renderPart


_renderPart	:: ParseTreeA (String -> String) -> String
_renderPart (MLiteral effect _ conts)
		= conts & escape & effect
_renderPart (MInt effect inf i)
	= _renderPart (MLiteral effect inf $ show i)
_renderPart (PtSeq effect _ pts)
	= pts |> _renderPart & unwords & effect


renderWithStyle	:: FullColoring -> Name -> String -> String
renderWithStyle fc style str
	= let	effect	= effects |> runProp fc style & chain in
		effect str



runProp	:: FullColoring -> Name -> (Name, Either Int String -> String -> String) -> String -> String
runProp _ _ _ ""	= ""
runProp _ _ _ " "	= " "
runProp fc style (key, effect) contents
	= let	mval	= getProperty fc style key 
		contValue
			= maybe contents (`effect` contents) mval
		in
		contValue


effects	:: [(Name, Either Int String -> String -> String)] 
effects	
      = [ ("foreground-color", \value contents -> "\\color[HTML]{" ++ color value ++ "}"++contents)
	, ("background-color", \value contents -> "\\colorbox[HTML]{"++ color value ++ "}{"++contents++"}")
	, ("font-style", fontstyle)
	, ("font-family", fontfamily)
	]


fontfamily	:: Either Int String -> String -> String
fontfamily (Right font) str
	= "{\\fontfamily{"++font++"}\\selectfont "++str++"}"
fontfamily _ str
	= str



fontstyle	:: Either Int String -> String -> String
fontstyle (Right "bold") str
	= "\\textbf{"++str++"}"
fontstyle (Right "italic") str
	= "\\emph{"++str++"}"

fontstyle _ str
	= str


color	:: Either Int String -> String
color (Left i)
	= intAsColor i
color (Right s)
	= s & tail |> toUpper


escape	:: String -> String
escape ""	= ""
escape ('\\':str)
	= "\\textbackslash" ++ escape str
escape ('{':str)
	= "\\{" ++ escape str
escape ('}':str)
	="\\}" ++ escape str
escape (c:str)
	= c : escape str