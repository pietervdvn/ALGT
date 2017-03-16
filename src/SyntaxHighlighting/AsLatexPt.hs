module SyntaxHighlighting.AsLatexPt where

import Utils.Utils

import Data.Char

import TypeSystem

import SyntaxHighlighting.Coloring


renderPT	:: FullColoring -> SyntaxStyle ->  ParseTree -> String
renderPT fc style pt	
	= determineStyle' style pt	
		|> renderWithStyle fc
		& _renderPart


_renderPart	:: ParseTreeA (String -> String) -> String
_renderPart (MLiteral effect _ conts)
		= (conts & escape & effect)
_renderPart (MInt effect inf i)
	= _renderPart (MLiteral effect inf $ show i)
_renderPart (PtSeq effect _ pts)
	= pts |> _renderPart & unwords & effect


renderWithStyle	:: FullColoring -> Maybe Name -> String -> String
renderWithStyle fc style str
	= let	effect	= effects |> runProp fc style & chain in
		effect str



runProp	:: FullColoring -> Maybe Name -> (Name, Either Int String -> String -> String) -> String -> String
runProp _ _ _ ""	= ""
runProp _ _ _ " "	= " "
runProp fc style (key, effect) contents
	= let	mval	= getProperty fc style key 
		contValue
			= maybe contents (\val -> effect val contents) mval
		in
		contValue


effects	:: [(Name, Either Int String -> String -> String)] 
effects	
      = [ ("foreground-color", (\value contents -> "\\color[HTML]{" ++ color value ++ "}"++contents))
	, ("background-color", (\value contents -> "\\colorbox[HTML]{"++ color value ++ "}{"++contents++"}"))
	]





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
