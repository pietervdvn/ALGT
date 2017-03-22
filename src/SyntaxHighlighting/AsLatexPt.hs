module SyntaxHighlighting.AsLatexPt where

import Utils.Utils

import Data.Char

import TypeSystem

import SyntaxHighlighting.Coloring
import SyntaxHighlighting.Renderer

import Text.PrettyPrint.ANSI.Leijen

data LatexRenderer	= LatexRenderer FullColoring SyntaxStyle

instance Renderer LatexRenderer where
	create	= LatexRenderer
	name _	= "LaTeX"
	renderParseTree pt (LatexRenderer fc style)
		= text $ renderPT fc style pt
	renderParseTree' pt (LatexRenderer fc style)
		= text $ renderPT fc style (deAnnot pt)
	renderParseTreeDebug pt (LatexRenderer fc style)
		= error "RenderPTDebug not supported"
	renderString styleName str (LatexRenderer fc _)
		= text $ renderWithStyle fc styleName str
	supported _	= effects |> fst


renderPT	:: FullColoring -> SyntaxStyle ->  ParseTree -> String
renderPT fc style pt	
	= determineStyle' style pt
		||>> renderWithStyle fc & cascadeAnnot (renderWithStyle fc "")
		& _renderPart & wrapper


wrapper	::	String -> String
wrapper	str
	= "{\\setlength{\\fboxsep}{0pt}"++str++"}"


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
	, ("font-decoration", fontdecoration)
	, ("font-style", fontstyle)
	, ("font-size", fontsize)
	, ("special", special)
	]


special		:: Either Int String -> String -> String
special (Right "hline") _
	= "\\rule"
special _ str
	= str


fontsize	:: Either Int String -> String -> String
fontsize (Left i) str
		= let 	commands	= fontsizes & filter ((<=) i . fst)
			command		= if null commands then "Huge" else head commands & snd in
			"\\"++command++"{"++str++"}"
fontsize _ str	= str


fontsizes
      = [ (6, "tiny")
	, (8, "scriptsize")
	, (10, "footnotesize")
	, (11, "small")
	, (12, "normalsize")
	, (14, "large")
	, (17, "Large")
	, (21, "LARGE")
	, (25, "huge")
	]


fontstyle	:: Either Int String -> String -> String
fontstyle (Right ('"':v)) str
	= fontstyle (Right $ init v) str
fontstyle (Right "bold") str
	= "\\textbf{"++str++"}"
fontstyle (Right "italic") str
	= "\\emph{"++str++"}"
fontstyle (Right "monospace") str
	= "\\texttt{"++str++"}"
fontstyle (Right []) str
	= str
fontstyle (Right vals) str
	= let	(prop, rest)	= break (==',') vals in
		fontstyle (Right prop) $ fontstyle (Right $ dropWhile (`elem` " ,") rest) str
fontstyle _ str
	= str

fontdecoration	:: Either Int String -> String -> String
fontdecoration (Right "underline") str
	= "\\underline{"++str++"}"
fontdecoration _ str
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
