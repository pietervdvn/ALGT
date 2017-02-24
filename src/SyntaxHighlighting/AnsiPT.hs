module SyntaxHighlighting.AnsiPT where

{-  Renders a parsetree with ANSI-Strings -}

import Utils.Utils

import qualified Assets as Assets
import TypeSystem
import TypeSystem.Parser.TargetLanguageParser

import Text.PrettyPrint.ANSI.Leijen

import SyntaxHighlighting.Coloring
import Data.List as L
import Data.Maybe
import Data.Char

import System.IO

import Control.Arrow ((&&&))

import Lens.Micro hiding ((&))

-- TODO test code
import AssetsHelper

renderPT	:: FullColoring -> SyntaxStyle -> ParseTree -> Doc
renderPT fc style pt
	= let	ptannot		= annot () pt & determineStyle style |> snd
		ptannot'	= ptannot |> fromMaybe (error "No style found") |> renderWithStyle fc
		in
		renderDoc ptannot'


renderDoc	:: ParseTreeA (String -> Doc) -> Doc
renderDoc (MLiteralA f _ s)
		= f s
renderDoc (MIdentifierA f _ s)
		= f s
renderDoc (MIntA f _ i)
		= f $ show i
renderDoc (PtSeqA _ _ pts)
		= pts |> renderDoc & foldl1 (<+>)


renderWithStyle	:: FullColoring -> Name -> String -> Doc
renderWithStyle fc styleN str
	= let	effect	= properties |> applyProperty fc styleN & chain
		in
		effect $ text str

applyProperty	:: FullColoring -> Name -> (Name, (String -> Doc -> Doc)) -> Doc -> Doc
applyProperty fc style (prop, effect)
	= case getProperty fc style prop of
		Nothing		-> id
		(Just v)	-> effect v


properties	:: [(Name, (String -> Doc -> Doc))]
properties
      = [ ("foreground-color", fst . closestColor)
	, ("background-color", snd . closestColor)
	, ("font-style", (\v -> fromMaybe plain $ L.lookup v styles)) ]


closestColor	:: String -> (Doc -> Doc, Doc -> Doc)
closestColor c
	= colors |> over _1 (colorDistance c)
		& sortOn fst
		& head & snd

colors	:: [(String, (Doc -> Doc, Doc -> Doc))]
colors
      = [ ("#000000", (dullblack, ondullblack))
	, ("#ff0000", (red, onred))
	, ("#00ff00", (green, ongreen))
	, ("#0000ff", (blue, onblue))
	, ("#ffff00", (yellow, onyellow))
	, ("#ff00ff", (magenta, onmagenta))
	, ("#00ffff", (cyan, oncyan))
	, ("#ffffff", (white, onwhite))
	
	, ("#808080", (black, onblack))
	, ("#C0C0C0", (dullwhite, ondullwhite))
	, ("#800000", (dullred, ondullred))
	, ("#008000", (dullgreen, ondullgreen))
	, ("#000080", (dullblue, ondullblue))
	, ("#808000", (dullyellow, ondullyellow))
	, ("#800080", (dullmagenta, ondullmagenta))
	, ("#008080", (dullcyan, ondullcyan))
	] 

styles	:: [(String, Doc -> Doc)]
styles
      = [ ("normal", deunderline . debold)
	, ("underline", underline)
	, ("bold", bold)
	, ("underlinedbold", underline . bold)
	]


t = do	fc	<- parseColoringFile "Assets: Terminal.style" Assets._Terminal_style
			& either error return
	pt	<- parseTargetLang (get tsSyntax stfl) "e" "Int test" "(If True Then (\\x : Int . x + 1) Else (\\x : Int . x + 12)) 1"
			& either error return
	print fc
	putDoc $ renderPT fc (stfl & get tsStyle) pt
	putStrLn ""

