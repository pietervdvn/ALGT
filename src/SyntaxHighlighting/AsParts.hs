 {-# LANGUAGE FlexibleInstances #-}
module SyntaxHighlighting.AsParts where

import Utils.Utils
import Utils.ToString

import TypeSystem

import SyntaxHighlighting.Coloring
import SyntaxHighlighting.Renderer

import Text.PrettyPrint.ANSI.Leijen

data PartsRenderer	= PartsRenderer SyntaxStyle

instance Renderer PartsRenderer where
	create _ 	= PartsRenderer
	name _		= "Parts"
	supported _	= []
	renderString _	= error "Not supported"
	renderParseTree' pt (PartsRenderer style)
			= text $ renderPT style pt
	renderParseTree	= error "Not supported"
	renderParseTreeDebug	
			= error "Not supported"

renderPT	:: SyntaxStyle -> ParseTreeA LocationInfo -> String
renderPT style pt
	= pt & determineStyle style 
		& _renderPart & unlines


_renderPart	:: ParseTreeA (LocationInfo, Maybe Name) -> [String]
_renderPart (PtSeq info _ pts)
	=  pts >>= _renderPart
_renderPart (MLiteral annot _ conts)
	= _show conts annot
_renderPart (MInt annot _ i)
	= _show (show i) annot

_show	:: String -> (LocationInfo, Maybe Name) -> [String]
_show conts (li, Nothing)
	= _show conts (li, Just "-")
_show contents (LocationInfo startL startC stopL stopC, Just style)
	= contents & lines & _showLines (startL, startC, style)

_showLines	:: (Int, Int, String) -> [String] -> [String]
_showLines _ []	= []
_showLines info ("":rest)
		= _showLines info rest
_showLines (startLine, startCol, style) (line:rest)
	= let	curLine	= [show startLine, show startCol, show startLine, show (startCol + length line) , style, show line] & unwords
		restLines	= _showLines (startLine + 1, 0, style) rest
		in
		curLine:restLines
		
