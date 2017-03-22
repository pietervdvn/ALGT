 {-# LANGUAGE FlexibleInstances #-}
module SyntaxHighlighting.AsParts where

import Utils.Utils

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
	= pt & determineStyle style & _renderPart & unlines


_renderPart	:: ParseTreeA (LocationInfo, Maybe Name) -> [String]
_renderPart (PtSeq info _ pts)
	=  _show "" info ++  (pts >>= _renderPart)
_renderPart (MLiteral annot _ conts)
	= _show conts annot
_renderPart (MInt annot _ i)
	= _show (show i) annot

_show	:: String -> (LocationInfo, Maybe Name) -> [String]
_show _ (li, Nothing)
	= []
_show contents (LocationInfo startL startC stopL stopC, Just style)
	= [show startL++","++show startC++";"++show stopL++","++show stopC++" " ++ style++" " ++show contents]
