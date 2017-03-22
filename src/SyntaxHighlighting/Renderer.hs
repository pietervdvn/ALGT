 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module SyntaxHighlighting.Renderer where

import TypeSystem
import Utils.Utils

import SyntaxHighlighting.Coloring

import Text.PrettyPrint.ANSI.Leijen

class Renderer renderer where
	create		:: FullColoring -> SyntaxStyle -> renderer
	name		:: renderer -> String
	supported	:: renderer -> [String]
	renderString	:: Name -> String -> renderer -> Doc
	renderParseTree'	:: ParseTreeA LocationInfo -> renderer -> Doc
	renderParseTree		:: ParseTree -> renderer -> Doc
	renderParseTreeDebug	:: ParseTree -> renderer -> Doc

