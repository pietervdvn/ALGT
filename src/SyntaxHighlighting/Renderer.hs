 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module SyntaxHighlighting.Renderer where

import TypeSystem
import Utils.Utils

import SyntaxHighlighting.Coloring

class Renderer renderer where
	create		:: FullColoring -> SyntaxStyle -> renderer
	name		:: renderer -> String
	supported	:: renderer -> [String]
	renderString	:: Name -> String -> renderer -> String
	renderParseTree'	:: ParseTreeA LocationInfo -> renderer -> String
	renderParseTree		:: ParseTree -> renderer -> String
	renderParseTreeDebug	:: ParseTree -> renderer -> String

