 {-# LANGUAGE RankNTypes #-}
module SyntaxHighlighting.Renderers (allRenderers, latex, ansi, svg, html, parts) where

import Utils.Utils

import SyntaxHighlighting.Renderer
import SyntaxHighlighting.AsAnsiPt
import SyntaxHighlighting.AsHTMLPt
import SyntaxHighlighting.AsLatexPt
import SyntaxHighlighting.AsSVGPt
import SyntaxHighlighting.AsParts

import SyntaxHighlighting.Coloring
import SyntaxHighlighting.Renderer

import Text.PrettyPrint.ANSI.Leijen

import TypeSystem

allRenderers	:: [(String, [String], Name -> String -> Doc)]
allRenderers	= [ansi' & getProps , html' & getProps, latex' & getProps, svg' & getProps, parts' & getProps]


getProps	:: Renderer r => r -> (String, [String], Name -> String -> Doc)
getProps r	= (name r, supported r, \nm str -> renderString nm str r)


e	= error ""


ansi'	= ansi e e
html'	= html e e
latex'	= latex e e
svg'	= svg e e
parts'	= parts e e


ansi	= create :: FullColoring -> SyntaxStyle -> AnsiRenderer
html	= create :: FullColoring -> SyntaxStyle -> HTMLRenderer
latex	= create :: FullColoring -> SyntaxStyle -> LatexRenderer
svg	= create :: FullColoring -> SyntaxStyle -> SVGRenderer
parts	= create :: FullColoring -> SyntaxStyle -> PartsRenderer

