module AssetsHelper where

import Assets
import Utils.Utils

import TypeSystem.Parser.TypeSystemParser
import TypeSystem

import SyntaxHighlighting.Coloring

stfl		= parseTypeSystem Assets._Test_STFL_language (Just "Assets/STFL.language") & either (error . show) id
stflSyntax	= get tsSyntax stfl

optionsSyntax	= parseTypeSystem Assets._Manual_Options_language (Just "Assets/Manual/Options.Language")
			& either (error . show) id
			& get tsSyntax


terminalStyle	= Assets._Terminal_style
			& parseColoringFile "Assets/Terminal.style"
			& either error id
