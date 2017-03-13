module AssetsHelper where

import Assets
import Utils.Utils

import TypeSystem.Parser.TypeSystemParser
import TypeSystem

import Data.List
import qualified Data.Map as M

import Control.Arrow ((&&&))

import SyntaxHighlighting.Coloring

stfl		= parseTypeSystem Assets._Test_STFL_language (Just "Assets/STFL.language") & either (error . show) id
stflSyntax	= get tsSyntax stfl

optionsSyntax	= parseTypeSystem Assets._Manual_Options_language (Just "Assets/Manual/Options.Language")
			& either (error . show) id
			& get tsSyntax


terminalStyle	= Assets._Terminal_style
			& parseColoringFile "Assets/Terminal.style"
			& either error id

knownStyles	:: M.Map Name FullColoring
knownStyles	= Assets.allAssets & filter ((".style" `isSuffixOf`) . fst)
			|> (fst &&& (\(fp, style) -> parseColoringFile ("Assets: "++fp) style & either error id))
			& M.fromList

fetchStyle name
		= let errMsg	= "No builtin style with name "++name ++ "\nBuiltin styles are:\n"
					++ (knownStyles & M.keys & unlines & indent)
			in
			checkExists (name++".style") knownStyles errMsg


minimalStyleTypes
	= Assets._MinimalStyles_txt & validLines
