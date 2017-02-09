module AssetsHelper where

import Assets
import Utils.Utils

import TypeSystem.Parser.TypeSystemParser
import TypeSystem

stfl		= parseTypeSystem Assets._Test_STFL_language (Just "Assets/STFL.language") & either (error . show) id
stflSyntax	= get tsSyntax stfl
