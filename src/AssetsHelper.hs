module AssetsHelper where

import Assets
import Utils.Utils

import TypeSystem.Parser.TypeSystemParser
import TypeSystem

stfl		= parseTypeSystem Assets._Test_STFL_typesystem (Just "Assets/STFL.typesystem") & either (error . show) id
stflSyntax	= get tsSyntax stfl
