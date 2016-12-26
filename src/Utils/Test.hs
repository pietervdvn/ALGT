module Utils.Test where

import AbstractInterpreter.Test 

import Utils.Utils
import Assets

import TypeSystem
import Parser.TypeSystemParser

t	= let 	stfl	= parseTypeSystem Assets._Test_STFL_typesystem (Just "Test_STFL")
		stfl'	= stfl & either (error . show) id
		in
		testAS $ tsSyntax stfl'
