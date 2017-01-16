module Changer.Test where

import TypeSystem
import Utils.Utils
import Utils.ToString
import TypeSystem.Parser.TypeSystemParser
import Assets

import Data.Either

import Changer.Changes
import Changer.ChangesParser



stfl	= parseTypeSystem Assets._Test_STFL_typesystem (Just "Test_STFL")& either (error . show) id
syntax	= get tsSyntax stfl

changes0	= Assets._Test_DynamizeSTFL_typesystem_changes
changes1	= Assets._Test_GradualizeSTFL_typesystem_changes

t'	= parseChanges stfl changes1 (Just "TEST-DYN-Changes") & either (error . show) id
t	= t' & fst & toParsable' (24::Int) & ("\n"++) & putStrLn

{--

t'	= & rights & toParsable' "\n"

--}
