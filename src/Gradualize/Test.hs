module Gradualize.Test where

{-  -}

import Utils.Utils
import Changer.Changes

import Gradualize.DynamicRuntime

import Utils.ToString
import AssetsHelper

t	= dynamized stfl ["=="] & either error id & toParsable' (24::Int)
		& putStrLn
