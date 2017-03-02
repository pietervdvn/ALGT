module Gradualize.Test where

{-  -}

import Utils.Utils
import Changer.Changes

import TypeSystem

import Gradualize.DynamicRuntime
import Gradualize.FunctionFixer

import Data.Map as M

import Utils.ToString
import AssetsHelper

t	= gradualizeFunc stfl "type" "?" "concr" "abstract" "dom"
		& either error id
