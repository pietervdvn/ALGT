 {-# LANGUAGE FlexibleContexts #-}
module Dynamize.Test where

import Prelude hiding (subtract)
import Utils.Utils
import Utils.ToString

import Dynamize.Dynamize

import Changer.Changes

import TypeSystem

import AssetsHelper
import Data.Map as M
import Data.List (sort)
import Data.Maybe

import AbstractInterpreter.RelationAnalysis
import AbstractInterpreter.AbstractSet
import AbstractInterpreter.ASSubtract



t	= do	let rel	= fromJust $ findRelation stfl "→"
		let rel'= fromJust $ findRelation stfl "✓"
		let ch = dynamize stfl "e" "TYPE ERROR" [rel] [rel']
		putStrLn $ toParsable' (16::Int) ch
		putStrLn $ toParsable' (16::Int) $ either error id $ applyChanges ch stfl
		putStrLn $ toParsable' (16::Int) ch

