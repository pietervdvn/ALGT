module Dynamize.Test where

{-  -}

import Utils.Utils
import Utils.ToString

import Dynamize.Dynamize
import AssetsHelper (stfl)

import Changer.Changes

import Data.Maybe
import TypeSystem

t	= do	let rel	= fromJust $ findRelation stfl "→"
		let ch = dynamize stfl "e" "TYPE ERROR" [rel]
		putStrLn $ toParsable' (16::Int) ch
		putStrLn $ toParsable' (16::Int) $ either error id $ applyChanges ch stfl
		putStrLn $ toParsable' (16::Int) ch
		
