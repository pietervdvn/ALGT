 {-# LANGUAGE FlexibleContexts #-}
module Dynamize.Test where

import Prelude hiding (subtract)
import Utils.Utils
import Utils.ToString

import Dynamize.Dynamize
import AssetsHelper (stfl)

import Changer.Changes

import TypeSystem

import AssetsHelper
import Data.Map as M
import Data.List (sort)
import Data.Maybe

import AbstractInterpreter.RelationAnalysis
import AbstractInterpreter.AbstractSet


t	= t1

t1	= do	let rel	= fromJust $ findRelation stfl "→"
		let rel'= fromJust $ findRelation stfl "✓"
		let ch = dynamize stfl "e" "TYPE ERROR" [rel] [rel']
		putStrLn $ toParsable' (16::Int) ch
		putStrLn $ toParsable' (16::Int) $ either error id $ applyChanges ch stfl
		putStrLn $ toParsable' (16::Int) ch
		

ra	= analyzeRelations stfl
s	= get raSyntax ra

asT	= generateAbstractSet s "" "type" & (:[])
asE	= generateAbstractSet s "" "e" & (:[])
asEl	= generateAbstractSet s "" "eL" & unfold s & (!! 2) & (:[])
asElArr	= generateAbstractSet s "" "(eL)(→)in0" & unfold s & (!! 1) & (:[])

subtractions
	= M.fromList [( ("eL", "(eL)(→)in0") ,"!(eL)(→)in0"), ( ("e", "(e)(→)in0") ,"!(e)(→)in0")]

t0	:: IO ()
t0	= do	putStr "asEl\n\t| "
		toParsIO asEl
		putStr "asElArr\n\t| "
		toParsIO asElArr
		putStr "asEl - asElArr:\n\t| "
		subtractAllWith s subtractions asEl asElArr
			& toParsIO

toParsIO vals	= vals & sort & toParsable' "\n\t| " & putStrLn

toCoParsIO vals	= vals & sort & toCoParsable' "\n\t| " & putStrLn


fAsSeq as	= fromAsSeq as & fromJust


tBool		=  ConcreteLiteral miT "Bool"
tInt		= ConcreteLiteral miT "Int"

arg1		= AsSeq miE  [EveryPossible miE "bool" "bool", ConcreteLiteral miE "::", tBool]
arg2		= AsSeq miE  [EveryPossible miE "number" "number", ConcreteLiteral miE "::", tInt]

miE		= ("e", -1)
miT		= ("typeL", -1)


t2	= subtractAll s asE [arg1,arg2]
t2a	= subtract s asE arg1
t3	= subtractAll s asT [tBool,tInt]

