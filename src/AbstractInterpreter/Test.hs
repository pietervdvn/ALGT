{-# LANGUAGE FlexibleContexts #-}
module AbstractInterpreter.Test where

import Prelude hiding (Subtract)
import TypeSystem
import TypeSystem.Parser.TypeSystemParser
import Utils.Utils
import Utils.ToString
import Utils.Unification
import Utils.LatticeImage
import Graphs.Lattice

import Assets

import AbstractInterpreter.AbstractSet as AS
import AbstractInterpreter.RuleInterpreter
import AbstractInterpreter.PatternMatcher

import AbstractInterpreter.Data

import Data.Map ((!), Map)
import qualified Data.Map as M
import Data.List
import Data.Bifunctor (first)

import Control.Monad
import Control.Arrow ((&&&))

import Main

t0	= iar
t1	= main' ["../Examples/STFL.typesystem"] & void
t2	= main' ["../Examples/STFL.typesystem", "--ia" ]& void

stfl	= parseTypeSystem Assets._Test_STFL_typesystem (Just "Test_STFL")& either (error . show) id
syntax	= get tsSyntax stfl


rls	= tsRules stfl ! "→"
evalCtx	= rls & filter ((==) "EvalCtx" . get ruleName) & head

e i	= generateAbstractSet syntax (show i) "e"
typ i	= generateAbstractSet syntax (show i) "type"

ePlus	= AsSeq ("e",0) [EveryPossible ("e",0) "0/0:0" "eL",ConcreteLiteral ("e",0) "+",EveryPossible ("e",0) "0/0:2" "e"]
ePlusN	= AsSeq ("e",0) [EveryPossible ("number",0) "0/0:0" "number",ConcreteLiteral ("e",0) "+",EveryPossible ("number",0) "0/0:2" "number"]


testRule	:: Rule -> IO ()
testRule rule	
	= do	putStrLn $ " Analysis of "++show (get ruleName rule)
		putStrLn $ "---------------"++replicate (length $ show $ get ruleName rule) '-'
		putStrLn $ toParsable $ interpretRule' stfl rule


evalRel	= stfl & get tsRelations & filter ((==) "→" . get relSymbol)  & head

holeMatch	= stfl & get tsRelations |> (id &&& buildHoleArgs) & M.fromList	:: Map Relation [Name]
holeNoMatch	= holeMatch ||>> ("No_match_"++)

buildHoleArgs r	= mapi (relModes r) |> first show ||>> show ||>> inParens |> uncurry (++) |> (get relSymbol r ++) 




iar		= rls |+> testRule & void
applicT		= rls |> interpretRule' stfl & concat |+> fillHoleWith holeMatch & either error id
{-
nonApplicTo	= subtractArgs syntax [e 0] applicTo
recApplic	= interpretRule' stfl evalCtx
applicTo'	= (nonApplicTo >>= interpretRule stfl evalCtx) |> possibleArgs :: [Arguments]
nonApplicTo'	= [subtractArgs syntax nonApp applicTo' | nonApp <- nonApplicTo ] & concat	:: [Arguments]
-}


s	:: (ToString' String a , Eq a) => [a] -> IO ()
s args	= (args & nub |> toParsable' "\n") & unlines & putStrLn


s'	:: [Arguments] -> IO ()
s' args	= (args & nub |> toCoParsable' "\n") & unlines & putStrLn
