module Gradualize.Test where

{-  -}

import Utils.Utils
import Changer.Changes

import TypeSystem

import AbstractInterpreter.AbstractSet

import Gradualize.DynamicRuntime
import Gradualize.FunctionFixer

import Data.Map as M

import Utils.ToString
import AssetsHelper

import Lens.Micro hiding ((&))


t	= t' >> pass

t'	:: IO (TypeSystem, Changes)
t'	= do	(ts, dyn, ch)	<- fixSyntax stfl "?" "type" ("concr", "typeSet") & either error return
		ts & toParsable' (24::Int) & putStrLn


		-- concretization	:: TypeSystem -> TypeName -> Name -> String -> [AbstractSet] -> ParseTree -> Either String [AbstractSet]
		let dynSet	= [generateAbstractSet (get tsSyntax ts) "" "type"]
		let concrFunc pt
				= concretization ts "type" "concr" dyn dynSet pt & either error id

		let testPT1	= PtSeq ("type", -1) [MLiteral ("typeL", 1) "Bool", MLiteral ("type",0)  "->", MLiteral ("typeL", 1) "Bool"]
		let testPT2	= PtSeq ("type", -1) [dyn, MLiteral ("type",0)  "->", MLiteral ("typeL", 1) "Bool"]
		let testPT3	= PtSeq ("type", -1) [dyn, MLiteral ("type",0)  "->", dyn]

		print $ concrFunc $ testPT2
		print $ concrFunc $ testPT3


		--gradualizeFunc	:: TypeSystem -> TypeName -> String -> Name -> Name -> Name -> Either String Clause
		let extraClause	= gradualizeFunc ts "type" "?" "concr" "abstract" "dom" & either error id
		
		putStrLn $ toParsable' ("domain-extra", 24::Int) extraClause

		return (ts, ch)
