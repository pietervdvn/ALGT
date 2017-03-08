module Gradualize.Test where

{-  -}

import Utils.Utils
import Changer.Changes

import TypeSystem

import AbstractInterpreter.AbstractSet

import Gradualize.DynamicRuntime
import Gradualize.FunctionFixer

import Text.PrettyPrint.ANSI.Leijen

import Data.Map as M
import Data.List as L

import Utils.ToString
import AssetsHelper

import Lens.Micro hiding ((&))


t	= t' >> pass

t'	:: IO (TypeSystem, Changes)
t'	= do	(ts, dyn, ch)	<- fixSyntax stfl "?" "type" & either error return
		ts & toParsable' (24::Int) & putStrLn
		let s		= get tsSyntax ts

		-- concretization	:: TypeSystem -> TypeName -> Name -> String -> [AbstractSet] -> ParseTree -> Either String [AbstractSet]
		let dynSet i	= [generateAbstractSet (get tsSyntax ts) (show i) "type"]
		let concrFunc	= concretization (dyn, dynSet)	:: ParseTree -> Arguments

		let ptBool	= MLiteral ("typeL", 1) "Bool"
		let testPT1	= PtSeq ("type", -1) [ptBool, MLiteral ("type",0)  "->", ptBool]
		let testPT2	= PtSeq ("type", -1) [dyn, MLiteral ("type",0)  "->", ptBool]
		let testPT3	= PtSeq ("type", -1) [dyn, MLiteral ("type",0)  "->", dyn]


		let testPT args0 args1
				= (do	arg0	<- args0 & concrFunc
					arg1	<- args1 & concrFunc
					let ass	
						= possibleResults ts "equate" [arg0, arg1] & either error id	:: [AbstractSet]
							
					let showed
						= inParens (toParsable arg0 ++ ", " ++ toParsable arg1) ++ " = " ++
							if L.null ass then "ɛ" else toParsable' "\n\t" ass
					return showed 
					)

		testPT dyn dyn
			|> (">>>"++) |+> putStrLn
	
		-- testPT3 & concrFunc & toParsable' " | " & putStrLn

		return (ts, ch)


