module Dynamize.Dynamize where

{- 
This module takes a typesystem and tries to 'dynamize' the evaluation rule.
It does this by
 1. Trying all possible expressions, unfolded over a limited depth
 2. Detecting which forms don't evaluate (and are 'Stuck')
 3. Introducing a "RUNTIME ERROR" to the values
 4. Adding evalution rules for those stuck states, so they evaluate to "RUNTIME ERROR"

Note that an evaluation relation which no strict order of evaluation might not work anymore, e.g.

"(" e ")" → e

And

e[e0] → e[e1]

Might diverge on:

eFaulty → "RUNTIME ERROR"

as both (non-same) derivations might be calculated

"(" eFaulty ")" → eFaulty

"(" eFaulty ")" → "(" "RUNTIME ERROR" ")"

 -}

import Utils.Utils

import TypeSystem
import Changer.Changes

import AbstractInterpreter.FullAnalysis

import Data.Map as M

import Control.Arrow

dynamize	:: TypeSystem -> TypeName -> String -> [Relation] -> Changes
dynamize ts rule typeErr addStuckStateRules
		= let	syntax		= get tsSyntax ts
			changedSyntax	= Edit rule ([Literal typeErr], syntax & getWSMode & (M.! rule))
			newRules	= addStuckStateRules |> calculateNewRulesFor ts & concat
						|> (\rule -> New (get ruleName rule) rule)
			in
			Changes "Dynamized" [changedSyntax] [] [] newRules




calculateNewRulesFor	:: TypeSystem -> Relation -> [Rule]
calculateNewRulesFor ts relation
	= let	analysisSyntax	= "hi" in
		error $ "Fix rules for "++get relSymbol relation





























