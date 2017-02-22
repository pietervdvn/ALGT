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
import Utils.ToString
import Prelude hiding (subtract)

import TypeSystem
import Changer.Changes
import Graphs.Lattice

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.ASSubtract
import AbstractInterpreter.RelationAnalysis

import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Maybe

import Control.Arrow
import Control.Monad

import Lens.Micro hiding ((&))

import Debug.Trace


dynamize'	:: TypeSystem -> TypeName -> String -> [Symbol] -> [Symbol] -> Either String Changes
dynamize' ts rule typeErr addStuckStateRules addTypeErrCase
	= inMsg "While dynamizing" $
	  do	let checkAllExists' keys	= checkAllExists keys (get tsRelations' ts) (\k -> "No relation with name "++k++" found in "++get tsName ts)
		checkExists rule (get (tsSyntax . bnf) ts) $ "Syntactic form "++show rule++" to dynamize does not exist."
		addStuckStateRules'	<- checkAllExists' addStuckStateRules 
		addTypeErrCase'		<- checkAllExists' addTypeErrCase 
		return $ dynamize ts rule typeErr addStuckStateRules' addTypeErrCase'


dynamize	:: TypeSystem -> TypeName -> String -> [Relation] -> [Relation] -> Changes
dynamize ts rule typeErr addStuckStateRules addTypeErrCase
		= let	syntax		= get tsSyntax ts
			typeErrExpr	= MParseTree $ MLiteral ("",-1) typeErr
			changedSyntax	= Edit rule ([Literal typeErr], syntax & getWSMode & (M.! rule))
			ra		= analyzeRelations ts
			newRules	= addStuckStateRules |> calculateNewRulesFor ts ra rule typeErrExpr & concat & nub
			newRules'	= addTypeErrCase |> (\rel ->
						Rule ("Extra "++get relSymbol rel) [] $ RelationMet rel [bnfAsExpr $ Literal typeErr])
			newRulesCh	= (newRules ++ newRules')
						|> (\rule -> New (get ruleName rule) rule)
			in
			Changes "Dynamized" [changedSyntax] [] [] newRulesCh

trace'		:: String -> [()]
trace' msg	= {--} pass {-} trace (">> Dynamizing: "++msg) $ pass --}


calculateNewRulesFor	:: TypeSystem -> RelationAnalysis -> TypeName -> Expression -> Relation -> [Rule]
calculateNewRulesFor ts ra exprType typeErrExpr relation
	= do	trace' $ "exprType: "++exprType++", relation to add stuck state errs: "++get relSymbol relation
		
		let raIntro	= get raIntroduced ra
		let s		= get raSyntax ra
		let createStuckRule'
				= createStuckRule s relation typeErrExpr

		-- Projects derived names onto their original name, e.g. !(eL)(→)in0 --> eL; (eL)(→)in0 --> eL; ...
		let tnsToSuper	= raIntro & M.keys |> (toParsable &&& get tnsSuper)
					& M.fromList

		-- The tns of nonmatching forms
		let tns		= TypeNameSpec exprType (get relSymbol relation) In 0 False

		trace' $ "TNS is "++toParsable tns
		let nonMatchingASS	= (ra & get raIntroduced) ! tns

		-- Subtypes of nonMatchingAS, should be substituted with "TYPE-ERR" once too (and should be expanded to create rules for too)
		let extraForms	= nonMatchingASS |> fromEveryPossible & catMaybes
		trace' $ "Extra forms :" ++ showComma extraForms
		let tnsToSuper'	= M.union tnsToSuper (extraForms |> (id &&& id) & M.fromList)
		

		let extraRules	= [calculateNewRulesFor ts ra (tnsToSuper ! extraForm) typeErrExpr relation
					| extraForm <- extraForms, extraForm `M.member` tnsToSuper] & concat

		-- these subtypes should not be added as rules, but expanded
		nonMatchingAS	<- subtractAll s nonMatchingASS (extraForms |> generateAbstractSet s "")


 		trace' $ toParsable nonMatchingAS
		let isRecursiveCall as	= fromEveryPossible as |> (`M.member` tnsToSuper) & fromMaybe False
		let recursivePaths	= searchPathAS isRecursiveCall nonMatchingAS
		trace' $ show recursivePaths

		if L.null recursivePaths then createStuckRule' nonMatchingAS:extraRules	-- no recursive forms, we return it 'as is'
		else do
			let typeErrAs		= fromExpression s "" typeErrExpr
			-- we replace **each** recursive form by "TYPE ERROR". This is because ((1 + 1) + "TYPE ERROR") _can_ be smallstepped, to 2 + "TYPE ERROR"
			let nonMatchingAS'	= L.foldl (\as path -> replaceAS as path typeErrAs) nonMatchingAS recursivePaths
			trace' ("Non matching as subsed: "++toParsable nonMatchingAS')
			-- path			<- recursivePaths
			-- let nonMatching		= replaceAS nonMatchingAS' path (fromExpression s "" typeErrExpr)
			createStuckRule' nonMatchingAS':extraRules
		



createStuckRule	:: Syntax -> Relation -> Expression -> AbstractSet -> Rule
createStuckRule s rel typeErr as
	= Rule ("Type Error: "++toParsable as) [] $ RelationMet rel [toExpression s as, typeErr]



replacePathByParent	:: Syntax -> Path -> Map TypeName TypeName -> AbstractSet -> AbstractSet
replacePathByParent s p tnsToSuper as
	= let	recName		= getAsAt as p & fromEveryPossible & fromJust	:: TypeName
		wantedName	= tnsToSuper ! recName				:: TypeName
		wantedAS	= generateAbstractSet s "" wantedName		:: AbstractSet
		in
		replaceAS as p wantedAS
