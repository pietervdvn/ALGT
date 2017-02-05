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
import AbstractInterpreter.RelationAnalysis

import Data.Map as M
import Data.List as L
import Data.Set as S

import Control.Arrow
import Control.Monad

import Lens.Micro hiding ((&))

dynamize	:: TypeSystem -> TypeName -> String -> [Relation] -> [Relation] -> Changes
dynamize ts rule typeErr addStuckStateRules addTypeErrCase
		= let	syntax		= get tsSyntax ts
			typeErrExpr	= MParseTree $ MLiteral ("",-1) typeErr
			changedSyntax	= Edit rule ([Literal typeErr], syntax & getWSMode & (M.! rule))
			newRules	= addStuckStateRules |> calculateNewRulesFor ts rule typeErrExpr & concat
			newRules'	= addTypeErrCase |> (\rel ->
						Rule ("Extra "++get relSymbol rel) [] $ RelationMet rel [bnfAsExpr $ Literal typeErr])
			newRulesCh	= (newRules ++ newRules')
						|> (\rule -> New (get ruleName rule) rule)
			in
			Changes "Dynamized" [changedSyntax] [] [] newRulesCh




calculateNewRulesFor	:: TypeSystem -> TypeName -> Expression -> Relation -> [Rule]
calculateNewRulesFor ts typeType typeErrExpr relation
	= do	let syntax	= get tsSyntax ts
		let symb	= get relSymbol relation
		let ra		= analyzeRelations ts
		(i, tps)	<- relation & relTypesWith In & mapi
		tp		<- allSubsetsOf (get lattice syntax) tps & S.toList
		let tns		= TypeNameSpec tp symb In i False
		let genConcl bnf
				= RelationMet relation [ bnfAsExpr bnf , typeErrExpr]
		createRulesFor ra genConcl tns



createRulesFor	:: RelationAnalysis -> (BNF -> Conclusion) -> TypeNameSpec -> [Rule]
createRulesFor ra genConcl tns
	= do	let syntax	= get raSyntax ra
		let recursive	= get raIntroduced ra & M.keys |> toParsable
		nonMatching	<- M.findWithDefault [] (toParsable tns) (get bnf syntax)
		let isRecursive	= calledRules nonMatching
					& any (`elem` recursive)
		guard (not isRecursive)
		let concl	= genConcl nonMatching
		let rule	= Rule ("TErr" ++ toParsable nonMatching) [] concl
		return rule


-- TODO introduce this guy
recursiveRule	:: Expression -> Relation -> TypeName -> Rule
recursiveRule errExpr relation tn
	= let	concl	= RelationMet relation [MEvalContext tn tn $ MVar tn (tn++"0"), errExpr] in
		Rule "TErrCtx" [Needed $ RelationMet relation [MVar tn (tn++"0"), errExpr]] concl










