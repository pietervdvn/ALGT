{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AbstractInterpreter.RuleInterpreter where

{-
This module defines an abstract intperpreter for Rules
-}


import TypeSystem
import Utils.Utils
import Utils.ToString
import Utils.Unification

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.Data
import AbstractInterpreter.PatternMatcher
import AbstractInterpreter.FunctionInterpreter

import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe
import Data.Bifunctor

import Control.Monad
import Control.Arrow ((&&&))

type AbstractConclusion 	= ConclusionA AbstractSet


data RuleApplication	= RuleApplication
	{ possibleArgs	:: Arguments
	, assignment	:: Assignments
	, conclusion	:: AbstractConclusion
	, predicates	:: [AbstractConclusion]
	} deriving (Show, Eq)


type RuleAnalysis = [RuleApplication]

isTrivial	:: RuleApplication -> Bool
isTrivial rapp
	= conclusion rapp `elem` predicates rapp

isRecursive	:: RuleApplication -> Bool
isRecursive rapp
	= (conclusion rapp & conclusionRel) `elem` (predicates rapp |> conclusionRel)


applySubs	:: (Assignments -> Substitution AbstractSet) -> RuleAnalysis -> RuleAnalysis
applySubs buildSubs ran
	= ran |> _applySubs buildSubs


impossibleArgs	:: Syntax -> RuleAnalysis -> [Arguments]
impossibleArgs s ran
	= let 	rel		= ran & head & conclusion & conclusionRel
		inTypes		= relTypesWith In rel & generateArgs s	:: Arguments
		-- the 'normal' inapplicable forms
		classicalImpArgs	= subtractArgs s inTypes (ran |> possibleArgs)
		in classicalImpArgs


fillHoleWith	:: Map Relation [Name] -> RuleApplication -> Either String RuleApplication
fillHoleWith assignments (RuleApplication possibleArgs assignment conclusion predicates)
	= do	subs	<- predicates |+> matchHoles assignments |> M.unions
		let possibleArgs'	= possibleArgs	|> substitute subs
		let assignment'		= assignment	|> first (substitute subs)
		let conclusion'		= conclusion	|> substitute subs
		let predicates'		= predicates	||>> substitute subs
		return (RuleApplication possibleArgs' assignment' conclusion' predicates')

matchHoles	:: Map Relation [Name] -> AbstractConclusion -> Either String (Substitution AbstractSet)
matchHoles assignments concl@(RelationMet r args)
	= do	let args'	= assignments ! r
		unless (all isEveryPossible args) $ Left $ "Could not match holes, "++toParsable concl++" contains structure with input/output arguments"
		let nms		= args |> (\(EveryPossible _ n _) -> n) 
		zip nms args' ||>> (\t -> EveryPossible (t, -1) t t) & M.fromList & return


_applySubs	:: (Assignments -> Substitution AbstractSet) -> RuleApplication -> RuleApplication
_applySubs buildSubs (RuleApplication args assignment concl preds)
	= let	subs	= buildSubs assignment in
		RuleApplication
			(args |> substitute subs)
			(assignment |> first (substitute subs))
			(concl |>  substitute subs)
			(preds ||>> substitute subs)



setConclusion ts concl ran
	= ran  |> _setConclusion ts concl

_setConclusion	:: TypeSystem -> Conclusion -> RuleApplication -> RuleApplication
_setConclusion ts concl rapp
	= rapp {conclusion = concl |> evalExpr' ts (assignment rapp) }


interpretRule'	:: TypeSystem -> Rule -> RuleAnalysis
interpretRule' ts rule@(Rule _ _ (RelationMet r _) )
	= let	args	= generateArgs (get tsSyntax ts) (relTypesWith In r)
		in
		interpretRule ts rule args


interpretRule	:: TypeSystem -> Rule -> Arguments -> RuleAnalysis
interpretRule ts (Rule _ predicates concl) args
	= let	baseAnalysis	= interpretConclusion ts concl args
		ranalysis	= foldr (handlePredicate ts) baseAnalysis predicates
		in
		ranalysis & setConclusion ts concl 
					


interpretConclusion	:: TypeSystem -> Conclusion -> Arguments -> RuleAnalysis
interpretConclusion ts (RelationMet r exprs) args
	= do	let syntax	= get tsSyntax ts
		let patterns	= filterMode In r exprs
		when (length patterns /= length args) $ error "Number of arguments does not match the expected input"
		assignments	<- zip patterns args |> uncurry (patternMatch syntax) & allCombinations
		assignment	<- foldl (\assgs0 assgs1 -> assgs0 >>= mergeAssgns syntax assgs1) [M.empty] assignments 
		let usedArgs	= patterns |> evalExpr' ts assignment	-- AKA the 'ins'
		let results	= filterMode Out r exprs |> evalExpr' ts assignment	-- AKA the 'outs'
		let concl	=  RelationMet r $ weaveMode (relModes r) usedArgs results
		return $ RuleApplication usedArgs assignment concl []



handlePredicate	:: TypeSystem -> Predicate -> RuleAnalysis -> RuleAnalysis
handlePredicate ts (TermIsA nm tp) ranalysis
	= ascribe (get tsSyntax ts) (nm, tp) ranalysis
handlePredicate ts (Same e1 e2) ranalysis
	= unifyPreds ts (e1, e2) ranalysis
handlePredicate ts (Needed concl) ranalysis
	= addNeeded ts concl ranalysis



addNeeded	:: TypeSystem -> ConclusionA Expression -> RuleAnalysis -> RuleAnalysis
addNeeded ts concl ran
	= ran |> _addNeeded ts concl


-- while the other two are just simple substititutions, a 'conclusion' predicate might introduce new variables
_addNeeded	:: TypeSystem -> ConclusionA Expression -> RuleApplication -> RuleApplication
_addNeeded ts concl ruleApp
	= let	assgn	= assignment ruleApp
		r	= conclusionRel concl
		outs	= conclusionArgs concl & filterMode Out r
		newVars	= (outs >>= usedVariables) & filter ((`M.notMember` assgn) . fst)
		newVars'= newVars |> (fst &&& uncurry (generateAbstractSet (get tsSyntax ts)))
				& M.fromList |> flip (,) Nothing	:: Assignments
		assgn'	= M.union newVars' assgn
		concl'	= concl |> evalExpr' ts assgn' 
		in
		ruleApp{ predicates = concl' : predicates ruleApp, assignment = assgn' }





ascribe	:: Syntax -> (Name, TypeName) -> RuleAnalysis -> RuleAnalysis
ascribe s (k, v) ranalysis
	= let 	-- we known (n1 :: number); and we now (n1 --> e1/0:2). Merge so that (e1/0:2 :: number)
		buildAsc' assgn =  ((assgn ! k) & fst & getName, v)
		-- actually build substitution, reuse the name
		buildSubs assgn	= buildAsc' assgn & uncurry M.singleton & M.mapWithKey (generateAbstractSet s)
		in
		applySubs buildSubs ranalysis	
		

unifyPreds	:: TypeSystem -> (Expression, Expression) -> RuleAnalysis -> RuleAnalysis
unifyPreds ts toUnify
	= applySubs (buildUnifyPred ts toUnify) 


buildUnifyPred	:: TypeSystem -> (Expression, Expression) -> Assignments -> Substitution AbstractSet
buildUnifyPred ts (e0, e1) assgn
	= let	as0	= evalExpr' ts assgn e0
		as1	= evalExpr' ts assgn e1
		in
		either error id $ unify as0 as1







evalExpr'	:: TypeSystem -> Assignments -> Expression -> AbstractSet
evalExpr' ts
	= evalExpr (get tsFunctions ts |> typesOf |> last) 



instance ToString RuleAnalysis where
	toParsable matchingForms
		= "Applicable to:\n" ++ indent (matchingForms |> possibleArgs ||>> eraseDetails & nub |> toParsable' ", " & unlines) ++
			"Results:\n" ++ indent (toParsable' "\n" matchingForms)


instance ToString RuleApplication where
	toParsable rapp@(RuleApplication _ _ concl preds)
		= let 	rec	= if isRecursive rapp then "(recursion) " else ""  in
			toCoParsable concl ++ if null preds then "" else "\t where "++ rec ++ toCoParsable' ", " preds

