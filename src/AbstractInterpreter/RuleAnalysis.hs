{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
module AbstractInterpreter.RuleAnalysis where

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
import AbstractInterpreter.FunctionAnalysis

import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe
import Data.Bifunctor

import Control.Monad
import Control.Arrow ((&&&))

import Lens.Micro hiding ((&))
import Lens.Micro.TH

type AbstractConclusion 	= ConclusionA AbstractSet


data RuleApplication	= RuleApplication
	{ _possibleArgs	:: Arguments
	, _assignment	:: Assignments
	, _conclusion	:: AbstractConclusion
	, _predicates	:: [AbstractConclusion]
	} deriving (Show, Eq)
makeLenses ''RuleApplication

type RuleAnalysis = [RuleApplication]

isTrivial	:: RuleApplication -> Bool
isTrivial rapp
	= get conclusion rapp `elem` get predicates rapp

isRecursive	:: RuleApplication -> Bool
isRecursive rapp
	= (get conclusion rapp & get conclusionRel) `elem` (get predicates rapp |> get conclusionRel)


applySubsSimple	:: Substitution AbstractSet -> RuleApplication -> RuleApplication
applySubsSimple subs rapp
	= rapp	& over possibleArgs 	(|> substitute subs)
		& over assignment 	(|> first (substitute subs))
		& over conclusion	(|> substitute subs)
		& over predicates	(||>> substitute subs)







interpretRule'	:: TypeSystem -> Rule -> RuleAnalysis
interpretRule' ts rule@(Rule _ _ concl )
	= let	argExprs	= filterMode In (get conclusionRel concl) (get conclusionArgs concl)
		args	= mapi argExprs |> (\(i, e) -> fromExpression (get tsSyntax ts) (show i) e)
		in
		interpretRule ts rule args


interpretRule	:: TypeSystem -> Rule -> Arguments -> RuleAnalysis
interpretRule ts (Rule _ preds concl) args
	= do	baseAnalysis	<- interpretConclusion ts concl args
		let analysis	= foldr (handlePredicate ts) baseAnalysis preds
		let assgn	= get assignment analysis
		-- we apply the conclusion and predicates again as to make sure all variables are evaluated
		analysis & set conclusion (evalConcl' ts assgn concl)
			& set predicates (preds & mapMaybe (evalPred' ts assgn))
			& return 
					


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



handlePredicate	:: TypeSystem -> Predicate -> RuleApplication -> RuleApplication
handlePredicate ts (TermIsA nm tp) ranalysis
	= ascribe (get tsSyntax ts) (nm, tp) ranalysis
handlePredicate ts (Same e1 e2) ranalysis
	= unifyPreds ts (e1, e2) ranalysis
handlePredicate ts (Needed concl) ranalysis
	= addNeeded ts concl ranalysis




-- while the other two are just simple substititutions, a 'conclusion' predicate might introduce new variables
addNeeded	:: TypeSystem -> ConclusionA Expression -> RuleApplication -> RuleApplication
addNeeded ts concl ruleApp
	= let	assgn	= get assignment ruleApp
		r	= get conclusionRel concl
		outs	= get conclusionArgs concl & filterMode Out r
		newVars	= (outs >>= usedVariables) & filter ((`M.notMember` assgn) . fst)
		newVars'= newVars |> (fst &&& uncurry (generateAbstractSet (get tsSyntax ts)))
				& M.fromList |> flip (,) Nothing	:: Assignments
		assgn'	= M.union newVars' assgn
		concl'	= concl |> evalExpr' ts assgn' 
		in
		ruleApp	& over predicates (concl':)
	
		& set assignment assgn'

applySubs	:: (Assignments -> Substitution AbstractSet) -> RuleApplication -> RuleApplication
applySubs buildSubs (RuleApplication args assignment concl preds)
	= let	subs	= buildSubs assignment in
		RuleApplication
			(args |> substitute subs)
			(assignment |> first (substitute subs))
			(concl |>  substitute subs)
			(preds ||>> substitute subs)




ascribe	:: Syntax -> (Name, TypeName) -> RuleApplication -> RuleApplication
ascribe s (k, v) ranalysis
	= let 	-- we known (n1 :: number); and we now (n1 --> e1/0:2). Merge so that (e1/0:2 :: number)
		buildAsc' assgn =  ((assgn ! k) & fst & getName, v)
		-- actually build substitution, reuse the name
		buildSubs assgn	= buildAsc' assgn & uncurry M.singleton & M.mapWithKey (generateAbstractSet s)
		in
		applySubs buildSubs ranalysis	
		

unifyPreds	:: TypeSystem -> (Expression, Expression) -> RuleApplication -> RuleApplication
unifyPreds ts toUnify
	= applySubs (buildUnifyPred ts toUnify) 


buildUnifyPred	:: TypeSystem -> (Expression, Expression) -> Assignments -> Substitution AbstractSet
buildUnifyPred ts (e0, e1) assgn
	= let	as0	= evalExpr' ts assgn e0
		as1	= evalExpr' ts assgn e1
		in
		either error id $ unify as0 as1



evalExpr'	:: TypeSystem -> Assignments -> Expression -> AbstractSet
evalExpr' ts assgn e
	= evalExpr (get tsFunctions ts |> typesOf |> last) assgn e & either error id


evalConcl'	:: TypeSystem -> Assignments -> Conclusion -> AbstractConclusion
evalConcl' ts assgn concl
	= concl |> evalExpr' ts assgn

evalPred'	:: TypeSystem -> Assignments -> Predicate -> Maybe AbstractConclusion
evalPred' ts assgns (Needed concl)
		= Just $ evalConcl' ts assgns concl
evalPred' _ _ _	= Nothing


instance ToString RuleAnalysis where
	toParsable matchingForms
		= ["Applicable to:"
			, indent (matchingForms |> get possibleArgs ||>> toParsable |> nub & nub |> intercalate ", " & unlines)
			, "Results:"
			, indent (toParsable' "\n" matchingForms)] & unlines


instance ToString RuleApplication where
	toParsable rapp@(RuleApplication _ _ concl preds)
		= let 	rec	= if isRecursive rapp then "(recursion) " else ""  in
			toCoParsable concl ++ if null preds then "" else "\n\t where "++ rec ++ toCoParsable' ", " preds

