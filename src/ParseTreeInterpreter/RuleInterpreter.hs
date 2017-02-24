module ParseTreeInterpreter.RuleInterpreter where

{-
This module defines an evaluator for rules
-}


import TypeSystem
import Utils.ToString
import Utils.Utils

import ParseTreeInterpreter.FunctionInterpreter

import Data.Map (Map, empty, findWithDefault)
import Data.Either
import Data.List

import Data.Bifunctor (first)

import Control.Monad

import Lens.Micro hiding ((&))


proofThat	:: TypeSystem -> Relation -> [ParseTree] -> Either String Proof
proofThat ts
	= proofThat' ts . get relSymbol

-- tries to deduce a proof for application to a given relation, might return multiple (top level) proofs
proofThats'	:: TypeSystem -> Symbol -> [ParseTree] -> Either String [Proof]
proofThats' ts symbol args
	= inMsg ("While trying to proof that ("++symbol++") is applicable to \""++ toParsable' ", " args ++"\"") $
	  do	rules	<- ts & get tsRules |> return & findWithDefault (Left $ "No rules about a relation with symbol "++show symbol) symbol
		let results	= rules |> flip (interpretRule ts) args
		let successfull	= rights results
		assert Left (not $ null successfull) $ "Not a single rule matched:\n"++unlines (lefts results)
		return successfull

-- For a given relation + input, deduces all possible proofs, and takes the proof with the smallest size (if all proofs give the same conclusion)
proofThat'	:: TypeSystem -> Symbol -> [ParseTree] -> Either String Proof
proofThat' ts symbol args
	= do	successfull	<- proofThats' ts symbol args
		let conclusions	= successfull & filter isProof |> _proofConcl & nub
		assert Left (1 == length conclusions) $ "Multiple rules provided a proof with different conclusions:\n"++ 
				(successfull |> toParsable |> lines ||>> ("#  "++) |> unlines >>= (++"\n"))
		let successfull'	= successfull & sortOn weight 
		return $ head successfull'




-- given a rule and 'input' expressions, (tries to) give a proof for it
interpretRule	:: TypeSystem -> Rule -> [ParseTree] -> Either String Proof
interpretRule ts r args
	= inMsg ("While trying to intepret the rule "++get ruleName r++" with "++ toParsable' ", " args) $
	  do	let concl@(RelationMet rel conclArgs)	= get ruleConcl r
		variables	<- patternMatchInputs ts (get rulePreds r) (rel, conclArgs) args
		(predicateProofs, vars')
				<- proofPredicates ts variables (get rulePreds r) 
		
		conclArgs'	<- conclArgs |> evalExpr ts vars' & allRight
		let concl'	= set conclusionArgs conclArgs' concl
		return $ Proof concl' r predicateProofs

proofPredicates :: TypeSystem -> VariableAssignments -> [Predicate] -> Either String ([Proof], VariableAssignments)
proofPredicates ts vars preds
	= proofPredicates' ts [] vars preds & first trd3

-- predicates are proven from left to right, as a left predicate might introduce variables used by a following predicate
proofPredicates' :: TypeSystem -> [Proof] -> VariableAssignments -> [Predicate] -> Either ([Proof], VariableAssignments, String) ([Proof], VariableAssignments)
proofPredicates' _ _ vars [] 	= return ([], vars)
proofPredicates' ts prevProofs vars (pred:preds)
	= do	let amendFailure	= first (\str -> (prevProofs, vars, str))
		(proof, vars')		<- proofPredicate ts vars preds pred & amendFailure
		variables		<- mergeVars vars vars' & amendFailure
		(proofs, variables')	<- proofPredicates' ts (prevProofs++[proof]) variables preds
		return (proof:proofs, variables')


proofPredicate	:: TypeSystem -> VariableAssignments -> [Predicate] -> Predicate -> Either String (Proof, VariableAssignments)
proofPredicate ts vars _ (TermIsA expr typ)
	= do	expr'	<- evalExpr ts vars (MVar typ expr)
		unless (alwaysIsA (get tsSyntax ts) (typeOf expr') typ) $ Left $
			expr ++ " = "++ toCoParsable expr' ++ " is not a "++show typ
		return (ProofIsA expr' typ, empty)
proofPredicate ts vars _ (Same e1 e2)
	= do	e1'	<- evalExpr ts vars e1
		e2' 	<- evalExpr ts vars e2
		unless (e1' == e2') $ Left $ 
			"Equality predicate not met: "++ toParsable e1 ++ "=" ++ toCoParsable e1' ++ " /= " ++ toCoParsable e2' ++"="++toParsable e2
		return (ProofSame e1' e1 e2, vars)

proofPredicate ts vars restingPreds (Needed concl)
	= proofConclusion' ts vars restingPreds concl


proofConclusion'	:: TypeSystem -> VariableAssignments -> [Predicate] -> Conclusion -> Either String (Proof, VariableAssignments)
proofConclusion' ts vars restingPreds (RelationMet relation args)
	= do	let inputArgs	= filterMode In relation args
		args'		<- inputArgs |+> evalExpr ts vars
		proof		<- proofThat ts relation args'
		let concl	= _proofConcl proof
		-- now, we take the 'out'-expresssions from the conclusion and pattern match those into the out of the needed
		let toMatch	= filterMode Out relation (zip args (get conclusionArgs concl))	:: [(Expression, ParseTree)]
		matched		<- matchAndMerge ts restingPreds toMatch	-- this predicate might contain a context evaluation
		return (proof, matched)


proofConclusion ts vars concl
	= proofConclusion' ts vars [] concl |> fst


-- pattern matches the given parsetrees (args), gives the assignment. Note that the given predicates are tried (in the case of an evaluation context, it might influence the choice of the hole)
patternMatchInputs	:: TypeSystem -> [Predicate] -> (Relation, [Expression]) -> [ParseTree] -> Either String VariableAssignments
patternMatchInputs ts predicates (rel, relationArgs) args
	= do	let inputTypes	= filterMode In rel (relType rel)
		assert Left (length inputTypes == length args) $ "Expected "++show (length inputTypes)++" arguments, but got "++show (length args)++" arguments instead" 
		let typesMatch arg expected	= assert Left (equivalent (get tsSyntax ts) (typeOf arg) expected) 
							("Expected type "++show expected++" for "++toCoParsable arg++" which has the type "++show (typeOf arg))
		zip args inputTypes |> uncurry typesMatch & allRight
		let patterns = filterMode In rel relationArgs
		matchAndMerge ts predicates $ zip patterns args 
		
		

	
-- pattern matches each parsetree into it's accompanying expression. Returns an assignment
matchAndMerge	:: TypeSystem -> [Predicate] -> [(Expression, ParseTree)] -> Either String VariableAssignments
matchAndMerge ts predicates patsArgs
	= do	let evalContextMatches assngs	= proofPredicates ts assngs predicates & void
		matches	<- patsArgs |+> uncurry (patternMatch (buildCtx' ts) evalContextMatches)
		matches & mergeVarss


