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

import Lens.Micro hiding ((&))


proofThat	:: TypeSystem -> Relation -> [ParseTree] -> Either String Proof
proofThat ts
	= proofThat' ts . get relSymbol

-- tries to deduce a proof for a given property, might return multiple (top level) proofs
proofThats'	:: TypeSystem -> Symbol -> [ParseTree] -> Either String [Proof]
proofThats' ts symbol args
	= inMsg ("While trying to proof that ("++symbol++") is applicable to \""++ toParsable' ", " args ++"\"") $
	  do	rules	<- ts & tsRules |> return & findWithDefault (Left $ "No rules about a relation with symbol "++show symbol) symbol
		let results	= rules |> flip (interpretRule ts) args
		let successfull	= rights results
		assert Left (not $ null successfull) $ "Not a single rule matched:\n"++unlines (lefts results)
		return successfull

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
	  do	let (RelationMet rel patterns)	= get ruleConcl r
		variables	<- patternMatchInputs ts (get rulePreds r) (rel, patterns) args
		(predicateProofs, vars')
				<- proofPredicates ts variables (get rulePreds r)
		let concl	= proofRelationMet ts (rel, patterns) vars' args
		return $ Proof concl r predicateProofs

proofRelationMet	:: TypeSystem -> (Relation, [Expression]) -> VariableAssignments -> [ParseTree] -> Conclusion'
proofRelationMet ts (rel, relationArgs) vars args
	= let	relationArgs'	= relationArgs |> evalExpr ts vars
		resultExprs	= weaveMode (relModes rel) args relationArgs'
		in RelationMet rel resultExprs
	


-- predicates are proven from left to right, as a left predicate might introduce variables used by a following predicate
proofPredicates :: TypeSystem -> VariableAssignments -> [Predicate] -> Either String ([Proof], VariableAssignments)
proofPredicates _ vars [] 	= return ([], vars)
proofPredicates ts vars (pred:preds)
	= do	(proof, vars')		<- proofPredicate ts vars preds pred
		variables		<- mergeVars vars vars'
		(proofs, variables')	<- proofPredicates ts variables preds
		return (proof:proofs, variables')


proofPredicate	:: TypeSystem -> VariableAssignments -> [Predicate] -> Predicate -> Either String (Proof, VariableAssignments)
proofPredicate ts vars _ (TermIsA expr typ)
	= let	expr'	= evalExpr ts vars (MVar typ expr) in
		if alwaysIsA (get tsSyntax ts) (typeOf expr') typ then
			return (ProofIsA expr' typ, empty)
			else Left ( expr ++ " = "++ toCoParsable expr' ++ " is not a "++show typ)
proofPredicate ts vars _ (Same e1 e2)
	= do	let e1'	= evalExpr ts vars e1
		let e2' = evalExpr ts vars e2
		if e1' == e2' then  return (ProofSame e1' e1 e2, vars)
			else Left $ "Equality predicate not met: "++ toParsable e1 ++ "=" ++ toCoParsable e1' ++ " /= " ++ toCoParsable e2' ++"="++toParsable e2

proofPredicate ts vars restingPreds (Needed (RelationMet relation args))
	= do	let inputArgs	= filterMode In relation args
		let args'	= inputArgs |> evalExpr ts vars
		proof		<- proofThat ts relation args'
		let concl	= _proofConcl proof
		-- now, we take the 'out'-expresssions from the conclusion and pattern match those into the out of the needed
		let toMatch	= filterMode Out relation (zip args (conclusionArgs concl))	:: [(Expression, ParseTree)]
		matched		<- matchAndMerge ts restingPreds toMatch	-- this predicate might contain a context evaluation
		return (proof, matched)



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
	= do	let evalContextMatches assngs	= proofPredicates ts assngs predicates & isRight
		matches	<- patsArgs |+> uncurry (patternMatch (get tsSyntax ts) evalContextMatches)
		matches & mergeVarss


