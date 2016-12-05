module RuleInterpreter where

{-
This module defines an evaluator for rules
-}


import Utils
import TypeSystem

import FunctionInterpreter

import Data.Map (Map, empty, findWithDefault)
import Data.Either
import Data.List


proofThat	:: TypeSystem -> Relation -> [ParseTree] -> Either String Proof
proofThat ts rel args
	= proofThat' ts (relSymbol rel) args

-- tries to deduce a proof for a given property, might return multiple (top level) proofs
proofThats'	:: TypeSystem -> Symbol -> [ParseTree] -> Either String [Proof]
proofThats' ts symbol args
	= inMsg ("While trying to proof that "++show symbol++" is applicable to "++show args) $
	  do	rules	<- ts & tsRules |> return & findWithDefault (Left $ "No rules about a relation with symbol "++show symbol) symbol
		let results	= rules |> flip (interpretRule ts) args
		let successfull	= rights results
		assert Left (not $ null successfull) $ "Not a single rule matched:\n"++unlines (lefts results)
		return successfull

proofThat'	:: TypeSystem -> Symbol -> [ParseTree] -> Either String Proof
proofThat' ts symbol args
	= do	successfull	<- proofThats' ts symbol args
		let conclusions	= successfull & filter isProof |> proofConcl & nub
		assert Left (1 == length conclusions) $ "Multiple rules provided a proof with different conclusions:\n"++ 
				(successfull |> show |> lines ||>> ("#  "++) |> unlines >>= (++"\n"))
		let successfull'	= successfull & sortOn weight 
		return $ head successfull'




-- given a rule and 'input' expressions, (tries to) give a proof for it
interpretRule	:: TypeSystem -> Rule -> [ParseTree] -> Either String Proof
interpretRule ts r args
	= inMsg ("While trying to intepret the rule "++ruleName r++" with args "++show args) $
	  do	let (RelationMet rel patterns _)	= ruleConcl r
		variables	<- patternMatchInputs ts (rel, patterns) args
		(preds, vars')	<- proofPredicates ts variables (rulePreds r)
		concl		<- proofRelationMet ts (rel, patterns) vars' args
		return $ Proof concl r preds


-- predicates are proven from left to right, as a left predicate might introduce variables used by a following predicate
proofPredicates :: TypeSystem -> VariableAssignments -> [Predicate] -> Either String ([Proof], VariableAssignments)
proofPredicates _ vars [] 	= return ([], vars)
proofPredicates ts vars (pred:preds)
	= do	(proof, vars')		<- proofPredicate ts vars pred
		variables		<- mergeVars vars vars' & maybe (Left "Conflicting variable assignments") return
		(proofs, variables')	<- proofPredicates ts variables preds
		return (proof:proofs, variables')


proofPredicate	:: TypeSystem -> VariableAssignments -> Predicate -> Either String (Proof, VariableAssignments)
proofPredicate ts vars (TermIsA expr typ)
	= let	expr'	= evalExpr ts vars expr in
		if alwaysIsA (tsSyntax ts) (typeOf expr') typ then
			return (ProofIsA expr' typ, empty)
			else Left (show expr ++ " is not a "++show typ)
proofPredicate ts vars (Needed (RelationMet relation args _))
	= do	let inputArgs	= zip args (relModes relation) & filter ((==) In . snd) |> fst	:: [Expression]
		let args'	= inputArgs |> evalExpr ts vars
		proof		<- proofThat ts relation args'
		let concl	= proofConcl proof
		-- now, we take the 'out'-expresssions from the conclusion and pattern match those into the out of the needed
		let toMatch	= filterMode Out relation (zip args (conclusionArgs concl))	:: [(Expression, ParseTree)]
		matched		<- matchAndMerge ts toMatch
		return (proof, matched)




patternMatchInputs	:: TypeSystem -> (Relation, [Expression]) -> [ParseTree] -> Either String VariableAssignments
patternMatchInputs ts (rel, relationArgs) args
	= do	let inputTypes	= filterMode In rel (relType rel)
		assert  Left(length inputTypes == length args) $ "Expected "++show (length inputTypes)++" arguments, but got "++show (length args)++" arguments instead" 
		let typesMatch arg expected	= assert Left (equivalent (tsSyntax ts) (typeOf arg) expected) 
							("Expected type "++expected++" for "++show arg++" of type "++typeOf arg)
		zip args inputTypes |> uncurry typesMatch & allRight
		let patterns = filterMode In rel relationArgs
		matchAndMerge ts $ zip patterns args 
		
		

proofRelationMet	:: TypeSystem -> (Relation, [Expression]) -> VariableAssignments -> [ParseTree] -> Either String Conclusion'
proofRelationMet ts (rel, relationArgs) vars args
	= do	resultExprs	<- zipModes ts vars (zip relationArgs $ relModes rel) args
		return $ relationMet' rel (resultExprs |> fst)
		
		
-- TODO add predicate injection
matchAndMerge	:: TypeSystem -> [(Expression, ParseTree)] -> Either String VariableAssignments
matchAndMerge ts patsArgs
	= do	matches	<- patsArgs |+> uncurry (patternMatch (tsSyntax ts) (const True)) & maybe (Left "Pattern match failed") return
		matches & mergeVarss & maybe (Left "Conflicting assignments") return


zipModes	:: TypeSystem -> VariableAssignments -> [(Expression, Mode)] -> [ParseTree] -> Either String [(ParseTree, Mode)]
zipModes ts assignments ((_, In):relationArgs) (arg:args)
		= do	tail	<- zipModes ts assignments relationArgs args
			return ((arg, In):tail)
zipModes ts assignments ((prototype, Out): relationArgs) args
		= do	let arg	= evalExpr ts assignments prototype
			tail	<- zipModes ts assignments relationArgs args
			return ((arg, Out):tail)
zipModes _ _ [] []
		= return []
zipModes _ _ _ _
		= Left $ "Not enough input arguments for relation"		
