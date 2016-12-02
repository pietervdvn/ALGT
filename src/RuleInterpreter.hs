module RuleInterpreter where

{-
This module defines an evaluator for rules
-}


import Utils
import TypeSystem

import FunctionInterpreter

import Data.Map (Map, empty, findWithDefault)
import Data.Either


proofThat	:: TypeSystem -> Relation -> [Expression] -> Either String Proof
proofThat ts rel args
	= proofThat' ts (relSymbol rel) args

-- tries to deduce a proof for a given property and input expressions
proofThat'	:: TypeSystem -> Symbol -> [Expression] -> Either String Proof
proofThat' ts symbol args
	= inMsg ("While trying to proof that "++show symbol++" is applicable to "++show args) $
	  do	rules	<- ts & tsRules |> return & findWithDefault (Left $ "No rules about a relation with symbol "++show symbol) symbol
		let results	= rules |> flip (interpretRule ts) args
		let successfull	= rights results
		assert Left (not $ null successfull) $ "Not a single rule matched:\n"++unlines (lefts results)
		assert Left (1 == length successfull) $ "Multiple rules provided a proof:\n"++show successfull
		return $ head successfull




-- given a rule and 'input' expressions, (tries to) give a proof for it
interpretRule	:: TypeSystem -> Rule -> [Expression] -> Either String Proof
interpretRule ts r args
	= inMsg ("While trying to intepret the rule "++ruleName r++" with args "++show args) $
	  do	let (RelationMet rel patterns)	= ruleConcl r
		variables	<- patternMatchInputs ts (rel, patterns) args
		(preds, vars')	<- rulePreds r |+> proofPredicate ts variables |> unzip
		variables'	<- mergeVarss (variables:vars') & maybe (Left "Conflicting variable assignments") return
		concl		<- proofRelationMet ts (rel, patterns) variables' args
		return $ Proof concl r preds


proofPredicate	:: TypeSystem -> Map Name Expression -> Predicate -> Either String (Proof, Map Name Expression)
proofPredicate ts vars (TermIsA expr typ)
	= let	expr'	= evalExpr ts vars expr in
		if alwaysIsA (tsSyntax ts) (typeOf expr') typ then
			return (ProofIsA expr' typ, empty)
			else Left (show expr ++ " is not a "++show typ)
proofPredicate ts vars (Needed (RelationMet relation args))
	= do	let inputArgs	= zip args (relModes relation) & filter ((==) In . snd) |> fst	:: [Expression]
		let args'	= inputArgs |> evalExpr ts vars
		proof		<- proofThat ts relation args'
		let concl	= proofConcl proof
		-- now, we take the 'out'-expresssions from the conclusion and pattern match those into the out of the needed
		let toMatch	= filterMode Out relation (zip args (conclusionArgs concl))	:: [(Expression, Expression)]
		matched		<- matchAndMerge ts toMatch
		return (proof, matched)




patternMatchInputs	:: TypeSystem -> (Relation, [Expression]) -> [Expression] -> Either String (Map Name Expression)
patternMatchInputs ts (rel, relationArgs) args
	= do	let inputTypes	= filterMode In rel (relType rel)
		assert  Left(length inputTypes == length args) $ "Expected "++show (length inputTypes)++" arguments, but got "++show (length args)++" arguments instead" 
		let typesMatch arg expected	= assert Left (equivalent (tsSyntax ts) (typeOf arg) expected) 
							("Expected type "++expected++" for "++showT arg)
		zip args inputTypes |> uncurry typesMatch & allRight
		let patterns = filterMode In rel relationArgs
		matchAndMerge ts $ zip patterns args 
		
		

proofRelationMet	:: TypeSystem -> (Relation, [Expression]) -> Map Name Expression -> [Expression] -> Either String Conclusion
proofRelationMet ts (rel, relationArgs) vars args
	= do	resultExprs	<- zipModes ts vars (zip relationArgs $ relModes rel) args
		return $ RelationMet rel (resultExprs |> fst)
		
		
matchAndMerge	:: TypeSystem -> [(Expression, Expression)] -> Either String (Map Name Expression)
matchAndMerge ts patsArgs
	= do	matches	<- patsArgs |+> uncurry (patternMatch (tsSyntax ts))  & maybe (Left "Pattern match failed") return
		matches & mergeVarss & maybe (Left "Conflicting assignments") return




zipModes	:: TypeSystem -> Map Name Expression -> [(Expression, Mode)] -> [Expression] -> Either String [(Expression, Mode)]
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
