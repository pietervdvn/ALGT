module ParseTreeInterpreter.PropertyProver where

{- 
Tests a given property against the given parsetrees.
More of a check for the property in other words!

If the predicates are met, we test wether the conclusion holds too
 -}


import Utils.Utils
import Utils.ToString

import TypeSystem

import ParseTreeInterpreter.RuleInterpreter
import ParseTreeInterpreter.FunctionInterpreter (VariableAssignments)

import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Either

import Control.Monad



{- If a predicate fails, a 'failed predicate proof' is returned (by Left).
 If all succeed, these are returned.
Note that assignments are proven left to right, as a predicate on the left might introduce variables
Also, introduced variables might be used multiple times (implying equal terms). If these terms turn out not to be the same, a 'Failed Predicate' Proof is returned as well
-}

testPropOn	:: TypeSystem -> Property -> VariableAssignments -> Either String PropertyProof
testPropOn ts prop vars
	= inMsg ("While testing property "++get propName prop++" with arguments {"++ toParsable' ", " vars++"}") $
	  do	checkAssignment ts prop vars
		let preds		= get propPreds prop
		let conclChoices	= get propConcl prop
		let predProofs		= proofPredicates' ts [] vars preds
		case predProofs of
			(Left (successfull, _, err))	-> return $ _packPredicateFailed vars preds successfull err
			(Right (proofs, vars'))		
				-> proofAConclusion ts conclChoices vars' |> uncurry (PropertyProof vars' proofs)


proofAConclusion	:: TypeSystem -> MultiConclusionA Expression -> VariableAssignments -> Either String (Int, Proof)
proofAConclusion ts mconcl vars
	= do	let conclChoices	= get multiConcls mconcl
		let possProofs		= conclChoices |> proofConclusion ts vars & mapi |> sndEffect
		let successFull		= rights possProofs

		let errMsg (concl, Left msg)	= "# "++toParsable concl ++" failed because:\n"++msg
		when (null successFull) $ Left $ 
			"Could not proof a single conclusion: " ++ (zip conclChoices possProofs |> errMsg & unlines)
		let selectedProof	= head successFull
		return selectedProof


_packPredicateFailed	:: VariableAssignments -> [Predicate] -> [Proof] -> String -> PropertyProof
_packPredicateFailed vars preds proofsSucc failedPredMsg
	= let	successfulls	= proofsSucc |> Right
		failer		= Left failedPredMsg
		addRest		= padR (length preds) (Left "Proof not attempted as previous predicate failed")	
		in
		PredicateFailed vars $ addRest (successfulls++[failer])
		




checkAssignment	:: TypeSystem -> Property -> VariableAssignments -> Either String ()
checkAssignment ts prop assgn'
	= let	needed	= neededVars prop
		assgn	= assgn' |> fst
		neededStr	= needed |> (\(n, t) -> " # "++n++" : "++t) & unlines & indent  in
	  inMsg "While checking the input given to test a property" $
	  inMsg ("Expected assignments are:\n"++neededStr) $
	  do	needed |> (\(n, t) -> checkExists n assgn $ "Variable "++n++":"++t++" not given")
			& allRight
		let checkTp (n, et) at 
				= unless (alwaysIsA (get tsSyntax ts) at et) $ Left $ "Could not match types of "++n++": expected a "++et++" but got a "++at
			
		needed |> (\(n, et) -> checkTp (n, et) ((assgn ! n) & typeOf))
			& allRight_



neededVars	:: Property -> [(Name, TypeName)]
neededVars prop
	= let	predProof	= get propPreds prop ++ (get (propConcl . multiConcls) prop |> Needed) in
		fst $ _neededVars [] predProof

_neededVars	:: [Name] -> [Predicate] -> ([(Name, TypeName)], [(Name, TypeName)])
_neededVars _ []	= ([], [])
_neededVars alreadyKnown (pred:preds)
	= let	(extraNeeded, generated)	= _neededVarsPred alreadyKnown pred
		alreadyKnown'			= alreadyKnown ++ (extraNeeded ++ generated) |> fst
		(extraNeededRec, generatedRec)	= _neededVars alreadyKnown' preds
		in
		(extraNeeded ++ extraNeededRec, generated ++ generatedRec)


{-
For a given predicate, calculate (Expected as input (and type), Generates as output (and type)
-}
_neededVarsPred	:: [Name] -> Predicate -> ([(Name, TypeName)], [(Name, TypeName)])
_neededVarsPred alreadyKnown (TermIsA nm tn)
 | nm `elem` alreadyKnown	= ([], [])
 | otherwise			= ([(nm, tn)], [])
_neededVarsPred alreadyKnown (Same e0 e1)
	= let 	used	= [e0, e1] >>= usedVariables
		needed	= used & filter ((`notElem` alreadyKnown) . fst)
		in
		(needed, [])
_neededVarsPred alreadyKnown (Needed concl)
	= let 	rel	= get conclusionRel concl
		exprs	= get conclusionArgs concl
		inExprs	= filterMode In  rel exprs >>= usedVariables
		needed	= inExprs & filter ((`notElem` alreadyKnown) . fst) 
		outExpr	= filterMode Out rel exprs >>= usedVariables
		in
		(needed, outExpr)


