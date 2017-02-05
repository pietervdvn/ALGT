module ParseTreeInterpreter.PropertyProver where

{- 

Creates a proof for the given parsetree.
Actually, more of a check.

If the predicates are met, we test wether the conclusion holds too

 -}


import Utils.Utils
import Utils.ToString

import TypeSystem

import AssetsHelper

import Debug.Trace

tProps	= stfl & get tsProps
tProp0	= tProps !! 0
tProp1	= tProps !! 1

{- If a predicate fails, a 'failed predicate proof' is returned (by Left).
 If all succeed, these are returned.
Note that assignments are proven left to right, as a predicate on the left might introduce variables
Also, introduced variables might be used multiple times (implying equal terms). If these terms turn out not to be the same, a 'Failed Predicate' Proof is returned as well

-}


{-
Depending on the used relations, some parsetrees should be given, some can be calculated.

Predicates are interpreted left to right, all variables should be known then for the actual conclusion.
checkInput	:: TypeSystem -> Property -> Either String [(Name, TypeName)]
checkInput ts prop


_checkInput	:: TypeSystem -> Property -> [(Name, TypeName)] -> Either String [(Name, TypeName)]
_checkInput ts prop alreadyKnown
	= do	let preds	= get propPreds prop
-}





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


