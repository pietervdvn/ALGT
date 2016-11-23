module MetaFunctionInterpreter where

{-
This module defines an interpreter for metafunctions. Metafunctions are evaluated here
-}

import Utils
import TypeSystem

import Control.Monad

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

{-
Evaluate an expression. For this, it has two contexts:
- Map Name Metafunction contains all declared functions
- Map Name MetaExpression contains all variables, obtained by pattern matching
-}

evalFunc	:: TypeSystem -> Name -> [ParseTree] -> MetaExpression
evalFunc ts funcName args
		= evalFunc' ts funcName (args |> ptToMetaExpr)


evalFunc'	:: TypeSystem -> Name -> [MetaExpression] -> MetaExpression
evalFunc' ts funcName args	
 | funcName `M.member` tsFunctions ts
	= let	func	= tsFunctions ts M.! funcName
		ctx	= Ctx (tsFunctions ts) M.empty [] in
		applyFunc ctx (funcName, func) args
 | otherwise
	= evalErr (Ctx (tsFunctions ts) M.empty []) $
		"evalFunc with unknown function: "++funcName	


data Ctx	= Ctx {	ctx_functions 	:: Map Name MetaFunction,
			ctx_vars	:: Map Name MetaExpression,
			ctx_stack	:: [(Name, [MetaExpression])] -- only used for errors
			}

-- applies given argument to the meta function. Starts by evaluating the args
applyFunc	:: Ctx -> (Name, MetaFunction) -> [MetaExpression] -> MetaExpression
applyFunc ctx (nm, MFunction tp clauses) args
	= let	args'	= args |> evaluate ctx
		stackEl	= (nm, args')
		ctx'	= ctx {ctx_stack = stackEl:ctx_stack ctx}
		clauseResults	= clauses |> evalClause ctx' args' & catMaybes
		in if null clauseResults then error $ "Not a single clause matched, even with error injection. This is a bug!" else
			head clauseResults


evalClause	:: Ctx ->  [MetaExpression] -> MetaClause -> Maybe MetaExpression
evalClause ctx args (MClause pats expr)
	= do	variabless	<- zip pats args |+> uncurry patternMatch
		variables	<- foldM mergeVars M.empty variabless
		let ctx'	= ctx {ctx_vars = variables}
		return $ evaluate ctx' expr


mergeVars	:: Map Name MetaExpression -> Map Name MetaExpression -> Maybe (Map Name MetaExpression)
mergeVars v1 v2
	= do	let common	= (v1 `M.intersection` v2) & M.keys
		let cv1		= common |> (v1 M.!)
		let cv2		= common |> (v2 M.!)
		if cv1 == cv2 
			then return (v1 `M.union` v2)
			else Nothing

{-
Disasembles an expression against a pattern
patternMatch pattern value
-}
patternMatch	:: MetaExpression -> MetaExpression -> Maybe (Map Name MetaExpression)
patternMatch (MVar v) expr	= Just $ M.singleton v expr
patternMatch (MLiteral s1) (MLiteral s2)
	| s1 == s2		= Just M.empty
	| otherwise		= Nothing
patternMatch (MInt s1) (MInt s2)
	| s1 == s2		= Just M.empty
	| otherwise		= Nothing
patternMatch (MSeq seq1) (MSeq seq2)
	= zip seq1 seq2 |+> uncurry patternMatch >>= foldM mergeVars M.empty
	

patternMatch (MCall nm _ _) _	= error $ "Using a function call in a pattern is not allowed"
patternMatch (MError msg) _	= error $ "Using an error in a pattern match is not allowed. Well, you've got your error now anyway. Happy now, you punk?"

patternMatch pat expr		= Nothing






evaluate	:: Ctx -> MetaExpression -> MetaExpression
evaluate ctx (MCall "plus" True [e1, e2])
	= let	e1'	= evaluate ctx e1
		e2'	= evaluate ctx e2 
		MInt i1	= e1'
		MInt i2	= e2' in
		if not (isMInt e1' && isMInt e2') then
			evalErr ctx $ "plus off a non-int element "++show e1'++", "++show e2'
		else
			MInt (i1 + i2)
			
evaluate ctx (MCall nm True args)
	= evalErr ctx $ "unknown builtin "++nm++" for arguments: "++showComma args

evaluate ctx (MCall nm False args)
 | nm `M.member` ctx_functions ctx
	= let	func	= ctx_functions ctx M.! nm in
		applyFunc ctx (nm, func) args
 | otherwise
	= evalErr ctx $ "unknown function: "++nm	

evaluate ctx (MVar nm)
 | nm `M.member` ctx_vars ctx	
	= ctx_vars ctx M.! nm
 | otherwise			
	= evalErr ctx $ "unkown variable "++nm


evaluate ctx (MSeq vals)	= vals |> evaluate ctx & MSeq
evaluate _ (MLiteral l)		= MLiteral l
evaluate _ (MInt i)		= MInt i

evaluate ctx (MError msg)	
	= let msgs	= ["In evaluating a meta function:", msg]++
				(ctx_stack ctx |> buildStackEl)
		in	error $ unlines msgs


evalErr	ctx msg	= evaluate ctx (MError $ "Undefined behaviour: "++msg)


buildStackEl	:: (Name, [MetaExpression]) -> String
buildStackEl (func, args)
	= "   In "++func++ inParens (args & showComma)
