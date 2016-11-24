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


evalFunc	:: TypeSystem -> Name -> [MetaExpression] -> MetaExpression
evalFunc ts funcName args	
 | funcName `M.member` tsFunctions ts
	= let	func	= tsFunctions ts M.! funcName
		ctx	= Ctx (tsSyntax ts) (tsFunctions ts) M.empty [] in
		applyFunc ctx (funcName, func) args
 | otherwise
	= evalErr (Ctx (tsSyntax ts) (tsFunctions ts) M.empty []) $
		"evalFunc with unknown function: "++funcName	


data Ctx	= Ctx { ctx_syntax	:: BNFRules,		-- Needed for typecasts
			ctx_functions 	:: Map Name MetaFunction,
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
	= do	variabless	<- zip pats args |+> uncurry (patternMatch $ ctx_syntax ctx )
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
patternMatch	:: BNFRules -> MetaExpression -> MetaExpression -> Maybe (Map Name MetaExpression)
patternMatch _ (MVar _ v) expr	= Just $ M.singleton v expr
patternMatch _ (MLiteral _ s1) (MLiteral _ s2)
	| s1 == s2		= Just M.empty
	| otherwise		= Nothing
patternMatch _ (MInt _ s1) (MInt _ s2)
	| s1 == s2		= Just M.empty
	| otherwise		= Nothing
patternMatch r (MSeq _ seq1) (MSeq _ seq2)
	= zip seq1 seq2 |+> uncurry (patternMatch r) >>= foldM mergeVars M.empty
patternMatch r (MCast as expr') expr
 | alwaysIsA r (typeOf expr) as	
	= patternMatch r expr' expr
 | otherwise	
	= Nothing

patternMatch _ (MCall _ "error" True _) _	
	= error $ "Using an error in a pattern match is not allowed. Well, you've got your error now anyway. Happy now, you punk?"
patternMatch _ (MCall _ nm _ _) _	
	= error $ "Using a function call in a pattern is not allowed"
patternMatch _ pat expr		
	= Nothing



evaluate	:: Ctx -> MetaExpression -> MetaExpression
evaluate ctx (MCall _ "plus" True es)
	= let	es'	= asInts ctx "plus" es in
		MInt ("number", 0) (sum es')
evaluate ctx (MCall _ "equal" True es)
	= let	[e1, e2]	= asInts ctx "equal" es in
		MInt ("number", 0) (if e1 == e2 then 1 else 0)
evaluate ctx (MCall _ "error" True exprs)
	= let	exprs'	= exprs |> evaluate ctx & showComma
		msgs	= ["In evaluating a meta function:", exprs']
		stack	= ctx_stack ctx |> buildStackEl
		in	error $ unlines $ stack ++ msgs
			
evaluate ctx (MCall _ nm True args)
	= evalErr ctx $ "unknown builtin "++nm++" for arguments: "++showComma args

evaluate ctx (MCall _ nm False args)
 | nm `M.member` ctx_functions ctx
	= let	func	= ctx_functions ctx M.! nm in
		applyFunc ctx (nm, func) args
 | otherwise
	= evalErr ctx $ "unknown function: "++nm	

evaluate ctx (MVar _ nm)
 | nm `M.member` ctx_vars ctx	
	= ctx_vars ctx M.! nm
 | otherwise			
	= evalErr ctx $ "unkown variable "++nm


evaluate ctx (MSeq tp vals)	= vals |> evaluate ctx & MSeq tp
evaluate _ (MLiteral ti l)	= MLiteral ti l
evaluate _ (MInt ti i)		= MInt ti i


evalErr	ctx msg	= evaluate ctx $ MCall "" "error" True [MLiteral ("", -1) ("Undefined behaviour: "++msg)]

asInts ctx bi exprs	= exprs |> evaluate ctx 
				|> (\e -> if isMInt e then e else error $ "Not an integer in the builtin "++bi++" expecting an int: "++ show e)
				|> (\(MInt _ i) -> i)

buildStackEl	:: (Name, [MetaExpression]) -> String
buildStackEl (func, args)
	= "   In "++func++ inParens (args & showComma)
