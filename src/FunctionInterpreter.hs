module FunctionInterpreter where

{-
This module defines an interpreter for functions. functions are evaluated here
-}

import Utils
import TypeSystem

import Control.Monad

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe


evalFunc	:: TypeSystem -> Name -> [Expression] -> Expression
evalFunc ts funcName args	
 | funcName `M.member` tsFunctions ts
	= let	func	= tsFunctions ts M.! funcName
		ctx	= Ctx (tsSyntax ts) (tsFunctions ts) M.empty [] in
		applyFunc ctx (funcName, func) args
 | otherwise
	= evalErr (Ctx (tsSyntax ts) (tsFunctions ts) M.empty []) $
		"evalFunc with unknown function: "++funcName	


data Ctx	= Ctx { ctx_syntax	:: BNFRules,		-- Needed for typecasts
			ctx_functions 	:: Map Name Function,
			ctx_vars	:: Map Name Expression,
			ctx_stack	:: [(Name, [Expression])] -- only used for errors
			}

-- applies given argument to the  function. Starts by evaluating the args
applyFunc	:: Ctx -> (Name, Function) -> [Expression] -> Expression
applyFunc ctx (nm, MFunction tp clauses) args
	= let	args'	= args |> evaluate ctx
		stackEl	= (nm, args')
		ctx'	= ctx {ctx_stack = stackEl:ctx_stack ctx}
		clauseResults	= clauses |> evalClause ctx' args' & catMaybes
		in if null clauseResults then error $ "Not a single clause matched, even with error injection. This is a bug!" else
			head clauseResults


evalClause	:: Ctx ->  [Expression] -> Clause -> Maybe Expression
evalClause ctx args (MClause pats expr)
	= do	variabless	<- zip pats args |+> uncurry (patternMatch $ ctx_syntax ctx )
		variables	<- foldM mergeVars M.empty variabless
		let ctx'	= ctx {ctx_vars = variables}
		return $ evaluate ctx' expr


mergeVars	:: Map Name Expression -> Map Name Expression -> Maybe (Map Name Expression)
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
patternMatch	:: BNFRules -> Expression -> Expression -> Maybe (Map Name Expression)
patternMatch _ (MVar _ v) expr	= Just $ M.singleton v expr
patternMatch _ (MLiteral _ s1) (MLiteral _ s2)
	| s1 == s2		= Just M.empty
	| otherwise		= Nothing
patternMatch _ (MInt _ s1) (MInt _ s2)
	| s1 == s2		= Just M.empty
	| otherwise		= Nothing
patternMatch r (MSeq _ seq1) (MSeq _ seq2)
	= zip seq1 seq2 |+> uncurry (patternMatch r) >>= foldM mergeVars M.empty
patternMatch r (MAscription as expr') expr
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



evaluate	:: Ctx -> Expression -> Expression
evaluate ctx (MCall _ "plus" True es)
	= let	(tp, es')	= asInts ctx "plus" es in
		MInt tp (sum es')
evaluate ctx (MCall _ "min" True es)
	= let	(tp, [e1, e2])	= asInts ctx "min" es in
		MInt tp (e1 - e2)
evaluate ctx (MCall _ "neg" True es)
	= let	(tp, [e1])	= asInts ctx "neg" es in
		MInt tp (-e1)
evaluate ctx (MCall _ "equal" True es)
	= let	(tp, [e1, e2])	= asInts ctx "equal" es in
		MInt tp (if e1 == e2 then 1 else 0)
evaluate ctx (MCall _ "error" True exprs)
	= let	exprs'	= exprs |> evaluate ctx & showComma
		msgs	= ["In evaluating a  function:", exprs']
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

asInts ctx bi exprs	
	= let 	exprs'	= exprs |> evaluate ctx 
				|> (\e -> if isMInt e then e else error $ "Not an integer in the builtin "++bi++" expecting an int: "++ show e)
				|> (\(MInt _ i) -> i)
		tp	= typeOf $ head exprs
		tp'	= if tp == "" then error $ "Declare a return type, by annotating the first argument of a builtin" else tp
		in
		((tp', -1), exprs')

buildStackEl	:: (Name, [Expression]) -> String
buildStackEl (func, args)
	= "   In "++func++ inParens (args & showComma)
