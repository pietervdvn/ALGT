module ParseTreeInterpreter.FunctionInterpreter where

{-
This module defines an interpreter for functions. 
-}

import TypeSystem
import Utils.TypeSystemToString
import Utils.ToString
import Utils.Utils

import Control.Monad

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.List (intercalate, intersperse)
import Data.Bifunctor (first)

evalFunc	:: TypeSystem -> Name -> [ParseTree] -> ParseTree
evalFunc ts funcName args	
 | funcName `M.member` tsFunctions ts
	= let	func	= tsFunctions ts M.! funcName in
		applyFunc (buildCtx' ts) (funcName, func) args
 | otherwise
	= evalErr (Ctx (tsSyntax ts) (tsFunctions ts) M.empty []) $
		"evalFunc with unknown function: "++funcName	

evalExpr	:: TypeSystem -> VariableAssignments -> Expression -> ParseTree
evalExpr ts vars
	= evaluate (buildCtx ts vars)

type VariableAssignments
		= Map Name (ParseTree, Maybe [Int])	-- If a path of numbers (indexes in the expression-tree) is given, it means a evaluation context is used
data Ctx	= Ctx { ctxSyntax	:: Syntax,		-- Needed for typecasts
			ctxFunctions 	:: Map Name Function,
			ctxVars	:: VariableAssignments,
			ctxStack	:: [(Name, [ParseTree])] -- only used for errors
			}

buildCtx ts vars 	= Ctx (tsSyntax ts) (tsFunctions ts) vars []
buildCtx' ts		= buildCtx ts M.empty


-- applies given argument to the  function. Starts by evaluating the args
applyFunc	:: Ctx -> (Name, Function) -> [ParseTree] -> ParseTree
applyFunc ctx (nm, MFunction tp clauses) args
 | length args /= length tp - 1
	= evalErr ctx $ "Number of arguments does not match. Expected "++show (length tp - 1)++" variables, but got "++show (length args)++" arguments instead"
 | otherwise
	= let	stackEl	= (nm, args)
		ctx'	= ctx {ctxStack = stackEl:ctxStack ctx}
		clauseResults	= clauses |> evalClause ctx' args & catMaybes
		in if null clauseResults then error "Not a single clause matched, even with error injection. This is a bug!" else
			head clauseResults


evalClause	:: Ctx ->  [ParseTree] -> Clause -> Maybe ParseTree
evalClause ctx args (MClause pats expr)
	= do	variabless	<- zip pats args |+> uncurry (patternMatch (ctxSyntax ctx) (const True))
					& either (const Nothing) Just
		variables	<- mergeVarss variabless & either (const Nothing) Just
		let ctx'	= ctx {ctxVars = variables}
		return $ evaluate ctx' expr


mergeVarss	:: [VariableAssignments] -> Either String VariableAssignments
mergeVarss	= foldM mergeVars M.empty

mergeVars	:: VariableAssignments -> VariableAssignments -> Either String VariableAssignments
mergeVars v1 v2
	= do	let common	= (v1 `M.intersection` v2) & M.keys
		let cv1		= common |> (v1 M.!)
		let cv2		= common |> (v2 M.!)
		let different	= zip common (zip cv1 cv2) & filter (uncurry (/=) . snd)
		let msg (nm, (assgn1, assgn2))
				= nm++" is assigned both "++ inParens (showAssgn assgn1) ++ " and "++ inParens (showAssgn assgn2)
		let failMsgs	= different |> msg & unlines
		if cv1 == cv2 then return (v1 `M.union` v2)
			else inMsg "Merging contexts failed: some variables are assigned different values" $ Left failMsgs

{-
Disasembles an expression against a pattern
patternMatch pattern value

The extra function (VariableAssignments -> Bool) injects a test, to test different evaluation contexts
-}
patternMatch	:: Syntax -> (VariableAssignments -> Bool) -> Expression -> ParseTree -> Either String VariableAssignments
patternMatch _ _ (MVar _ v) expr
	= return $ M.singleton v (expr, Nothing)
patternMatch _ _ (MParseTree (MLiteral _ s1)) (MLiteral _ s2)
	| s1 == s2		= return M.empty
	| otherwise		= Left $ "Not the same literal: "++s1++ " /= " ++ s2
patternMatch _ _ (MParseTree (MInt _ s1)) (MInt _ s2)
	| s1 == s2		= return M.empty
	| otherwise		=  Left $ "Not the same int: "++show s1++ " /= " ++ show s2
patternMatch _ _ (MParseTree (MIdentifier _ s1)) (MIdentifier _ s2)
	| s1 == s2		= return M.empty
	| otherwise		= Left $ "Not the same identifier: "++s1++ " /= " ++ s2
patternMatch r f (MParseTree (PtSeq mi pts)) pt
	= patternMatch r f (MSeq mi (pts |> MParseTree)) pt
patternMatch r f s1@(MSeq _ seq1) s2@(PtSeq _ seq2)
 | length seq1 /= length seq2	= Left $ "Sequence lengths are not the same: "++toParsable s1 ++ " /= "++toCoParsable s2
 | otherwise			= zip seq1 seq2 |+> uncurry (patternMatch r f) >>= foldM mergeVars M.empty

patternMatch r f (MAscription as expr') expr
 | alwaysIsA r (typeOf expr) as	
	= patternMatch r f expr' expr
 | otherwise	
	= Left $ toCoParsable expr ++" is not a "++show as

patternMatch r extraCheck (MEvalContext tp name hole) value@(PtSeq _ _)
	= patternMatchContxt r extraCheck (tp, name, hole) value
		

patternMatch _ _ (MCall _ "error" True _) _	
	= error "Using an error in a pattern match is not allowed. Well, you've got your error now anyway. Happy now, you punk?"
patternMatch _ _ (MCall _ nm _ _) _	
	= error "Using a function call in a pattern is not allowed"
patternMatch _ _ pat expr		
	= Left $ "Could not match "++toParsable pat++" /= "++toCoParsable expr


patternMatchContxt	:: Syntax -> (VariableAssignments -> Bool) -> (TypeName, Name, Expression) -> ParseTree -> Either String VariableAssignments
patternMatchContxt r extraCheck evalCtx fullContext@(PtSeq _ values)
	= do	let matchMaker	= makeMatch r extraCheck evalCtx fullContext	:: (ParseTree, [Int]) -> Either String VariableAssignments
		depthFirstSearch' matchMaker [] values


makeMatch	:: Syntax -> (VariableAssignments -> Bool) -> (TypeName, Name, Expression) -> ParseTree -> (ParseTree, [Int]) -> Either String VariableAssignments
makeMatch r extraCheck (tp, name, holePattern) fullContext (holeFiller, path)
	= do	let baseAssign	= M.singleton name (fullContext, Just path)	:: VariableAssignments
		holeAssgn	<- patternMatch r extraCheck holePattern holeFiller
		assgn'		<- mergeVars baseAssign holeAssgn
		assert Left (extraCheck assgn') "Extra patterns (for the rule) failed"
		return assgn'

-- depth first search, excluding self match
depthFirstSearch'	:: ((ParseTree, [Int]) -> Either String VariableAssignments) -> [Int] -> [ParseTree] -> Either String VariableAssignments
depthFirstSearch' matchMaker path values
	= do	let deeper	= zip [0..] values |> (\(i, pt) -> depthFirstSearch matchMaker (path++[i]) pt)
		firstRight deeper

depthFirstSearch	:: ((ParseTree, [Int]) -> Either String VariableAssignments) -> [Int] -> ParseTree -> Either String VariableAssignments
depthFirstSearch matchMaker path pt@(PtSeq _ values)
	= do	let deeper	= depthFirstSearch' matchMaker path values
		let self	= matchMaker (pt, path)
		firstRight [deeper, self]
depthFirstSearch matchMaker path pt	= matchMaker (pt, path)




evaluate	:: Ctx -> Expression -> ParseTree
evaluate ctx (MCall _ "plus" True es)
	= let	(tp, es')	= asInts ctx "plus" es in
		MInt tp (sum es')
evaluate ctx (MCall _ "min" True es)
	= let	(tp, [e1, e2])	= asInts ctx "min" es in
		MInt tp (e1 - e2)
evaluate ctx (MCall _ "mul" True es)
	= let	(tp, es')	= asInts ctx "mul" es in
		MInt tp (product es')
evaluate ctx (MCall _ "div" True es)
	= let	(tp, [e1, e2])	= asInts ctx "min" es in
		MInt tp (e1 `div` e2)
evaluate ctx (MCall _ "mod" True es)
	= let	(tp, [e1, e2])	= asInts ctx "min" es in
		MInt tp (e1 `mod` e2)

evaluate ctx (MCall _ "neg" True es)
	= let	(tp, [e1])	= asInts ctx "neg" es in
		MInt tp (-e1)



evaluate ctx (MCall _ "equal" True es)
	= let	(tp, [e1, e2])	= asInts ctx "equal" es in
		MInt tp (if e1 == e2 then 1 else 0)
evaluate ctx (MCall _ "error" True exprs)
	= let	exprs'	= exprs |> evaluate ctx & toParsable' ", "
		msgs	= ["In evaluating a function:", exprs']
		stack	= ctxStack ctx |> buildStackEl
		in	error $ unlines $ stack ++ msgs
evaluate ctx (MCall _ "newvar" True [identifier, nonOverlap])
	= case evaluate ctx identifier of
		(MIdentifier (basetype, _) nm)
			-> unusedIdentifier nonOverlap (Just nm) basetype
		expr	-> unusedIdentifier nonOverlap Nothing (typeOf expr)
			
evaluate ctx (MCall _ nm True args)
	= evalErr ctx $ "unknown builtin "++nm++" for arguments: "++ toParsable' ", " args

evaluate ctx (MCall _ nm False args)
 | nm `M.member` ctxFunctions ctx
	= let	func	= ctxFunctions ctx M.! nm
		args'	= args |> evaluate ctx in
		applyFunc ctx (nm, func) args'
 | otherwise
	= evalErr ctx $ "unknown function: "++nm	

evaluate ctx (MVar _ nm)
 | nm `M.member` ctxVars ctx	
	= fst $ ctxVars ctx M.! nm
 | otherwise			
	= evalErr ctx $ "unkown variable "++nm

evaluate ctx (MEvalContext _ nm hole)
 | nm `M.member` ctxVars ctx	
	= let	hole'	= evaluate ctx hole
		(context, path)	= ctxVars ctx M.! nm
		path'	= fromMaybe (error $ nm++" was not captured using an evaluation context") path
		in
		replace context path' hole'
 | otherwise			
	= evalErr ctx $ "unkown variable (for evaluation context) "++nm


evaluate ctx (MSeq tp vals)	= vals |> evaluate ctx & PtSeq tp
evaluate ctx (MParseTree pt)	= pt
evaluate ctx e			= evalErr ctx $ "Fallthrough on evaluation in Function interpreter: "++toParsable e++" with arguments:\n"++
					(ctxVars ctx & M.toList |> showVarAssgn & unlines)

showVarAssgn	:: (Name, (ParseTree, Maybe [Int])) -> String
showVarAssgn (nm, assgn)
	= nm ++ " = "++ showAssgn assgn

showAssgn	:: (ParseTree, Maybe [Int]) -> String
showAssgn (pt, mPath)
		= toParsable pt++
		maybe "" (\path -> "\tContext path is "++show path) mPath

evalErr	ctx msg	= evaluate ctx $ MCall "" "error" True [MParseTree $ MLiteral ("", -1) ("Undefined behaviour: "++msg)]

asInts ctx bi exprs	
	= let 	exprs'	= exprs |> evaluate ctx 
				|> (\e -> if isMInt' e then e else error $ "Not an integer in the builtin "++bi++" expecting an int: "++ toParsable e)
				|> (\(MInt _ i) -> i)
		tp	= typeOf $ head exprs
		tp'	= if tp == "" then error "Declare a return type, by annotating the first argument of a builtin" else tp
		in
		((tp', -1), exprs')

buildStackEl	:: (Name, [ParseTree]) -> String
buildStackEl (func, args)
	= "   In "++func++ inParens (toParsable' ", " args)
