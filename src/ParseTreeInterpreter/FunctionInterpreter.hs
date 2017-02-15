 {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-} 
module ParseTreeInterpreter.FunctionInterpreter (evalFunc, evalExpr, VariableAssignments, mergeVars, mergeVarss, patternMatch) where

{-
This module defines an interpreter for functions. 
-}

import TypeSystem
import Utils.ToString
import Utils.Utils

import Control.Monad

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.List (intercalate, intersperse)
import Data.Bifunctor (first)
import Data.Either


type VariableAssignments	= VariableAssignmentsA ParseTree

evalFunc	:: TypeSystem -> Name -> [ParseTree] -> Either String ParseTree
evalFunc ts funcName args	
 | funcName `M.member` get tsFunctions ts
	= let	func	= get tsFunctions ts M.! funcName in
		applyFunc (buildCtx' ts) (funcName, func) args
 | otherwise
	= evalErr (Ctx (get tsSyntax ts) (get tsFunctions ts) M.empty []) $
		"evalFunc with unknown function: "++funcName	

evalExpr	:: TypeSystem -> VariableAssignments -> Expression -> Either String ParseTree
evalExpr ts vars
	= evaluate (buildCtx ts vars)

data Ctx	= Ctx { ctxSyntax	:: Syntax,		-- Needed for typecasts
			ctxFunctions 	:: Map Name Function,
			ctxVars	:: VariableAssignments,
			ctxStack	:: [(Name, [ParseTree])] -- only used for errors
			}

buildCtx ts vars 	= Ctx (get tsSyntax ts) (get tsFunctions ts) vars []
buildCtx' ts		= buildCtx ts M.empty


-- applies given argument to the  function. Starts by evaluating the args
applyFunc	:: Ctx -> (Name, Function) -> [ParseTree] -> Either String ParseTree
applyFunc ctx (nm, MFunction tp clauses) args
 | length args /= length tp - 1
	= evalErr ctx $ "Number of arguments does not match. Expected "++show (length tp - 1)++" variables, but got "++show (length args)++" arguments instead"
 | otherwise
	= do	let stackEl		= (nm, args)
		let ctx'		= ctx {ctxStack = stackEl:ctxStack ctx}
		let clauseResults	= clauses |> evalClause ctx' args & rights
		when (null clauseResults) $ Left "Not a single clause matched, even with error injection. This is a bug!"
		return $ head clauseResults


evalClause	:: Ctx ->  [ParseTree] -> Clause -> Either String ParseTree
evalClause ctx args (MClause pats expr)
	= do	variabless	<- zip pats args |+> uncurry (patternMatch (ctxSyntax ctx) (const True))	-- TODO use lookahead here for eval contexts in functions
		variables	<- mergeVarss variabless
		let ctx'	= ctx {ctxVars = variables}
		evaluate ctx' expr


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
	= do	let matchMaker	= makeMatch r extraCheck evalCtx fullContext	:: (ParseTree, Path) -> Either String VariableAssignments
		depthFirstSearch' matchMaker [] values


makeMatch	:: Syntax -> (VariableAssignments -> Bool) -> (TypeName, Name, Expression) -> ParseTree -> (ParseTree, Path) -> Either String VariableAssignments
makeMatch r extraCheck (tp, name, holePattern) fullContext (holeFiller, path)
	= do	let baseAssign	= M.singleton name (fullContext, Just path)	:: VariableAssignments
		holeAssgn	<- patternMatch r extraCheck holePattern holeFiller
		assgn'		<- mergeVars baseAssign holeAssgn
		assert Left (extraCheck assgn') "Extra patterns (for the rule) failed"
		return assgn'

-- depth first search, excluding self match
depthFirstSearch'	:: ((ParseTree, Path) -> Either String VariableAssignments) -> Path -> [ParseTree] -> Either String VariableAssignments
depthFirstSearch' matchMaker path values
	= do	let deeper	= zip [0..] values |> (\(i, pt) -> depthFirstSearch matchMaker (path++[i]) pt)
		firstRight deeper

depthFirstSearch	:: ((ParseTree, Path) -> Either String VariableAssignments) -> Path -> ParseTree -> Either String VariableAssignments
depthFirstSearch matchMaker path pt@(PtSeq _ values)
	= do	let deeper	= depthFirstSearch' matchMaker path values
		let self	= matchMaker (pt, path)
		firstRight [deeper, self]
depthFirstSearch matchMaker path pt	= matchMaker (pt, path)



evaluate	:: Ctx -> Expression -> Either String ParseTree
evaluate ctx (MCall tp "plus" True es)
	= _applyIntFunction' ctx tp "plus" sum es
evaluate ctx (MCall tp "min" True es)
	= _applyIntFunction ctx tp "min" (1, \(i:is) -> i - sum is) es
evaluate ctx (MCall tp "mul" True es)
	= _applyIntFunction' ctx tp "mul" product es
evaluate ctx (MCall tp "div" True es)
	= _applyIntFunction ctx tp "div" (1, \(i:is) -> i `div` product is) es
evaluate ctx (MCall tp "mod" True es)
	= _applyIntFunction ctx tp "mod" (1, \(i:is) -> i `mod` product is) es

evaluate ctx (MCall tp "neg" True es)
	= _applyIntFunction ctx tp "neg" (1, \(i:_) -> -i) es

evaluate ctx (MCall tp "equal" True es)
	= _applyIntFunction ctx tp "equal" (2, \(i:is) -> if all (==i) is then 1 else 0) es


evaluate ctx (MCall _ "error" True exprs)
	= do	exprs'	<- exprs |+> evaluate ctx |> toParsable' ", "
		let msgs	= ["In evaluating a function:", exprs']
		let stack	= ctxStack ctx |> buildStackEl
		Left $ unlines $ stack ++ msgs
evaluate ctx (MCall _ "newvar" True [identifier, nonOverlap])
	= do	evalled	<- evaluate ctx identifier
		case evalled of
			(MIdentifier (basetype, _) nm)
				-> return $ unusedIdentifier nonOverlap (Just nm) basetype
			expr	-> return $ unusedIdentifier nonOverlap Nothing (typeOf expr)
			
evaluate ctx (MCall _ nm True args)
	= evalErr ctx $ "unknown builtin "++nm++" for arguments: "++ toParsable' ", " args

evaluate ctx (MCall _ nm False args)
 | nm `M.member` ctxFunctions ctx
	= do	let func	= ctxFunctions ctx M.! nm
		args'		<- args |+> evaluate ctx
		applyFunc ctx (nm, func) args'
 | otherwise
	= evalErr ctx $ "unknown function: "++nm	

evaluate ctx (MVar _ nm)
 | nm `M.member` ctxVars ctx	
	= return $ fst $ ctxVars ctx M.! nm
 | otherwise			
	= evalErr ctx $ "unkown variable "++nm

evaluate ctx (MEvalContext _ nm hole)
 | nm `M.member` ctxVars ctx	
	= do	hole'	<- evaluate ctx hole
		let (context, path)	= ctxVars ctx M.! nm
		path'	<- maybe (Left $ nm++" was not captured using an evaluation context") return path
		return $ replace context path' hole'
 | otherwise			
	= evalErr ctx $ "unkown variable (for evaluation context) "++nm


evaluate ctx (MSeq tp vals)	= do	vals'	<- vals |+> evaluate ctx 
					return $ PtSeq tp vals'
evaluate ctx (MParseTree pt)	= return pt
evaluate ctx (MAscription tn expr)
				= evaluate ctx expr
-- evaluate ctx e			= evalErr ctx $ "Fallthrough on evaluation in Function interpreter: "++toParsable e++" with arguments:\n(This is a bug, pietervdvn added a clause to little)\n"++
-- 					(ctxVars ctx & M.toList |> showVarAssgn & unlines)

showVarAssgn	:: (Name, (ParseTree, Maybe Path)) -> String
showVarAssgn (nm, assgn)
	= nm ++ " = "++ showAssgn assgn

showAssgn	:: (ParseTree, Maybe Path) -> String
showAssgn (pt, mPath)
		= toParsable pt++
		maybe "" (\path -> "\tContext path is "++show path) mPath

evalErr		:: Ctx -> String -> Either String ParseTree
evalErr	ctx msg	= evaluate ctx $ MCall "" "error" True [MParseTree $ MLiteral ("", -1) ("Undefined behaviour: "++msg)]


_applyIntFunction	:: Ctx -> TypeName -> Name -> (Int, [Int] -> Int) -> [Expression] -> Either String ParseTree
_applyIntFunction ctx tp funcName (minimumNeeded, f) es
	= do	is	<- asInts ctx funcName es
		when (length es < minimumNeeded) $ Left $ "Builtin integer function '!" ++ funcName++"' needs at least "++show minimumNeeded++" arguments"
		return $ MInt (tp, 0) (f is)

_applyIntFunction' ctx tp funcName f
	= _applyIntFunction ctx tp funcName (0, f)
asInts ctx bi exprs	
	= exprs |+> evaluate ctx 
		|++> (\e -> if isMInt' e then return e else Left $ "Not an integer in the builtin "++bi++" expecting an int: "++ toParsable e)
		||>> (\(MInt _ i) -> i)

buildStackEl	:: (Name, [ParseTree]) -> String
buildStackEl (func, args)
	= "   In "++func++ inParens (toParsable' ", " args)
