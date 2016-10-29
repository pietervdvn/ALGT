module TypeSystem.MetaFunctionInterpreter where

{-
This module interprets metafunctions 
-}

import Utils
import Parser.TsAST
import Parser.StlcAST

import Data.Map as M
import Data.List as L
import Data.Either

import Control.Monad

type Context	= Map Name MetaExpression

-- Reduces an expression to normal form. Note: not context is given, as these are substituted
reduceExpression	:: MetaExpression -> Either String MetaExpression
reduceExpression (MFVariable name)	
	= Left $ "Unknown variable: "++name
reduceExpression (MEApp fExpr argExprs)
	= do	argsRed	<- argExprs |+> reduceExpression
		fRed	<- reduceExpression fExpr
		case fRed of 
			MEFunction mf	-> applyFunction argsRed mf
			_		-> Left $ "Not a function ("++show fRed++"), but applied to "++show argsRed
reduceExpression e
	= return $ e -- function/concrete type are in normal form




applyFunction	:: [MetaExpression] -> MetaFunction -> Either String MetaExpression
applyFunction args (MF name _ clauses)
	= let	applied	= clauses |> applyClause args
		results	= rights applied in
		if L.null results then
			Left $ "Could not apply function "++name++", all patterns failed:\n"++ (lefts applied|> ('\t':) & unlines)
		else
			return $ head results


applyClause	:: [MetaExpression] -> MetaClause -> Either String MetaExpression
applyClause args (MFC pats expr)
 | length pats /= length args	
	= Left $ ("Length of the arguments and given patterns don't match"++
			", expected "++show (length pats)++" arguments but got "++show (length args)++" instead")
 | otherwise	
	= do	ctx	<- zip pats args
				|> uncurry (applyPattern empty) & sequence
				>>= foldM mergeContexts empty
	 	return $ subs ctx expr


mergeContexts	:: Context -> Context -> Either String Context
mergeContexts ctx1 ctx2
	= let 	errMsg k v1 v2	= "Context merge failed for name "++k++", expressions don't match: "++show v1++" /= "++show v2
		notSame	= ctx1 `intersection` ctx2 & keys |> (\k -> (k, ctx1 ! k == ctx2 ! k)) & L.filter (not . snd) |> fst
		errMsgs	= notSame |> (\k -> errMsg k (ctx1 ! k) (ctx2 ! k)) in
	  if L.null notSame then return (M.union ctx1 ctx2) else Left $ concat errMsgs


-- Expects fully reduced expressions to deconstruct and check equality
applyPattern	:: Context -> MetaPattern -> MetaExpression -> Either String Context
applyPattern ctx (MPAssign nm) expr
 | nm `member` ctx	
	= if expr == ctx ! nm then return ctx 
	  else Left $ "Patterns with the same name ("++show nm++") don't match: " ++ show (ctx ! nm) ++ " /= "++show expr
 | otherwise		= return $ M.insert nm expr ctx
applyPattern ctx (MPDestructArrow p1 p2) (METype (ArrowT t1 t2))
	= do	ctx1	<- applyPattern ctx p1 $ METype t1
		ctx2	<- applyPattern ctx p2 $ METype t2
		mergeContexts ctx1 ctx2
		
		
applyPattern _ p expr	= Left $ "Could not match pattern with expression "++show expr

subs	:: Context -> MetaExpression -> MetaExpression
subs ctx original@(MFVariable nm)
	= findWithDefault original nm ctx
subs ctx (MEApp e1 args)
	= MEApp (subs ctx e1) (args |> subs ctx)


--- testing examples ---

t1	= ArrowT BoolT NatT
t2	= ArrowT t1 t1

e	= METype 

p1	= MPDestructArrow (MPAssign "T1") (MPAssign "T2")
p2	= MPAssign "T"

clause1	= MFC [p2, p2] $ MFVariable "T"
clause2 = MFC [p1] $ MFVariable "T1"

dom	= MEFunction $ MF "dom" (MTArrow MType MType) [clause2]
