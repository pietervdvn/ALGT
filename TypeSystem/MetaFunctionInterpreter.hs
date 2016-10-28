module TypeSystem.MetaFunctionInterpreter where

{-
This module interprets metafunctions 
-}

import Utils
import Parser.TsAST
import Parser.StlcAST

import Data.Map as M
import Data.List as L

import Control.Monad

type Context	= Map Name MetaExpression




applyClause	:: MetaClause -> [MetaExpression] -> Either String MetaExpression
applyClause (MFC pats expr) args
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
subs ctx (MEApp e1 e2)
	= MEApp (subs ctx e1) (subs ctx e2)


--- testing examples ---

t1	= ArrowT BoolT NatT
t2	= ArrowT t1 t1

e	= METype 

p1	= MPDestructArrow (MPAssign "T") (MPAssign "T")
p2	= MPAssign "T"

clause1	= MFC [p2, p2] $ MFVariable "T"
