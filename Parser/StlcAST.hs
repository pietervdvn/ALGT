module Parser.StlcAST where

import Utils 
import Data.Set (Set, fromList, toList)

{-
This module defines the Syntax Tree
-}

data StaticType = BoolT | NatT | ArrowT StaticType StaticType
	deriving (Ord, Eq)

isArrow (ArrowT _ _)	= True
isArrow _		= False

isBaseType NatT		= True
isBaseType BoolT	= True
isBaseType _		= False

allArrows	:: Set StaticType -> Bool
allArrows ts	= ts & toList & all isArrow

splitArrows	:: [StaticType] -> Maybe ([StaticType], [StaticType])
splitArrows (ArrowT t1 t2:tail)
		= do	(t1s, t2s)	<- splitArrows tail
			return (t1:t1s, t2:t2s)
splitArrows []	= Just ([], [])
splitArrows _	= Nothing

instance Show StaticType where
	show BoolT	= "Bool"
	show NatT	= "Nat"
	show (ArrowT t1 t2)
			= inParens (show t1 ++ " -> " ++ show t2)


data Expr 	= BoolE Bool
		| NatE Int 
		| NatsE		-- Placeholder for all natural numbers
		| PlusE Expr Expr 
		| IfThenElse Expr Expr Expr 
		| VarE String
		| AppE Expr Expr 
		| LambdaE String StaticType Expr
	deriving (Ord, Eq)

instance Show Expr where
	show (BoolE b)	= show b
	show (NatE i)	= show i
	show NatsE	= "0.."
	show (PlusE e1 e2)
			= inParens (show e1++" + "++show e2)
	show (IfThenElse cond e1 e2)
			= inParens ("If "++show cond++" Then " ++ show e1++" Else "++show e2)
	show (VarE str)	= str
	show (AppE f a)	= inParens (show f++" "++show a)
	show (LambdaE str t e)
			= inParens("\\ "++str++":"++show t++" -> "++show e)
