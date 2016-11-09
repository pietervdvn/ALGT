module Parser.StlcAST where

import Utils 

{-
This module defines the Syntax Tree
-}

data Type = BoolT | NatT | ArrowT Type Type | UnkownT
	deriving (Ord, Eq)


allType	= [BoolT, NatT] ++ [ArrowT x y | x <- allType, y <- allType]

isArrow (ArrowT _ _)	= True
isArrow _		= False

inParens str	= "("++str++")"

instance Show Type where
	show BoolT	= "Bool"
	show NatT	= "Nat"
	show (ArrowT t1 t2)
			= inParens (show t1 ++ " -> " ++ show t2)
	show (UnkownT)	= "?"


data Expr 	= BoolE Bool
		| NatE Int 
		| NatsE		-- Placeholder for all natural numbers
		| PlusE Expr Expr 
		| IfThenElse Expr Expr Expr 
		| VarE String
		| AppE Expr Expr 
		| LambdaE String Type Expr
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
