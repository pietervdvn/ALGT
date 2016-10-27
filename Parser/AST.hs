module AST where

{-
This module defines the Syntax Tree
-}

data Type = BoolT | NatT | Arrow Type Type | Unknown
	deriving (Ord, Eq, Show)


data Expr = BoolE Bool | NatE Int | SuccE Expr | IfThenElse Expr Expr Expr | VarE String | AppE Expr Expr | LambdaE String Type Expr
	deriving (Ord, Eq, Show)
