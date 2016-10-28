module Parser.StlcAST where

{-
This module defines the Syntax Tree
-}

data Type = BoolT | NatT | ArrowT Type Type | UnknownT
	deriving (Ord, Eq, Show)


data Expr = BoolE Bool | NatE Int | PlusE Expr Expr | IfThenElse Expr Expr Expr | VarE String | AppE Expr Expr | LambdaE String Type Expr
	deriving (Ord, Eq, Show)
