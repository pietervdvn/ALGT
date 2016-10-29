module Parser.TsAST where

{-
This module defines the AST for TypeSystems
-}

import Utils
import Parser.StlcAST

data MetaType	= MType | MTArrow MetaType MetaType
	deriving (Ord, Eq)

instance Show MetaType where
	show MType = "Type"
	show (MTArrow t1 t2)	= show t1 ++ " -> " ++ show t2

data MetaExpression
	= MFVariable Name 
	| MEApp MetaExpression [MetaExpression]
	| MEFunction MetaFunction
	| METype Type -- This is a 'normal' type - as defined in StlcAST!
	deriving (Show, Ord, Eq)
data MetaPattern
	= MPAssign Name | MPDestructArrow MetaPattern MetaPattern
	deriving (Ord, Eq)
instance Show MetaPattern where
	show (MPAssign name) = name
	show (MPDestructArrow t1 t2)	= show t1 ++ " -> " ++ show t2

data MetaClause
		= MFC [MetaPattern] MetaExpression
	deriving (Show, Ord, Eq)
data MetaFunction
		= MF Name MetaType [MetaClause]
	deriving (Show, Ord, Eq)


data Typing	= Typing Expr Type
	deriving (Show, Ord, Eq)
data Predicate	= TypingInContext Typing
		| ContextEntails Typing
	deriving (Show, Ord, Eq)
		
