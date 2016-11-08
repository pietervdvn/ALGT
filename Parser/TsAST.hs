module Parser.TsAST where

{-
This module defines the AST for TypeSystems
-}

import Utils
import Parser.StlcAST

import Data.List (intersperse)

import Data.Map (Map)

------------------------ Metafunctions -------------------------


data MetaType	= MType | MTArrow MetaType MetaType
	deriving (Ord, Eq)

instance Show MetaType where
	show MType = "Type"
	show (MTArrow t1 t2)	= show t1 ++ " -> " ++ show t2

data MetaExpression
	= MFVariable Name 
	| MEApp MetaExpression [MetaExpression]
	| MEFunction MetaFunction
	| MEFunctionName Name
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
		= MF {mfName::Name, mfType :: MetaType, mfClauses :: [MetaClause]}
	deriving (Show, Ord, Eq)

------------------------ Rules ---------------------------------

data Typing	= Typing Expr MetaExpression
	deriving (Ord, Eq)
data Predicate	= TypingInContext Typing
		| ContextEntails Typing
		| EqualExprs MetaExpression MetaExpression
	deriving (Ord, Eq)
		
data Rule	= Rule Name [Predicate] [Predicate]
	deriving (Ord, Eq)


instance Show Typing where
	show (Typing e t)
		= show e ++" : "++show t


instance Show Predicate where
	show (TypingInContext typing)
		= show typing ++ " in $"
	show (ContextEntails typing)
		= "$ |- "++show typing
	show (EqualExprs me1 me2)
		= show me1 ++ " == "++show me2

instance Show Rule where
	show (Rule name cond cons)
		= let	nme	= " "++inParens name++" "
			indentL	= 1 + length nme
			indent	= replicate indentL ' '
			top	= cond |> show & intersperse "    " & concat
			bottom	= cons |> show & intersperse "    " & concat
			lineL	= 2 + max (length top) (length bottom)
			line	= replicate lineL '-'
			in
			"\n"++indent ++ top ++ "\n" ++ nme ++ line ++ "\n" ++ indent ++ bottom
			

------------------------ Typesystemfile ------------------------

data TypeSystem 	= TypeSystem {tsName :: Name, tsContextSymbol :: String, tsRules :: [Rule], tsFunctions :: Map Name MetaFunction }
	deriving (Show)

