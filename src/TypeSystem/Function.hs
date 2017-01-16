 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module TypeSystem.Function where


import Utils.Utils
import Utils.ToString

import TypeSystem.Types
import TypeSystem.Expression

import Data.Map
import Data.List (intercalate)

-- Patterns, used to deconstruct values (parsetrees) and capture variables, to calculate the end expression
data Clause	= MClause {mecPatterns :: [Expression], mecExpr :: Expression}
	deriving (Show, Ord, Eq)

instance FunctionlyTyped Clause where
	typesOf (MClause pats e)	= (pats |> typeOf) ++ [typeOf e]

{- functions do transform syntax trees (by rewrite rules) and are often used in typechecking and evaluation
	Pattern matching goes from first to last clause
-}
data Function	= MFunction Type [Clause]
	deriving (Show, Ord, Eq)

getClauses (MFunction _ clauses)	= clauses

instance FunctionlyTyped Function where
	typesOf (MFunction t _)	= t


type Functions	= Map Name Function




instance Refactorable TypeName Clause where
	refactor ftn (MClause pats expr)
		= MClause (pats |> refactor ftn) (refactor ftn expr)


instance Refactorable TypeName Function where
	refactor ftn (MFunction tp clauses)
		= MFunction (tp |> ftn) (clauses |> refactor ftn)

instance Refactorable TypeName Functions where
	refactor ftn funcs	= funcs |> refactor ftn



instance Refactorable FunctionName Clause where
	refactor ffn (MClause pats expr)
		= MClause (pats |> refactor ffn) (refactor ffn expr)

instance Refactorable FunctionName Function where
	refactor ffn (MFunction tp clauses)
		= MFunction tp (clauses |> refactor ffn)


instance Refactorable FunctionName Functions where
	refactor ffn funcs
		= funcs |> refactor ffn & mapKeys (unliftFunctionName ffn)


instance ToString' (Name, Int) Clause where
	show' 	= clauseWith show
	toParsable'
		= clauseWith toParsable
	toCoParsable'	= clauseWith toCoParsable
	debug'		= clauseWith debug

clauseWith exprShow (fname, i) (MClause pats expr)
	= let	head	= fname ++ inParens (pats |> exprShow & commas)
		spacing	= replicate (i - length head) ' ' ++ (if length head > i then "\n"++replicate i ' ' else "") ++ " = "
		tail	= exprShow expr in
		head ++ spacing ++ tail



instance ToString' (Name, Int) Function where
	show' nmi	= funcWith "" (show' nmi) nmi
	toParsable' nmi	= funcWith "" (toParsable' nmi) nmi
	toCoParsable' nmi
			= funcWith "" (toCoParsable' nmi) nmi
	debug' nmi	= funcWith "" (debug' nmi) nmi


funcWith underSignature showClause (name, int) (MFunction tp clauses)
	= let	sign	= name ++ replicate (int - length name) ' ' ++ " : "++ intercalate " -> " tp
		sign'	= sign ++ underSignature
		-- we drop the last clause, as it is an automatically added error clause for non exhaustive patterns
		clauses'	= init clauses
		clss	= clauses' |> showClause in
		(sign':clss) & unlines



