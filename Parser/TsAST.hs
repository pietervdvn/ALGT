module TsAST where

{-
This module defines the AST for TypeSystems
-}

import StlcAST

data Typing	= Typing Expr Type
data Predicate	= TypingInContext Typing
		|  ContextEntails 
