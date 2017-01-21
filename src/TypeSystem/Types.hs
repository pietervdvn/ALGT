 {-# LANGUAGE MultiParamTypeClasses #-}
module TypeSystem.Types where

{-
This module defines synonyms to work with types and typeable
-}

import Utils.Utils (Name)

type TypeName	= Name

newtype FunctionName	= FunctionName {_fname :: Name}
liftFunctionName	:: (Name -> Name) -> FunctionName -> FunctionName
liftFunctionName fn (FunctionName name)
			= FunctionName $ fn name
unliftFunctionName 	:: (FunctionName -> FunctionName) -> Name -> Name
unliftFunctionName ffn n
			= _fname $ ffn $ FunctionName n


newtype RuleName	= RuleName {_ruleNameWrapper :: Name}
liftRuleName	:: (Name -> Name) -> RuleName -> RuleName
liftRuleName fn (RuleName name)
			= RuleName $ fn name
unliftRuleName 	:: (RuleName -> RuleName) -> Name -> Name
unliftRuleName ffn n
			= _ruleNameWrapper $ ffn $ RuleName n

type Symbol		= Name

newtype RelationSymbol	= RelationSymbol {_RelationSymbol :: Symbol}
liftRelationSymbol	:: (Name -> Name) -> RelationSymbol -> RelationSymbol
liftRelationSymbol fn (RelationSymbol name)
			= RelationSymbol $ fn name
unliftRelationSymbol 	:: (RelationSymbol -> RelationSymbol) -> Name -> Name
unliftRelationSymbol ffn n
			= _RelationSymbol $ ffn $ RelationSymbol n






type Type	= [TypeName]	

class SimplyTyped a where
	typeOf	:: a -> TypeName

class FunctionlyTyped a where
	typesOf	:: a -> Type


class Refactorable n a where
	-- replaces all values within the context, e.g. rename BNF-rules in a syntax
	refactor	:: (n -> n) -> a -> a
 

refactor'	:: (Refactorable n a, Eq n) => [(n, n)] -> a -> a
refactor' list	= refactor (_refFunc list)

_refFunc	:: (Eq n) => [(n, n)] -> n -> n
_refFunc [] n	= n 
_refFunc ((n0, nRep):rest) n1
 | n0 == n1	= nRep
 | otherwise	= _refFunc rest n1

