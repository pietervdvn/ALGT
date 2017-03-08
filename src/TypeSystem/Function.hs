 {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module TypeSystem.Function where


import Utils.Utils
import Utils.ToString

import TypeSystem.Types
import TypeSystem.ParseTree
import TypeSystem.Syntax
import TypeSystem.Expression

import qualified Data.Map as M
import Data.Map
import Data.List (intercalate)
import qualified Data.List as L

import Control.Arrow ((&&&))

import Lens.Micro hiding ((&))
import Lens.Micro.TH



-- The assignment of names to variables
type VariableAssignmentsA a
		= Map Name (a, Maybe Path)	-- If a path of numbers (indexes in the expression-tree) is given, it means a evaluation context is used



instance (ToString a, Show a) => ToString' String (VariableAssignmentsA a) where
	toParsable'	= _toStringVarAssgn toParsable
	toCoParsable'	= _toStringVarAssgn toCoParsable
	debug'		= _toStringVarAssgn debug
	show'		= const show


_toStringVarAssgn sPt sep vars
	= vars & M.toList |> _showVarPair sPt & intercalate sep
_showVarPair sPt (nm, (pt, mPath))
	= nm ++ " --> " ++ sPt pt ++ maybe "" (\pth -> " (With a hole at "++showComma pth++")") mPath



findAssignment		:: Name -> VariableAssignmentsA a -> Maybe (a, Maybe [Int])
findAssignment		= M.lookup



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





data BuiltinFunction = BuiltinFunction
	{ _bifName	:: Name
	, _bifDescr	:: String
	, _bifInArgs	:: Either (TypeName, Int) [TypeName]	-- Either (at least i times someType) or (someType -> someType); .* if everything is possible 
	, _bifResultType:: TypeName			-- Use .* if a type should be given explicitly or ɛ if any type is possible
	, _bifApply	:: Either ([Int] -> Int) ([ParseTree] -> ParseTree)  
	} 
makeLenses ''BuiltinFunction


numbers	:: Int -> Either (TypeName, Int) a
numbers i	= Left ("Number", i)

numbers0	= numbers 0
numbers1	= numbers 1

builtinFunctions'
	= builtinFunctions |> (get bifName &&& id) & M.fromList
builtinFunctions
      = [ BuiltinFunction "plus" "Gives a sum of all arguments (0 if none given)" 
		numbers0 "Number"
		$ Left sum

	, BuiltinFunction "min" "Gives the first argument, minus all the other arguments"
		numbers1 "Number" 
		$ Left (\(i:is) -> i - sum is)

	, BuiltinFunction "mul" "Multiplies all the arguments. (1 if none given)"
		numbers0 "Number"
		$ Left product

	, BuiltinFunction "div" "Gives the first argument, divided by the product of the other arguments. (Integer division, rounded down))"
		numbers1 "Number"
		$ Left (\(i:is) -> i `div` product is)
	, BuiltinFunction "mod" "Gives the first argument, module the product of the other arguments."
		numbers1 "Number"
		$ Left (\(i:is) -> i `mod` product is)
	, BuiltinFunction "neg" "Gives the negation of the argument"
		(Right ["Number"]) "Number"
		$ Left (\[i] -> -1)
	, BuiltinFunction "equal" "Checks that all the arguments are equal. Gives 1 if so, 0 if not."
		(Right [topSymbol, topSymbol]) "Number"
		$ Right (\(e:es) -> MInt ("Number", 0) $ if all (e ==) es then 1 else 0)
	, BuiltinFunction "error" "Stops the function, gives a stack trace. When used in a rule, this won't match a predicate"
		(Left (topSymbol, 0)) bottomSymbol 
		$ Right (\pts -> pts & toParsable' " " & MLiteral (bottomSymbol, 0))
				
	, BuiltinFunction "subs" ("(expression to replace, to replace with, in this expression) "
			++ "Replaces each occurence of the first expression by the second, in the third argument."
			++" You'll want to explictly type this one, by using `subs:returnType(\"x\", \"41\", \"x + 1\")`")
		(Right [topSymbol, topSymbol, topSymbol]) topSymbol
		$ Right (\[searchFor, replaceBy, inExpr] ->
			let	paths	= search (==searchFor) inExpr
				folded	= L.foldl (\pt path -> replace pt path replaceBy) inExpr paths
				in folded)
	]







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
		clss	= clauses |> showClause in
		(sign':clss) & unlines



