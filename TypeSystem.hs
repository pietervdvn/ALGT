module TypeSystem where

{-
This module defines the AST for TypeSystems
-}

import Utils

import Data.List (intersperse, intercalate)

import Data.Map (Map)
import qualified Data.Map as M

------------------------ Syntax -------------------------
------------------------ Syntax: BNF -------------------------


{- Syntax is described in a Backus-Naur format, a simple naive parser is constructed from it. -}

data BNFAST 	= Literal String	-- Literally parse 'String'
		| Identifier		-- Parse an identifier
		| Number		-- Parse a number
		| BNFRuleCall Name	-- Parse the rule with the given name
		| Seq [BNFAST]		-- Sequence of parts
	deriving (Show, Eq)


{-Represents a syntax: the name of the rule + possible parseways -}
type BNFRules	= Map Name [BNFAST]

bnfNames	:: BNFRules -> [Name]
bnfNames	=  M.keys

------------------------ Syntax: Actually parsed stuff -------------------------

data ParseTree	= Token String	-- Contents
		| PtNumber Int
		| PtSeq [ParseTree]
		| RuleParse Name Int ParseTree -- causing rule and choice index (thus which option caused) + actual contents
	deriving (Eq, Ord, Show)



ptToMetaExpr	:: ParseTree -> MetaExpression
ptToMetaExpr (Token s)
		= MLiteral s
ptToMetaExpr (PtNumber i)
		= MInt i
ptToMetaExpr (PtSeq pts)
		= pts |> ptToMetaExpr & MSeq
ptToMetaExpr (RuleParse _ _ pt)
		= ptToMetaExpr pt

------------------------ Metafunctions -------------------------

-- Metafunctions do transform syntax trees (by rewrite rules) and are often used in typechecking and evaluation


type MetaTypeName	= Name
data MetaType	= MType MetaTypeName			-- This 'Name' refers to a BNF-rule, which declares a type
		| MTArrow MetaType MetaType
	deriving (Ord, Eq)

flatten	:: MetaType -> [MetaTypeName]
flatten (MType t)	= [t]
flatten (MTArrow head tail)
			= flatten head ++ flatten tail

-- A metaExpression is always based on a corresponding syntacic rule. It can be both for deconstructing a parsetree or constructing one (depending wether it is used as a pattern or not)
-- TODO add typing
type Builtin	= Bool
data MetaExpression
	= MVar Name
	| MLiteral String
	| MInt Int
	| MSeq [MetaExpression]
	| MCall Name Builtin [MetaExpression]	-- not allowed in pattern matching
	| MError String
	deriving (Ord, Eq)

isMInt	:: MetaExpression -> Bool
isMInt (MInt _)	= True
isMInt _	= False



data MetaClause	= MClause {mecPatterns :: [MetaExpression], mecExpr :: MetaExpression}
	deriving (Ord, Eq)


data MetaFunction	= MFunction MetaType [MetaClause]
	deriving (Ord, Eq)


instance Show MetaFunction where
	show (MFunction tp clauses)
		= let	sign	= ": "++show tp
			clss	= clauses |> show in
			(sign:clss) & intercalate "\n"

type MetaFunctions	= Map Name MetaFunction








------------------------ Rules ---------------------------------

data Typing	= Typing ParseTree MetaExpression
	deriving (Ord, Eq)
data Predicate	= TypingInContext Typing
		| ContextEntails Typing
		| EqualExprs MetaExpression MetaExpression
	deriving (Ord, Eq)
		
data Rule	= Rule Name [Predicate] [Predicate]
	deriving (Ord, Eq)


			

------------------------ Typesystemfile ------------------------

{-Represents a full typesystem file-}
data TypeSystem 	= TypeSystem {tsName :: Name, 	-- what is this typesystem's name?
					tsContextSymbol :: String, -- how is the context printed?
					tsSyntax	:: BNFRules,	-- synax of the language
					tsFunctions 	:: MetaFunctions,	-- syntax metafunctions of the TS 
					tsRules 	:: [Rule]	-- predicates and inference rules of the type system, most often used for typing rules
					}
	deriving (Show)





---------------------------------------------------------------------------
------------------------------ UTILITIES ----------------------------------
---------------------------------------------------------------------------

instance Show MetaType where
	show (MType nm) = nm
	show (MTArrow t1 t2)	= show t1 ++ " -> " ++ show t2

instance Show MetaExpression where
	show (MVar n)		= n
	show (MLiteral s)	= show s
	show (MInt i)		= show i
	show (MSeq exprs)	= exprs |> show & unwords & inParens
	show (MCall nm builtin args)
				= let args'	= args & showComma & inParens
				  in
					(if builtin then "!" else "") ++ nm++args'
	show (MError msg)	= "ERROR "++msg

instance Show MetaClause where
	show (MClause pats expr)
		= let 	args = pats & showComma & inParens
			in
			args ++ " = " ++ show expr

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

