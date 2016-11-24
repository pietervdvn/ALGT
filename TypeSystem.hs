module TypeSystem where

{-
This module defines the AST for TypeSystems
-}

import Utils

import Data.List (intersperse, intercalate)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List

------------------------ Syntax -------------------------
------------------------ Syntax: BNF -------------------------


{- Syntax is described in a Backus-Naur format, a simple naive parser is constructed from it. -}

data BNFAST 	= Literal String	-- Literally parse 'String'
		| Identifier		-- Parse an identifier
		| Number		-- Parse a number
		| BNFRuleCall Name	-- Parse the rule with the given name
		| Seq [BNFAST]		-- Sequence of parts
	deriving (Show, Eq)


fromSingle	:: BNFAST -> Maybe BNFAST
fromSingle (Seq [bnf])	= Just bnf
fromSingle (Seq _)	= Nothing
fromSingle bnf		= Just bnf


fromRuleCall	:: BNFAST -> Maybe Name
fromRuleCall (BNFRuleCall nm)	= Just nm
fromRuleCall _			= Nothing

{-Represents a syntax: the name of the rule + possible parseways -}
type BNFRules	= Map Name [BNFAST]

bnfNames	:: BNFRules -> [Name]
bnfNames r	=  M.keys r & sortOn length & reverse


{-
Consider following BNF:
x ::= ... | y | ...
y ::= ...

This means that every 'y' also (and always) is an 'x'

alwaysIsA searches these relations:

alwaysIsA rules 'y' 'x'	--> True

-}
alwaysIsA	:: BNFRules -> Name -> Name -> Bool
alwaysIsA rules sub super
 | super == ""	= True	-- The empty string is used in dynamic cases, thus are equivalent to everything
 | sub == super	= True
 | super `M.notMember` rules
	= error $ "Unknwown super name: "++show super
 | otherwise	-- super-rule should contain a single occurence, sub or another rule
	= let	superR	= (rules M.! super) |> fromSingle & catMaybes
		-- this single element should be a BNFRuleCall
		superR'	= superR |> fromRuleCall & catMaybes
		-- either sub is an element from superR', or it has a rule which is a super for sub
		-- we don't have to worry about loops; as that would block parsing
		in sub `elem` superR' || or (superR' |> alwaysIsA rules sub)
				
-- Either X is a Y, or Y is a X
equivalent	:: BNFRules -> Name -> Name -> Bool
equivalent r x y
		= alwaysIsA r x y || alwaysIsA r y x

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

-- ["t0", "t1", "t2"]  -->  t0 -> (t1 -> t2)
unflatten	:: [MetaTypeName] -> MetaType
unflatten [t]	= MType t
unflatten (t:ts)= MTArrow (MType t) (unflatten ts)

toSimpleType	:: MetaType -> Maybe MetaTypeName
toSimpleType (MType nm)	= Just nm
toSimpleType _		= Nothing

toSimpleType'	:: MetaType -> Either String MetaTypeName
toSimpleType' tm	= maybe (Left ("Not a simple type: "++show tm)) Right (toSimpleType tm)

-- A metaExpression is always based on a corresponding syntacic rule. It can be both for deconstructing a parsetree or constructing one (depending wether it is used as a pattern or not)
type Builtin	= Bool
type MInfo	= (MetaTypeName, Int)
data MetaExpression
	= MVar MInfo Name
	| MLiteral MInfo String
	| MInt MInfo Int
	| MSeq MInfo [MetaExpression]	
	| MCall MetaTypeName Name Builtin [MetaExpression]	-- not allowed in pattern matching
	| MCast MetaTypeName MetaExpression -- checks wether the expression is built by this smaller rule.
	deriving (Ord, Eq)

isMInt	:: MetaExpression -> Bool
isMInt (MInt _ _)	= True
isMInt _	= False


typeOf	:: MetaExpression -> MetaTypeName
typeOf (MVar (tp, _) _)	= tp
typeOf (MLiteral (tp, _) _)
			= tp
typeOf (MInt (tp, _) _) = tp
typeOf (MSeq (tp, _) _)	= tp
typeOf (MCall tp _ _ _)	= tp
typeOf (MCast tp _)	= tp

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
{-
data Typing	= Typing ParseTree MetaExpression
	deriving (Ord, Eq)
data Predicate	= TypingInContext Typing
		| ContextEntails Typing
		| EqualExprs MetaExpression MetaExpression
	deriving (Ord, Eq)
		
data Rule	= Rule Name [Predicate] [Predicate]
	deriving (Ord, Eq)

-}

			

------------------------ Typesystemfile ------------------------

{-Represents a full typesystem file-}
data TypeSystem 	= TypeSystem {tsName :: Name, 	-- what is this typesystem's name?
					tsContextSymbol :: String, -- how is the context printed?
					tsSyntax	:: BNFRules,	-- synax of the language
					tsFunctions 	:: MetaFunctions	-- syntax metafunctions of the TS 
					-- tsRules 	:: [Rule]	-- predicates and inference rules of the type system, most often used for typing rules
					}
	deriving (Show)




---------------------------------------------------------------------------
------------------------------ UTILITIES ----------------------------------
---------------------------------------------------------------------------

instance Show MetaType where
	show (MType nm) = nm
	show (MTArrow t1 t2)	= show t1 ++ " -> " ++ show t2

instance Show MetaExpression where
	show (MVar mt n)	= n ++ showTI mt
	show (MLiteral mt s)	= show s ++ showTI mt
	show (MInt mt i)	= show i ++ showTI mt
	show (MSeq mt exprs)	= exprs |> show & unwords & inParens & (++ showTI mt)
	show (MCall mt nm builtin args)
				= let args'	= args & showComma & inParens
				  in (if builtin then "!" else "") ++ nm ++ args' ++ ": "++show mt
	show (MCast nm expr)	= (show expr ++ ":" ++ nm) & inParens

showTI ("", _)	= ""
showTI (mt, -1) = ": "++mt
showTI (mt, i)	= ": "++mt++"."++show i

show' (MVar mt n)	= "METAVAR: "++show n
show' (MLiteral _ s)	= s
show' (MInt _ i)	= show i
show' (MSeq mt exprs)	= exprs |> show' & unwords & inParens
show' (MCall mt nm builtin args)
			= let args'	= args & showComma & inParens
			  in "METACALL: " ++ (if builtin then "!" else "") ++ nm ++ args' ++ ": "++show mt
show' (MCast _ expr)	= show' expr & inParens



instance Show MetaClause where
	show (MClause pats expr)
		= let 	args = pats & showComma & inParens
			in
			args ++ " = " ++ show expr
{-
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
-}

