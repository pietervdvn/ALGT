module AbstractInterpreter.AbstractParseTree where

{-
This module defines an abstract type tree
-}
import Utils
import TypeSystem

import Data.Map
import Data.List (intercalate, intersperse)

import Control.Arrow ((&&&))


data AbstractSet'
	= EveryPossible MInfo Name TypeName 	-- The name is used to identify different expressions, used to diverge on pattern matching
	| ConcreteLiteral MInfo String
	| ConcreteIdentifier MInfo Name
	| ConcreteInt MInfo Name
	| AsSeq MInfo [AbstractSet']
	| Choice TypeName [AbstractSet']
	deriving (Ord, Eq)

instance SimplyTyped AbstractSet' where
	typeOf as	= _typeOf as & either id fst
	
_typeOf (EveryPossible mi _ _)		= Right mi
_typeOf (ConcreteLiteral mi _)		= Right mi
_typeOf (ConcreteIdentifier mi _)	= Right mi
_typeOf (ConcreteInt mi _)		= Right mi
_typeOf (AsSeq mi _)			= Right mi
_typeOf (Choice tp _)			= Left tp

type AbstractSet	= (BNFRules, AbstractSet')

typeOf' as	= typeOf $ snd as

generateAbstractSet	:: BNFRules -> Name -> TypeName -> AbstractSet
generateAbstractSet r n tm
			= generateAbstractSet' r (tm, -1) n (BNFRuleCall tm)


generateAbstractSet' 			:: BNFRules -> (TypeName, Int) -> Name -> BNFAST -> AbstractSet
generateAbstractSet' r mi name bnfAst	= (r, _generateAbstractSet' r mi name bnfAst)

_generateAbstractSet'			:: BNFRules -> (TypeName, Int) -> Name -> BNFAST -> AbstractSet'
_generateAbstractSet' r mi n (Literal s)	= ConcreteLiteral mi s
_generateAbstractSet' r mi n Identifier		= ConcreteIdentifier mi n
_generateAbstractSet' r mi n Number		= ConcreteInt mi n
_generateAbstractSet' r mi n (BNFRuleCall tp)
	| tp `member` r	= EveryPossible mi n tp
	| otherwise	= error $ "No bnf-rule with the name " ++ tp
_generateAbstractSet' r mi n (BNFSeq bnfs)
			= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet' r mi (n++":"++show i) bnf) & AsSeq mi


unfold			:: AbstractSet -> AbstractSet
unfold (r, EveryPossible _ n e)
		= let	bnfs	= r ! e
		  	choices	= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet' r (e, i) (n++"/"++show i) bnf)
		  in (r, Choice e choices)
unfold (r, AsSeq mi ass)
		= (r, ass |> unfold' r & AsSeq mi)
unfold (r, Choice mi ass)
		= (r, ass |> unfold' r & Choice mi)
unfold as	= as

unfold'		:: BNFRules -> AbstractSet' -> AbstractSet'
unfold' r as	= unfold (r, as) & snd

instance Show AbstractSet' where
	show (EveryPossible _ _ name)	= name 
	show (ConcreteLiteral _ s)	= show s
	show (ConcreteIdentifier _ nm)	= "Identifier:"++nm
	show (ConcreteInt _ nm)		= "Number:"++nm
	show (AsSeq _ ass)		= ass |> show & unwords
	show (Choice _ ass)		= ass |> show & intercalate " | " & (\s -> "{"++s++"}")
