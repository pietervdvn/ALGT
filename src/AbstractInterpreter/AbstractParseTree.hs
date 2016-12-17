module AbstractInterpreter.AbstractParseTree where

{-
This module defines an abstract type tree
-}
import Utils.Utils
import TypeSystem

import Data.Map
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (intercalate, intersperse, nub)

import Control.Arrow ((&&&))


data AbstractSet'
	= EveryPossible MInfo Name TypeName 	-- The name is used to identify different expressions, used to diverge on pattern matching
	| ConcreteLiteral MInfo String
	| ConcreteIdentifier MInfo Name
	| ConcreteInt MInfo Name
	| AsSeq MInfo [AbstractSet']		-- Sequence
	deriving (Ord, Eq)

instance SimplyTyped AbstractSet' where
	typeOf as	= _typeOf as & either id fst
	
_typeOf (EveryPossible mi _ _)		= Right mi
_typeOf (ConcreteLiteral mi _)		= Right mi
_typeOf (ConcreteIdentifier mi _)	= Right mi
_typeOf (ConcreteInt mi _)		= Right mi
_typeOf (AsSeq mi _)			= Right mi

type AbstractSet	= (Syntax, AbstractSet')

typeOf' as	= typeOf $ snd as

generateAbstractSet	:: Syntax -> Name -> TypeName -> AbstractSet
generateAbstractSet r n tm
			= generateAbstractSet' r (tm, -1) n (BNFRuleCall tm)


generateAbstractSet' 			:: Syntax -> (TypeName, Int) -> Name -> BNF -> AbstractSet
generateAbstractSet' r mi name bnf	= (r, _generateAbstractSet' r mi name bnf)

_generateAbstractSet'			:: Syntax -> (TypeName, Int) -> Name -> BNF -> AbstractSet'
_generateAbstractSet' r mi n (Literal s)	= ConcreteLiteral mi s
_generateAbstractSet' r mi n Identifier		= ConcreteIdentifier mi n
_generateAbstractSet' r mi n Number		= ConcreteInt mi n
_generateAbstractSet' r mi n (BNFRuleCall tp)
	| tp `member` (getBNF r)
			= EveryPossible mi n tp
	| otherwise	= error $ "No bnf-rule with the name " ++ tp
_generateAbstractSet' r mi n (BNFSeq bnfs)
			= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet' r mi (n++":"++show i) bnf) & AsSeq mi


unfold		:: AbstractSet -> [AbstractSet]
unfold (r, as)	= zip (repeat r) (unfold' r as)

unfold'		:: Syntax -> AbstractSet' ->  [AbstractSet']
unfold' r (EveryPossible _ n e)
		= let	bnfs	= getBNF r ! e
		  	choices	= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet' r (e, i) (n++"/"++show i) bnf)
		  in choices & nub
unfold' r as	= [as]


instance Show AbstractSet' where
	show (EveryPossible _ _ name)	= name 
	show (ConcreteLiteral _ s)	= show s
	show (ConcreteIdentifier _ nm)	= "Identifier:"++nm
	show (ConcreteInt _ nm)		= "Number:"++nm
	show (AsSeq _ ass)		= ass |> show & unwords
