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
	= EveryPossible MInfo TypeName 
	| ConcreteLiteral MInfo String
	| ConcreteIdentifier MInfo
	| ConcreteInt MInfo
	| AsSeq MInfo [AbstractSet']
	| Choice TypeName [AbstractSet']
	deriving (Ord, Eq)

type AbstractSet	= (BNFRules, AbstractSet')

generateAbstractSet	:: BNFRules -> TypeName -> AbstractSet
generateAbstractSet r tm
			= generateAbstractSet' r (tm, -1) (BNFRuleCall tm)


generateAbstractSet' 			:: BNFRules -> (TypeName, Int) -> BNFAST -> AbstractSet
generateAbstractSet' r mi bnfAst	= (r, _generateAbstractSet' r mi bnfAst)

_generateAbstractSet'			:: BNFRules -> (TypeName, Int) -> BNFAST -> AbstractSet'
_generateAbstractSet' r mi (Literal s)	= ConcreteLiteral mi s
_generateAbstractSet' r mi Identifier	= ConcreteIdentifier mi
_generateAbstractSet' r mi Number	= ConcreteInt mi
_generateAbstractSet' r mi (BNFRuleCall n)
	| n `member` r	= EveryPossible mi n
	| otherwise	= error $ "No bnf-rule with the name " ++ n
_generateAbstractSet' r mi (BNFSeq bnfs)
					= bnfs |> _generateAbstractSet' r mi & AsSeq mi


unfold			:: AbstractSet -> AbstractSet
unfold (r, EveryPossible _ e)
		= let	bnfs	= r ! e
		  	choices	= zip [0..] bnfs |> (\(i, bnf) -> _generateAbstractSet' r (e, i) bnf)
		  in (r, Choice e choices)
unfold (r, AsSeq mi ass)
		= (r, ass |> (const r &&& id) |> unfold |> snd & AsSeq mi)
unfold (r, Choice mi ass)
		= (r, ass |> (const r &&& id) |> unfold |> snd & Choice mi)
unfold as	= as


instance Show AbstractSet' where
	show (EveryPossible _ name)	= name 
	show (ConcreteLiteral _ s)	= show s
	show (ConcreteIdentifier _)	= "Identifier"
	show (ConcreteInt _)		= "Number"
	show (AsSeq _ ass)		= ass |> show & unwords
	show (Choice _ ass)		= ass |> show & intercalate " | " & (\s -> "{"++s++"}")
