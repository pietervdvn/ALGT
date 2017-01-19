module AbstractInterpreter.AbstractSet where

{-
This module defines an abstract type tree, which represents the infinite unfolding of a BNF-rule
-}


import Prelude hiding (subtract)
import Utils.Utils
import Utils.ToString
import TypeSystem
import Utils.Unification

import Data.Map (Map, (!), member, fromList, toList)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.List as L
import Data.List (intercalate, intersperse, nub)
import Data.Tuple (swap)

import Control.Arrow ((&&&))
import Control.Monad

import Debug.Trace -- TODO

data AbstractSet
	= EveryPossible MInfo Name TypeName	-- The name is used to identify different expressions, used to diverge on pattern matching
	| ConcreteLiteral MInfo String
	| ConcreteIdentifier MInfo Name
	| ConcreteInt MInfo Name
	| AsSeq MInfo [AbstractSet]		-- Sequence
	deriving (Ord, Eq, Show)



-- for unification
instance Node AbstractSet where
	hasChildren (AsSeq _ as)	= not $ L.null as
	hasChildren _	= False
	
	getChildren (AsSeq _ as)	= as

	newChildren (AsSeq mi _)	= AsSeq mi

	sameSymbol (AsSeq mi0 _) (AsSeq mi1 _)	
				= mi0 == mi1
	sameSymbol a b		= a == b

	isVar EveryPossible{}		= True
	isVar _				= False

	getName (EveryPossible _ n _)	= n



toBNF			:: AbstractSet -> BNF
toBNF (EveryPossible _ _ tp)
			= BNFRuleCall tp
toBNF (ConcreteLiteral mi s)
			= Literal s
toBNF (ConcreteIdentifier _ _)
			= Identifier
toBNF (ConcreteInt _ _)
			= Number
toBNF (AsSeq _ ass)	= ass |> toBNF & BNFSeq



reverseSyntax	:: Syntax -> Map [AbstractSet] Name
reverseSyntax synt
	= let prepBNFs tp bnfs	= bnfs	|> generateAbstractSet' synt "" tp
					|> eraseDetails
					& sort in
	  synt	& get bnf 
		& M.mapWithKey prepBNFs
		& toList 
		|> swap & fromList

{-
Tries to simplify the abstractset, eventually by refolding.
Names will be invalidated afterwards

E.g. ["True", "False"] -> ["Bool"]

-}
refold		:: Syntax -> Map [AbstractSet] Name -> [AbstractSet] -> [AbstractSet]
refold _ _ [a]	= [a]
refold syntax revTable as
		= let	as'	= as |> eraseDetails & nub
			grouped	= as' & groupBy mightFoldSeq |> foldGroup syntax revTable & concat
			matched	= revTable & M.lookup (sort grouped)
					& maybe grouped (\tn -> [EveryPossible _eMI "" tn])
			in
			trace ("\n\nFolding "++toParsable' ", " as++" became "++toParsable' ", " matched) $ matched


mightFoldSeq	:: AbstractSet -> AbstractSet -> Bool
mightFoldSeq (AsSeq _ s1) (AsSeq _ s2)
 | length s1 /= length s2	= False
 | otherwise		= True
mightFoldSeq a1 a2		= False


foldGroup	:: Syntax -> Map [AbstractSet] Name -> [AbstractSet] -> [AbstractSet]
foldGroup _ _ []		= []
foldGroup _ _ [as]		= [as]
foldGroup syntax revTable ass
 | not $ isAsSeq $ head ass	= ass
	-- All 'details' have been erased, and the entire group has the same structure
	-- In other words, all are a sequence with only differences on e.g. "True" 
 | otherwise	= do	let seqqed	= ass 	|> fromAsSeq & transpose	:: [[AbstractSet]]
			let seqqed'	= seqqed |> head			:: [AbstractSet]
			-- now, all 'indices' should be the same, except for one focus point
			let diffPoints	= seqqed |> (\(a:as) -> all ((==) a) as)
						& mapi & filter (not . snd)	:: [(Int, Bool)]
			if length diffPoints /= 1 then ass else do
				let i		= head diffPoints & fst
				a		<- (seqqed !! i) & refold syntax revTable	:: [AbstractSet]
				let mergedSeq	= (take i seqqed') ++ [a] ++ (drop (i + 1) seqqed')
				return $ AsSeq _eMI mergedSeq

generateAbstractSet	:: Syntax -> Name -> TypeName -> AbstractSet
generateAbstractSet s n tp
			= generateAbstractSet' s n tp (BNFRuleCall tp)

generateAbstractSet'	:: Syntax -> Name -> TypeName -> BNF -> AbstractSet
generateAbstractSet' s n tp bnf
			= _generateAbstractSet s (tp, -1) n bnf


_generateAbstractSet				:: Syntax -> (TypeName, Int) -> Name -> BNF -> AbstractSet
_generateAbstractSet r mi n (Literal s)		= ConcreteLiteral mi s
_generateAbstractSet r mi n Identifier		= ConcreteIdentifier mi n
_generateAbstractSet r mi n Number		= ConcreteInt mi n
_generateAbstractSet r mi n (BNFRuleCall tp)
	| tp `member` getBNF r
			= EveryPossible mi n tp
	| otherwise	= error $ "No bnf-rule with the name " ++ tp
_generateAbstractSet r mi n (BNFSeq bnfs)
			= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet r mi (n++":"++show i) bnf) & AsSeq mi



generateArgs		:: Syntax -> [TypeName] -> [AbstractSet]
generateArgs s tps 	= tps & mapi |> (\(i, t) -> generateAbstractSet s (show i) t)



unfold		:: Syntax -> AbstractSet ->  [AbstractSet]
unfold r (EveryPossible _ n e)
		= let	bnfs	= getBNF r ! e
		  	choices	= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet r (e, i) (n++"/"++show i) bnf)
		  in choices & nub
unfold r as	= [as]


-- Unfolds until everything is a sequence (thus no more 'everyPossible's in the set)
unfoldFull	:: Syntax -> AbstractSet -> [AbstractSet]
unfoldFull syntax as
	= do	as'	<- unfold syntax as
		if isEveryPossible as' then unfoldFull syntax as'
			else return as'


{- Given abstract sets, removes the second from this set
e.g.

a	::= "x" | "y" | b
b	::= a "~" b | b

subtract [a] b	--> "x" | "y"
subtract [a] "x"	--> "y" | b	-- note that b still can contain an 'x'

-}



_subtract	:: Syntax -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract syntax e@(EveryPossible _ _ tp) eminus@(EveryPossible _ _ tpMinus)
 | subsetOf syntax e eminus		= []
 | tp & mightContainA syntax tpMinus	= [_subtract syntax e' eminus | e' <- unfold syntax e] & concat
 | otherwise				= return e
_subtract syntax s@(AsSeq mi seq) smin@(AsSeq _ seqMin)
 | not (sameForm seq seqMin)	= return s
 | otherwise
	= do	pointWise	<- zip seq seqMin |> uncurry (_subtract syntax)
					& replacePointwise seq
		return $ AsSeq mi pointWise
_subtract syntax s minus
 | subsetOf syntax s minus	
		= []
 | isEveryPossible s && alwaysIsA' syntax minus s
		= let	unfolded	= unfold syntax s
			subbedS 	= unfolded >>= (\s' -> _subtract syntax s' minus)
			subbedS'	= nub subbedS
			in
			if subbedS' == unfolded then return s else subbedS'
				
 | otherwise		= return s




subtract	:: Syntax -> [AbstractSet] -> AbstractSet -> [AbstractSet]
subtract syntax ass minus
	= nub $ do	as	<- ass
			_subtract syntax as minus




subtractAll	:: Syntax -> [AbstractSet] -> [AbstractSet] -> [AbstractSet]
subtractAll syntax b minus
		= nub $ L.foldl (subtract syntax) b minus





subsetOf	:: Syntax -> AbstractSet -> AbstractSet -> Bool
subsetOf s (AsSeq _ subs) (AsSeq _ supers)
	= zip subs supers & all (uncurry $ subsetOf s)
subsetOf s sub super@EveryPossible{}
	= alwaysIsA' s sub super
subsetOf s sub@EveryPossible{} super
	= False
subsetOf s concreteSub concreteSuper
	= sameStructure concreteSub concreteSuper

--------------------- BORING UTILS ---------------------------------


isEveryPossible			:: AbstractSet -> Bool
isEveryPossible EveryPossible{}	= True
isEveryPossible _		= False

isConcrete 			:: AbstractSet -> Bool
isConcrete ConcreteLiteral{}	= True
isConcrete ConcreteIdentifier{}	= True
isConcrete ConcreteInt{}	= True
isConcrete _			= False

isAsSeq				:: AbstractSet -> Bool
isAsSeq AsSeq{}			= True
isAsSeq _			= False


fromAsSeq			:: AbstractSet -> [AbstractSet]
fromAsSeq (AsSeq _ seq)		= seq

-- Simple heuristic if it is worth it to subtract two abstract sequences
sameForm	:: [AbstractSet] -> [AbstractSet] -> Bool
sameForm [] []	= True
sameForm (a:as) (b:bs)
 | isConcrete a && isConcrete b	= sameStructure a b
 | otherwise			= sameForm as bs
sameForm _ _	= False



sameStructure	:: AbstractSet -> AbstractSet -> Bool
sameStructure as bs
	= eraseDetails as == eraseDetails bs


-- erases variable names and producing rules
eraseDetails	:: AbstractSet -> AbstractSet
eraseDetails (EveryPossible _ _ tn)
		= EveryPossible _eMI "" tn
eraseDetails (ConcreteLiteral _ s)
		= ConcreteLiteral _eMI s
eraseDetails (ConcreteIdentifier _ _)
		= ConcreteIdentifier _eMI ""
eraseDetails (ConcreteInt _ _)
		= ConcreteInt _eMI ""
eraseDetails (AsSeq _ ass)
		= ass |> eraseDetails & AsSeq _eMI
_eMI		= ("", -1)


getAt		:: AbstractSet -> Path -> AbstractSet
getAt as []	= as
getAt (AsSeq mi orig) (i:rest)
 | length orig <= i
	= error $ "Invalid getAt path: index "++show i++" to big for " ++toParsable' " " orig
 | otherwise
	= let	(init, head:tail)	= splitAt i orig in
		getAt head rest
getAt rest path
	= error $ "Invalid getAt path: not a sequence, but trying to get the path "++show path++" on " ++toParsable rest



replaceAS	:: AbstractSet -> Path -> AbstractSet -> AbstractSet
replaceAS _ [] toPlace	= toPlace
replaceAS (AsSeq mi orig) (i:rest) toPlace
 | length orig <= i
	= error $ "Invalid substitution path: index "++show i++" to big for " ++toParsable' " " orig
 | otherwise
	= let	(init, head:tail)	= splitAt i orig
		head'		= replaceAS head rest toPlace in
		(init ++ (head':tail)) & AsSeq mi
replaceAS rest path toReplace
	= error $ "Invalid substitution path: not a sequence, but trying to execute the path "++show path++" on " ++toParsable rest





instance SimplyTyped AbstractSet where
	typeOf as	= _typeOf as & either id fst
	
_typeOf (EveryPossible _ _ tn)		= Left tn
_typeOf (ConcreteLiteral mi _)		= Right mi
_typeOf (ConcreteIdentifier mi _)	= Right mi
_typeOf (ConcreteInt mi _)		= Right mi
_typeOf (AsSeq mi _)			= Right mi





instance ToString AbstractSet where
	toParsable (EveryPossible _ _ name)	= name 
	toParsable (ConcreteLiteral _ s)	= show s
	toParsable (ConcreteIdentifier _ nm)	= "Identifier"
	toParsable (ConcreteInt _ nm)		= "Number"
	toParsable (AsSeq _ ass)		= ass |> toParsable & unwords & inParens
	
	toCoParsable (EveryPossible _ n tp)	= tp++n 
	toCoParsable (ConcreteLiteral _ s)	= show s
	toCoParsable (ConcreteIdentifier _ nm)	= "Identifier"++nm
	toCoParsable (ConcreteInt _ nm)		= "Number"++nm
	toCoParsable (AsSeq _ ass)		= ass |> toCoParsable & unwords & inParens


	debug	= show
