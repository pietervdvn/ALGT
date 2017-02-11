 {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module AbstractInterpreter.AbstractSet
		(AbstractSet(..), Arguments
		, generateAbstractSet, generateArgs, fromExpression, toBNF
		, unfold, unfoldFull
		, replaceAS, getAsAt
		, refold, refoldWithout
		, isEveryPossible, fromEveryPossible, fromAsSeq, fromAsSeq'
		, getAsName
		, containsRule, containsRuleAS) where

{-
This module defines an abstract type tree, which represents the infinite unfolding of a BNF-rule
-}


import Prelude hiding (subtract)
import Utils.Utils
import Utils.ToString
import TypeSystem
import Utils.Unification

import Graphs.Lattice

import Data.Map (Map, (!), member, fromList, toList)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.List as L
import Data.List (intercalate, intersperse, nub)
import Data.Maybe

import Control.Arrow ((&&&))
import Control.Monad

import Lens.Micro hiding ((&))

type GeneratingType	= TypeName
data AbstractSet
	= EveryPossible 	GeneratingType Name TypeName	-- The name is used to identify different expressions, used to diverge on pattern matching
	| ConcreteLiteral 	GeneratingType String
	| ConcreteIdentifier 	GeneratingType Name
	| ConcreteInt 		GeneratingType Name
	| AsSeq 		GeneratingType [AbstractSet]		-- Sequence
	deriving (Ord, Eq, Show)




type Arguments	= [AbstractSet]


------------------------------------------ GENERATION -----------------------------------------------------------------

generateArgs		:: Syntax -> [TypeName] -> [AbstractSet]
generateArgs s tps 	= tps & mapi |> (\(i, t) -> generateAbstractSet s (show i) t)

generateAbstractSet	:: Syntax -> Name -> TypeName -> AbstractSet
generateAbstractSet s n tp
			= generateAbstractSet' s n tp (BNFRuleCall tp)


generateAbstractSet'	:: Syntax -> Name -> TypeName -> BNF -> AbstractSet
generateAbstractSet' s n tp bnf
			= _generateAbstractSet s tp n bnf



_generateAbstractSet					:: Syntax -> TypeName -> Name -> BNF -> AbstractSet
_generateAbstractSet r generator n (Literal s)		= ConcreteLiteral generator s
_generateAbstractSet r generator n Identifier		= ConcreteIdentifier generator n
_generateAbstractSet r generator n Number		= ConcreteInt generator n
_generateAbstractSet r generator n (BNFRuleCall tp)
	| tp `member` getBNF r
			= EveryPossible generator n tp
	| otherwise	= error $ "No bnf-rule with the name " ++ tp
_generateAbstractSet r generator n (BNFSeq bnfs)
			= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet r generator (n++":"++show i) bnf) & AsSeq generator




fromExpression			:: Syntax -> Name -> Expression -> AbstractSet
fromExpression s n (MParseTree (MLiteral mi l))
				= ConcreteLiteral (fst mi) l
fromExpression s n (MParseTree (MIdentifier mi _))
				= ConcreteIdentifier (fst mi) n
fromExpression s n (MParseTree (MInt mi _))
				= ConcreteInt (fst mi) n
fromExpression s n (MParseTree (PtSeq mi pts))
				= pts |> MParseTree & MSeq mi & fromExpression s n 
fromExpression s n (MVar tn _)	= generateAbstractSet s n tn
fromExpression s n (MSeq mi exprs)
				= mapi exprs |> (\(i, e) -> fromExpression s (n++":"++show i) e) & AsSeq (fst mi)
fromExpression s n (MCall tn _ _ _)
				= generateAbstractSet s n tn
fromExpression s n (MAscription _ e)
				= fromExpression s n e
fromExpression s n (MEvalContext tn _ _)
				= generateAbstractSet s n tn


---------------------------------------------- UNFOLDING ---------------------------------------------------------------


unfold		:: Syntax -> AbstractSet ->  [AbstractSet]
unfold r (EveryPossible _ n e)
		= let	bnfs	= getBNF r ! e
		  	choices	= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet r e (n++"/"++show i) bnf)
		  in choices & nub
unfold r as	= [as]


-- Unfolds until everything is a sequence (thus no more 'everyPossible's in the set). You'll probably end up with something infinite, so be carefull
unfoldFull	:: Syntax -> AbstractSet -> [AbstractSet]
unfoldFull syntax as
	= do	as'	<- unfold syntax as
		if isEveryPossible as' then unfoldFull syntax as'
			else return as'

-- Unfolds all "EveryPossible" in the abstractset exactly once
unfoldAll	:: Syntax -> AbstractSet -> [AbstractSet]
unfoldAll syntax (AsSeq mi seq)
	= do	seq'	<- seq |> unfoldAll syntax & allCombinations
		return $ AsSeq mi seq'
unfoldAll syntax as@EveryPossible{}
	= unfold syntax as
unfoldAll syntax as
	= [as]









-------------------------------------------- FOLDING ----------------------------------------------

-- TODO recheck



refold		:: Syntax -> [AbstractSet] -> [AbstractSet]
refold s	= refoldWithout s []


-- same as refold, but given rulenames *wont* be folded. (e.g. x ::= a | b; [a, b] will *not* fold to [x] if x is given)
refoldWithout s dontFold as
		= let	revTable	= reverseSyntax s
			revTable'	= revTable & filter ((`notElem` dontFold) . snd)
			in
			sort $ refold' s revTable' as

reverseSyntax	:: Syntax -> [([AbstractSet], Name)]
reverseSyntax synt
	= let prepBNFs tp bnfs	= bnfs	|> generateAbstractSet' synt "" tp
					|> eraseNames
					& sort in
	  synt	& get bnf 
		& M.mapWithKey prepBNFs
		& toList 
		|> swap

{-
Tries to simplify the abstractset, eventually by refolding.
Names will be invalidated afterwards

E.g. ["True", "False"] -> ["Bool"]

-}
refold'		:: Syntax -> [([AbstractSet], Name)] -> [AbstractSet] -> [AbstractSet]
refold' _ _ [a]	= [a]
refold' syntax revTable as
		= let	as'	= as |> eraseNames & nub
			grouped	= as' & groupBy mightFoldSeq |> foldGroup syntax revTable
			grouped'	= grouped |> eatSubexpressions syntax
						& concat & sort
			matched	= foldl lookupFold grouped' revTable
			in
			matched


lookupFold		:: [AbstractSet] -> ([AbstractSet], Name) -> [AbstractSet]
lookupFold as ([], _)	= as 
lookupFold as (needed, becomes)
	= if needed `isSubsequenceOf` as then
		EveryPossible becomes "" becomes : (as L.\\ needed)
		else	as


eatSubexpressions	:: Syntax -> [AbstractSet] -> [AbstractSet]
eatSubexpressions _ []	= []
eatSubexpressions _ [as]
			= [as]
eatSubexpressions s ass
	= let	l			= get lattice s
		(everyPosss, rest)	= ass & L.partition isEveryPossible
		everyPoss		= everyPosss |> fromEveryPossible & catMaybes
		allSubs			= everyPoss |> allSubsetsOf l |> S.toList & concat	:: [TypeName]
		unneeded		= allSubs ++ everyPoss
		rest'			= rest & filter (\as -> typeOf as `notElem` unneeded) 
		everyPossResting	= (everyPoss L.\\ allSubs)
		everyPosss'		= everyPosss & L.filter (\as -> typeOf as `elem` everyPossResting)
		in
		everyPosss' ++ rest'


mightFoldSeq	:: AbstractSet -> AbstractSet -> Bool
mightFoldSeq (AsSeq _ s1) (AsSeq _ s2)
 | length s1 /= length s2	= False
 | otherwise			= 1 == zip s1 s2 & filter (uncurry (/=)) & length
mightFoldSeq a1 a2		= True


diffPoints		:: [[AbstractSet]] -> [Int]
diffPoints seqqed	= seqqed |> (\(a:as) -> all (== a) as)
				& mapi & filter (not . snd)
				|> fst	:: [Int]


foldGroup	:: Syntax -> [([AbstractSet], Name)] -> [AbstractSet] -> [AbstractSet]
foldGroup _ _ []		= []
foldGroup _ _ [as]		= [as]
foldGroup syntax revTable ass
 | not $ all isAsSeq ass	= ass
	-- All 'details' have been erased, and the entire group has the same structure
	-- In other words, all are a sequence with only differences on e.g. "True" 
 | otherwise	= do	let tp		= typeOf $ head ass
			let seqqed	= ass 	|> fromAsSeq & catMaybes & transpose
										:: [[AbstractSet]]
			let diffPts	= diffPoints seqqed			:: [Int]
			let seqqed'	= seqqed |> head			:: [AbstractSet]
			-- now, all 'indices' should be the same, except for one focus point
			if length diffPts /= 1 then ass else do
				let i		= head diffPts
				a		<- (seqqed !! i) & refold' syntax revTable	:: [AbstractSet]
				let mergedSeq	= take i seqqed' ++ [a] ++ drop (i + 1) seqqed'
				return $ AsSeq tp mergedSeq

--------------------------------------------------------- FROM ABSTRACT SET --------------------------------------------------


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






---------------------------------------------------------- BORING UTILS ---------------------------------------------------------


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



-- erases variable names
eraseNames	:: AbstractSet -> AbstractSet
eraseNames	= overAsName (const "")


-- erases producing rules
eraseGenerators	:: AbstractSet -> AbstractSet
eraseGenerators	= overGenerator (const "")


eraseDetails	:: AbstractSet -> AbstractSet
eraseDetails	= eraseNames . eraseGenerators



------------------------------------------- Espacially boring stuff. Simple getters setters --------------------------------------

isEveryPossible			:: AbstractSet -> Bool
isEveryPossible EveryPossible{}	= True
isEveryPossible _		= False

fromEveryPossible		:: AbstractSet -> Maybe TypeName
fromEveryPossible (EveryPossible _ _ tn)
				= Just tn
fromEveryPossible _		= Nothing

isConcrete 			:: AbstractSet -> Bool
isConcrete ConcreteLiteral{}	= True
isConcrete ConcreteIdentifier{}	= True
isConcrete ConcreteInt{}	= True
isConcrete _			= False

isAsSeq				:: AbstractSet -> Bool
isAsSeq AsSeq{}			= True
isAsSeq _			= False


fromAsSeq			:: AbstractSet -> Maybe [AbstractSet]
fromAsSeq (AsSeq _ seq)		= Just seq
fromAsSeq _			= Nothing

fromAsSeq'			:: AbstractSet -> [AbstractSet]
fromAsSeq' as			= fromMaybe [as] $ fromAsSeq as



-- Recursively edits all names
overAsName	:: (Name -> Name) -> AbstractSet -> AbstractSet
overAsName f (EveryPossible gen n tn)
		= EveryPossible gen (f n) tn
overAsName f (ConcreteIdentifier gen n)
		= ConcreteIdentifier gen $ f n
overAsName f (ConcreteInt gen n)
		= ConcreteInt gen $ f n
overAsName f (AsSeq gen ass)
		= ass |> overAsName f & AsSeq gen


getAsName	:: AbstractSet -> Maybe Name
getAsName (EveryPossible _ n _)
		= Just n
getAsName (ConcreteLiteral _ _)
		= Nothing
getAsName (ConcreteIdentifier _ n)
		= Just n
getAsName (ConcreteInt _ n)
		= Just n
getAsName (AsSeq _ _)		
		= Nothing



instance SimplyTyped AbstractSet where
	typeOf (EveryPossible _ _ tn)		= tn
	typeOf (ConcreteLiteral gen _)		= gen
	typeOf (ConcreteIdentifier gen _)	= gen
	typeOf (ConcreteInt gen _)		= gen
	typeOf (AsSeq gen _)			= gen

generatorOf		:: AbstractSet -> TypeName
generatorOf (EveryPossible gen _ _)	= gen
generatorOf as				= typeOf as



-- Recursively edits all generators
overGenerator	:: (GeneratingType -> GeneratingType) -> AbstractSet -> AbstractSet
overGenerator f (EveryPossible gen n tn)
		= EveryPossible (f gen) n tn
overGenerator f (ConcreteIdentifier gen n)
		= ConcreteIdentifier (f gen) n
overGenerator f (ConcreteInt gen n)
		= ConcreteInt (f gen) n
overGenerator f (AsSeq gen ass)
		= ass |> overGenerator f & AsSeq (f gen)




getAsAt		:: AbstractSet -> Path -> AbstractSet
getAsAt as []	= as
getAsAt (AsSeq mi orig) (i:rest)
 | length orig <= i
	= error $ "Invalid getAsAt path: index "++show i++" to big for " ++toParsable' " " orig
 | otherwise
	= let	(_, head:_)	= splitAt i orig in
		getAsAt head rest
getAsAt rest path
	= error $ "Invalid getAsAt path: not a sequence, but trying to get the path "++show path++" on " ++toParsable rest



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






containsRuleAS	:: [TypeName] -> AbstractSet -> Bool
containsRuleAS tns (EveryPossible _ _ tn)
		= tn `elem` tns
containsRuleAS tns (AsSeq _ seqs)
		= any (containsRuleAS tns) seqs
containsRuleAS _ _	= False













instance Refactorable TypeName AbstractSet where
	refactor ftn (EveryPossible gen n tn)
			= EveryPossible (ftn gen) n (ftn tn)
	refactor ftn (AsSeq gen seq)
			= seq |> refactor ftn & AsSeq (ftn gen) 
	refactor ftn as	= overGenerator ftn as


instance ToString AbstractSet where
	toParsable (EveryPossible _ _ name)	= name 
	toParsable (ConcreteLiteral _ s)	= show s
	toParsable (ConcreteIdentifier _ nm)	= "Identifier"
	toParsable (ConcreteInt _ nm)		= "Number"
	toParsable (AsSeq _ ass)		= ass |> toParsable & unwords & inParens
	
	toCoParsable as@(EveryPossible _ n tp)		= tp++n
	toCoParsable as@(ConcreteLiteral _ s)		= show s ++ _to as
	toCoParsable as@(ConcreteIdentifier _ nm)	= "Identifier"++nm ++ _to as
	toCoParsable as@(ConcreteInt _ nm)		= "Number"++nm ++ _to as
	toCoParsable as@(AsSeq _ ass)			= ass |> toCoParsable & unwords & inParens ++ _to as


	debug	= show
_to as	= " : "++typeOf as



