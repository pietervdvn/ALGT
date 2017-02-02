 {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module AbstractInterpreter.AbstractSet where

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
					|> eraseDetails
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
		= let	as'	= as |> eraseDetails & nub
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
		EveryPossible (becomes, -1) "" becomes : (as L.\\ needed)
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
				return $ AsSeq (tp, -1) mergedSeq









generateAbstractSet'	:: Syntax -> Name -> TypeName -> BNF -> AbstractSet
generateAbstractSet' s n tp
			= _generateAbstractSet s (tp, -1) n



generateAbstractSet	:: Syntax -> Name -> TypeName -> AbstractSet
generateAbstractSet s n tp
			= generateAbstractSet' s n tp (BNFRuleCall tp)




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




fromExpression			:: Syntax -> Name -> Expression -> AbstractSet
fromExpression s n (MParseTree (MLiteral mi l))
				= ConcreteLiteral mi l
fromExpression s n (MParseTree (MIdentifier mi _))
				= ConcreteIdentifier mi n
fromExpression s n (MParseTree (MInt mi _))
				= ConcreteInt mi n
fromExpression s n (MParseTree (PtSeq mi pts))
				= pts |> MParseTree & MSeq mi & fromExpression s n 
fromExpression s n (MVar tn _)	= generateAbstractSet s n tn
fromExpression s n (MSeq mi exprs)
				= mapi exprs |> (\(i, e) -> fromExpression s (n++":"++show i) e) & AsSeq mi
fromExpression s n (MCall tn _ _ _)
				= generateAbstractSet s n tn
fromExpression s n (MAscription _ e)
				= fromExpression s n e
fromExpression s n (MEvalContext tn _ _)
				= generateAbstractSet s n tn


generateArgs		:: Syntax -> [TypeName] -> [AbstractSet]
generateArgs s tps 	= tps & mapi |> (\(i, t) -> generateAbstractSet s (show i) t)



unfold		:: Syntax -> AbstractSet ->  [AbstractSet]
unfold r (EveryPossible _ n e)
		= let	bnfs	= getBNF r ! e
		  	choices	= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet r (e, i) (n++"/"++show i) bnf)
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





{- Given abstract sets, removes the second from this set
e.g.

a	::= "x" | "y" | b
b	::= a "~" b | b

subtract [a] b	--> "x" | "y"
subtract [a] "x"	--> "y" | b	-- note that b still can contain an 'x'

-}

_subtract'	:: Syntax -> Map (TypeName, TypeName) TypeName -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract' s k e emin
 | e == emin	= []
 | otherwise	= _subtract s k e emin


_subtract	:: Syntax -> Map (TypeName, TypeName) TypeName -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract syntax known e@(EveryPossible _ _ tp) eminus@(EveryPossible _ _ tpMinus)
 | (tp, tpMinus) `M.member` known	= let	tn	= known M.! (tp, tpMinus) in
						[generateAbstractSet syntax "" tn]
 | subsetOf syntax e eminus		= []
 | tp & mightContainA syntax tpMinus	= [_subtract' syntax known e' eminus | e' <- unfold syntax e] & concat
 | otherwise				= return e
_subtract syntax known s@(AsSeq mi seq) smin@(AsSeq _ seqMin)
 | not (sameForm seq seqMin)	= return s
 | otherwise
	= do	pointWise	<- zip seq seqMin |> uncurry (_subtract' syntax known)
					& replacePointwise seq
		return $ AsSeq mi pointWise
_subtract syntax known s minus
 | subsetOf syntax s minus	
		= []
 | isEveryPossible s && alwaysIsA' syntax minus s
		= let	unfolded	= unfold syntax s
			subbedS 	= unfolded >>= (\s' -> _subtract' syntax known s' minus)
			subbedS'	= nub subbedS
			in
			if subbedS' == unfolded then return s else subbedS'
				
 | otherwise		= return s


isSubexpressionOf	:: Syntax -> TypeName -> AbstractSet -> Bool
isSubexpressionOf s everyPossible doesContain
	= alwaysIsA s everyPossible (typeOf doesContain)


subtract	:: Syntax -> [AbstractSet] -> AbstractSet -> [AbstractSet]
subtract s	= subtractWith s M.empty

subtractWith	:: Syntax -> Map (TypeName, TypeName) TypeName -> [AbstractSet] -> AbstractSet -> [AbstractSet]
subtractWith syntax known ass minus
	= nub $ do	as	<- ass
			_subtract' syntax known as minus



subtractAll	:: Syntax -> [AbstractSet] -> [AbstractSet] -> [AbstractSet]
subtractAll syntax 
		= subtractAllWith syntax M.empty

subtractAllWith	:: Syntax -> Map (TypeName, TypeName) TypeName -> [AbstractSet] -> [AbstractSet] -> [AbstractSet]
subtractAllWith syntax known ass minuses
		= nub $ L.foldl (subtractWith syntax known) ass minuses

-- tells wether this AS contains the second AS completely
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

-- Simple heuristic if it is worth it to subtract two abstract sequences
sameForm	:: [AbstractSet] -> [AbstractSet] -> Bool
sameForm [] []	= True
sameForm (a:as) (b:bs)
 | isConcrete a && isConcrete b	= sameStructure a b
 | otherwise			= sameForm as bs
sameForm _ _	= False



sameStructure	:: AbstractSet -> AbstractSet -> Bool
sameStructure as bs
	= eraseTypes as == eraseTypes bs


-- erases variable names and producing rules
eraseDetails	:: AbstractSet -> AbstractSet
eraseDetails (EveryPossible mi _ tn)
		= EveryPossible (tn, -1) "" tn
eraseDetails (ConcreteLiteral mi s)
		= ConcreteLiteral (_eMI mi) s
eraseDetails (ConcreteIdentifier mi _)
		= ConcreteIdentifier (_eMI mi) ""
eraseDetails (ConcreteInt mi _)
		= ConcreteInt (_eMI mi) ""
eraseDetails (AsSeq mi ass)
		= ass |> eraseDetails & AsSeq (_eMI mi)
_eMI (tn, _)	= (tn, -1)


eraseTypes	:: AbstractSet -> AbstractSet
eraseTypes (EveryPossible mi _ tn)
		= EveryPossible (_noMI mi) "" tn
eraseTypes (ConcreteLiteral mi s)
		= ConcreteLiteral (_noMI mi) s
eraseTypes (ConcreteIdentifier mi _)
		= ConcreteIdentifier (_noMI mi) ""
eraseTypes (ConcreteInt mi _)
		= ConcreteInt (_noMI mi) ""
eraseTypes (AsSeq mi ass)
		= ass |> eraseTypes & AsSeq (_noMI mi)
_noMI _		= ("", -1)


getAt		:: AbstractSet -> Path -> AbstractSet
getAt as []	= as
getAt (AsSeq mi orig) (i:rest)
 | length orig <= i
	= error $ "Invalid getAt path: index "++show i++" to big for " ++toParsable' " " orig
 | otherwise
	= let	(_, head:_)	= splitAt i orig in
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




containsRuleAS	:: [TypeName] -> AbstractSet -> Bool
containsRuleAS tns (EveryPossible _ _ tn)
		= tn `elem` tns
containsRuleAS tns (AsSeq _ seqs)
		= any (containsRuleAS tns) seqs
containsRuleAS _ _	= False


getAsName		:: AbstractSet -> Maybe Name
getAsName (EveryPossible _ n _)	= Just n
getAsName (ConcreteLiteral _ _)	= Nothing
getAsName (ConcreteIdentifier _ n)= Just n
getAsName (ConcreteInt _ n)	= Just n
getAsName (AsSeq _ _)		= Nothing



instance SimplyTyped AbstractSet where
	typeOf as	= _typeOf as & either id fst
	
_typeOf (EveryPossible _ _ tn)		= Left tn
_typeOf (ConcreteLiteral mi _)		= Right mi
_typeOf (ConcreteIdentifier mi _)	= Right mi
_typeOf (ConcreteInt mi _)		= Right mi
_typeOf (AsSeq mi _)			= Right mi

generatorOf		:: AbstractSet -> TypeName
generatorOf (EveryPossible mi _ _)	= fst mi
generatorOf as				= typeOf as


instance Refactorable TypeName AbstractSet where
	refactor ftn (EveryPossible mi n tn)
			= EveryPossible (refactor ftn mi) n (ftn tn)
	refactor ftn (ConcreteLiteral mi s)
			= ConcreteLiteral (refactor ftn mi) s
	refactor ftn (ConcreteIdentifier mi n)
			= ConcreteIdentifier (refactor ftn mi) n
	refactor ftn (ConcreteInt mi i)
			= ConcreteInt (refactor ftn mi) i
	refactor ftn (AsSeq mi seq)
			= seq |> refactor ftn & AsSeq (refactor ftn mi) 


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
