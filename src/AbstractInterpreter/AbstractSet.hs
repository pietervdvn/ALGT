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
import Graphs.UnionFind

import Data.Map (Map, (!), member, fromList, toList)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.List as L
import Data.List (intercalate, intersperse, nub)
import Data.Maybe

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.State hiding (get)

import Lens.Micro hiding ((&))

type GeneratingType	= TypeName
data AbstractSet
	= EveryPossible 	GeneratingType Name TypeName	-- The name is used to identify different expressions, used to diverge on pattern matching
	| ConcreteLiteral 	GeneratingType String
	| ConcreteBuiltin	GeneratingType BNF Name		-- Some possible builtin, e.g. Any, Identifier, ...
	| ConcreteInt 		GeneratingType Name
	| AsSeq 		GeneratingType Int [AbstractSet]		-- Sequence, generated with choice
	deriving (Ord, Eq, Show)




type Arguments	= [AbstractSet]


------------------------------------------ GENERATION -----------------------------------------------------------------

generateArgs		:: Syntax -> [TypeName] -> [AbstractSet]
generateArgs s tps 	= tps & mapi |> (\(i, t) -> generateAbstractSet s (show i) t)

generateAbstractSet	:: Syntax -> Name -> TypeName -> AbstractSet
generateAbstractSet s n tp
			= generateAbstractSet' s n tp (error "Should not be used", BNFRuleCall tp)


generateAbstractSet'	:: Syntax -> Name -> TypeName -> (Int, BNF) -> AbstractSet
generateAbstractSet' s n tp
			= _generateAbstractSet s tp n



_generateAbstractSet					:: Syntax -> TypeName -> Name -> (Int, BNF) -> AbstractSet
_generateAbstractSet r generator n (_, Literal s)	= ConcreteLiteral generator s
_generateAbstractSet r generator n (_, Number)		= ConcreteInt generator n
_generateAbstractSet r generator n (_, BNFRuleCall tp)
	| tp `member` getBNF r
			= EveryPossible generator n tp
	| otherwise	= error $ "No bnf-rule with the name " ++ tp
_generateAbstractSet r generator n (choice, BNFSeq bnfs)
			= mapi bnfs
				|> (\(i, bnf) -> _generateAbstractSet r generator (n++":"++show i) (choice, bnf))
				& AsSeq generator choice
_generateAbstractSet r generator n (_, builtin)
			= ConcreteBuiltin generator builtin n




fromExpression			:: Syntax -> Name -> Expression -> AbstractSet
fromExpression _ n (MParseTree pt)
				= fromParseTree n pt
fromExpression s n (MVar tn _)	= generateAbstractSet s n tn
fromExpression s n (MSeq (tn, i) exprs)
 | i < 0			= error $ "AbstractSet: fromexpression: invalid MSeq choice number: "++show i
 | otherwise			= mapi exprs |> (\(i, e) -> fromExpression s (n++":"++show i) e) & AsSeq tn i
fromExpression s n (MCall tn _ _ _)
				= generateAbstractSet s n tn
fromExpression s n (MAscription _ e)
				= fromExpression s n e
fromExpression s n (MEvalContext tn _ _)
				= generateAbstractSet s n tn



fromParseTree			:: Name -> ParseTree -> AbstractSet
fromParseTree _ (MLiteral mi l)
				= ConcreteLiteral (fst mi) l
fromParseTree n (MInt mi _)
				= ConcreteInt (fst mi) n
fromParseTree n (PtSeq (gen, i) pts)
				= mapi pts |> (\(i, pt) -> fromParseTree (n++":"++show i) pt)
					& AsSeq gen i

---------------------------------------------- UNFOLDING ---------------------------------------------------------------


unfold		:: Syntax -> AbstractSet ->  [AbstractSet]
unfold r (EveryPossible _ n tp)
		= let	bnfs	= getBNF r ! tp
		  	choices	= mapi bnfs |> (\(i, bnf) -> _generateAbstractSet r tp (n++"/"++show i) (i, bnf))
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
unfoldAll syntax (AsSeq gen i seq)
	= do	seq'	<- seq |> unfoldAll syntax & allCombinations
		return $ AsSeq gen i seq'
unfoldAll syntax as@EveryPossible{}
	= unfold syntax as
unfoldAll syntax as
	= [as]









-------------------------------------------- FOLDING ----------------------------------------------


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
	= let prepBNFs tp bnfs	= bnfs	& mapi
					|> generateAbstractSet' synt "" tp
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
mightFoldSeq (AsSeq _ i s1) (AsSeq _ j s2)
 | length s1 /= length s2
	|| i /= j		= False
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
	-- In other words, all are a sequence with only differences on one point 
 | otherwise	= do	let allSame	= ass |> (typeOf &&& getSeqNumber) & nub & length & (==1)
			unless allSame $ error "foldGroup: weird combination, not all types and choices are the same"
			let tp		= typeOf $ head ass
			let choice	= getSeqNumber $ head ass
			let seqqed	= ass 	|> fromAsSeq & catMaybes & transpose
										:: [[AbstractSet]]
			let diffPts	= diffPoints seqqed			:: [Int]
			let seqqed'	= seqqed |> head			:: [AbstractSet]
			-- now, all 'indices' should be the same, except for one focus point
			if length diffPts /= 1 then ass else do
				let i		= head diffPts
				a		<- (seqqed !! i) & refold' syntax revTable	:: [AbstractSet]
				let mergedSeq	= take i seqqed' ++ [a] ++ drop (i + 1) seqqed'
				return $ AsSeq tp choice mergedSeq

--------------------------------------------------------- FROM ABSTRACT SET --------------------------------------------------


toBNF			:: AbstractSet -> BNF
toBNF (EveryPossible _ _ tp)
			= BNFRuleCall tp
toBNF (ConcreteLiteral mi s)
			= Literal s
toBNF (ConcreteBuiltin _ bnf _)
			= bnf
toBNF (ConcreteInt _ _)
			= Number
toBNF (AsSeq _ _ ass)	= ass |> toBNF & BNFSeq





toExpression		:: Syntax -> AbstractSet -> Expression
toExpression s as		
	= evalState (_toExpression s (typeOf as) (typeOf as, error "No choice number given") as) M.empty


_toExpression		:: Syntax -> TypeName -> (TypeName, Int) -> AbstractSet -> State (Map TypeName Int) Expression
_toExpression _ exp (genWith, _) (EveryPossible _ _ tp)
  	= do	nm	<- _getName tp
		let var	= MVar tp nm
		if tp == exp then return var else
			-- expected type is bigger than actual type: ascription
			return $ MAscription tp var
_toExpression _ _ mi (ConcreteLiteral _ str)
	= return $ MParseTree $ MLiteral mi str
_toExpression _ _ mi (ConcreteBuiltin genTp _ _)
	= do	nm	<- _getName genTp
		return $ MVar genTp nm
_toExpression _ _ mi (ConcreteInt genTp _)
	= do	nm	<- _getName genTp
		return $ MVar genTp nm

_toExpression s _ _ (AsSeq gen choice seq)
	= do	let prototype	= (get bnf s ! gen !! choice) & fromSeq'	:: [BNF]
		let prototypeNms	= prototype |> fromRuleCall |> fromJust	:: [TypeName]
		zip prototypeNms seq
			|+> (\(bnf, as) -> 
				_toExpression s bnf (gen, choice) as)
			|> MSeq (gen, choice)

_getName		:: TypeName -> State (Map TypeName Int) Name
_getName tn
	= do	i	<- gets (M.findWithDefault 0 tn)
		modify (M.insert tn (i+1))
		return $ if i == 0 then tn else tn ++ show i



---------------------------------------------------------- BORING UTILS ---------------------------------------------------------


-- for unification
instance Node AbstractSet where
	hasChildren (AsSeq _ _ as)	= not $ L.null as
	hasChildren _	= False
	
	getChildren (AsSeq _ _ as)	= as

	newChildren (AsSeq gen i _)	= AsSeq gen i

	sameSymbol (AsSeq gen0 i0 _) (AsSeq gen1 i1 _)	
					= gen0 == gen1 && i0 == i1
	sameSymbol a b			= a == b

	isVar EveryPossible{}		= True
	isVar _				= False

	getName (EveryPossible _ n _)	= n





-- Have the abstractSet - trees the same structure? 
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
isConcrete ConcreteBuiltin{}	= True
isConcrete ConcreteInt{}	= True
isConcrete _			= False

isAsSeq				:: AbstractSet -> Bool
isAsSeq AsSeq{}			= True
isAsSeq _			= False


fromAsSeq			:: AbstractSet -> Maybe [AbstractSet]
fromAsSeq (AsSeq _ _ seq)	= Just seq
fromAsSeq _			= Nothing

getSeqNumber			:: AbstractSet -> Int
getSeqNumber (AsSeq _ i _)	= i

fromAsSeq'			:: AbstractSet -> [AbstractSet]
fromAsSeq' as			= fromMaybe [as] $ fromAsSeq as

packAsSeq			:: TypeName -> Int -> [AbstractSet] -> AbstractSet
packAsSeq _ _ [as]		= as
packAsSeq gen i as		= AsSeq gen i as


-- Recursively edits all names
overAsName	:: (Name -> Name) -> AbstractSet -> AbstractSet
overAsName f (EveryPossible gen n tn)
		= EveryPossible gen (f n) tn
overAsName f (ConcreteLiteral gen s)
		= ConcreteLiteral gen s	-- Literals don't have a name!
overAsName f (ConcreteBuiltin gen bnf n)
		= ConcreteBuiltin gen bnf $ f n
overAsName f (ConcreteInt gen n)
		= ConcreteInt gen $ f n
overAsName f (AsSeq gen i ass)
		= ass |> overAsName f & AsSeq gen i


getAsName	:: AbstractSet -> Maybe Name
getAsName (EveryPossible _ n _)
		= Just n
getAsName ConcreteLiteral{}
		= Nothing
getAsName (ConcreteBuiltin _ _ n)
		= Just n
getAsName (ConcreteInt _ n)
		= Just n
getAsName AsSeq{}	
		= Nothing



instance SimplyTyped AbstractSet where
	typeOf (EveryPossible _ _ tn)		= tn
	typeOf (ConcreteLiteral gen _)		= gen
	typeOf (ConcreteBuiltin gen _ _)	= gen
	typeOf (ConcreteInt gen _)		= gen
	typeOf (AsSeq gen _ _)			= gen

generatorOf		:: AbstractSet -> TypeName
generatorOf (EveryPossible gen _ _)	= gen
generatorOf as				= typeOf as



-- Recursively edits all generators
overGenerator	:: (GeneratingType -> GeneratingType) -> AbstractSet -> AbstractSet
overGenerator f (EveryPossible gen n tn)
		= EveryPossible (f gen) n tn
overGenerator f (ConcreteLiteral gen n)
		= ConcreteLiteral (f gen) n
overGenerator f (ConcreteBuiltin gen bnf n)
		= ConcreteBuiltin (f gen) bnf n
overGenerator f (ConcreteInt gen n)
		= ConcreteInt (f gen) n
overGenerator f (AsSeq gen i ass)
		= ass |> overGenerator f & AsSeq (f gen) i






getAsAt		:: AbstractSet -> Path -> AbstractSet
getAsAt as []	= as
getAsAt (AsSeq _ _ orig) (i:rest)
 | length orig <= i
	= error $ "Invalid getAsAt path: index "++show i++" to big for " ++toParsable' " " orig
 | otherwise
	= let	(_, head:_)	= splitAt i orig in
		getAsAt head rest
getAsAt rest path
	= error $ "Invalid getAsAt path: not a sequence, but trying to get the path "++show path++" on " ++toParsable rest



replaceAS	:: AbstractSet -> Path -> AbstractSet -> AbstractSet
replaceAS _ [] toPlace	= toPlace
replaceAS (AsSeq gen choice orig) (i:rest) toPlace
 | length orig <= i
	= error $ "Invalid substitution path: index "++show i++" to big for " ++toParsable' " " orig
 | otherwise
	= let	(init, head:tail)	= splitAt i orig
		head'		= replaceAS head rest toPlace in
		(init ++ (head':tail)) & AsSeq gen choice
replaceAS rest path toReplace
	= error $ "Invalid substitution path: not a sequence, but trying to execute the path "++show path++" on " ++toParsable rest



searchPathAS	:: (AbstractSet -> Bool) -> AbstractSet -> [Path]
searchPathAS pred as@(AsSeq g i seq)
	= let	rest	= mapi seq
				>>= (\(i, as) -> searchPathAS pred as |> (i:))
		in
		if pred as then []:rest else rest
searchPathAS pred as
	= [ [] | pred as] 



containsRuleAS	:: [TypeName] -> AbstractSet -> Bool
containsRuleAS tns (EveryPossible _ _ tn)
		= tn `elem` tns
containsRuleAS tns (AsSeq _ _ seqs)
		= any (containsRuleAS tns) seqs
containsRuleAS _ _	= False













instance Refactorable TypeName AbstractSet where
	refactor ftn (EveryPossible gen n tn)
			= EveryPossible (ftn gen) n (ftn tn)
	refactor ftn (AsSeq gen i seq)
			= seq |> refactor ftn & AsSeq (ftn gen) i
	refactor ftn as	= overGenerator ftn as


instance ToString AbstractSet where
	toParsable (EveryPossible _ _ name)	= name 
	toParsable (ConcreteLiteral _ s)	= show s
	toParsable (ConcreteBuiltin _ bnf nm)	= toParsable bnf
	toParsable (ConcreteInt _ nm)		= "Number"
	toParsable (AsSeq _ _ ass)		= ass |> toParsable & unwords & inParens
	
	toCoParsable as@(EveryPossible _ n tp)		= tp++n
	toCoParsable as@(ConcreteLiteral _ s)		= show s ++ _to as
	toCoParsable as@(ConcreteBuiltin _ bnf nm)	= toParsable bnf ++nm ++ _to as
	toCoParsable as@(ConcreteInt _ nm)		= "Number"++nm ++ _to as
	toCoParsable as@(AsSeq _ _ ass)			= ass |> toCoParsable & unwords & inParens ++ _to as ++ "(gen: "++generatorOf as++"/"++show (getSeqNumber as)++")"


	debug	= show
_to as	= "::"++typeOf as



