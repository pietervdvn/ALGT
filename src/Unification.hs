module Unification (unify, substitute, occursCheck) where

{-
This module implements a general unification for trees
-}

import Utils.Utils
import Data.Map (Map, (!), member)
import qualified Data.Map as M
import Data.Set (Set, insert, findMin, deleteMin)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.Bifunctor
import Control.Arrow ((&&&))

import Control.Monad.State


class (Eq a, Ord a) => Node a where
	hasChildren	:: a -> Bool
	getChildren	:: a -> [a]
	newChildren	:: a -> [a] -> a
	sameSymbol	:: a -> a -> Bool
	isVar		:: a -> Bool
	getName		:: a -> Name

type Substitution a 	= Map Name a


substitute	:: Node a => Substitution a -> a -> a
substitute s a	= substitute' s a & fst

-- Returns true if something was changed
substitute'	:: Node a => Substitution a -> a -> (a, Bool)
substitute' subs a
 | isVar a && getName a `member` subs
		= (subs ! getName a, True)
 | hasChildren a
		= let	(ch', changed)	= getChildren a |> substitute' subs & unzip |> or
			a'		= if changed then newChildren a ch' else a
			in
			(a', changed)
 | otherwise	= (a, False)




-- unify (without infinite tree check). Do an additional 'occurs check' if you want this
unify	:: (Node a) => a -> a -> Either String (Substitution a)
unify a b
	= let	st	= UnifSt S.empty (S.singleton (a,b)) Nothing 
		st'	= execState runUnify st in
		buildSubs st'
		

buildSubs	:: UnifSt a -> Either String (Substitution a)
buildSubs unifSt
 | isJust (errMsg unifSt)	= Left $ fromJust $ errMsg unifSt
 | not $ S.null $ nonCanon unifSt
				= Left "Unification didn't run till the end. This is a bug"
 | otherwise			= Right $ M.fromList $ S.toList $ canonForm unifSt


-- Returns variables for which the term contains the variable itself
occursCheck	:: (Node a) => Substitution a -> [Name]
occursCheck s
	= s & M.toList & filter (uncurry contains) |> fst


contains	:: (Node a) => Name -> a -> Bool
contains x a
 | isVar a	= x == getName a
 | hasChildren a
		= getChildren a & any (contains x)
 | otherwise	= False



data UnifSt a	= UnifSt
	{ canonForm	:: Set (String, a)
	, nonCanon	:: Set (a, a)
	, errMsg	:: Maybe String
	} 

type USt a	= State (UnifSt a)


runUnify	:: (Node a) => USt a ()
runUnify	= 
	do	done	<- gets nonCanon |> S.null
		err	<- gets errMsg |> isJust
		unless (done || err) (unifyStep >> runUnify)

unifyStep	:: (Node a) => USt a ()
unifyStep	=
	do	(t0, t1)	<- poll
		case handleUnif t0 t1 of
			Left err	
				-> modify (\st -> st {errMsg = Just err})
			Right (mSubs, newNonCanon)
				-> do	addSubs mSubs
					modify (\st -> st{nonCanon = S.union newNonCanon $ nonCanon st})

		
		
handleUnif	:: Node a => a -> a -> Either String (Maybe (String,a), Set (a, a))
handleUnif a b
 | a == b	= return (Nothing, S.empty)
 | hasChildren a && hasChildren b
	= do	let	as	= getChildren a
		let bs	= getChildren b
		unless (sameSymbol a b) $ Left "Not the same symbol"
		unless (length as == length bs) $  Left "Not the same number of children"
		retQueue $ S.fromList $ zip as bs
 | isVar a && isVar b
	= do	let na	= getName a
		let nb	= getName b
		retSubs $ if na < nb then (na, b) else (nb, a)
 | isVar a	= retSubs (getName a, b)
 | isVar b	= retSubs (getName b, a)


addSubs		:: (Node a, Ord a) => Maybe (String, a) -> USt a ()
addSubs Nothing	= return ()
addSubs (Just (k, t))
	= do	-- substitute this over all things in the state, including 'canonical' forms; these might become non canonical again by this.
		canonF			<- gets canonForm
		nonCanonF		<- gets nonCanon
		let s			= M.singleton k t
		let (dupK, canonForm')	= canonF & S.toList ||>> substitute s & L.partition ((==) k . fst)
		let dupK'		= dupK |> first (const t) 
		let nonCanonF'		= nonCanonF & S.toList |> mapBoth (substitute s)
		let nonCanon'		= S.union (S.fromList dupK') (S.fromList nonCanonF')
		st			<- get
		put $ st{ canonForm = S.insert (k, t) $ S.fromList canonForm', nonCanon = nonCanon'}




poll	:: USt a (a, a)
poll	= do	nc	<- gets nonCanon 
		modify (\st -> st{ nonCanon = deleteMin nc})
		return $ findMin nc


retSubs s	= return (Just s, S.empty)
retQueue q	= return (Nothing, q)

