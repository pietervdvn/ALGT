module AbstractInterpreter.ASSubtract (subtract, subtractWith, subtractAll, subtractAllWith, subtractArg, subtractArgs) where

{- This module defines `subset of` and `subtract` for abstract set, + examples against STFL-}

import Prelude hiding (subtract)
import Utils.Utils
import Utils.ToString

import AbstractInterpreter.AbstractSet

import TypeSystem

import Data.List as L
import Data.Map (Map, member, (!))

import qualified Data.Map as M

import Debug.Trace



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


subtractArgs	:: Syntax -> Arguments -> [Arguments] -> [Arguments]
subtractArgs s args []
		= [args]
subtractArgs s args (minus:minuses)
		= do	args'	<- subtractArg s args minus
			subtractArgs s args' minuses


subtractArg	:: Syntax -> Arguments -> Arguments -> [Arguments]
subtractArg s args minus
 | length args /= length minus	= error "Length of arguments in minus don't match; this is weird"
 | otherwise	= let	pointWise	= zip args minus |> (\(e, emin) -> subtract s [e] emin)
			in	
			replacePointwise args pointWise


-------------------------------------------------------- Actual subtraction algorithm  ----------------------------------------------------


{- Given abstract sets, removes the second from this set
e.g.

a	::= "x" | "y" | b
b	::= a "~" b | b

subtract [a] b	--> "x" | "y"
subtract [a] "x"	--> "y" | b	-- note that b still can contain an 'x'

'K' represents 'known subtractions', e.g. e `subtract` (e)(→)in0 = !(e)(→)in0

-}

_subtract'	:: Syntax -> Map (TypeName, TypeName) TypeName -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract' s k e emin
 | isEveryPossible e && isEveryPossible emin
	&& (typeOf e, typeOf emin) `member` k	
			= [generateAbstractSet s "" (k ! (typeOf e, typeOf emin))]
 | e == emin		= []
 | isSubsetOf s e emin	= []
 | not (alwaysIsA' s emin e)
	-- the type of emin is no subset of the type of e; this means that emin can never be a part of e; implying we don't have to do anything
			= [e]
 | otherwise		= _subtract s k e emin




_subtract	:: Syntax -> Map (TypeName, TypeName) TypeName -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract s k e@EveryPossible{} emin	-- e is no subset of emin, emin is (at most) a possible form of emin
 	= do	e'	<- unfold s e
		_subtract' s k e' emin
_subtract s k e@(AsSeq gen choice seq) emin@(AsSeq genMin choiceMin seqMin)
 | gen == genMin && choice == choiceMin
	= do	let diffPoints	= ((get bnf s ! gen) !! choice)
					& fromSeq'
					|> isRuleCall	:: [Bool]
		-- Only where rulecalls are in the prototype, we can subtract. The rest should be the same anyway
		let subbedSeq'	=  zip3 diffPoints seq seqMin
					|> (\(isDiffPoint, e', eMin') -> if isDiffPoint then _subtract' s k e' eMin' else [e']) 
					:: [[AbstractSet]]
		seq'		<- replacePointwise seq subbedSeq'	:: [[AbstractSet]]
		return $ AsSeq gen choice seq'
 | otherwise	= [e]
_subtract s k e eMin@EveryPossible{}
		= [e]	-- Because not `e isSubsetOf eMin`, thus eMin can never change e
_subtract s k e eMin
		= [e]	-- only concrete values/seqs are left. These should be totally equal to be able to subtract... but these are already filtered by _subtract'



{-

a	::= "a" "b" | "a" "c"

{a} - {"a" "b"}	= {"a" "b" | "a" "c"} - {"a" "b"}


b	::= "x" a

{b} - {"x" ("a" "b")}
	= {"x" ("a" "b") | "x" ("a" "c")} -  {"x" ("a" "b")}
	= {"x" ("a" "c")}

{b} - {"x" ("a" "b")} - {"x" ("a" "c")}
	= {"x" ("a" "b") | "x" ("a" "c")} -  {"x" ("a" "b")} - {"x" ("a" "c")}
	= {"x" ("a" "c")} - {"x" ("a" "c")}



-}


-- True if isSub == isSuper or isSub is a strict subset of isSuper
isSubsetOf	:: Syntax -> AbstractSet -> AbstractSet -> Bool
isSubsetOf s isSub isSuper@EveryPossible{}
	= alwaysIsA s (generatorOf isSub) (typeOf isSuper)
isSubsetOf s isSub@EveryPossible{} isSuper
	-- Note that we don't allow trivial rules - so we can not have two names which turn out to be equal
	= unfold s isSub |> (\isSub' -> isSubsetOf s isSub' isSuper) & and
isSubsetOf s isSub@(AsSeq genSub _ seqSub) concreteSuper@(AsSeq genSuper _ seqSuper)
 | not $ alwaysIsA s genSub genSuper	-- The same sequence can not occur in multiple types, as that triggers an error message. This implies that we can use the generating types
	= False
 | otherwise
	= zip seqSub seqSuper |> uncurry (isSubsetOf s) & and
isSubsetOf _ concreteSub concreteSuper
	-- At this point, only concrete values are left. (Literals, Identifiers and Int's, or perhaps a single AsSeq)
	= sameStructure concreteSub concreteSuper
		
{-

a	::= "a"

isSubsetOf a "a" = True


-}
