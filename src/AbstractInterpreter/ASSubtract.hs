module AbstractInterpreter.ASSubtract (subtract, subtractWith, subtractAll, subtractAllWith, subtractArg, subtractArgs) where

{- This module defines `subset of` and `subtract` for abstract set, + examples against STFL-}

import Prelude hiding (subtract)
import Utils.Utils

import AbstractInterpreter.AbstractSet

import TypeSystem

import Data.List as L
import Data.Map (Map)
import qualified Data.Map as M

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

-}

_subtract'	:: Syntax -> Map (TypeName, TypeName) TypeName -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract' s k e emin
 | e == emin		= []
 | isSubsetOf s e emin	= []



-- True if isSub == isSuper or isSub is a strict subset of isSuper
isSubsetOf	:: Syntax -> AbstractSet -> AbstractSet -> Bool
isSubsetOf s isSub isSuper@EveryPossible{}
	= alwaysIsA s (generatorOf isSub) (typeOf isSuper)
isSubsetOf s isSub@EveryPossible{} isSuper
	-- Note that we don't allow trivial rules - so we can not have two names which turn out to be equal
	= unfold s isSub |> (\isSub' -> isSubsetOf s isSub' isSuper) & and
isSubsetOf s isSub@(AsSeq genSub seqSub) concreteSuper@(AsSeq genSuper seqSuper)
 | not $ alwaysIsA s genSub genSuper	-- The same sequence can not occur in multiple types, as that triggers an error message. This implies that we can use the generating types
	= False
 | otherwise
	= zip seqSub seqSuper |> uncurry (isSubsetOf s) & and
isSubsetOf _ concreteSub concreteSuper
	-- At this point, only concrete values are left. (Literals, Identifiers and Int's)
	= sameStructure concreteSub concreteSuper
		
{-

a	::= "a"

isSubsetOf a "a" = True


-}
