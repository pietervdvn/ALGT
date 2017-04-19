module AbstractInterpreter.ASSubtract (subtract, subtractWith, subtractAll, subtractAllWith, subtractArg, subtractArgs, _subtractAllWith, _subtractWith) where

{- This module defines `subset of` and `subtract` for abstract set, + examples against STFL-}

import Prelude hiding (subtract)
import Utils.Utils
import Utils.ToString

import AbstractInterpreter.AbstractSet

import TypeSystem

import Data.List as L
import Data.Map (Map, member, (!))

import qualified Data.Map as M

import Control.Monad


subtract	:: Syntax -> [AbstractSet] -> AbstractSet -> [AbstractSet]
subtract s	= subtractWith s M.empty

subtractWith	:: Syntax -> Map (TypeName, TypeName) [AbstractSet] -> [AbstractSet] -> AbstractSet -> [AbstractSet]
subtractWith
	= _subtractWith False

_subtractWith	:: Bool -> Syntax -> Map (TypeName, TypeName) [AbstractSet] -> [AbstractSet] -> AbstractSet -> [AbstractSet]
_subtractWith debug syntax known ass minus
	= nub $ do	as		<- ass
			let subbed = _subtract' debug syntax known as minus
			subbed


subtractAll	:: Syntax -> [AbstractSet] -> [AbstractSet] -> [AbstractSet]
subtractAll syntax 
		= subtractAllWith syntax M.empty

subtractAllWith	:: Syntax -> Map (TypeName, TypeName) [AbstractSet] -> [AbstractSet] -> [AbstractSet] -> [AbstractSet]
subtractAllWith = _subtractAllWith False

_subtractAllWith	:: Bool -> Syntax -> Map (TypeName, TypeName) [AbstractSet] -> [AbstractSet] -> [AbstractSet] -> [AbstractSet]
_subtractAllWith debug syntax known ass []
		= ass
_subtractAllWith debug syntax known ass (minus:minuses)
		= let	ass'	= _subtractWith debug syntax known ass minus
			in
			_subtractAllWith debug syntax known ass' minuses



subtractArgs	:: Syntax -> Arguments -> [Arguments] -> [Arguments]
subtractArgs s args []
		= [args]
subtractArgs s args (minus:minuses)
		= do	args'	<- subtractArg s args minus
			subtractArgs s args' minuses


subtractArg	:: Syntax -> Arguments -> Arguments -> [Arguments]
subtractArg s args minus
 | length args /= length minus	= error "Length of arguments in minus don't match; this is weird"
 | otherwise	= let	pointWise	= zip args minus |> (\(eArg, emin) -> subtract s [eArg] emin)	:: [[AbstractSet]]
			in
			if length pointWise /= length args then error "HUH?" else
				replacePointwise args pointWise



-------------------------------------------------------- Actual subtraction algorithm  ----------------------------------------------------


{- Given abstract sets, removes the second from this setXxX
e.g.

a	::= "x" | "y" | b
b	::= a "~" b | b

subtract [a] b	--> "x" | "y"
subtract [a] "x"	--> "y" | b	-- note that b still can contain an 'x'

'K' represents 'known subtractions', e.g. e `subtract` (e)(→)in0 = !(e)(→)in0

-}

trace' debug msg e emin
	= let	msg'	= ">> "++msg++" with:\n"
				++"   e = "++toParsable e++"\t: "++typeOf e++"\n"
				++"   emin = "++toParsable emin++"\t: "++typeOf emin
		in if debug then {- trace msg'--} error $ "Tracing not enabled; anyway: "++msg'
			else id


_subtract'	:: Bool -> Syntax -> Map (TypeName, TypeName) [AbstractSet] -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract' debug s k e emin
 | isEveryPossible e && isEveryPossible emin
	&& (typeOf e, typeOf emin) `member` k	
			= let 	result	= k ! (typeOf e, typeOf emin) in
				trace' debug ("In the special known set, becoming: "++toParsable' " | " result) e emin
					result
 | e == emin		= trace' debug "Shortcut equality" e emin []
 | isSubsetOf s e emin
		= trace' debug "Shortcut isSubsetOf" e emin []
 | otherwise		
	= let	{-debug'	= debug || 
			(toParsable e == "(\"If\" (\"(\" e \")\") \"Then\" e \"Else\" e)" && 
				toParsable emin == "(\"If\" (e)(→)in0 \"Then\" e \"Else\" e)")-}
		debug'	= debug
		subbed	= _subtract debug' s k e emin 
		in
		trace' debug' ("> Subtraction gave: \n | " ++ toParsable' "\n | " subbed) e emin subbed


_subtract	:: Bool -> Syntax -> Map (TypeName, TypeName) [AbstractSet] -> AbstractSet -> AbstractSet -> [AbstractSet]
_subtract debug s k e@EveryPossible{} emin	-- e is no subset of emin, emin is (at most) a possible form of emin
 	= trace' debug "Case 0 left everyPossible" e emin $
	  let	unfolded	= unfold s e
		subbed		= [_subtract' debug s k e' emin | e' <- unfolded] & concat
		-- if nothing has changed, just return [e]
		in if unfolded == subbed then [e] else subbed
_subtract debug s k e@(AsSeq gen choice seq) emin@(AsSeq genMin choiceMin seqMin)
	-- If the bnf-choice generating it is the same, then we roll! We should lookup the protoypes, for the case of an identical choice in different rules
 | prototypesMatch' s (gen, choice) (genMin, choiceMin)
	= do	let diffPoints	= getPrototype s (gen, choice)	-- Don't use genMin as prototype here, it might have more literals
					|> isRuleCall	:: [Bool]
		-- Only where rulecalls are in the prototype, we can subtract. The rest should be the same anyway
		let subbedSeq	=  zip3 diffPoints seq seqMin
					|> (\(isDiffPoint, e', eMin') -> if isDiffPoint then _subtract' debug s k e' eMin' else []) 
					:: [[AbstractSet]]
		unless (length subbedSeq == length seq) $ error $ "Bug: length of subbedSeq /= length of seq\n"
			 ++ unlines	[ "e: "++show e
					, "emin: "++show emin
					, "e-proto: "++show (getPrototype s (gen, choice))
					, "emin-proto: "++show (getPrototype s (genMin, choiceMin))

					, "seq: "++show seq
					, "seqMin: "++show seqMin
					, "subbedSeq: "++ show subbedSeq
					, "diffpoints: "++show diffPoints
					, ""
					]

		seq'		<- replacePointwise seq subbedSeq	:: [[AbstractSet]]
		trace' debug ("Case 1, 2 matching seqs, diffpoints are "++show diffPoints) e emin $
	  		return $ AsSeq gen choice seq'
 | otherwise	= trace' debug ("Case 1.1: seqs no match ("++show (getPrototype s (gen, choice)) ++" /= "++show (getPrototype s (genMin, choiceMin))++ ")")
				e emin [e]
_subtract debug s k e emin@EveryPossible{}
 | (typeOf e, typeOf emin) `member` k
		= if head (typeOf e) == '!' then trace' True "FAILED ASSERTION" e emin $ error "ASSERTION FAILED: No negative form should be expanded"
			else trace' debug "Case 2.0: right everyPossible no match special case" e emin $ _subtractAllWith debug s k [e] (unfold s emin)

 | otherwise
	-- Because not `e isSubsetOf eMin`, thus eMin can never change e
		= trace' debug "Case 2: right EveryPossible no match" e emin [e]
_subtract debug s k e emin
		= trace' debug "Case 3: leftovers" e emin [e]	-- only concrete values/seqs are left. These should be totally equal to be able to subtract... but these are already filtered by _subtract'


prototypesMatch'	:: Syntax -> (TypeName, Int) -> (TypeName, Int) -> Bool
prototypesMatch' s a b
	= prototypesMatch (getPrototype s a) (getPrototype s b)


prototypesMatch	:: [BNF] -> [BNF] -> Bool
prototypesMatch [] []	= True
prototypesMatch [] _	= False
prototypesMatch _  []	= False
prototypesMatch (BNFRuleCall "":as) (_:bs)
		= prototypesMatch as bs
prototypesMatch (_:as) (BNFRuleCall "":bs)
		= prototypesMatch as bs
prototypesMatch (a:as) (b:bs)
		= a == b && prototypesMatch as bs

	

getPrototype	:: Syntax -> (TypeName, Int) -> [BNF]
getPrototype s (tn, choice)
	= let 	bnfseq	= fromSeq' $ (get bnf s ! tn) & safeIndex ("getPrototype: no prototype defined for type "++tn) choice in
		bnfseq & filter (not . isDissapearing) |> (\bnf -> if isRuleCall bnf then BNFRuleCall "" else bnf)

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
 | length seqSub /= length seqSuper
	= False
 | otherwise
	= zip seqSub seqSuper |> uncurry (isSubsetOf s) & and
isSubsetOf _ concreteSub concreteSuper
	-- At this point, only concrete values are left. (Literals, Identifiers and Int's, or perhaps a single AsSeq)
	= sameStructure concreteSub concreteSuper
		



