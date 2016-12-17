module AbstractInterpreter.AbstractPatternMatcher where

{-
This module defines 
-}

import Utils.Utils
import TypeSystem

import AbstractInterpreter.AbstractParseTree

import qualified Data.Map as M
import Data.Map (Map, (!), intersection, union, empty, singleton, keys)

import Debug.Trace

data Assumption	a 
	= HasValue a
	| SameValueAs Name
	| Not (Assumption a)
	deriving (Show)
type Assumptions a	= Map Name (Assumption a)

type Assignments	= (Map Name AbstractSet', Assumptions Int, Assumptions String)


patternMatch' r pat as
	= trace ("Trying to match "++show pat++" ~ "++show as) $ patternMatch r pat as


patternMatch	:: Syntax -> Expression -> AbstractSet' -> [Assignments]
patternMatch _ (MCall _ "error" True _) _	
	= error $ "Using an error in a pattern match is not allowed. Well, you've got your error now anyway. Happy now, you punk?"
patternMatch _ (MCall _ nm _ _) _	
	= error $ "Using a function call in a pattern is not allowed"


patternMatch r (MVar _ v) as
		= assign v as 
patternMatch r (MParseTree (MLiteral _ s1)) (ConcreteLiteral _ s2)
	| s1 == s2		= returnE
	| otherwise		= returnF $ "Not the same literal: "++s1++ " /= " ++ s2
patternMatch r (MParseTree (MInt _ s1)) (ConcreteInt _ n)
	= assumeInt n s1
patternMatch r (MParseTree (MIdentifier _ s1)) (ConcreteIdentifier _ n)
	= assumeStr n s1
patternMatch r (MParseTree (PtSeq mi pts)) pt
	= patternMatch r (MSeq mi (pts |> MParseTree)) pt
patternMatch r s1@(MSeq _ seq1) s2@(AsSeq _ seq2)
 | length seq1 /= length seq2	= returnF $ "Sequence lengths are not the same: "++show s1 ++ " <~ "++show s2
 | otherwise			
	= do	somePossibleMatch	<- zip seq1 seq2 |+> uncurry (patternMatch r)
		mergeAssgnss somePossibleMatch

patternMatch r (MAscription as expr') expr
 | alwaysIsA r (typeOf expr) as	
	= patternMatch r expr' expr
 | otherwise	
	= returnF $ show expr ++" is not a "++show as

patternMatch r (MEvalContext tp name hole) value@(AsSeq _ _)
	= error $ "TODO/HELP how do I work with evaluation context in abstract interpretation?"

patternMatch r pat as@(EveryPossible _ _ _)
	= do	choice		<- unfold' r as
		patternMatch r pat choice
patternMatch _ pat as
	= trace ("Failing match "++show pat++" ~ "++show as) []


mergeAssgnss	:: [Assignments] -> [Assignments]
mergeAssgnss []	= returnE
mergeAssgnss (a:as)	
		= do	tail	<- mergeAssgnss as
			mergeAssgns a tail


mergeAssgns	:: Assignments -> Assignments -> [Assignments]
mergeAssgns (assgs0, assumpsInt0, assumpsStr0) (assgs1, assumpsInt1, assumpsStr1)
	= do	let commonAssgns	= assgs0 `intersection` assgs1 & keys
		if not $ null commonAssgns then error $ "TODO: implement unification to fix merge assgns" else cont
		assumpsInt	<- mergeAssumptions assumpsInt0 assumpsInt1
		assumpsStr	<- mergeAssumptions assumpsStr0 assumpsStr1
		return ((assgs0 `union` assgs1), assumpsInt, assumpsStr)
		



mergeAssumptions	:: Show a => Map Name (Assumption a) -> Map Name (Assumption a) -> [Map Name (Assumption a)]
mergeAssumptions ass0 ass1
	= do	let common	= ass0 `intersection` ass1
		assert error (M.null common) $ "TODO: merge assumptions: "++show common
		return (ass0 `union` ass1)








returnE	= [(empty, empty, empty) ]

-- error messages. Might actually be used somewhere in the future
returnF	str	= []

assign n v	= [(singleton n v, empty, empty)]
assumeInt n i	= [(empty, singleton n $ HasValue i, empty)]
assumeStr n s	= [(empty, empty, singleton n $ HasValue s)]

