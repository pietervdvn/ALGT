module AbstractInterpreter.PatternMatcher where

{-
This module defines pattern matching over abstract sets
-}

import TypeSystem
import Utils.Utils
import Utils.ToString
import Utils.Unification

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.Assignment

import qualified Data.Map as M
import Data.Map (Map, (!), intersection, union, empty, singleton, keys)
import Data.List as L

import Control.Arrow ((&&&))



patternMatch	:: Syntax -> Expression -> AbstractSet -> [Assignments]
patternMatch _ (MCall _ "error" True _) _	
	= error "Using an error in a pattern match is not allowed. Well, you've got your error now anyway. Happy now, you punk?"
patternMatch _ (MCall _ nm _ _) _	
	= error "Using a function call in a pattern is not allowed"


patternMatch r (MVar _ v) as
		= assign v as 
patternMatch r (MParseTree (MLiteral _ s1)) (ConcreteLiteral _ s2)
	| s1 == s2		= returnE
	| otherwise		= returnF $ "Not the same literal: "++s1++ " /= " ++ s2
patternMatch r (MParseTree (MInt _ s1)) (ConcreteInt _ n)
	= returnE
patternMatch r (MParseTree (MIdentifier _ s1)) (ConcreteIdentifier _ n)
	= returnE
patternMatch r (MParseTree (PtSeq mi pts)) pt
	= patternMatch r (MSeq mi (pts |> MParseTree)) pt
patternMatch r s1@(MSeq _ seq1) s2@(AsSeq _ seq2)
 | length seq1 /= length seq2	= returnF $ "Sequence lengths are not the same: "++toParsable s1 ++ " /= "++toParsable s2
 | otherwise			
	= do	somePossibleMatch	<- zip seq1 seq2 |+> uncurry (patternMatch r)
		mergeAssgnss r somePossibleMatch

patternMatch r ascr@(MAscription as expr') expr
 | alwaysIsA r (typeOf expr) as	
	= patternMatch r expr' expr
 | mightContainA r as (typeOf expr)
	= do	possExpr	<- unfold r expr & filter isEveryPossible
		patternMatch r ascr possExpr
 | otherwise	
	= returnF $ "Failed ascription: "++show expr ++" is not a "++show as

patternMatch syntax evalCtx@(MEvalContext tp name hole) value
	= do	-- first of all, the current expression should (be able to) contain the sub expression
		-- this sub-expression (hole) is always a complete expression; thus we search a 'EveryPossible' within our asseq. (The eventually unfolded input)
		let neededType		= typeOf hole
		value'			<- unfoldFull syntax value	-- contexts only work within another expression, so we already unfold. (Searchmatches might unfold more)
		(match, pths)		<- searchMatches syntax neededType [] value'
		pth			<- pths

		-- we calculate assignments induced by the hole-expression...
		let replacedExpr	= getAsAt match pth	-- the subexpression that is thrown away
		let holeName		= getName replacedExpr
		let holeType		= typeOf replacedExpr
		holeAssignment		<- patternMatch syntax hole (generateAbstractSet syntax (holeName++"$") holeType)
		-- and calculate the form of the hole
		let hole'		= evalExpr M.empty {-functions are not possible in the patterns anyway-} holeAssignment hole
						& either error id
		let match'		= replaceAS match pth hole'
		let ctxAssignment	= M.singleton name (match', Just pth)
		mergeAssgns syntax holeAssignment ctxAssignment
patternMatch r pat as@EveryPossible{}
	= do	choice		<- unfold r as
		patternMatch r pat choice
patternMatch _ pat as
	= []



{-

Consider
x ::= a b | c b | d
b ::= "B" z

Given x["B" z], we want to search a possible unfolding of x, containing the possible sets, e.g.

[(a b, [1]), (c b, [1]) 

-}
searchMatches	:: Syntax -> TypeName -> [TypeName] -> AbstractSet -> [(AbstractSet, [Path])]
searchMatches syntax neededType noExpand as@(EveryPossible _ _ t)
 | alwaysIsA syntax t neededType
			= return (as, [[]])
 | t `elem` noExpand	= []
 | otherwise		
	= do	as'	<- unfold syntax as
		searchMatches syntax neededType (t:noExpand) as'
searchMatches syntax neededType noExpand (AsSeq mi seq)
	= do	seq'	<- mapi seq |> (\(i, as) -> 
				if isEveryPossible as && (typeOf as & mightContainA syntax neededType) then 
					searchMatches syntax neededType noExpand as ||>> (|> (i:))
					 else [(as, [])])
				& allCombinations	:: [[ (AbstractSet, [Path]) ]]
		let (seq'', pthss)	= unzip seq'	:: ([AbstractSet], [ [Path] ])
		return (AsSeq mi seq'', concat pthss)
searchMatches _ _ _ _	= []

		





------------------------ Tools ---------------------------------------------

returnE	= [empty]
assign n v	= [singleton n (v, Nothing)]
-- error messages. Might actually be used somewhere in the future
returnF	str	= []


