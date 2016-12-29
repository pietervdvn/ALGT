module AbstractInterpreter.AbstractPatternMatcher where

{-
This module defines 
-}

import TypeSystem
import Utils.Utils
import Utils.TypeSystemToString
import Utils.ToString
import Utils.Unification

import AbstractInterpreter.AbstractParseTree

import qualified Data.Map as M
import Data.Map (Map, (!), intersection, union, empty, singleton, keys)
import Data.List as L

import Control.Arrow ((&&&))

data Assumption	a 
	= HasValue a
	| SameValueAs Name
	| Not (Assumption a)
	deriving (Show)
type Assumptions a	= Map Name (Assumption a)

type Path		= [Int]
type Assignments	= (Map Name (AbstractSet', Maybe Path), Assumptions Int, Assumptions String)


findAssignment		:: Name -> Assignments -> Maybe (AbstractSet', Maybe [Int])
findAssignment nm (assgns, _, _)
			= M.lookup nm assgns



patternMatch	:: Syntax -> Expression -> AbstractSet' -> [Assignments]
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
	= assumeInt n s1
patternMatch r (MParseTree (MIdentifier _ s1)) (ConcreteIdentifier _ n)
	= assumeStr n s1
patternMatch r (MParseTree (PtSeq mi pts)) pt
	= patternMatch r (MSeq mi (pts |> MParseTree)) pt
patternMatch r s1@(MSeq _ seq1) s2@(AsSeq _ seq2)
 | length seq1 /= length seq2	= returnF $ "Sequence lengths are not the same: "++toParsable s1 ++ " /= "++show s2
 | otherwise			
	= do	somePossibleMatch	<- zip seq1 seq2 |+> uncurry (patternMatch r)
		mergeAssgnss r somePossibleMatch

patternMatch r ascr@(MAscription as expr') expr
 | alwaysIsA r (typeOf expr) as	
	= patternMatch r expr' expr
 | mightContainA r as (typeOf expr)
	= do	possExpr	<- unfold' r expr & filter isEveryPossible
		patternMatch r ascr possExpr
 | otherwise	
	= error $ show expr ++" is not a "++show as

patternMatch syntax evalCtx@(MEvalContext tp name hole) value
	= do	-- first of all, the current expression should (be able to) contain the sub expression
		-- this sub-expression (hole) is always a complete expression; thus we search a 'EveryPossible' within our asseq. (The eventually unfolded input)
		let neededType		= typeOf hole
		value'			<- unfold' syntax value	-- contexts only work within another expression, so we unfold once
		(match, pths)		<- searchMatches syntax neededType [] value'
		pth			<- pths
		let match'		= replaceAS match pth (asAS hole)
		holeAssignment		<- patternMatch syntax hole (generateAbstractSet syntax "" neededType & snd)
		let ctxAssignment	= (M.singleton name (match', Just pth), M.empty, M.empty )
		mergeAssgns syntax holeAssignment ctxAssignment
patternMatch r pat as@EveryPossible{}
	= do	choice		<- unfold' r as
		patternMatch r pat choice
patternMatch _ pat as
	= []




searchMatches	:: Syntax -> TypeName -> [TypeName] -> AbstractSet' -> [(AbstractSet', [Path])]
searchMatches syntax neededType noExpand as@(EveryPossible _ _ t)
 | t == neededType	= return (as, [[]])
 | t `elem` noExpand	= []
 | otherwise		
	= do	as'	<- unfold' syntax as
		searchMatches syntax neededType (t:noExpand) as'
searchMatches syntax neededType noExpand (AsSeq mi seq)
	= do	seq'	<- seq |> (\as -> if isEveryPossible as then searchMatches syntax neededType noExpand as else [(as, [])])
				& allCombinations	:: [[ (AbstractSet', [Path]) ]]
		let (seq'', pthss)	= unzip seq'	:: ([AbstractSet'], [ [Path] ])
		let pths	= mapi pthss >>= (\(i, pths) -> pths |> (i:))
		return (AsSeq mi seq'', pths)
searchMatches _ _ _ _	= []

		


mergeAssgnss	:: Syntax -> [Assignments] -> [Assignments]
mergeAssgnss _ []
		= returnE
mergeAssgnss syntax (a:as)	
		= do	tail	<- mergeAssgnss syntax as
			mergeAssgns syntax a tail


mergeAssgns	:: Syntax -> Assignments -> Assignments -> [Assignments]
mergeAssgns syntax (assgs0, assumpsInt0, assumpsStr0) (assgs1, assumpsInt1, assumpsStr1)
	= do	let commonAssgns	= assgs0 `intersection` assgs1 & keys
		mergedAssgns		<- commonAssgns |> ((!) assgs0 &&& (!) assgs1)
						& filter (uncurry (/=))
						|> uncurry (mergeAssgn syntax)
						& allRight & either (const []) (:[])
		let mergedAssgns'	= zip commonAssgns mergedAssgns
		let assgs'	= M.union (M.fromList mergedAssgns') $ M.union assgs0 assgs1
		assumpsInt	<- mergeAssumptions assumpsInt0 assumpsInt1
		assumpsStr	<- mergeAssumptions assumpsStr0 assumpsStr1
		return (assgs', assumpsInt, assumpsStr)

mergeAssgn	:: Syntax -> (AbstractSet', Maybe [Int]) -> (AbstractSet', Maybe [Int]) -> Either String (AbstractSet', Maybe [Int])
mergeAssgn syntax (a, pathA) (b, pathB)
 | pathA /= pathB	= Left "Context paths don't match"
 | otherwise
	= inMsg ("While trying to unify "++show a++" and "++show b) $
		do	
			substitution	<- unifySub (smallestOf syntax) a b
			return $ (substitute substitution a, pathA)

-- we add a special rule, that these variables *can* be matched with each other, despite having a different symbol
smallestOf	:: Syntax -> AbstractSet' -> AbstractSet' -> Maybe AbstractSet'
smallestOf syntax a b		
 | alwaysIsA' syntax a b	= Just a
 | alwaysIsA' syntax b a	= Just b
 | otherwise			= Nothing 


mergeAssumptions	:: Show a => Map Name (Assumption a) -> Map Name (Assumption a) -> [Map Name (Assumption a)]
mergeAssumptions ass0 ass1
	= do	let common	= ass0 `intersection` ass1
		assert error (M.null common) $ "TODO: merge assumptions: "++show common
		return (ass0 `M.union` ass1)








returnE	= [(empty, empty, empty) ]

-- error messages. Might actually be used somewhere in the future
returnF	str	= []

assign n v	= [(singleton n (v, Nothing), empty, empty)]
assumeInt n i	= [(empty, singleton n $ HasValue i, empty)]
assumeStr n s	= [(empty, empty, singleton n $ HasValue s)]

