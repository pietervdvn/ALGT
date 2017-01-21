{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module AbstractInterpreter.Data where


{-
This module defines helper data structures used throughout abstract interpretation.
Think assignments, analysis data, ...
-}

import TypeSystem
import Utils.Utils
import Utils.ToString

import Utils.Unification

import AbstractInterpreter.AbstractSet as As

import Data.List
import Data.Map (Map, (!), keys, intersection)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe

import Lens.Micro hiding ((&))
import Lens.Micro.TH

import Control.Arrow ((&&&))

-------------------- ASSIGNMENT ----------------------------------

type Arguments	= [AbstractSet]



subtractArgs	:: Syntax -> Arguments -> [Arguments] -> [Arguments]
subtractArgs s args []
		= [args]
subtractArgs s args (minus:minuses)
		= do	args'	<- subtractArg s args minus
			subtractArgs s args' minuses


subtractArg	:: Syntax -> Arguments -> Arguments -> [Arguments]
subtractArg s args minus
 | length args /= length minus	= error "Length of arguments in minus don't match; this is weird"
 | otherwise	= let	pointWise	= zip args minus |> uncurry (_subtract s)  in	
			replacePointwise args pointWise

type Assignments	= Map Name (AbstractSet, Maybe Path)

instance ToString' Int Assignments where
	toParsable'	= _toString toParsable
	toCoParsable'	= _toString toCoParsable
	debug'		= _toString debug
	show' _		= show

_toString	:: (AbstractSet -> String) -> Int -> Assignments -> String
_toString showAs i dict
	= dict & M.toList |> (\(nm, (as, mPath)) -> padR i ' ' nm ++ " --> " ++ showAs as) & unlines



findAssignment		:: Name -> Assignments -> Maybe (AbstractSet, Maybe [Int])
findAssignment		= M.lookup


mergeAssgnss	:: Syntax -> [Assignments] -> [Assignments]
mergeAssgnss _ []
		= [M.empty]
mergeAssgnss syntax (a:as)	
		= do	tail	<- mergeAssgnss syntax as
			mergeAssgns syntax a tail


mergeAssgns	:: Syntax -> Assignments -> Assignments -> [Assignments]
mergeAssgns syntax assgs0 assgs1
	= do	let commonAssgns	= assgs0 `intersection` assgs1 & keys
		mergedAssgns		<- commonAssgns |> ((!) assgs0 &&& (!) assgs1)
						& filter (uncurry (/=))
						|> uncurry (mergeAssgn syntax)
						& allRight & either (const []) (:[])
		let mergedAssgns'	= zip commonAssgns mergedAssgns
		let assgs'	= M.union (M.fromList mergedAssgns') $ M.union assgs0 assgs1
		return assgs'

mergeAssgn	:: Syntax -> (AbstractSet, Maybe [Int]) -> (AbstractSet, Maybe [Int]) -> Either String (AbstractSet, Maybe [Int])
mergeAssgn syntax (a, pathA) (b, pathB)
 | pathA /= pathB	= Left "Context paths don't match"
 | otherwise
	= inMsg ("While trying to unify "++toParsable a++" and "++toParsable b) $
		do	
			substitution	<- unifySub (smallestOf syntax) a b
			return (substitute substitution a, pathA)

-- we add a special rule for unification, that these variables *can* be matched with each other, despite having a different symbol
smallestOf	:: Syntax -> AbstractSet -> AbstractSet -> Maybe AbstractSet
smallestOf syntax a b		
 | alwaysIsA' syntax a b	= Just a
 | alwaysIsA' syntax b a	= Just b
 | otherwise			= Nothing 





evalExpr	:: Map Name TypeName -> Assignments -> Expression -> AbstractSet
evalExpr _ assgns (MParseTree (MLiteral mi token))
		= ConcreteLiteral mi token
evalExpr _ assgns (MParseTree (MIdentifier mi nm))
		= ConcreteLiteral mi nm
evalExpr _ assgns (MParseTree (MInt mi i))
		= ConcreteLiteral mi (show i)
evalExpr _ assgns (MParseTree seq@(PtSeq mi _))
		= ConcreteLiteral mi (show seq)
evalExpr _ assgns (MVar _ n)
		= fromMaybe (error $ "Unknown variable: "++show n) (findAssignment n assgns) & fst
evalExpr f _ (MCall defType n builtin _)
		= let tp = if builtin then defType else M.findWithDefault (error $ "AbstractInterpreter.Data: evalExpr: function not found: "++ n) n f in
			EveryPossible (tp, -1) " (Function call - ID not retrievable)" tp
evalExpr f assgns (MSeq mi exprs)
		= exprs |> evalExpr f assgns & AsSeq mi
evalExpr f assgns (MAscription t e)
		= let	e'	= evalExpr f assgns e in
			if typeOf e' == t then e' else error "Ascription failed"
evalExpr f assgns (MEvalContext _ nm hole)	
		= let	(ctx, Just path)	= fromMaybe (error $ "Unknwown variable"++show nm) $ findAssignment nm assgns
			hole'			= evalExpr f assgns hole
			in
			replaceAS ctx path hole'


------------------------------------- ANALYSIS ------------------------------------------------------------


data Analysis	= Analysis 
		{ _results	:: Map Int (Map Arguments AbstractSet)	-- clausenr, arguments --> value
		, _leftOvers	:: Map Int (Set Arguments, Bool)	-- Unhandled cases before starting pattern i; -1 indicates fallthrough. The boolean represents 'Uses equality and might not match, despite having the correct form'
		} deriving (Show, Eq)
makeLenses ''Analysis

emptyAnalysis	= Analysis M.empty M.empty

addResult	:: Int -> [(Arguments, AbstractSet)] -> Analysis -> Analysis
addResult i res
	= over results (M.insert i $ M.fromList res)


addLeftover	:: Int -> Bool -> [Arguments] -> Analysis -> Analysis
addLeftover i noEquality argss
	= over leftOvers (M.insert i (S.fromList argss, not noEquality) )

-- Get what will fall through all the patterns
leftOver	:: Analysis -> Set Arguments
leftOver analysis
		= analysis & get leftOvers & (! (-1)) & fst

instance ToString Analysis where
	toParsable (Analysis results lo)	
			= results |> M.toList & M.mapWithKey (\i ls -> ls |> showRes i ) 
				& M.elems & concat
				& unlines
				++ showNonMatching lo

showRes	:: Int -> (Arguments, AbstractSet) -> String
showRes	i (args, res)
	= padR 30 ' ' (show i ++ "   "++toCoParsable' " , " args) ++
		"\t--> "++
		 padR 15 ' ' (toCoParsable res) ++ " : " ++ show (typeOf res)


showNonMatching	:: Map Int (Set Arguments, Bool) -> String
showNonMatching args
	= let	indices	= args & M.keys & sort & tail	-- drop the -1 'leftovers'
		showArgs args	= args & S.toList |> toParsable' ", " & unlines
		noUsageNote b	= if not b then "" else
					"\nNote: This clause uses equality in the patterns and might not match. No arguments are thus used in this abstract interpretation."
		heading (i, b)	= if i == -1 then "Falltrough-cases:" else 
					"Arguments which might reach clause "++show i++":" ++ noUsageNote b
		showSet i (args, passThrough)
				= if S.null args then (if i == (-1) then "No falltrough possible" else "") 
					else heading (i, passThrough) ++ "\n" ++indent (showArgs args)
		args'		= args & M.mapWithKey showSet in
		(indices ++ [-1]) |> (args' !) & filter (not . null) & unlines
		
		



