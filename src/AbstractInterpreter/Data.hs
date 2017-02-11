{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module AbstractInterpreter.Data where


{-
This module defines helper data structures used throughout abstract interpretation.
Think assignments, analysis data, ...
-}

import Prelude hiding (subtract)

import TypeSystem
import Utils.Utils
import Utils.ToString

import Utils.Unification

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.ASSubtract as As


import Data.List
import Data.Map (Map, (!), keys, intersection)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe

import Lens.Micro hiding ((&))
import Lens.Micro.TH

import Control.Arrow ((&&&))
import Control.Monad

-------------------- ASSIGNMENT ----------------------------------



type Assignments	= VariableAssignmentsA AbstractSet



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


evalExpr f assgns e
	= inMsg ("While abstractly evaluating the expression "++toParsable e) $ _evalExpr f assgns e


_evalExpr	:: Map Name TypeName -> Assignments -> Expression -> Either String AbstractSet
_evalExpr _ assgns (MParseTree (MLiteral mi token))
		= return $ ConcreteLiteral (fst mi) token
_evalExpr _ assgns (MParseTree (MIdentifier mi nm))
		= return $ ConcreteLiteral (fst mi) nm
_evalExpr _ assgns (MParseTree (MInt mi i))
		= return $ ConcreteLiteral (fst mi) (show i)
_evalExpr _ assgns (MParseTree seq@(PtSeq mi _))
		= return $ ConcreteLiteral (fst mi) (show seq)
_evalExpr _ assgns (MVar _ n)
		= checkExists n assgns ("Unknown variable: "++show n) |> fst
_evalExpr f _ (MCall defType n builtin _)
		= do	tp <- if builtin then return defType 
				else checkExists n f $ "AbstractInterpreter.Data: _evalExpr: function not found: "++ n
			return $ EveryPossible tp " (Function call - ID not retrievable)" tp
_evalExpr f assgns (MSeq mi exprs)
		= exprs |+> _evalExpr f assgns |> AsSeq (fst mi)
_evalExpr f assgns (MAscription t e)
		= do	e'	<- _evalExpr f assgns e
			unless (typeOf e' == t) $ Left $ "Ascription of "++toParsable e++" as "++t ++ " failed"
			return e'
_evalExpr f assgns (MEvalContext _ nm hole)	
		= do	(ctx, Just path)	<- checkExists nm assgns $ "Unknwown variable"++show nm
			hole'			<- _evalExpr f assgns hole
			return $ replaceAS ctx path hole'


