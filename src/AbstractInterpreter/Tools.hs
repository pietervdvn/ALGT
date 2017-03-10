module AbstractInterpreter.Tools where

{-
This module reexports functions to check totality and stricter values
-}

import TypeSystem
import Utils.Utils
import Utils.ToString

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.Assignment
import AbstractInterpreter.FunctionAnalysis
import AbstractInterpreter.RuleAnalysis as RI
import AbstractInterpreter.MinimalTypes

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Data.Maybe

import Control.Monad
import Control.Arrow ((&&&))





checkTS	ts
	= let 	fs		= get tsFunctions ts
		analysises	= fs |> analyzeFunction' ts	:: Map Name FunctionAnalysis
		in
		[checkTotality ts analysises, checkDeadClauses ts analysises, checkStrictestTypes ts] & allRight_ & inMsg "Warning"

checkTotality	:: TypeSystem -> Map Name FunctionAnalysis -> Either String ()
checkTotality ts analysises
	= do	let syntax	= get tsSyntax ts
		let leftOvers	= analysises |> get functionLeftOvers
					& M.filter (not . S.null) & M.toList
									:: [(Name, Set Arguments)]
		leftOvers |> totalityErr & allRight_



totalityErr	:: (Name, Set Arguments) -> Either String ()
totalityErr (nm, over)
	= inMsg ("While checking the totality of function "++show nm) $
	  inMsg "Following calls will fall through" $ do
	 	let msgs	= over & S.toList |> toParsable' ", " |> inParens |> (nm++) & unlines
		Left msgs



checkDeadClauses	:: TypeSystem -> Map Name FunctionAnalysis -> Either String ()
checkDeadClauses ts analysises
	= analysises & M.toList |> checkDeadClausesFor ts & allRight_


checkDeadClausesFor	:: TypeSystem -> (Name, FunctionAnalysis) -> Either String ()
checkDeadClausesFor ts (nm, analysis)
	= do	let f	= get tsFunctions ts ! nm
		inMsg ("While checking liveability of every clause in function "++show nm) $ 
			analysis & get clauseAnalysises |> checkDeadClause (nm, f) & allRight_


checkDeadClause	:: (Name, Function) -> ClauseAnalysis -> Either String ()
checkDeadClause (nm, MFunction _ clauses) ca
	= let 	isDead	= (ca & get results & M.null)
		clause	= clauses & safeIndex ("No clause found in checkDeadClauses") (get clauseIndex ca)
		msg	= toParsable' (nm, 24::Int) clause ++ " will never match anything, as the possible arguments are already consumed"
		in
	  	when isDead $ Left msg



checkStrictestTypes	:: TypeSystem -> Either String ()
checkStrictestTypes ts
	= inMsg "While checking that the functions do have the strictest possible types" $
	  do	let stricter 	= stricterTypes ts (get tsFunctions ts) & M.toList
		let origType n	= (get tsFunctions ts ! n) & typesOf & last
		stricter |> (\(nm, tp) -> show nm++" can be typed as "++show tp++", instead of a "++show (origType nm)) |> Left & allRight_
		
		
