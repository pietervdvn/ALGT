module AbstractInterpreter.Tools where

{-
This module reexports functions to check totality and stricter values
-}

import TypeSystem
import Utils.Utils
import Utils.ToString

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.Data
import AbstractInterpreter.FunctionInterpreter
import AbstractInterpreter.MinimalTypes

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad

import Control.Arrow ((&&&))

interpretFunction	:: TypeSystem -> Name -> Analysis
interpretFunction ts n
	= interpretFunction' (get tsSyntax ts) (get tsFunctions ts |> typesOf |> last)	(get tsFunctions ts ! n)

checkTS	ts
	=  [checkTotality ts, checkStrictestTypes ts] & allRight_ & inMsg "Warning"

checkTotality	:: TypeSystem -> Either String ()
checkTotality ts
	= do	let fs		= get tsFunctions ts
		let fsTps	= fs |> typesOf |> last
		let syntax	= get tsSyntax ts
		let analysises	= fs |> interpretFunction' syntax fsTps	:: Map Name Analysis
		let leftOvers	= analysises |> leftOver & M.filter (not . null) & M.toList
		leftOvers |> totalityErr & allRight_
		analysises & M.toList |> checkDeadClauses ts & allRight_



totalityErr	:: (Name, Set Arguments) -> Either String ()
totalityErr (nm, over)
	= inMsg ("While checking the totality of function "++show nm) $
	  inMsg "Following calls will fall through" $ do
	 	let msgs	= over & S.toList |> toParsable' ", " |> inParens |> (nm++) & unlines
		Left msgs
		

checkDeadClauses	:: TypeSystem -> (Name, Analysis) -> Either String ()
checkDeadClauses ts (nm, analysis)
	= inMsg ("While checking liveability of every clause in function "++show nm) $ 
	  inMsg "Some clauses will never match:" $ do
		let deadClauses	= analysis & get results & M.filter M.null & M.keys
		let clauses	= ts & get tsFunctions & (M.! nm) & getClauses	:: [Clause]
		let deadClauses'= deadClauses |> (show &&& (!!) clauses)	:: [(String, Clause)]
		let msgs	= deadClauses' ||>> toParsable' (nm, 30:: Int)
					||>> ("   "++)
					|> uncurry (++)
		unless (null deadClauses) $ Left $ unlines msgs
	  



checkStrictestTypes	:: TypeSystem -> Either String ()
checkStrictestTypes ts
	= inMsg "While checking that the functions do have the strictest possible types" $
	  do	let stricter 	= stricterTypes (get tsSyntax ts) (get tsFunctions ts) & M.toList
		let origType n	= (get tsFunctions ts ! n) & typesOf & last
		stricter |> (\(nm, tp) -> show nm++" can be typed as "++show tp++", instead of a "++show (origType nm)) |> Left & allRight_
		
		
