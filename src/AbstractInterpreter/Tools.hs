module AbstractInterpreter.Tools where

{-
This module reexports functions to check totality and stricter values
-}

import TypeSystem
import Utils.Utils
import Utils.ToString

import AbstractInterpreter.AbstractParseTree
import AbstractInterpreter.AbstractFunctionInterpreter
import AbstractInterpreter.MinimalTypes

import Data.Map (Map, (!))
import qualified Data.Map as M



checkTS	ts
	=  [checkTotality ts, checkStrictestTypes ts] & allRight_ & inMsg "Warning"

checkTotality	:: TypeSystem -> Either String ()
checkTotality ts
	= do	let fs		= tsFunctions ts
		let fsTps	= fs |> typesOf |> last
		let syntax	= tsSyntax ts
		let analysises	= fs |> interpretFunction' syntax fsTps	:: Map Name Analysis
		let leftOver	= analysises |> leftOvers & M.filter (not . null) & M.toList
		leftOver |> totalityErr & allRight_

totalityErr	:: (Name, [Arguments]) -> Either String ()
totalityErr (nm, over)
	= inMsg ("While checking the totality of function "++show nm) $
	  inMsg "Following calls will fall through" $ do
	 	let msgs	= over |> toParsable' ", " |> inParens |> (nm++) & unlines
		Left msgs
		


checkStrictestTypes	:: TypeSystem -> Either String ()
checkStrictestTypes ts
	= inMsg "While checking that the functions do have the strictest possible types" $
	  do	let stricter 	= stricterTypes (tsSyntax ts) (tsFunctions ts) & M.toList
		let origType n	= (tsFunctions ts ! n) & typesOf & last
		stricter |> (\(nm, tp) -> show nm++" can be typed as "++show tp++", instead of a "++show (origType nm)) |> Left & allRight_
		
		
