module AbstractInterpreter.Constraints where

import Utils.Utils

import Data.Map as M
import Data.Maybe

import Graphs.UnionFind

import Control.Monad


data Constraint	= SameAs Name 
		| HasValue (Either Int String)
	deriving (Show, Eq)

type Constraints 
		= Map Name Constraint


fromSameAsConstraint	:: Constraint -> Maybe Name
fromSameAsConstraint (SameAs n)
		= Just n
fromSameAsConstraint _
		= Nothing

isValueConstraint		:: Constraint -> Bool
isValueConstraint (HasValue _)	= True
isValueConstraint _		= False

{-

- "Same as" will always point to the smallest (alphabetically) name
- HasValue will always be at the smallest name

{"a" --> SameAs "b"} will be rewritten as {"b" sameAs "a"}
{"a" --> SameAs "b", "b" --> HasValue (Left 0)} --> {"a" --> HasValue (Left 0), "b" --> HasValue (Left 0)} 

-}
normalizeConstraints	:: Constraints -> Constraints
normalizeConstraints constraints
	= let	connected
			= constraints |> fromSameAsConstraint & M.toList |> sndEffect & catMaybes
				:: [(Name, Name)]
		lowestRepr	= unionFind connected & M.filterWithKey (/=) |> SameAs
		hasV	= constraints & M.filter isValueConstraint
		in
		hasV `M.union` lowestRepr




addConstraint	:: (Name, Constraint) -> Constraints ->  Either String Constraints
addConstraint (nm, SameAs nm') constraints
 | nm == nm'	= return constraints
 | nm < nm'	= _addConstraint (nm', SameAs nm) constraints
 | otherwise	= _addConstraint (nm, SameAs nm') constraints
addConstraint c constraints
		= _addConstraint c constraints

_addConstraint	:: (Name, Constraint) -> Constraints -> Either String Constraints
_addConstraint (nm, constraint) constraints
 | nm `M.member` constraints
		= inMsg ("While adding the constraint "++show nm++": "++show constraint) $ do
			let c	= constraints M.! nm
			if c == constraint then return constraints else 
				case c of
					c@(HasValue v)	-> Left $ "Another value was found: "++either show show v
					(SameAs x)	-> addConstraint (x, constraint) constraints
 | otherwise	= M.insert nm constraint constraints & return


mergeConstraints	:: Constraints -> Constraints -> Either String Constraints
mergeConstraints a b
	= foldM (\b constraint -> addConstraint constraint b ) b (M.toList a) 

