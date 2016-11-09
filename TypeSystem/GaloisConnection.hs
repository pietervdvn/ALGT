module TypeSystem.GaloisConnection where

import Utils

import Data.Set as Set

import Parser.StlcAST as STLC

class GaloisConnection t where
	fullType		:: Set t
	concretization	:: t -> Set t
	abstraction	:: Set t -> Maybe t



instance GaloisConnection STLC.Type where
	fullType	= fromList STLC.allType
	
	concretization UnkownT	
		= fullType
	concretization NatT
		= singleton NatT
	concretization BoolT
		= singleton BoolT
	concretization (ArrowT t1 t2)
		= fromList (ArrowT <$> toList (concretization t1) <*> toList (concretization t2))

	abstraction t
 	 | Set.null t		= Nothing	-- undefined
	 | t == singleton NatT	= Just $ NatT
	 | t == singleton BoolT	= Just $ BoolT
	 | allArrows t		= do	(t1s, t2s)	<- toList t & _splitArrows
					t1		<- abstraction $ fromList t1s
					t2		<- abstraction $ fromList t2s
					return $ ArrowT t1 t2
	 | otherwise 		= Just UnkownT


allArrows	:: Set Type -> Bool
allArrows ts	= ts & toList & all isArrow

_splitArrows	:: [Type] -> Maybe ([Type], [Type])
_splitArrows (ArrowT t1 t2:tail)
		= do	(t1s, t2s)	<- _splitArrows tail
			return (t1:t1s, t2:t2s)
_splitArrows []	= Just ([], [])
_splitArrows _	= Nothing
