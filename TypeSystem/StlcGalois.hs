module TypeSystem.StlcGalois where

{-
This module defines the Galois connection for the types of the simply typed lambda calculus
-}

import Utils

import Data.Set as Set

import Parser.StlcAST as STLC

import TypeSystem.GaloisConnection



instance GaloisConnection StaticType where
	-- concretization :: 
	concretization UnkownT	
		= Universe
	concretization (StaticType (ArrowT t1 t2))
		= error "TODO" -- fromList (ArrowT <$> toList (concretization t1) <*> toList (concretization t2))
	concretization t
		= Set' $ singleton $ NatT


	abstraction (Set' t)
 	 | Set.null t		= Nothing	-- undefined
	 | t == singleton NatT	= Just $ StaticType $ NatT
	 | t == singleton BoolT	= Just $ StaticType $ BoolT
	 | allArrows t		= do	(t1s, t2s)	<- toList t & splitArrows	-- all will be arrows here, as by the 'allArrows' guard
					(StaticType t1)	<- abstraction $ fromList' t1s	-- TODO what if this is unknown???
					(StaticType t2)	<- abstraction $ fromList' t2s
					return $ StaticType $ ArrowT t1 t2
	 | otherwise 		= Just UnkownT
	abstraction (Universe)	= Just UnkownT



