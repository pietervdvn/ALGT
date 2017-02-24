 {-# LANGUAGE TemplateHaskell #-}
module SyntaxHighlighting.Coloring(Coloring(..), FullColoring, emptyFullColoring, addColoring) where

{- Defines rendering properties for styles -}

import Utils.Utils

import Data.Map as M

import Lens.Micro hiding ((&))
import Lens.Micro.TH

type Property	= String

data Coloring	= Coloring
	{ _coloringFallback	:: Name
	, _coloringProperties	:: Map Property String
	}
makeLenses ''Coloring

rootColoring	= Coloring "" M.empty

newtype FullColoring
	= FullColoring (Map Name Coloring)

emptyFullColoring
	= M.singleton "" rootColoring

addColoring	:: (Name, Coloring) -> FullColoring -> Either String FullColoring
addColoring (n, c) (FullColoring fc)
	= do	checkExists (get coloringFallback c) fc $ "Fallback style "++show (get coloringFallback c)++" does not exist"
		return $ FullColoring $ M.insert n c fc


