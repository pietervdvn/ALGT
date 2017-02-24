 {-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module TypeSystem.SyntaxStyle where

{-
This module defines style highlightings for rules
-}

import Utils.Utils
import Utils.ToString
import Data.Map as M
import Data.List (sort)

import TypeSystem.Types
import TypeSystem.ParseTree

import Lens.Micro.TH
import Lens.Micro hiding ((&))

import Debug.Trace

-- A little extra, cause I like fancy colors

data SyntaxStyle = SyntaxStyle
	{ _baseStyles	:: Map TypeName Name
	, _extraStyles	:: Map (TypeName, Int) Name
	, _styleRemaps	:: Map Name Name	-- e.g. noise maps to comment
	} deriving (Show)

makeLenses ''SyntaxStyle


determineStyle	:: SyntaxStyle -> ParseTreeA a -> ParseTreeA (a, Maybe Name)
determineStyle styling pt
	= let	(genType, choiceI)	=  get ptaInf pt
		specificStyle		= M.lookup (genType, choiceI) $ get extraStyles styling
		baseStyle		= M.lookup genType $ get baseStyles styling
		style			= firstJusts [specificStyle, baseStyle]
		in
		case pt of
			PtSeqA a i pts	-> pts |> determineStyle styling & PtSeqA (a, style) i
			pt		-> pt |> (\a -> (a, style))
			


instance Refactorable TypeName SyntaxStyle where
	refactor ftn style
		= over baseStyles (M.mapKeys ftn) $ over extraStyles (M.mapKeys (over _1 ftn)) style

instance ToString SyntaxStyle where
	toParsable (SyntaxStyle baseStyles extraStyles styleRemaps)
		= let	styles	= M.toList baseStyles ++
				  (M.toList extraStyles |> over _1 (\(nm, i) -> nm++"."++show i)) ++
				  (M.toList styleRemaps |> over _1 show)
			styles'	= styles ||>> show |> (\(key, val) -> key ++ "\t -> "++val)
			in
			styles' & sort & unlines

