 {-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module TypeSystem.SyntaxStyle where

{-
This module defines style highlightings for rules
-}

import Utils.Utils
import Utils.ToString
import Data.Map as M hiding (filter)
import Data.List (sort, intercalate, tails)
import Data.Maybe (catMaybes, listToMaybe)

import TypeSystem.Types
import TypeSystem.ParseTree

import Lens.Micro.TH
import Lens.Micro hiding ((&))

import Debug.Trace

import Data.Bifunctor (first)

-- A little extra, cause I like fancy colors


data SyntaxStyle = SyntaxStyle
	{ _styles	:: Map [(TypeName, Maybe Int)] Name
	} deriving (Show, Eq)

makeLenses ''SyntaxStyle

determineStyle'	:: SyntaxStyle -> ParseTree -> ParseTreeA (Maybe Name)
determineStyle' styling pt
	= pt & annot () & determineStyle styling |> snd

determineStyle	:: SyntaxStyle -> ParseTreeA a -> ParseTreeA (a, Maybe Name)
determineStyle styling
	= _determineStyle (_prepStyle' styling) [] Nothing



data PreppedStyle = PreppedStyle
	{ _preppedStyle	:: Map [TypeName] [([Maybe Int], Name)]
	} deriving (Show, Eq)


_prepStyle'	:: SyntaxStyle -> PreppedStyle
_prepStyle' (SyntaxStyle style)
	= style & M.toList & _prepStyle & merge & M.fromList & PreppedStyle

_prepStyle	:: [([(TypeName, Maybe Int)], Name)] -> [([TypeName], ([Maybe Int], Name))]
_prepStyle []	= []
_prepStyle ((pth, nm):rest)
	= let	(nms, ints)	= unzip pth
		rest'		= _prepStyle rest in
		((nms, (ints, nm)):rest')


_determineStyle	:: PreppedStyle -> [(Name, Int)] -> Maybe Name -> ParseTreeA a -> ParseTreeA (a, Maybe Name)
_determineStyle style path fallBack node
	= let	path'		= get ptaInf node :path
		styleName	= firstJusts [_lookupStyleSuffix style path', fallBack]
		in
		case node of
			(PtSeq a info pts) -> 	pts |> _determineStyle style (info:path) styleName & PtSeq (a, styleName) info
			node		-> node |> flip (,) styleName

_lookupStyleSuffix	:: PreppedStyle -> [(Name, Int)] -> Maybe Name
_lookupStyleSuffix ps path
	= path & reverse & tails & init |> _lookupStyle ps & firstJusts

_lookupStyle	:: PreppedStyle -> [(Name, Int)] -> Maybe Name
_lookupStyle (PreppedStyle style) path
	= do	let	(nms, ints)	= unzip path
		choices	<- M.lookup nms style
		_lookupStyle' choices ints

_lookupStyle'	:: [([Maybe Int], Name)] -> [Int] -> Maybe Name
_lookupStyle' choices path
	= let checkPath path'	= zip path path' |> sndEffect & catMaybes & all (uncurry (==))
		in
		choices & filter (checkPath . fst) 
			|> snd
			& listToMaybe


instance Refactorable TypeName SyntaxStyle where
	refactor ftn style
		= style	& over styles (M.mapKeys (|> first ftn))

instance ToString SyntaxStyle where
	toParsable (SyntaxStyle styles)
		= styles & M.toList |> (\(pth, val) -> _showPth pth ++"\t-> "++show val) & unlines


_showPth	:: [(String, Maybe Int)] -> String
_showPth pth
	= pth |> _showPthEl & intercalate "."

_showPthEl (tn, Nothing)
	= tn
_showPthEl (tn, Just i)
	= tn ++ "." ++ show i

