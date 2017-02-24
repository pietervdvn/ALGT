 {-# LANGUAGE TemplateHaskell #-}
module SyntaxHighlighting.Coloring(Coloring(..), FullColoring, emptyFullColoring, addColoring, t) where

{- Defines rendering properties for styles -}

import Utils.Utils
import Utils.ToString

import qualified Assets as Assets
import TypeSystem
import TypeSystem.Parser.TargetLanguageParser
import ParseTreeInterpreter.FunctionInterpreter

import Data.Maybe
import Data.Map as M

import Lens.Micro hiding ((&))
import Lens.Micro.TH

type Prop	= String

data Coloring	= Coloring
	{ _coloringFallback	:: Name
	, _coloringProperties	:: Map Prop String
	} deriving (Show)
makeLenses ''Coloring

rootColoring	= Coloring "" M.empty

newtype FullColoring
	= FullColoring (Map Name Coloring)
	deriving (Show)

emptyFullColoring
	= FullColoring $ M.singleton "" rootColoring

addColoring	:: (Name, Coloring) -> FullColoring -> Either String FullColoring
addColoring (n, c) (FullColoring fc)
	= do	checkExists (get coloringFallback c) fc $ "Fallback style "++show (get coloringFallback c)++" does not exist"
		return $ FullColoring $ M.insert n c fc


parseColoringFile	:: FilePath -> String -> Either String FullColoring
parseColoringFile fp coloring
	= do	ts	<- parseTypeSystem Assets._Style_language (Just "Assets: Style.language")
		pt	<- parseTargetLang (get tsSyntax ts) "styleFile" fp coloring
		pt'	<- evalFunc ts "expandFile" [pt]
		error $ show $ buildFC ts pt'
		return emptyFullColoring




buildFC		:: TypeSystem -> ParseTree -> Either String FullColoring
buildFC ts file@(PtSeq _ [_, t, _, defs, b1s])
	= do	nmT	<- evalFunc ts "titleN" [t]
		nm	<- fromPtToken nmT |> return & fromMaybe (Left $ "Not a token: "++toParsable nmT)
		tree	<- evalFunc ts "fallbacks" [file]
		
		let tree'	= fallbackTree tree 
		
		error $ show $ coloring defs

				

coloring	::  ParseTree -> Map Prop String
coloring (MLiteral _ "\n")
	= M.empty
coloring (PtSeq _ [PtSeq _ [MLiteral _ k, MLiteral _ "=", MLiteral _ v], nl, rest])
	= error $ show k
coloring fb pt
	= error $ "Coloring: falltrhough: "++toParsable pt


fallbackTree	:: ParseTree -> Map Name Name
fallbackTree (MLiteral _ "$")
		= M.empty
fallbackTree (PtSeq _ [PtSeq _ [MLiteral _ k, MLiteral _ "->", MLiteral _ v], _, fbs])
		= M.insert k v $ fallbackTree fbs
fallbackTree ft	= error $ "FallbackTree: fallthrough: "++toParsable ft


t	= do	f	<- readFile "Examples/Terminal.style"
		fc	<- parseColoringFile "Examples/Terminal.style" f & either error return
		print fc
