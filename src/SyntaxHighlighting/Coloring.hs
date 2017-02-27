 {-# LANGUAGE TemplateHaskell #-}
module SyntaxHighlighting.Coloring(Coloring(..), FullColoring, emptyFullColoring, addColoring, colorDistance, parseColoringFile, getProperty, toSVGColorScheme, intAsColor) where

{- Defines rendering properties for styles -}

import Utils.Utils
import Utils.ToString

import Utils.Image

import qualified Assets
import TypeSystem
import TypeSystem.Parser.TargetLanguageParser
import ParseTreeInterpreter.FunctionInterpreter

import Data.Maybe
import Data.Map as M
import Data.Char as Ord

import Lens.Micro hiding ((&))
import Lens.Micro.TH

import Control.Monad

type Prop	= String

data Coloring	= Coloring
	{ _coloringFallback	:: Name
	, _coloringProperties	:: Map Prop (Either Int String)
	} deriving (Show)
makeLenses ''Coloring

rootColoring	= Coloring "" M.empty

data FullColoring = FullColoring 
	{ _fcName	:: Name
	, _fcProps	:: Map Name Coloring
	}
	deriving (Show)
makeLenses ''FullColoring

emptyFullColoring nm
	= FullColoring nm $ M.singleton "" rootColoring


getProperty	:: FullColoring -> Name -> Prop -> Maybe (Either Int String)
getProperty fc@(FullColoring _ fcStyles) style prop
 | style == ""	= Nothing
 | otherwise
	= do	(Coloring fb props)	<- M.lookup style fcStyles
		firstJusts [M.lookup prop props, getProperty fc fb prop]

addColoring	:: (Name, Coloring) -> FullColoring -> Either String FullColoring
addColoring (n, c) (FullColoring fcn fc)
	= do	checkExists (get coloringFallback c) fc $ "Fallback style "++show (get coloringFallback c)++" does not exist"
		return $ FullColoring fcn $ M.insert n c fc


parseColoringFile	:: FilePath -> String -> Either String FullColoring
parseColoringFile fp coloring
	= do	ts	<- parseTypeSystem Assets._Style_language $ Just "Assets: Style.language"
		pt	<- parseTargetLang (get tsSyntax ts) "styleFile" fp coloring
		pt'	<- evalFunc ts "expandFile" [pt]
		buildFC ts pt'




buildFC		:: TypeSystem -> ParseTree -> Either String FullColoring
buildFC ts file@(PtSeq i [ws, t, cons, defs, b1s])
	= do	fc	<- buildFC ts (PtSeq i [ws, t, cons, defs])
		tree	<- evalFunc ts "fallbacks" [file]
		let fallbacks	= fallbackTree tree 
		colorings	<- overBlocksRec coloringForBlock b1s
					|> prepColoring fallbacks & allRight'
		foldM (flip addColoring) fc colorings
buildFC ts file@(PtSeq _ [_, t, _, defs])
	= do	nmT	<- evalFunc ts "titleN" [t]
		nm	<- fromPtToken nmT |> return & fromMaybe (Left $ "Not a token: "++toParsable nmT)
		let defColoring	= Coloring "" $ coloring defs
		addColoring (nm, defColoring) (emptyFullColoring nm)
		


prepColoring	:: Map Name Name -> (Name, Map Prop (Either Int String)) -> Either String (Name, Coloring)
prepColoring fallBacks (nm, props)
	= do	fallBack	<- checkExists nm fallBacks $ "Weird, no fallback for "++nm
		return (nm, Coloring fallBack props)


coloringForBlock	:: (Name, Char, ParseTree) -> (Name, Map Prop (Either Int String))
coloringForBlock (nm, _, props)
	= (nm, coloring props)

overBlocks	:: ((Name, Char, ParseTree, Maybe ParseTree) -> a) -> ParseTree -> [a]
overBlocks f (PtSeq (_,0) [blck, blcks])
	= f (dissectBlock blck) : overBlocks f blcks
overBlocks f blck
	= [f $ dissectBlock blck]


overBlocksRec	:: ((Name, Char, ParseTree) -> a) -> ParseTree -> [a]
overBlocksRec f pt
	= overBlocks (_overBlocksRec f) pt & concat

_overBlocksRec	:: ((Name, Char, ParseTree) -> a) -> (Name, Char, ParseTree, Maybe ParseTree) -> [a]
_overBlocksRec f (nm, chr, pt, Nothing)
	= [f (nm, chr, pt)]
_overBlocksRec f (nm, chr, pt, Just nested)
	= f (nm, chr, pt) : overBlocksRec f nested
				

dissectBlock	:: ParseTree -> (Name, Char, ParseTree, Maybe ParseTree)
dissectBlock (PtSeq _ [header@(PtSeq _ [MLiteral _ nm, _, MLiteral _ level, _]), props])
	= (nm, head level, props, Nothing)
dissectBlock (PtSeq _ [header@(PtSeq _ [MLiteral _ nm, _, MLiteral _ level, _]), props, blocks])
	= (nm, head level, props, Just blocks)
dissectBlock pt
	= error $ "dissectBlock: fallthrough: "++debug pt

coloring	::  ParseTree -> Map Prop (Either Int String)
coloring (MLiteral _ "\n")
	= M.empty
coloring (PtSeq _ [PtSeq _ [MLiteral _ k, MLiteral _ "=", MLiteral _ v], _])
	= M.singleton k (Right $ init $ tail v)
coloring (PtSeq _ [PtSeq _ [MLiteral _ k, MLiteral _ "=", MLiteral _ v], _, rest])
	= M.insert k (Right $ init $ tail v) $ coloring rest
coloring (PtSeq _ [PtSeq _ [MLiteral _ k, MLiteral _ "=", MInt _ v], _])
	= M.singleton k (Left v)
coloring (PtSeq _ [PtSeq _ [MLiteral _ k, MLiteral _ "=", MInt _ v], _, rest])
	= M.insert k (Left v) $ coloring rest
coloring pt
	= error $ "Coloring: fallthrough: "++debug pt



fallbackTree	:: ParseTree -> Map Name Name
fallbackTree (MLiteral _ "$")
		= M.empty
fallbackTree (PtSeq _ [PtSeq _ [MLiteral _ k, MLiteral _ "->", MLiteral _ v], _, fbs])
		= M.insert k v $ fallbackTree fbs
fallbackTree ft	= error $ "FallbackTree: fallthrough: "++toParsable ft

-- Distance between hex colors
colorDistance	:: String -> String -> Int
colorDistance ('#':col0) ('#':col1)
	= let	(r0, (g0, b0))	= col0 |> digitToInt & splitAt 2 |> splitAt 2
		(r1, (g1, b1))	= col1 |> digitToInt & splitAt 2 |> splitAt 2
		n [a,b]		= 16*a + b
		in 
		abs (n r0 - n r1) + abs (n g0 - n g1) + abs (n b0 - n b1)
colorDistance col0 col1
	= error $ "Not colors: "++col0++", "++col1

intAsColor	:: Int -> String
intAsColor i
	= let	b	= i ` mod` 256
		i'	= i `div` 256
		g	= i' `mod` 256
		r	= (i' `div` 256) `mod` 256
		rgb	= [r, g, b] >>= (\j -> [intToDigit (j `div` 16), intToDigit (j `mod` 16)])
		in
		'#':(show r ++","++ show g ++","++ show b)
		

toSVGColorScheme	:: Maybe Name -> FullColoring -> ColorScheme
toSVGColorScheme style fc	
	= let	style'		= fromMaybe (get fcName fc) style
		property p def	= getProperty fc style' p & fromMaybe def
		property' p def	= property p (Right def) & either (const def) id 
		properti p def	= property p (Left def) 
					& either id (const def)	:: Int
				
		fg		= property' "foreground-color" "#000000"
		bg		= property' "background-color" "#ffffff"
		lineColor	= property' "svg-line-color" fg
		lineThickness	= properti "svg-line-thickness" 1
		dotSize		= properti "svg-dotsize" 4
		fontSize	= properti "svg-fontsize" 20
		in
		CS fg bg lineColor fontSize lineThickness dotSize




instance ToString Coloring where

	debug (Coloring fallback dict)
		= ["Fallback: "++fallback
			, dict |> either show id & M.toList |> (\(k, v) -> k ++ " = " ++ v) & unlines
			] & unlines 

instance ToString FullColoring where

	debug (FullColoring n d)
		= let fc	= d |> debug & M.toList |> (\(k, v) -> k ++ "\n" ++ indent v) & unlines in
			inHeader "" n '-' fc
