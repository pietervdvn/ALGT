 {-# LANGUAGE TemplateHaskell #-}
module SyntaxHighlighting.Coloring(FullColoring, parseColoringFile, getProperty, definedStyles, intAsColor, colorDistance, highestComponent) where

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

data FullColoring = FullColoring 
	{ _fcPt	:: ParseTree
	, _fcTs	:: TypeSystem
	}
	deriving (Show)
makeLenses ''FullColoring


getProperty	:: FullColoring -> Name -> Prop -> Maybe (Either Int String)
getProperty (FullColoring pt ts) "" prop
	= either (const Nothing) return $ inMsg ("While searching prop "++prop++" in the default values") $ do
		propPt	<- _asID ts prop
		found	<- evalFunc ts "getDefaultPropertyFor" [pt, propPt]
		_extractValue found
		
getProperty fc@(FullColoring pt ts) style prop
 	= either (const Nothing) return $ inMsg ("While searching a value for "++show style++" and "++prop) $ do	
		propPt	<- _asID ts prop
		stylePt	<- _asID ts style
		found	<- evalFunc ts "getPropertyFor" [pt, stylePt, propPt]
		_extractValue found


_asID ts name	= parseTargetLang (get tsSyntax ts) "identifier" "coloring.hs:getProperty:id" name 
					& inMsg ("Not a valid stylename or property name: "++name)

_extractValue	:: ParseTree -> Either String (Either Int String)
_extractValue (MLiteral _ _ "?")
		= Left "No value found"	
_extractValue (MLiteral _ ("color",0) str)
		= return $ Right str
_extractValue (MLiteral _ ("String", 0) str)
		= return $ Right str
_extractValue (MInt _ _ i)
		= return $ Left i
_extractValue pt
		= error $ "Coloring: unexpected parsetree; probably due to some weird styling file. Run with --plain to disable syntax highlighting"++show pt

terminalStyle		= parseColoringFile "Assets: Terminal" Assets._Terminal_style

parseColoringFile	:: FilePath -> String -> Either String FullColoring
parseColoringFile fp input
	= do	ts	<- parseTypeSystem Assets._Style_language $ Just "Assets: Style.language"
		pt	<- parseTargetLang (get tsSyntax ts) "styleFile" fp input
		pt'	<- evalFunc ts "expandFile" [pt]
		return $ FullColoring pt' ts


_extractStyles	:: ParseTree -> [String]
_extractStyles (PtSeq _ ("knownStyles", 0) [MLiteral _ _ name, rest])
		= name : _extractStyles rest
_extractStyles (MLiteral _ _ name)
		= [name]
_extractStyles pt
		= error $ "Coloring: unexpected parsetree for style extraction: "++show pt


definedStyles	:: FullColoring -> [String]
definedStyles (FullColoring pt ts)
	= either error id $ do
		pt'	<- evalFunc ts "knownStylesIn" [pt]
		return $ _extractStyles pt'

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


highestComponent	:: String -> Int
highestComponent ('#':col)
	= let	(r, (g, b))	= col |> digitToInt & splitAt 2 |> splitAt 2
		n [a,b]		= 16*a + b
		in 
		maximum [n r, n g, n b]

intAsColor	:: Int -> String
intAsColor i
	= let	b	= i ` mod` 256
		i'	= i `div` 256
		g	= i' `mod` 256
		r	= (i' `div` 256) `mod` 256
		rgb	= [r, g, b] >>= (\j -> [intToDigit (j `div` 16), intToDigit (j `mod` 16)])
		in
		'#':(show r ++","++ show g ++","++ show b)
		

