 {-# LANGUAGE TemplateHaskell #-}
module SyntaxHighlighting.Coloring(FullColoring, parseColoringFile, getProperty, toSVGColorScheme, intAsColor, colorDistance) where

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

import Debug.Trace

type Prop	= String

data FullColoring = FullColoring 
	{ _fcPt	:: ParseTree
	, _fcTs	:: TypeSystem
	}
	deriving (Show)
makeLenses ''FullColoring


getProperty	:: FullColoring -> Maybe Name -> Prop -> Maybe (Either Int String)
getProperty (FullColoring pt ts) Nothing prop
	= either (flip trace Nothing) return $ inMsg ("While searching prop "++prop++" in the default values") $ do
		propPt	<- _asID ts prop
		found	<- evalFunc ts "getDefaultPropertyFor" [pt, propPt]
		_extractValue found
		
getProperty fc@(FullColoring pt ts) (Just style) prop
 	= either (flip trace Nothing) return $ inMsg ("While searching a value for "++show style++" and "++prop) $ do	
		propPt	<- _asID ts prop
		stylePt	<- _asID ts style
		found	<- evalFunc ts "getPropertyFor" [pt, stylePt, propPt]
		_extractValue found


_asID ts name	= parseTargetLang (get tsSyntax ts) "identifier" "coloring.hs:getProperty:id" name 
					& inMsg ("Not a valid stylename or property name: "++name)

_extractValue	:: ParseTree -> Either String (Either Int String)
_extractValue (MLiteral _ "?")
		= Left $ "No value found"	
_extractValue (MLiteral ("color",0) str)
		= return $ Right str
_extractValue (MLiteral ("value", 0) str)
		= return $ Right str
_extractValue (MInt ("value", 1) i)
		= return $ Left i
_extractValue pt
		= error $ "Coloring: unexpected parsetree; probably due to some weird styling file. Run with --plain to disable syntax highlighting"++show pt


parseColoringFile	:: FilePath -> String -> Either String FullColoring
parseColoringFile fp input
	= do	ts	<- parseTypeSystem Assets._Style_language $ Just "Assets: Style.language"
		pt	<- parseTargetLang (get tsSyntax ts) "styleFile" fp input
		pt'	<- evalFunc ts "expandFile" [pt]
		return $ FullColoring pt' ts





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
	= let	property p def	= getProperty fc style p & fromMaybe def
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




