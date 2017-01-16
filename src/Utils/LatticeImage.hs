{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Utils.LatticeImage(latticeSVG, ColorScheme(..), terminalCS, whiteCS) where

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m, stringValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Text.Blaze.Svg.Renderer.String (renderSvg)-- DO NOT USE THE PRETTY RENDERER; text won't be centered then
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Map as M
import Data.Map (Map)

import Lens.Micro hiding ((&))
import Lens.Micro.TH

import Utils.Utils

import Control.Monad

data ColorScheme	= CS {	_fg		:: String,
				_bg		:: String,
				_lineColor	:: String,
				_fontSize	:: Int,
				_lineThickness	:: Int,
				_dotSize	:: Int
				}
terminalCS	= CS "#00ff00" "#000000" "#00ff00" 20 1 4
whiteCS		= CS "#000000" "#ffffff" "#000000" 20 1 4

makeLenses ''ColorScheme

type X	= Int
type Y 	= Int

type W	= Int
type H	= Int

-- Example:
latticeGroups	:: [[String]]
latticeGroups	= [["{}"], ["{a}","{b}","{c}"],["{a, b}","{a, c}","{b, c}"], ["{a, b, c}"]]

latticeConn	:: [(String, String)]
latticeConn	= [ ("{a}", "{a, b}"), ("{a}", "{a, c}"),  ("{b}", "{a, b}"),  ("{b}", "{b, c}"),  ("{c}", "{a, c}") ,  ("{c}", "{b, c}")]

latticeDashed	:: [(String, String)]
latticeDashed	= [ ("{}", "{a}"),  ("{}", "{b}"),  ("{}", "{c}"), ("{a, b, c}", "{a, b}"),  ("{a, b, c}", "{b, c}"),  ("{a, b, c}", "{a, c}")]

main :: IO ()
main = do	let cs	= terminalCS
		let a = latticeSVG 25 cs (latticeGroups, latticeConn, latticeDashed)
				
		putStrLn a
		writeFile "test.svg" a





intValue	:: Int -> S.AttributeValue
intValue i	= stringValue $ show i



packageSVG	:: (Int, Int) -> (Int, Int) -> S.Svg -> String
packageSVG (pxW, pxH) (viewBoxW, viewBoxH) svg
	= let	svg'	= S.docTypeSvg	! A.version "1.1"
					! A.width (intValue pxW)
					! A.height (intValue pxH)
					! A.viewbox (stringValue $ "0 0 "++show viewBoxW++" "++show viewBoxH)
					$ svg
		in
		renderSvg svg'


latticeSVG	:: Int -> ColorScheme ->  ([[String]], [(String, String)], [(String, String)]) -> String
latticeSVG pxW cs (groupsS, connectedS, connectedDashedS)
	= let	groups	= groupsS ||>> Text.pack
		connected	= connectedS |> over _1 Text.pack |> over _2 Text.pack
		connectedDashed	= connectedDashedS |> over _1 Text.pack |> over _2 Text.pack
		
		(w', h')	= neededWidth (get fontSize cs) groups
		factor		= 1
		(w, h)		= (w' * factor, h' * factor)
		cs'		= over fontSize (*factor) $ over dotSize (*factor) $ over lineThickness (*factor) cs
		svg	= do	S.rect ! A.width (intValue w) ! A.height (intValue h) ! A.fill (stringValue $ get bg cs)
				lattice cs' w h groups connected connectedDashed
		in 
		packageSVG (w*pxW, h*pxW) (w, h) svg

lattice		:: ColorScheme -> W -> H -> [[Text]] -> [(Text, Text)] -> [(Text, Text)] -> S.Svg
lattice cs w h groups connected connectedDashed
	= do	let groups'	= positions w h groups 
		let queryPos	= M.fromList $ concat groups'
		connected |+> uncurry (drawLineBetween cs False queryPos)
		connectedDashed |+> uncurry (drawLineBetween cs True queryPos)
		let gl		= length groups `div` 2
		groups' & take gl & concat |+> annotatedDot cs True
		groups' & drop gl & concat |+> annotatedDot cs False

		pass



positions	:: W -> H -> [[Text]] -> [[(Text, (X, Y))]]	
positions w h groups
	= let	groups'	= groups |> positionsX w
		yF	= h `div` length groups'
		ys	= [1..length groups'] & reverse |> (* yF) |> (\i -> i - (yF `div` 2))
		in
		zip groups' ys |> (\(nmsX, y) -> nmsX |> (\(nm, x) -> (nm, (x, y)) ))

neededWidth	:: Int -> [[Text]] -> (W, H)
neededWidth fontSize groups
	= let	h	= length groups
		w	= groups ||>> Text.length ||>> (+5) |> sum & maximum
		in
		(w  * fontSize, h * 6 * fontSize)
		


positionsX	:: W -> [Text] -> [(Text, X)]
positionsX w nms
	= let	xF	= w `div` length nms
		xs	= [1..length nms] |> (* xF) |> (\i -> i - (xF `div` 2))
		in
		zip nms xs


drawLineBetween	:: ColorScheme -> Bool -> Map Text (X, Y) -> Text -> Text -> S.Svg
drawLineBetween cs dashed coors start end
	= do	let find x	= M.findWithDefault (error $ "LatticeImage: I do not know what "++Text.unpack x++" is") x coors 
		let start'	= find start
		let end'	= find end
		drawLine cs dashed start' end'

drawLine	:: ColorScheme -> Bool -> (X, Y) -> (X, Y) -> S.Svg
drawLine cs dashed (x0,y0) (x1, y1)
	= let 	lt	= get lineThickness cs
		pth = S.path ! A.d (stringValue $ ["M", show x0, show y0, ","
						, show x1, show y1] & unwords) 
			! A.stroke (stringValue $ get lineColor cs)
			! A.strokeWidth (intValue $ get lineThickness cs)
			! A.strokeLinecap "round"
	   in if dashed then pth ! A.strokeDasharray (stringValue $ [show $ 5 * lt] & unwords) else pth

{-
Height: 
fs * 3
-}
annotatedDot	:: ColorScheme -> Bool -> (Text, (X, Y)) -> S.Svg
annotatedDot cs under (nm, (x, y))
	= do	let fs	= get fontSize cs
		let nml	= Text.length nm
		let rW	= round $ fromIntegral (fs * nml) * 0.65
		let offsetY	= if under then fs `div` 2 else negate 2 * fs
		unless (Text.null nm) $ do 
			S.rect ! A.x (intValue $ x - rW `div` 2)
				! A.y (intValue $ y + offsetY ) 
				! A.width (intValue rW)
				! A.height (intValue $ fs + fs `div` 2)
				! A.fill (stringValue $ get bg cs)
				! A.fillOpacity "0.5"
			S.text_ ! A.x (intValue x)
				! A.y (intValue $ y + fs + offsetY) 
				! A.fontSize (intValue fs)
				! A.textAnchor (stringValue "middle")
				! A.fontFamily "mono"
				! A.fill (stringValue $ get fg cs)
				$ S.text  nm
		S.circle ! A.r (intValue $ get dotSize cs) ! A.cx (intValue x) ! A.cy (intValue y) ! A.fill (stringValue $ get fg cs)


