{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Utils.LatticeImage(latticeSVG, ColorScheme(..), terminalCS, whiteCS) where

import Utils.Utils
import Utils.Image

import Text.Blaze.Svg11 ((!), stringValue)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import qualified Data.Map as M

import Lens.Micro hiding ((&))


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





latticeSVG	:: Int -> ColorScheme ->  ([[String]], [(String, String)], [(String, String)]) -> String
latticeSVG pxFactor cs (groupsS, connectedS, connectedDashedS)
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
		packageSVG (w*pxFactor, h*pxFactor) (w, h) svg

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





