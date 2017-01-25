{-# LANGUAGE OverloadedStrings #-}
module Utils.LatticeImage(latticeSVG, ColorScheme(..), terminalCS, whiteCS, linesIntersect) where

import Utils.Utils
import Utils.Image

import Text.Blaze.Svg11 ((!), stringValue)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import qualified Data.Map as M
import Data.List
import Data.Map (Map)

import Control.Arrow ((&&&))

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
	= let	maxWidth	= groupsS |> length & maximum
		groupsS'	= head groupsS : init (tail groupsS |> padR maxWidth []) ++ [last groupsS]
		groups		= groupsS' ||>> Text.pack
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
	= do	let bestGroups	= bestPos w h groups (connected ++ connectedDashed)
		let groups'	= positions w h bestGroups |> filter (not . Text.null . fst)
		let queryPos	= M.fromList $ concat groups'
		connected |+> uncurry (drawLineBetween cs False queryPos)
		connectedDashed |+> uncurry (drawLineBetween cs True queryPos)
		let gl		= length groups `div` 2
		groups' & take gl & concat |+> annotatedDot cs True
		groups' & drop gl & concat |+> annotatedDot cs False
		pass



bestPos		:: W -> H -> [[Text]] -> [(Text, Text)] -> [[Text]]
bestPos w h groups conns
	= let	iter	= foldr (optimizeRow w h conns) groups [0..length groups - 1]
		in
		if iter == groups then iter
			else bestPos w h iter conns
		



optimizeRow	:: W -> H -> [(Text, Text)] -> Int -> [[Text]] -> [[Text]]
optimizeRow w h conns i ts
	= let	start		= take i ts
		(toOpt:end)	= drop i ts
		scoreOf' row	= scoreOf w h conns (start ++ [row] ++ end)
		optRow		= permutations toOpt & take 50 {-TODO remove this factor-} |> (scoreOf' &&& id)
					& sortOn fst & head & snd
		in
		start ++ [optRow] ++ end
		
		


scoreOf		:: W -> H -> [(Text, Text)] -> [[Text]] -> (Int, Float)
scoreOf w h conns ts	
	= let	poss		= positions w h ts |> filter (not . Text.null . fst)
		queryPos	= M.fromList $ concat poss
		in score queryPos conns

score			:: Map Text (X, Y) -> [(Text, Text)] -> (Int, Float)
score dict conns	= (crossings dict conns, distances dict conns)

distances		:: Map Text (X, Y) -> [(Text, Text)] -> Float
distances dict conns	= conns |> uncurry (distanceBetween' dict) & sum

crossings		:: Map Text (X, Y) -> [(Text, Text)] -> Int
crossings queryPos connections
	= (do	let lines	= connections |> lookupPoints queryPos
		l1	<- lines
		l2	<- lines
		return $ if linesIntersect l1 l2 then 1 else 0)
		& sum & (`div` 2)



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





