 {-# LANGUAGE TemplateHaskell #-}
module Utils.ParseTreeImage where

{- Draws parsetrees as images -}

import Utils.Utils
import Utils.Image

import TypeSystem

import Text.Blaze.Svg11 ((!), stringValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.Map as M

import Lens.Micro hiding ((&))
import Lens.Micro.TH

import Control.Monad
import Control.Monad.State hiding (get)

import Control.Arrow ((&&&))

data Point	= Point 
		{ _name		:: Text
		, _contents	:: Text
		, _x		:: X
		, _y		:: Y
		, _showUnderDot	:: Bool
		} deriving (Show)

makeLenses ''Point
data PPS	= PPS	{ _cs		:: ColorScheme
			, _fs		:: Int	-- fontsize
			, _hDiff	:: Y	-- size between layers
			, _currName	:: Text	-- current running name, for unique identification
			, _conns	:: [(Text, Text)]	-- Connections
			} 
makeLenses ''PPS			

coor		:: Point -> (X, Y)
coor p		= (get x p, get y p)

parseTreeSVG	:: Int -> ColorScheme -> ParseTree -> String
parseTreeSVG factor cs pt
	= let	((points, w, h), state)	= runState (pointPositions pt) $ startState cs
		pointsDict	= points |> (get name &&& (get x &&& get y))
					& M.fromList
		connections	= get conns state
		svg	= do	S.rect ! A.width (intValue w) ! A.height (intValue h) ! A.fill (stringValue $ get bg cs)
				points |+> renderPoint cs
				connections |+> uncurry (drawLineBetween cs False pointsDict)
				pass
		in 
		packageSVG (w*factor, h*factor) (w, h) svg


renderPoint	:: ColorScheme -> Point -> S.Svg
renderPoint cs point
	= annotatedDot cs (get showUnderDot point)
		(get contents point, (get x point, get y point))





startState cs
	= let fs	= get fontSize cs in
		PPS cs fs (fs * 3) (Text.pack "_") []

get'	= gets . get

withName	:: Int -> State PPS a -> State PPS a
withName i st	= do	oldName	<- get' currName
			let newName	= mappend oldName $ Text.pack ("." ++ show i)
			modify (set currName newName)
			a	<- st
			modify (set currName oldName)
			return a


pointPositions	:: ParseTree -> State PPS ([Point], W, H)
pointPositions (MLiteral _ content)
	= do	w	<- get' fs
		ds	<- get' (cs . dotSize) 
		let w'	= w * length content
		n	<- get' currName
		let p	= Point n (Text.pack content)
				(w' `div` 2) (ds*2) True
		return ([p], w', 2*(ds + w))
pointPositions (MIdentifier mi nm)
	= pointPositions (MLiteral mi ("<"++nm++">"))
pointPositions (MInt mi i)
	= pointPositions (MIdentifier mi $ show i)
pointPositions (PtSeq mi@(tpName, choice) pts)
	= do	(pointss, ws, hs)
			<- pts	|> pointPositions
				 & mapi |+> uncurry withName
				|> unzip3

		nm		<- get' currName
		let conns'	= zip (repeat nm) $ pointss |> head |> get name
		modify (over conns (conns' ++))


		yDiff		<- get' hDiff
		let points'	= pointss 
					& mapi |> uncurry (movePointsRelative ws)
					& concat
					|> over y (yDiff +)

		h		<- get' fs
		ds		<- get' $ cs . dotSize
		let width	= sum ws
		let node	= Point nm (Text.pack $ tpName ++ "." ++ show choice)
					(width `div` 2)
					(2 * (ds + h))
					False

		return (node:points', width, yDiff + maximum hs)


movePointsRelative	:: [W] -> Int -> [Point] -> [Point]
movePointsRelative widest index (top:children)
	= let	topPos	= sum (take index widest) + ((widest !! index) `div` 2)
		xDiff	= topPos - get x top
		in
		(top:children) |> over x (+ xDiff)


