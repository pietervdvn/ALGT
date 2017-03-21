 {-# LANGUAGE TemplateHaskell #-}
module SyntaxHighlighting.ParseTreeImage where

{- Draws parsetrees as images -}

import Utils.Utils
import Utils.Image

import TypeSystem

import SyntaxHighlighting.Coloring

import Text.Blaze.Svg11 ((!), stringValue)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Maybe

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
		, _style	:: Maybe Name
		} deriving (Show)

makeLenses ''Point
data PPS	= PPS	{ _cs		:: ColorScheme
			, _fs		:: Int	-- fontsize
			, _hDiff	:: Y	-- size between layers
			, _currName	:: Text	-- current running name, for unique identification (used to draw the lines)
			, _conns	:: [(Text, Text)]	-- Connections
			} 
makeLenses ''PPS			

coor		:: Point -> (X, Y)
coor p		= (get x p, get y p)

parseTreeSVG	:: TypeSystem -> Int -> FullColoring -> ParseTree -> String
parseTreeSVG ts factor fc pt
	= let	pt'		= determineStyle' (get tsStyle ts) pt
		cs		= toSVGColorScheme "" fc
		((points, w, h), state)	
				= runState (pointPositions pt') $ startState cs
		pointsDict	= points |> (get name &&& (get x &&& get y))
					& M.fromList
		connections	= get conns state
		svg	= do	S.rect ! A.width (intValue w) ! A.height (intValue h) ! A.fill (stringValue $ get bg cs)
				connections |+> uncurry (drawLineBetween cs False pointsDict)
				points |+> renderPoint fc
				pass
		in 
		packageSVG (w*factor, h*factor) (w, h) svg


renderPoint	:: FullColoring -> Point -> S.Svg
renderPoint fc point
	= let cs	= fc & toSVGColorScheme (get style point & fromMaybe "") in
		annotatedDot cs (get showUnderDot point)
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


pointPositions	:: ParseTreeA (Maybe Name) -> State PPS ([Point], W, H)
pointPositions (MLiteral style _ content)
	= do	w	<- get' fs
		ds	<- get' (cs . dotSize)
		let w'	= w * length content
		n	<- get' currName
		let p	= Point n (Text.pack content)
				(w' `div` 2) (ds*2) True
				style
		return ([p], w', 2*(ds + w))
pointPositions (MInt style mi i)
	= pointPositions (MLiteral style mi $ show i)
pointPositions (PtSeq style mi@(tpName, choice) pts)
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
					style

		return (node:points', width, yDiff + maximum hs)


movePointsRelative	:: [W] -> Int -> [Point] -> [Point]
movePointsRelative widest index (top:children)
	= let	topPos	= sum (take index widest) + ((widest !! index) `div` 2)
		xDiff	= topPos - get x top
		in
		(top:children) |> over x (+ xDiff)


