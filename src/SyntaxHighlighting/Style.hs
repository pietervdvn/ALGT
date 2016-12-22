module SyntaxHighlighting.Style where

{-
This module defines a GTK-sourceview style
-}
import Utils.Utils
import Utils.ToString
import Utils.XML
import Assets
import System.Directory
import Control.Monad


data Color	= CA {colorName :: Name, colorValue :: String}
	deriving (Show)
data StyleElem	= MapTo {styleName :: Name, styleRef :: Name}
		| StyleElem {styleName :: Name, styleAttrs :: [Attr]}
	deriving (Show)


data Style	=
	Style	{ name	:: Name
		, desc	:: String
		, humanName	:: String
		, author	:: [Name]
		, colors	:: [Color]
		, styles	:: [StyleElem]
		} deriving (Show)



deployStyle	:: Style -> IO ()
deployStyle style
	= do	home	<- getHomeDirectory
		check style & either error return
		writeFile (home ++ "/.local/share/gtksourceview-3.0/styles/"++name style++".xml") $ render style

-- TODO actually use these tests!
instance Check Style where
	check style
		= inMsg ("While checking the style "++show (name style)) $
		  do	let checks = checkHasDefaultStyles style : (styles style |> checkStyleElem style)
			checks & allRight_		


checkHasDefaultStyles	:: Style -> Either String ()
checkHasDefaultStyles style
	= inMsg "While checking if all needed default tags are present" $
	  let 	expected	= Assets._GTKSourceViewOptions_DefaultStyleElems
					& validLines |> words |> head	:: [String]
		in expected |> ("def:"++) |> checkHasStyleElem style & allRight_

checkStyleElem	:: Style -> StyleElem -> Either String ()
checkStyleElem style (MapTo s target)
	= inMsg ("While checking the target of style "++show s) $
		checkHasStyleElem style target
checkStyleElem _ _
	= return ()


checkHasStyleElem	:: Style -> String -> Either String ()
checkHasStyleElem style name
	= unless (name `elem` knownStyles style) $ Left $
		"Style "++name++" not defined in this style"


checkHasColor	:: String -> Style -> Either String ()
checkHasColor color style
	= unless (color `elem` knownColors style) $ Left $ "Color "++color++" not defined in this style"




knownColors style
	= style & colors |> colorName
knownStyles style
	= style & styles |> styleName


instance ToString Style where
	toParsable style
		= xmlHeader ++ render style
			 

instance ToString Color where
	toParsable (CA n v)
		= attrTag "color" [SA "name" n, SA "value" v]

instance ToString StyleElem where
	toParsable (MapTo id target)
		= attrTag "style" [SA "name" id, SA "use-style" target]
	toParsable (StyleElem n attrs )
		= attrTag "style" $ SA "name" n:attrs

render	:: Style -> XML
render (Style name desc humanName authors colors styles)
	= inT' "style-scheme" [SA "id" name, SA "_name" humanName, SA "version" "1.0"] $
			(authors |> inLT "author" & unlines) ++ "\n"++
			inLT "_description" desc ++ "\n\n"++
			(colors |> toParsable & unlines)  ++ "\n\n" ++
			(styles |> toParsable & unlines)



t

