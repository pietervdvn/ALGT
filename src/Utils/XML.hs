module Utils.XML where

import Data.List (intercalate)

type Tag 	= String
type XML	= String

data Attr	= BA String Bool
		| SA String String
	


xmlHeader	= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n"

instance Show Attr where
	show (SA n s)
		= n++"="++show s
	show (BA n b)
		= show (SA n $ if b then "true" else "false")

_renderAttrs	:: [Attr] -> String
_renderAttrs attrs
		= " "++ unwords (map show attrs)

comment		:: String -> XML
comment ""	= ""
comment str	= "<!-- "++str++" -->"

indent		:: String -> String
indent str	= intercalate "\n" $ map ("  "++) $ lines str

open		:: Tag -> XML
open tag	=  "<" ++ tag ++ ">"

open'		:: Tag -> [Attr] -> XML
open' tag attrs	= open $ tag ++ _renderAttrs attrs

close		:: Tag -> Tag
close tag	= "</"++ tag ++">"

attrTag		:: Tag -> [Attr] -> Tag
attrTag tag attrs
		=  "<"++tag++ _renderAttrs attrs++" />"



inT		:: Tag -> String -> XML
inT tag cont 
		=  open tag ++ "\n" ++ indent cont ++ "\n" ++ close tag

inLT		:: Tag -> String -> XML
inLT tag cont 
		=  open tag ++ cont ++ close tag


inT'		:: Tag -> [Attr] -> String -> XML
inT' tag attrs cont
		=  open' tag attrs ++ "\n" ++ indent cont ++ "\n" ++ close tag

inLT'		:: Tag -> [Attr] -> String -> XML
inLT' tag attrs cont
		=  open' tag attrs ++ cont ++ close tag

unlines' strs	= unlines $ filter (not . null) strs
