module Utils.ToStringExtra where

{-
This module defines a few extra data structures usefull for toString
-}

import Utils.Utils

-- Show MInfo tuples
showTI ("", _)	= ""
showTI (mt, -1) = ": "++mt
showTI (mt, i)	= ": "++mt++"."++show i

data ShowParens	= Parens | NoParens | NotOnRoot
	deriving (Show)

deepen NotOnRoot	= Parens
deepen p		= p

least Parens p		= p
least NotOnRoot	Parens	= NotOnRoot
least NotOnRoot p	= p
least NoParens _	= NoParens

inParens' Parens	= inParens
inParens' _		= id

