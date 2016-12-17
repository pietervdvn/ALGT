module Utils.Test where

import AbstractInterpreter.Test
import Main
import TypeSystem

t	= do 	(ts, _)	<- main' ["../Examples/STFL.typesystem", "../Examples/STFL.example", "e"]
		-- testAS (tsSyntax ts)
		print ts
		return ()
