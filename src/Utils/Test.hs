module Utils.Test where

import AbstractInterpreter.Test
import Main
import TypeSystem
import Utils.ToString

t	= do 	(ts, _)	<- main' ["../Examples/STFL.typesystem", "../Examples/STFL.example", "e"]
		-- testAS (tsSyntax ts)
		putStrLn $ toParsable ts
		return ()
