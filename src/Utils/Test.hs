module Utils.Test where

import AbstractInterpreter.Test
import Main
import Utils.Utils
import TypeSystem
import Utils.ToString

import Changer.Changes
import Changer.ChangesParser



t	= do 	(ts, _)	<- main' ["../Examples/STFL.typesystem", "../Examples/STFL.example", "e"]
		changes'<- parseChangesFile ts "../Examples/STFL.typesystem-changes"
		(changes,ts')	<- changes' & either (error . show) return

		print changes

		writeFile "../Output/GTFL.typesystem" $ toParsable ts'
		putStrLn "Gradualized a bunch of stuff!"
		return ()
