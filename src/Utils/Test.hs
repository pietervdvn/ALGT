module Utils.Test where

import AbstractInterpreter.Test
import Main
import Utils.Utils
import TypeSystem
import Utils.ToString

import Gradualization.Gradualization
import Gradualization.Parser.GradualizationParser



t	= do 	(ts, _)	<- main' ["../Examples/STFL.typesystem", "../Examples/STFL.example", "e"]
		grad'	<- parseGradualizationFile ts "../Examples/STFL.gradualization"
		grad	<- grad' & either (error . show) return

		print grad

		ts'	<- gradualize  grad ts & either error return
		writeFile "../Output/GTFL.typesystem" $ toParsable ts'
		putStrLn "Gradualized a bunch of stuff!"
		return ()
