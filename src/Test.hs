module Test where

{-
This module defines testing of STFL.example, used to dev against
-}

import Utils

import Control.Monad

import TypeSystem

import Parser.TypeSystemParser
import Parser.TargetLanguageParser
import Parser.ExpressionParser
import Parser.FunctionParser
import FunctionInterpreter
import RuleInterpreter

import Text.Parsec

import Data.Maybe
import Data.Either
import Data.Map (Map, fromList, (!))

import Main


	
tf	= do	(ts, examples)	<- main' ["../Examples/STFL.typesystem","../Examples/STFL.example", "e","eval","--step","--line-by-line"]
		print ts
		putStrLn "\n\n=====================================================================================\n\n"
		proofExamples ts examples [] "~~>" "EvaluationTrees.txt"
		proofExamples ts examples [MLiteral ("typingContext", 1) ";"] "|-" "TypingTrees.txt"

proofExamples ts examples args symbol file
	= do	let proofs	= examples |> (:[]) |> (args++) |> proofThat' ts symbol	:: [Either String Proof]
		let shown	= proofs |> showProofWithDepth
		let pretty	= zip examples shown >>= (\(ex, proof) -> "> " ++ show ex ++ "\n"++ proof)
		writeFile ("../Output/"++file) pretty


t	= tf


showProofWithDepth		:: Either String Proof -> String
showProofWithDepth (Left str)	= str
showProofWithDepth (Right proof)= "(Proof weight: "++show (weight proof)++", proof depth: "++ show (depth proof) ++")\n\n"++show proof++"\n\n\n"



