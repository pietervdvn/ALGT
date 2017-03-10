module AbstractInterpreter.QuickCheck where

{-  -}

import Utils.Utils
import Utils.ToString

import TypeSystem
import AbstractInterpreter.AbstractSet

import ParseTreeInterpreter.PropertyTester
import ParseTreeInterpreter.FunctionInterpreter

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import System.Random
import qualified Control.Monad.State as State
import Control.Monad.State hiding (get)

import Control.Arrow ((&&&))

data PropFail	= PropFail
	{ _pfPropN	:: Name
	, _pfMsg	:: String
	, _pfAssgn	:: VariableAssignments
	} deriving (Show)


instance ToString PropFail where
	toParsable (PropFail nm msg assgn)
		= let 	assgnMsg	= assgn |> fst |> toParsable |> (" = "++) & M.toList |> uncurry (++) & unlines in
			"# Property "++show nm++" failed with assignment:\n"++indent assgnMsg++"\nBecause:\n"++indent msg



quickCheckProps	:: (RandomGen random) => Int -> TypeSystem -> Name -> Int -> random -> Either String ([VariableAssignments], [PropFail])
quickCheckProps numberOfTests ts propN depth r
	= take numberOfTests (genRandoms r) |+> quickCheckProp ts propN depth |> unzip ||>> catMaybes

quickCheckProp	:: (RandomGen random) => TypeSystem -> Name -> Int -> random -> Either String (VariableAssignments, Maybe PropFail)
quickCheckProp ts propName depth random
	= do	prop	<- checkPropertyExists ts propName
		let assgn	= neededVars prop
					||>> randomParseTree (get tsSyntax ts) depth random
					||>> fst ||>> (id &&& const Nothing)
					& M.fromList 	:: VariableAssignments
		return (assgn, case testPropOn ts prop assgn of
			Left msg	-> Just $ PropFail propName msg assgn
			Right _		-> Nothing)

randomParseTrees		:: (RandomGen random) => Int -> Syntax -> TypeName -> Int -> random -> ([ParseTree], random)
randomParseTrees neededPts s tn depth
	= runState (replicateM neededPts $ _generateRandomPT s depth tn)

randomParseTree		:: (RandomGen random) => Syntax -> Int -> random -> TypeName -> (ParseTree, random)
randomParseTree s depth random tn
	= runState (_generateRandomPT s depth tn) random


_generateRandomPT	:: (RandomGen random) => Syntax -> Int -> TypeName -> State random ParseTree
_generateRandomPT s depth tn
	= do	rInt	<- getR
		intChoice	<- selectRand ([3,4,5,6,7,8,9,10,32,64,100,128,256,512]	:: [Int])
		let ints	= [0,1,2, intChoice, rInt]
		-- selects given builtin ++ choices
		rands		<- builtinSyntax |> fst |> dropSnd3 |+> selectRand'
		let bi nm	= rands & lookup nm
					& maybe (error $ "No such builtin: "++nm) (:[])
		
		let ass		= generateAbstractSet s "" tn & unfoldDepth s 3
		as		<- selectRand ass
		let pts		= toParsetree s (ints, bi) as
		selectRand pts			


getR		:: (RandomGen random) => State random Int
getR	= do	r	<- State.get
		let (i, r') = next r
		put r'
		return i

selectRand'	:: (RandomGen random) => (x, [a]) -> State random (x, a)
selectRand' (x, as)
	= do	r	<- getR
		return (x, as !! (r `mod` length as))


selectRand	:: (RandomGen random) => [a] -> State random a
selectRand as	= selectRand' ((), as) |> snd
