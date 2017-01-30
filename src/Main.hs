module Main (module M, main) where

import PureMain as M
import Utils.Test as M
import Utils.CreateAssets as M
import Utils.ArgumentParser

import Control.Monad

import System.Environment


main	:: IO ()
main	= do	args	<- getArgs
		main' testAll args
		return ()

