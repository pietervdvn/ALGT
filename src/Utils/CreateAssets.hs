module CreateAssets where

{-
This module defines a small tool, creating asset files
-}

import System.Directory
import Data.List
import Data.Bifunctor
import qualified Data.Map as M
import Data.Foldable

import Control.Monad

(|>)		= flip fmap
(|+>)		= forM
(&)		= flip ($)

dirConts	:: FilePath -> IO [FilePath]
dirConts fp 
	= do	files	<- getDirectoryContents fp 
		let files'	= files & delete "." & delete ".." |> ((fp++"/") ++)
		print files'
		mode		<- files' |+> doesDirectoryExist	:: IO [Bool]
		let (directories', normalFiles')	
				=  zip files' mode & partition snd 	:: ([(FilePath, Bool)], [(FilePath, Bool)])
		let directories	= directories' |> fst
		let normalFiles	= normalFiles' |> fst
		recursive	<- directories |+> dirConts
		return $ normalFiles ++ concat recursive
		

replacements	= M.fromList [('.', '_'), ('-', '_'), ('/', '_')]


replace c	= M.findWithDefault c c replacements

name		:: String -> String
name fp		= fp |> replace & ("_"++)


header dev
	= "module Assets where"++ (if dev then "\n\nimport System.IO.Unsafe (unsafePerformIO)" else "")
		++"\n\n-- Automatically generated\n-- This file contains all assets, loaded via 'unsafePerformIO' or hardcoded as string, not to need IO for assets\n\n\n"

fileLine	:: Bool -> FilePath -> String -> IO String
fileLine dev origDir file
	= do	let name'	= drop (1 + length origDir) $ name file
		let pragma	= if dev then "{-# NOINLINE "++name'++" #-}\n" else ""
		let devAssgn	= "unsafePerformIO $ readFile "++show file
		contents	<- if dev then return devAssgn else
					fmap show (readFile file)
		return $ pragma ++ name' ++ "\t = "++contents

createAssets'	:: Bool -> FilePath -> IO String
createAssets' dev fp
	= do	files		<- dirConts fp
		contents	<- files |+> fileLine dev fp
		return $ header dev ++ unlines contents

createAssets	:: Bool -> FilePath -> FilePath -> IO ()
createAssets dev fp target
	= do	contents	<- createAssets' dev fp
		writeFile target contents

