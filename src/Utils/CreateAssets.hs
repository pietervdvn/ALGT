module CreateAssets where

{-
This module defines a small tool, creating asset files
-}

import System.Directory
import Utils.Utils
import Data.List
import Data.List.Utils (replace)
import Data.Bifunctor
import Data.Foldable

import Control.Monad


dirConts	:: FilePath -> IO [FilePath]
dirConts fp 
	= do	files	<- getDirectoryContents fp |> delete "." |> delete ".."
		let files'	= files |> ((fp++"/") ++)
		print files'
		mode		<- files' |+> doesDirectoryExist	:: IO [Bool]
		let (directories', normalFiles')	
				=  zip files' mode & partition snd 	:: ([(FilePath, Bool)], [(FilePath, Bool)])
		let directories	= directories' |> fst
		let normalFiles	= normalFiles' |> fst
		putStrLn ("Found asset files: "++ showComma normalFiles)
		putStrLn ("Directories: "++showComma directories)
		recursive	<- directories |+> dirConts
		return $ normalFiles ++ concat recursive
		

replacements	= [(".", "_"), ("-", "_"), ("/", "_")]
name fp		= foldr (uncurry replace) fp replacements & ("_"++)


header dev
	= "module Assets where"++ (if dev then "\n\nimport System.IO.Unsafe (unsafePerformIO)" else "")
		++"\n\n-- Automatically generated\n-- This file contains all assets, loaded via 'unsafePerformIO' or hardcoded as string, not to need IO for assets\n\n\n"

fileLine	:: Bool -> FilePath -> String -> IO String
fileLine dev origDir file
	= do	let name'	= drop (1 + length origDir) $ name file
		let pragma	= if dev then "{-# NOINLINE "++name'++" #-}\n" else ""
		let devAssgn	= "unsafePerformIO $ readFile "++show file
		contents	<- if dev then return devAssgn else
					readFile file |> show
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

