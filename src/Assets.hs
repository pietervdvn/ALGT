module Assets where

import System.IO.Unsafe (unsafePerformIO)

-- Automatically generated
-- This file contains all assets, loaded via 'unsafePerformIO' or hardcoded as string, not to need IO for assets


TestFile_txt	 = unsafePerformIO $ readFile "Assets/TestFile.txt"
TestAsset_Txt	 = unsafePerformIO $ readFile "Assets/TestAsset.Txt"
Recursive_RecDoc	 = unsafePerformIO $ readFile "Assets/Recursive/RecDoc"
