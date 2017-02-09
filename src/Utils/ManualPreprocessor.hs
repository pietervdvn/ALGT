module Utils.ManualPreprocessor where

{- Takes variables, substitutes them in the manual -}

import System.Directory
import System.Process

import Utils.Utils
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Data.Char
import Utils.Version
import Data.Time.Clock
import Data.Time.Calendar
 
import Utils.CreateAssets
import Assets

buildVariables	:: Map String String
buildVariables
      = [ ("version", version & fst |> show & intercalate ".")
	, ("versionmsg", version & snd)
	] & M.fromList


manualAssets	
	= allAssets & filter (("Manual/Files/" `isPrefixOf`) . fst)







buildVariablesIO	:: IO (Map String String)
buildVariablesIO
	= do	(y,m,d)	<- getCurrentTime >>= return . toGregorian . utctDay
		let date	= show y ++ "-"++show m ++ "-" ++ show d
		let dict'	= [("date", date)] & M.fromList
		return $ M.union dict' buildVariables

















preprocess	:: Map String String -> String -> Either String String
preprocess _ []	= return ""
preprocess vars ('$':'$':str)
	= do	let (name, rest)= break (not . isAlpha) str
		let name'	= name |> toLower 
		value		<- checkExists name' vars ("No variable $$"++name')
		rest'		<- preprocess vars rest
		return (value ++ rest')
preprocess vars (ch:str)
	= do	rest	<- preprocess vars str
		return $ ch:rest


preprocessTo	:: FilePath -> FilePath -> IO ()
preprocessTo inp outp
	= do	str		<- readFile inp
		vars		<- buildVariablesIO
		let str'	= preprocess vars str & either error id
		writeFile outp str'
		putStrLn $ "Processed "++inp++" and saved it as "++outp

preprocessDir	:: FilePath -> (FilePath -> FilePath) -> IO ()
preprocessDir fp destination
	= do	contents	<- dirConts fp
		let contents'	= contents & filter (".md" `isSuffixOf`)
		contents' |+> (\fp -> preprocessTo fp (destination fp))
		pass		 



autoPreprocess	:: IO ()
autoPreprocess
	= do	preprocessDir "src/Assets/Manual" outputFile
		runCommand "src/Assets/Manual/build.sh"
		pass

outputFile	:: FilePath -> FilePath
outputFile fp
	= let	(name, path)	= fp & reverse & break (=='/')
					& mapBoth reverse
		in path++".bin/"++name
