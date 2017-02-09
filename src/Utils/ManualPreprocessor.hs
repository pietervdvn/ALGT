module Utils.ManualPreprocessor where

{- Takes variables, substitutes them in the manual -}

import System.Directory
import Utils.Utils
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Data.Char
import Utils.Version

import Utils.CreateAssets

buildVariables	:: Map String String
buildVariables
      = [ ("version", version & fst |> show & intercalate ".")
	, ("versionmsg", version & snd)
	] & M.fromList

buildVariablesIO	:: IO (Map String String)
buildVariablesIO
	= do	return buildVariables



preprocess	:: Map String String -> String -> Either String String
preprocess _ []	= return ""
preprocess vars ('$':'$':str)
	= do	let (name, rest)= break (not . isAlpha) str
		let name'	= name |> toLower 
		value		<- checkExists name' vars ("No variable $$"++name')
		rest'		<- preprocess vars rest
		return (value ++ rest')


preprocessTo	:: FilePath -> FilePath -> IO ()
preprocessTo inp outp
	= do	str		<- readFile inp
		vars		<- buildVariablesIO
		let str'	= preprocess vars str & either error id
		writeFile outp str'

preprocessDir	:: FilePath -> IO ()
preprocessDir fp
	= do	contents	<- dirConts fp
		let contents'	= contents & filter (".md" `isSuffixOf`)
		contents' |+> (\fp -> preprocessTo fp ("bin/"++fp))
		pass		 


autoPreprocess	:: IO ()
autoPreprocess
	= preprocessDir "src/Assets/Manual"


