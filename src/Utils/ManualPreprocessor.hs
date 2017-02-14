module Utils.ManualPreprocessor where

{- Takes variables, substitutes them in the manual -}

import System.Directory
import System.Process

import System.IO.Unsafe

import Utils.Utils
import Utils.ArgumentParser
import Utils.ToString
import Utils.PureIO hiding (writeFile, readFile, putStrLn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Utils.Version
import Data.Time.Clock
import Data.Time.Calendar
 
import Utils.CreateAssets
import Assets
import AssetsHelper
import PureMain

import Text.Parsec
import TypeSystem
import TypeSystem.Parser.TargetLanguageParser
import qualified TypeSystem.Parser.BNFParser as BNFParser
import qualified TypeSystem.Parser.ParsingUtils as ParsingUtils

import Control.Arrow

import Control.Concurrent
import Debug.Trace

import Lens.Micro hiding ((&))

buildVariables	:: Map String String
buildVariables
      = [ ("version", version & fst |> show & intercalate ".")
	, ("versionmsg", version & snd)
	, ("builtinEscapes", BNFParser.builtinEscapes	
				|> over (_1 . _1) (\c -> '\\':[c])
				& makeTable)
	, ("wsModeInfo", BNFParser.wsModeInfo & makeTable)
	, ("whitespace", ParsingUtils.whitespace |> (:[]) |> show |> verbatim & intercalate ",")
	, ("builtinSyntax", BNFParser.builtinSyntax |> over _1 fst |> unmerge3r
			|> (\(bnf, expl, regex) -> verbatim bnf ++ "|" ++ expl ++ "|" ++ verbatim regex)
			& unlines)
	, ("regexIdentifier", BNFParser.builtinSyntax 
		|> over _1 fst
		& lookup "Identifier" 
		& fromMaybe (error "BUG in manualpreprocessor: no Identifier regex for builtin")
		& snd)
	] & M.fromList 





makeTable	:: [((String, a), String)] -> String
makeTable vals
	= vals |> over _1 fst |> escapedTable & unlines

escapedTable	:: (String, String) -> String
escapedTable (inp, expl)
	= padR 16 ' ' (verbatim inp) ++ expl



manualAssets	:: IO (Map String String)
manualAssets
	= do	let files	= allAssets |> fst & filter ("Manual/Files/" `isPrefixOf`)
		contents	<- files |> (drop (length "Manual/Files/") &&&  readFile . ("src/Assets/"++)) |+> sndEffect
		contents & M.fromList & return




buildVariablesIO	:: IO (Map String String)
buildVariablesIO
	= do	(y,m,d)	<- getCurrentTime |> (toGregorian . utctDay)
		let date	= show y ++ "-"++show m ++ "-" ++ show d
		let dict'	= [("date", date)] & M.fromList
		assets	<- manualAssets
		return $ M.unions [dict', assets, buildVariables]



verbatim	:: String -> String
verbatim str	= "`" ++ str ++ "`"








genArg		:: Map String String -> String -> Either String (String, Map String String)
genArg vars ('$':'$':str)
	= do	let (name, (action, _))
				= break (\c -> not (isAlpha c || c =='/' || c == '.')) str
					|> options
		value		<- checkExists name vars ("No variable $$"++name)
		return (name, M.singleton name $ action value)
genArg _ str
	= return (str, M.empty)



preprocess	:: Map String String -> String -> Either String String
preprocess _ []	= return ""
preprocess vars ('$':'$':'(':str)
	= do	let (args, (action, rest))
				= break (==')') str 
					|> tail
					|> options
		
		(args', input)	<- args & words |> genArg vars & allRight |> unzip
		let (_, Just parsedArgs)
				= unsafePerformIO $ parseArgs ([-1::Int], "ManualPreprocessor tests") args'
		

		let output	= mainPure parsedArgs
					& runPureOutput (M.unions input)
					& removeCarriageReturns
					& get stdOut & unlines

		rest'	<- preprocess vars rest
		return (action output ++ rest')
preprocess vars ('$':'$':str)
	= do	let (name, (action, rest))
				= break (\c -> not (isAlpha c || c =='/' || c == '.')) str
					|> options
		value		<- checkExists name vars ("No variable $$"++name)
		rest'		<- preprocess vars rest
		return (action value ++ rest')
preprocess vars (ch:str)
	= do	rest	<- preprocess vars str
		return $ ch:rest


options	:: String -> (String -> String, String)

options ('!':'i':'n':'d':'e':'n':'t':rest)
	= let	(action, rest')	= options rest
		in
		(\str -> str & action & lines |> ("        "++) & intercalate "\n", rest')
options ('!':rest)
	= let	(option, str)	= span (`elem` "0123456789[].") rest
		pt	= parse (parseSyntax optionsSyntax "option") "Assets/Manual/Options.language" option
				& either (error . show) id
		action	= matchOptionBody pt
		(action', str')	= options str
		in (action' . action, str')
options str
	= (id, str)


matchOptionBody	:: ParseTree -> String -> String
matchOptionBody (PtSeq _ [parO, MInt _ i, MLiteral _ "..", MInt _ j, parC]) str
	= let	i'	= if inclusivePar parO then i-1 else i
		j'	= if not $ inclusivePar parC then j else j-1
		in
		str & lines & take j' & drop i' & unlines
matchOptionBody (PtSeq _ [parO, MInt _ i, MLiteral _ "..", parC]) str
	= let	i'	= if inclusivePar parO then i-1 else i
		action	= if not $ inclusivePar parC then id else init
		in
		str & lines & action & drop i' & intercalate "\n"

matchOptionBody (MInt _ i) str
	= let	lined	= lines str in
		if i <= length lined then lined !! (i-1)
			else error $ "To little lines to get nr "++show i++" out of\n"++(str & lines & mapi |> (\(i, l) -> padR 3 ' ' (show $ 1 + i) ++ l) & unlines)
matchOptionBody pt str
	= error $ "Can't handle option "++toParsable pt


inclusivePar	:: ParseTree -> Bool
inclusivePar (MLiteral _ "[")	= True
inclusivePar (MLiteral _ "]")	= False



preprocessTo	:: FilePath -> FilePath -> IO ()
preprocessTo inp outp
	= do	str		<- readFile inp
		vars		<- buildVariablesIO
		putStr $ "Processing "++inp++"..."
		let str'	= preprocess vars str & either error id
		writeFile outp str'
		putStrLn $ "\rProcessed "++inp++" and saved it as "++outp

preprocessDir	:: FilePath -> (FilePath -> FilePath) -> IO ()
preprocessDir fp destination
	= do	contents	<- dirConts fp
		let contents'	= contents & filter (".md" `isSuffixOf`)
		contents' |+> (\fp -> preprocessTo fp (destination fp))
		pass		 



outputFile	:: FilePath -> FilePath
outputFile fp
	= let	(name, path)	= fp & reverse & break (=='/')
					& mapBoth reverse
		in path++".bin/"++name

autoPreprocess	:: IO ()
autoPreprocess
	= do	preprocessDir "src/Assets/Manual" outputFile
		runCommand "src/Assets/Manual/build.sh"
		pass

contentsChanged	:: FilePath -> IO (Map FilePath UTCTime)
contentsChanged fp
	= do	files	<- dirConts fp
		files |> (id &&& getModificationTime) |+> sndEffect
			|> M.fromList


autoRecreate'	:: Map FilePath UTCTime ->  IO ()
autoRecreate' lastEdits
	= do	lastEdits'	<- contentsChanged "src/Assets/Manual"
		let millisecs	= 750
		-- let animation	= ["|","/","-","\\"] 
		let animations	= ["    .","    . ","  .  "," .   ",".    "] 
		let animation	= animations ++ reverse animations

		let animation'	= animation |> ("\r "++)
		let frameLength	= 1000 `div` length animation'
		if lastEdits == lastEdits' then do
			animation' |+> (\frame -> threadDelay (frameLength*millisecs) >> putStr frame)
			pass
		else
			autoPreprocess
		autoRecreate' lastEdits'

autoRecreate	= autoRecreate' M.empty
