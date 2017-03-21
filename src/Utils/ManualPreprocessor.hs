module Utils.ManualPreprocessor where

{- Takes variables, substitutes them in the manual -}

import System.Directory
import System.Process
import System.IO.Unsafe

import Utils.Utils
import Utils.Version
import Utils.ArgumentParser
import Utils.ToString
import Utils.PureIO hiding (writeFile, readFile, putStrLn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Data.Time.Clock
import Data.Time.Calendar
import Data.Either
 
import Utils.CreateAssets
import Assets
import AssetsHelper
import PureMain

import Text.Parsec
import TypeSystem
import TypeSystem.Parser.TargetLanguageParser
import qualified TypeSystem.BNF as BNFParser
import qualified TypeSystem.Parser.ParsingUtils as ParsingUtils

import qualified SyntaxHighlighting.AsLatexPt as Latex
import qualified SyntaxHighlighting.Renderers as Renderers


import ParseTreeInterpreter.FunctionInterpreter as FuncInp

import Control.Arrow
import Control.Monad

import Control.Concurrent

import System.Random

import Lens.Micro hiding ((&))

buildVariables	:: Map String String
buildVariables
      = [ ("version", version & fst |> show & intercalate ".")
	, ("versionmsg", version & snd)
	, ("builtinEscapes", ParsingUtils.builtinEscapes	
				|> over (_1 . _1) (\c -> '\\':[c])
				& makeTable)
	, ("wsModeInfo", BNFParser.wsModeInfo & makeTable)
	, ("whitespace", ParsingUtils.whitespace |> (:[]) |> show |> verbatim & intercalate ",")
	, ("builtinSyntax", BNFParser.builtinSyntax |> over _1 fst3 |> unmerge3r
			|> (\(bnf, expl, regex) -> verbatim bnf ++ "|" ++ expl ++ "|" ++ verbatim regex)
			& unlines)
	, ("bnfKeywords", BNFParser.builtinSyntax |> fst |> fst3 & unlines)
	, ("regexIdentifier", BNFParser.builtinSyntax 
		|> over _1 fst3
		& lookup "Identifier" 
		& fromMaybe (error "BUG in manualpreprocessor: no Identifier regex for builtin")
		& snd)
	, ("expressionExamples", TypeSystem.expressionExamples
			|> (\(e, n, pat, expr) -> [verbatim (toParsable e), n, expr]
				& intercalate " | " )
			& unlines)
	, ("patternExamples", TypeSystem.expressionExamples
			|> (\(e, n, pat, expr) -> [verbatim (toParsable e), pat]
				& intercalate " | " )
			& unlines)
	, ("builtinFunctions", builtinFunctions
			& advancedTable (\bif -> [verbatim $ get bifName bif, get bifDescr bif
					, verbatim $ argText (get bifInArgs bif) (get bifResultType bif)
					]))
	, ("styles", AssetsHelper.knownStyles & M.keys |> (\str ->  str & reverse & drop 6 & reverse) & list)
	, ("stylesSupport", AssetsHelper.minimalStyleTypes |> (" - "++) & unlines)
	, ("styleMatrix", styleMatrix)
	, ("styleSupported", Renderers.allRenderers |> dropTrd3 & supportedTable)
	, ("variables", buildVariables & M.keys |> (" - "++) & unlines & ("Variables in the manual preprocessor are: "++))
	] & M.fromList 



supportedTable	:: [(String, [String])] -> String
supportedTable props
	= let	renderNames	= props |> fst
		header	= ("| Attribute":renderNames) & intercalate "\t|"
		header'	= "|:--------------" ++ concat (replicate (length renderNames) "|:-----:")
		possible	= props & unmerge |> swap & merge & M.fromList
		row renderers	= [ if known `elem` renderers then "✓" else "" | known <- renderNames ] & intercalate " | "	:: String
		possible'	= possible |> row & M.toList |> (\(k, v) -> verbatim k ++ "\t|" ++ v) :: [String]
		in
		([header, header'] ++ possible') & unlines


list		:: [String] -> String
list strs	= strs |> (" - "++) & unlines

argText		:: Either (TypeName, Int) [TypeName] -> TypeName -> String
argText (Left (inTp, nr)) resT
	= (inTp++" -> " >>= replicate nr) ++ inTp ++ "* -> " ++ resT
argText (Right tps) resT
	= (tps++[resT]) & intercalate " -> "


advancedTable	:: (a -> [String]) -> [a] -> String
advancedTable f as
		= as |> f |> intercalate "\t | " & unlines

makeTable	:: [((String, a), String)] -> String
makeTable vals
	= vals |> over _1 fst |> escapedTable & unlines

escapedTable	:: (String, String) -> String
escapedTable (inp, expl)
	= padR 16 ' ' (verbatim inp) ++ expl


latexTable	:: [String] -> [[String]] -> String
latexTable headers contents
	= let	header	= "\\begin{longtable}[c]{@{}"++replicate (length headers) 'l' ++ "@{}}\n\\toprule\n"
				++ intercalate " & " headers
				++ "\\tabularnewline\n\\midrule\\endhead"	:: String
		contents'	= contents |> intercalate " & " |> (++" \\\\\n") & concat	:: String
		bottom	= "\\bottomrule\n\\end{longtable}"
		in
		[header, contents', bottom] & unlines


styleMatrix	:: String
styleMatrix 
	= let	matrix	= AssetsHelper.minimalStyleTypes |> (\styleN -> styleN:renderStyleRow styleN)
		known	= AssetsHelper.knownStyles & M.keys |> reverse |> drop 6 |> reverse
				& ("Style":)
		in
		latexTable known matrix

renderStyleRow	:: Name -> [String]
renderStyleRow styleN
	= do	fc	<- AssetsHelper.knownStyles & M.elems
		return $ Latex.renderWithStyle fc styleN styleN
		



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








genArg		:: Map String String -> String -> IO (String, Map String String)
genArg vars ('$':'$':str)
	= do	let (name, (action, _))
				= break (\c -> not (isAlpha c || c =='/' || c == '.')) str
					|> options
		value		<- checkExists name vars ("No variable $$"++name) & either fail return
		return (name, M.singleton name $ action value)
genArg _ str
	= return (str, M.empty)


genArgs		:: Map String String -> String -> IO ([String], Map String String)
genArgs vars str
	= str & words |+> genArg vars |> unzip ||>> M.unions


runIsolated	= runIsolated' (++ " --plain --style WhiteFlat")

runIsolated'	:: (String -> String) -> (String, Int) -> (FilePath -> FilePath) -> Map String String -> String -> IO (Output, String -> String, String)
runIsolated' argEdit line target vars str
	= do	let (args, (action, rest))
				= break (==')') str 
					|> tail
					|> options
		putStrLn $ show line ++ " Running with input args "++show args
		(args', input)	<- genArgs vars (argEdit args)
		(_, Just parsedArgs)
				<- parseArgs ([-1::Int], "ManualPreprocessor run") args'
		let output	= mainPure parsedArgs
					& isolateFailure
					& runPureOutput defaultConfig (mkStdGen 0) input
					& removeCarriageReturns
					& removeUnchanged
		rest'	<- preprocess line target vars rest
		return (output, action, rest')


preprocess	:: (String, Int) -> (FilePath -> FilePath) -> Map String String -> String -> IO String
preprocess _ _ _ []	= return ""
preprocess line target vars ('$':'$':'!':'(':str)
	= do	
		(output', action, rest)		<- runIsolated' id line target vars str
		let output		= get stdOut output' & unlines
		return (output & action ++ rest)
preprocess line target vars ('$':'$':'(':str)
	= do	
		(output', action, rest)		<- runIsolated line target vars str
		let output		= get stdOut output' & unlines
		let wrapFile str	= "\\begin{lstlisting}[style=terminal]\n"++str++"\n\n\\end{lstlisting}"
		return (output & action & wrapFile ++ rest)
preprocess line destination vars ('$':'$':'s':'v':'g':'(':str)
	= do	(output, action, rest)	<- runIsolated line destination vars str
		let svgs	= output & changedFiles
					& M.toList
		svgs |+> (\(n, v) -> 
			do	fileExists	<- doesFileExist n
				unless fileExists $ writeFile (destination n) v
				putStrLn $ if fileExists then "Already exists " ++ destination n else  "Saved "++destination n
				)
		let dropExt s	= if ".svg" `isSuffixOf` s then s & reverse & drop 4 & reverse else error $ "File without svg extension: "++s
		let named	= svgs |> fst |> (\n -> "("++ dropExt n++".png)") & concat

		rest'	<- preprocess line destination vars rest
		return (action named ++ rest')

preprocess line target vars ('$':'$':str)
	= do	let (name, (action, rest))
				= break (\c -> not (isAlpha c || c =='/' || c == '.')) str
					|> options
		value		<- checkExists name vars ("No variable $$"++name) & either error return
		rest'		<- preprocess line target vars rest
		return (action value ++ rest')
preprocess (file, line) target vars ('\n':str)
	= do	rest	<- preprocess (file, line + 1) target vars str
		return $ '\n':rest
preprocess line target vars (ch:str)
	= do	rest	<- preprocess line target vars str
		return $ ch:rest





options	:: String -> (String -> String, String)
options('!':'k':'e':'y':'w':'o':'r':'d':rest)
	= let	(action, rest')	= options rest
		in
		(\str -> "      <keyword>" ++ str ++ "</keyword>", rest')
options ('!':'i':'n':'d':'e':'n':'t':rest)
	= let	(action, rest')	= options rest
		in
		(\str -> str & action & lines |> ("        "++) & intercalate "\n", rest')
options('!':'f':'i':'l':'e':rest)
	= let	(action, rest')	= options rest
		wrapFile str	= "\\begin{lstlisting}\n"++str++"\\end{lstlisting}\n"
		in
		(\str -> str & wrapFile & action, rest')
options ('!':rest)
	= let	(option, str)	= span (`elem` "0123456789[].,") rest
		pt	= parseTargetLang optionsSyntax "option" "Assets/Manual/Options.language" option
				& either error id
		action	= matchOptionBody pt
		(action', str')	= options str
		in (action' . action, str')
options str
	= (id, str)



matchOptionBody	:: ParseTree -> String -> String
matchOptionBody (PtSeq _ _ [body, MLiteral _ _ ",", body']) str
	= let	action	= matchOptionBody body
		action'	= matchOptionBody body'
		in
		action str ++ "\n" ++ action' str
matchOptionBody (PtSeq _ _ [parO, MInt _ _ i, MLiteral _ _ "..", MInt _ _ j, parC]) str
	= let	i'	= if inclusivePar parO then i-1 else i
		j'	= if not $ inclusivePar parC then j else j-1
		in
		str & lines & take j' & drop i' & unlines
matchOptionBody (PtSeq _ _ [parO, MInt _ _ i, MLiteral _ _ "..", parC]) str
	= let	i'	= if inclusivePar parO then i-1 else i
		action	= if not $ inclusivePar parC then id else init
		in
		str & lines & action & drop i' & intercalate "\n"

matchOptionBody (MInt _ _ i) str
	= let	lined	= lines str in
		if i <= length lined then lined !! (i-1)
			else error $ "To little lines to get nr "++show i++" out of\n"++(str & lines & mapi |> (\(i, l) -> padR 3 ' ' (show $ 1 + i) ++ l) & unlines)
matchOptionBody pt str
	= error $ "Can't handle option "++toParsable pt


inclusivePar	:: ParseTree -> Bool
inclusivePar (MLiteral _ _ "[")	= True
inclusivePar (MLiteral _ _ "]")	= False











preprocessTo	:: FilePath -> (FilePath -> FilePath) -> (FilePath -> FilePath) -> IO ()
preprocessTo inp destination svgDestination
	= do	str		<- readFile inp
		vars		<- buildVariablesIO
		putStrLn $ "Processing "++inp++"..."
		str'		<- preprocess (inp, 1) svgDestination vars str
		writeFile (destination inp) str'
		putStrLn $ "\rProcessed "++inp++" and saved it as "++destination inp


preprocessDir	:: FilePath -> (FilePath -> FilePath) -> (FilePath -> FilePath) -> IO ()
preprocessDir fp destination svgDestination
	= do	contents	<- dirConts fp
		let contents'	= contents & filter (".md" `isSuffixOf`)
		contents' |+> (\fp -> preprocessTo fp destination svgDestination)
		pass		 

outputFile'	:: FilePath -> FilePath -> FilePath
outputFile' path name
	= path++".bin/"++name


outputFile	:: FilePath -> FilePath
outputFile fp
	= let	(name, path)	= fp & reverse & break (=='/')
					& mapBoth reverse
		in path++".bin/"++name

autoPreprocess	:: IO ()
autoPreprocess
	= do	let path	= "src/Assets/Manual/"
		runCommand "src/Assets/Manual/prebuild.sh"
		preprocessDir path (\nm -> outputFile path ++ (reverse nm & takeWhile (/= '/') & reverse )) (outputFile' path)
		runCommand "src/Assets/Manual/build.sh"
		pass

contentsChanged	:: FilePath -> IO (Map FilePath UTCTime)
contentsChanged fp
	= do	files	<- dirConts fp
		files 	& filter (not . (".bin" `isInfixOf`))
			& filter (not . ("/Output/" `isInfixOf`))
			|> (id &&& getModificationTime) |+> sndEffect
			|> M.fromList


diff		:: (Ord k, Eq v) => Map k v -> Map k v -> Map k (v, v)
diff m0 m1	= M.intersectionWith (,) m0 m1 & M.filter (uncurry (/=))

autoRecreate'	:: Map FilePath UTCTime ->  IO ()
autoRecreate' lastEdits
	= do	lastEdits'	<- contentsChanged "src/Assets/Manual"
		let millisecs	= 750
		-- let animation	= ["|","/","-","\\"] 
		let animations	= ["|","/","-","\\"] 
		let animation	= animations -- ++ reverse animations

		let animation'	= animation |> ("\r "++)
		let frameLength	= 1000 `div` length animation'
		if lastEdits == lastEdits' then do
			animation' |+> (\frame -> threadDelay (frameLength*millisecs) >> putStr frame)
			pass
		else do
			putStrLn $ "CHANGED: "++ show (M.toList $ diff lastEdits lastEdits')
			autoPreprocess
		autoRecreate' lastEdits'

autoRecreate	= autoRecreate' M.empty
