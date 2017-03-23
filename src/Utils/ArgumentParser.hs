module Utils.ArgumentParser (parseArgs, Args(..), ExampleFile(..), Config(..), NeedsFiles(..), DynamizeArgs(..), getConfigFile, writeConfigFile, removeConfigFile, actionSpecified)where

{-
This module defines parsing of the arguments and reading/writing of the config file
-}

import TypeSystem
import AssetsHelper as AH

import Utils.Utils
import Utils.PureIO hiding (writeFile, readFile, putStrLn)

import Data.Monoid ((<>))
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Data.Maybe
import qualified Data.Map as M

import Control.Monad

import Options.Applicative

import System.Directory
import System.Exit

import Assets
import qualified Data.ByteString as B




descriptionText	= "ALGT (Automated Language Generation Tool)- automatically parse, interpret and proof properties of aribtrary languages.\n\n"++
			"This tool parses a '.language'-file, where the syntax of your language is defined. With this, your target file is parsed."++
			"In the language file, rewrite rules (or functions) can be defined - or better, rules defining properties can be defined."++
			"These can be applied to your target language to interpret, typecheck or proof some other property."

showVersion (v, vId)	= (v |> show & intercalate ".") ++", "++show vId
headerText v
		= "Automated Language Generation Tool (version "++ showVersion v ++" )"


class ActionSpecified a where
	actionSpecified	:: a -> Bool


data MainArgs	= MainArgs 
			{ showVersionNr	:: () -> ()
			, manualPDF	:: Bool
			, manualHTML	:: Bool
			, realArgs	:: Maybe Args
			, runTests	:: Bool
			, saveGTKSourceview	:: Bool
			, styles	:: Bool
			}

instance ActionSpecified MainArgs where
	actionSpecified args
		= [realArgs] |> (args &) |> isJust & or
			|| [manualPDF, manualHTML, runTests] |> (args &) & or

data Args = Args 	{ tsFile		:: String
			, exampleFiles		:: [ExampleFile]
			, changeFiles		:: [FilePath]
			, dumpTS		:: Bool
			, noCheck		:: Bool
			, numberOfQuickChecks	:: String
			, interpretAbstract	:: Bool
			, interpretFunctionAbs	:: [String]
			, interpretRulesAbstract:: Bool
			, interpretRules	:: [String]
			, subtypingSVG		:: Maybe String
			, iraSVG		:: Maybe String
			, dynamizeArgs		:: Maybe DynamizeArgs
			, styleName		:: Name
			, styleCSS		:: Bool
			, interactiveArg	:: Maybe Symbol
			, noMakeupArg		:: Bool
			, shortProofs		:: Maybe String
			}
	deriving (Show)

instance NeedsFiles Args where
	filesNeeded args	
		= let	exFiles		= exampleFiles args >>= filesNeeded
			styleFile	= [styleName args | ".style" `isSuffixOf` styleName args]
			in tsFile args : (changeFiles args ++ exFiles ++ styleFile)

instance ActionSpecified Args where
	actionSpecified args
		= [interpretFunctionAbs, interpretRules] 		|> (args &) |> (not . null) & or
		  	|| [exampleFiles]				|> (args &) |> (not . null) & or
			|| [subtypingSVG, iraSVG] 	|> (args &) |> isJust & or
			|| [dynamizeArgs] 		|> (args &) |> isJust & or
			|| [dumpTS, interpretAbstract, interpretRulesAbstract, styleCSS]	|> (args &) & or

data ExampleFile	= ExFileArgs
	{ fileName	:: FilePath
	, parser	:: Name
	, lineByLine	:: Bool
	, ruleSymbol	:: Maybe Symbol
	, function	:: Maybe Name
	, stepByStep	:: Maybe Name
	, testProp	:: Maybe Name
	, testAllProps	:: Bool
	, verbose	:: Bool
	, ptSvg		:: Maybe Name
	, renderHTML	:: Bool
	, renderHTMLNoCss	:: Bool
	, renderLatex	:: Bool
	, renderParts	:: Bool
	} deriving (Show)

data DynamizeArgs	= DynamizeArgs
	{ typeRule	:: TypeName
	, stuckError	:: String
	, relsToErr	:: [Symbol]
	, relsToAdd	:: [Symbol]
	} deriving (Show)



instance ActionSpecified ExampleFile where
	actionSpecified args
		= [testAllProps, renderHTML, renderLatex, renderHTMLNoCss, renderParts] |> (args &) & or
			|| ([ruleSymbol, function, stepByStep, testProp, ptSvg] |> (args &) |> isJust & or)

	
instance NeedsFiles ExampleFile where
	filesNeeded exfile	= [fileName exfile | fileName exfile /= "."]


data Config	= Config 	
	{ autoSyntaxes	:: [Int] }
		deriving (Show, Read, Eq)

emptyConfig
	= Config []


parseArgs	:: ([Int], String) -> [String] -> IO (Bool, Maybe Args)
parseArgs version strs	
	= do	let result	= execParserPure defaultPrefs (parserInfo version) strs
		MainArgs doShowVersion saveManPDF saveManHTML args runTests saveGTKSourceview styles
				<- handleParseResult result
		return $ doShowVersion ()
		when saveManPDF $ do
			B.writeFile "ALGT_Manual.pdf" _Manual_Output_ALGT_Manual_pdf
			putStrLn "Manual saved as ALGT_Manual.pdf"
			exitSuccess
		when saveManHTML $ do
			writeFile "ALGT_Manual.html" _Manual_Output_ALGT_Manual_html
			putStrLn "Manual saved as ALGT_Manual.html"
			exitSuccess
		when saveGTKSourceview $ do
			writeFile "language.lang" _language_lang
			putStrLn "Saved 'language.lang' to current directory. Move it to: '~/.local/share/gtksourceview-3.0/language-specs' to enable highlighting"
			exitSuccess
		when styles $ do
			putStrLn $ AH.knownStyles & M.keys & unlines
			exitSuccess
		return (runTests, args)



dataPath	= getXdgDirectory XdgData "ALGT"
configPath	= dataPath |> (++"/config")

getConfigFile	:: IO Config
getConfigFile	
	= do	fp	<- configPath
		exists	<- doesFileExist fp
		if exists then
			readFile fp |> read
		else
			return emptyConfig

writeConfigFile	:: Config -> IO ()
writeConfigFile config
 | config == emptyConfig	= removeConfigFile
 | otherwise
	= do	fp	<- dataPath
		createDirectoryIfMissing True fp
		cfp	<- configPath
		putStrLn $ "# Config updated ("++show cfp++")"
		writeFile cfp $ show config

removeConfigFile	:: IO ()
removeConfigFile
	= do	cfp	<- configPath
		exists	<- doesFileExist cfp
		when exists $ removeFile cfp

parserInfo	:: ([Int], String) -> ParserInfo MainArgs
parserInfo v	= parserInfo' (mainArgs $ showVersion v) v

parserInfo' parser v
	= info (helper <*> parser)
			(fullDesc <> progDesc descriptionText <> header (headerText v))




targetFile	:: Parser ExampleFile
targetFile
	= ExFileArgs <$> 
		argument str
			(metavar "TARGET-PROGRAM-FILE"
			<> help "Filepath of the target programming language; use . to read from stdIn"
			<> action "file")
		<*> argument str
			(metavar "PARSE-TYPE"
			<> help "Parse the target program with this bnf-rule as type")
		<*> switch
			(long "line-by-line"
			<> short 'l'
			<> help "Parse the target file line by line")
		<*> optional (strOption 
			 (metavar "RELATION"
			 <> long "relation"
			 <> short 'r'
			 <> help "Prove that this relation is applicable to the example file" ))
		<*> optional (strOption 
			(metavar "FUNCTION"
			 <> long "function"
			 <> short 'f'
			 <> help "Apply given function to the program"))
		<*> optional (strOption
			(metavar "FUNCTION"
			<> long "step-by-step"
			<> short 's'
			<> help "Apply the given function, step by step, until the result of the function is the same as the input"
			))
		<*> optional (strOption
			(metavar "PROPERTY"
			<> long "test-property"
			<> long "tp"
			<> help "Tests the given property on all the examples. The property should take exactly one PARSER-TYPE argument. Only reports failures (unless with ppp)"))
		<*> switch
			(long "test-all-properties"
			<> long "tap"
			<> long "tp*"
			<> long "tpa"
			<> help "Tests all properties on all the examples. The property should take exactly one PARSER-TYPE argument. Only reports failures (unless with ppp)")
		<*> switch
			(long "print-property-proofs"
			<> long "ppp"
			<> help "With 'test-property' and 'test-all-properties': print full proofs of each application of the properties")		
		<*> optional (strOption
			(metavar "svg-name"
			<> long "parstree-svg"
			<> long "ptsvg"
			<> help "Create a neat svg-image of each parsetree"
			))
		<*> switch
			(long "html"
			<> help "Render the file as styled HTML")
		<*> switch
			(long "html-no-css"
			<> help "Render the file as html, without css")
		<*> switch
			(long "latex"
			<> help "Render the file as styled LaTeX")
		<*> switch
			(long "parts"
			<> help "Render the file as parts, e.g. for other programs")

args	:: Parser Args
args	= Args <$> argument str
			(metavar "LANGUAGE-FILE"
			<> help "FilePath of the language-file"
			<> action "file")
		<*> many targetFile
		<*> many (strOption
			(metavar "LANGUAGE-CHANGES-FILE"
			<> long "changes"
			<> short 'c'
			<> help "Apply the given changes to the type-system file. These changes are applied before any other action"
			))
		<*> switch
			(long "dump-language-file"
			<> long "dlf"
			<> help "Dump the parsed type system, usefull for debugging purposes")
		<*> switch
			(long "no-check"
			<> long "nc"
			<> help "Supress liveabilitychecks, totality checker, quickcheck and others")
		<*> strOption
			(long "quickcheck-runs"
			<> metavar "NUMBER-OF-TESTS"
			<> value "8"
			<> help "Number of times each property is tested against random arguments. Default: 100")
		<*> switch
			(long "interpret-abstractly"
			<> long "ia"
			<> help "Interpret each function over all possible values")
		<*> many (strOption
			(metavar "FUNCTION-TO-INTERPRET"
			<> long "ifa"
			<> long "interpret-function-abstract"
			<> help "Interpret a single function abstractly"))
		<*> switch
			(long "interpret-relations-abstractly"
			<> long "ira"
			<> help "Interpret each relation over all possible values")
		<*> many (strOption
			(metavar "RELATION-TO-INTERPRET"
			<> long "ir"
			<> long "interpret-relation"
			<> help "Only run this relation abstractly"))
		<*> optional (strOption
			(metavar "SVG-PATH"
			<> long "lsvg"
			<> help "Create a SVG of the subset relationship between BNF-rules"))
		<*> optional (strOption
			(metavar "SVG-PATH"
			<> long "irasvg"
			<> help "Create a SVG of the subset relationship between BNF-rules, created by the abstract rule analysises"))
		<*> optional dynamizeArgsParser
		<*> strOption (
			metavar "STYLE-NAME"
			<> long "style"
			<> value "Terminal"
			<> help "Color scheme for parsetrees and svg files"
			<> completer (mkCompleter (\part -> knownStyles & M.keys & filter (part `isPrefixOf`) & return))
			)
		<*> switch (
			long "css"
			<> help "Create a css file out of the stylefile"
			)
		<*> optional (strOption
			(metavar "RULE-TO-INTERPRET"
			<> long "repl"
			<> long "interactive"
			<> short 'i'
			<> help "Run this rule interactively"))
		<*> switch(
			long "plain"
			<> long "no-makeup"
			<> help "Prints the parsetrees as flat text and not as colored output; usefull to capture output in e.g. LaTeX"
			<> hidden)
		<*> optional (strOption
			(long "short-proofs"
			<> metavar "DISTANCE-BETWEEN-PREDICATES"
			<> help "Prints proofs without their names and with given spaces in between"
			<> hidden))


		
dynamizeArgsParser	:: Parser DynamizeArgs
dynamizeArgsParser
	= DynamizeArgs
		<$> strOption
			(metavar "SYNTAX-RULE-EXPRESSION"
			<> long "dynamize-syntax"
			<> help "The type to add 'type-err' to")
		<*> strOption
			(metavar "STUCK-STATE"
			<> long "error"
			<> help "The literal representing a type error/stuck state")
		<*> many (strOption
			(metavar "REL-TO-ANALYZE"
			<> long "rel"
			<> help "The relation symbol for which stuck states should be introduced (e.g. 'smallstep')"))
		<*> many (strOption
			(metavar "REL-TO-ADD"
			<> long "reladd"
			<> help "The relation symbol for which stuck states should added trivially (e.g. 'is canonical')"))


mainArgs	:: String -> Parser MainArgs
mainArgs versionMsg
	= MainArgs <$> infoOption versionMsg
			(long "version"
			<> short 'v'
			<> help "Show the version number and text")
		<*> switch
			(long "manual-pdf"
			<> help "Save the manual file (pdf) to the working directory")
		<*> switch
			(long "manual-html"
			<> help "Save the manual file (html) to the working directory")
		<*> optional args
		<*> switch
			(long "test"
			<> help "Run the integration tests"
			<> hidden)
		<*> switch
			(long "install-gtk-sourceview"
			<> help "Saves the language spec for .language files. Move it to the language specs file for gtk-sourceview to enable syntax highlighting in editors using GTK-sourceview (GEdit, Pluma,...")
		<*> switch
			(long "styles"
			<> help "What styles are default?")
