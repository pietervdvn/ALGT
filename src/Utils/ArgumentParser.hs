module Utils.ArgumentParser (parseArgs, Args(..), ExampleFile(..), Config(..), NeedsFiles(..),  getConfig, writeConfig, removeConfig)where

{-
This module defines parsing of the arguments and reading/writing of the config file
-}

import TypeSystem
import Utils.Utils

import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.Maybe

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


class NeedsFiles a where
	filesNeeded	:: a -> [FilePath]
	

data MainArgs	= MainArgs 
			{ showVersionNr	:: Bool
			, manual	:: Bool
			, realArgs	:: Maybe Args
			, runTests	:: Bool
			}

data Args = Args 	{ tsFile		:: String
			, exampleFiles		:: [ExampleFile]
			, changeFile		:: [FilePath]
			, dumbTS		:: Bool
			, interpretAbstract	:: Bool
			, interpretRulesAbstract:: Bool
			, interpretRules	:: [String]
			, iraSVG		:: Maybe String
			, createSVG		:: Maybe String
			}
	deriving (Show)
instance NeedsFiles Args where
	filesNeeded args	= tsFile args : (changeFile args ++ (exampleFiles args >>= filesNeeded))


data ExampleFile	= ExFileArgs
	{ fileName	:: FilePath
	, parser	:: Name
	, lineByLine	:: Bool
	, symbol	:: Maybe Symbol
	, function	:: Maybe Name
	, stepByStep	:: Maybe Name
	, testProp	:: Maybe Name
	, testAllProps	:: Bool
	, verbose	:: Bool
	, ptSvg		:: Maybe Name
	} deriving (Show)
	
instance NeedsFiles ExampleFile where
	filesNeeded exfile	= [fileName exfile]


data Config	= Config 	
	{ autoSyntaxes	:: [Int] }
		deriving (Show, Read, Eq)

emptyConfig
	= Config []


parseArgs	:: ([Int], String) -> [String] -> IO (Bool, Maybe Args)
parseArgs version strs	
	= do	let result	= execParserPure defaultPrefs (parserInfo version) strs
		MainArgs doShowVersion saveMan args runTests
				<- handleParseResult result
		when doShowVersion $ do
			putStrLn (showVersion version)
			exitSuccess
		when saveMan $ do
			B.writeFile "ALGT_Manual.pdf" _Manual_ALGT_Manual_pdf
			putStrLn "Manual saved as ALGT_Manual.pdf"
			exitSuccess
		return (runTests, args)



dataPath	= getXdgDirectory XdgData "ALGT"
configPath	= dataPath |> (++"/config")

getConfig	:: IO Config
getConfig	
	= do	fp	<- configPath
		exists	<- doesFileExist fp
		if exists then
			readFile fp |> read
		else
			return emptyConfig

writeConfig	:: Config -> IO ()
writeConfig config
 | config == emptyConfig	= removeConfig
 | otherwise
	= do	fp	<- dataPath
		createDirectoryIfMissing True fp
		cfp	<- configPath
		putStrLn $ "# Config updated ("++show cfp++")"
		writeFile cfp $ show config

removeConfig	:: IO ()
removeConfig
	= do	cfp	<- configPath
		exists	<- doesFileExist cfp
		when exists $ removeFile cfp



parserInfo v	= info (helper <*> mainArgs)
			(fullDesc <> progDesc descriptionText <> header (headerText v))




targetFile	:: Parser ExampleFile
targetFile
	= ExFileArgs <$> 
		argument str
			(metavar "TARGET-PROGRAM-FILE"
			<> help "Filepath of the target programming language"
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
			 <> help "Proof that this relation is applicable to the example file" ))
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
			(long "interpret-abstractly"
			<> long "ia"
			<> help "Interpret each function over all possible values")
		<*> switch
			(long "interpret-rules-abstractly"
			<> long "ira"
			<> help "Interpret each rule over all possible values")
		<*> many (strOption
			(metavar "RULE-TO-INTERPRET"
			<> long "ir"
			<> long "interpret-rule"
			<> help "Only run this rule abstractly"))
		<*> optional (strOption
			(metavar "SVG-PATH"
			<> long "irasvg"
			<> help "Create a SVG of the subset relationship between BNF-rules of the rules"))
		<*> optional (strOption
			(metavar "SVG-PATH"
			<> long "lsvg"
			<> help "Create a SVG of the subset relationship between BNF-rules"))
		

mainArgs	:: Parser MainArgs
mainArgs	
	= MainArgs <$> switch
			(long "version"
			<> short 'v'
			<> help "Show the version number and text")
		<*> switch
			(long "manual"
			<> help "Save the manual file (pdf) to given path")
		<*> optional args
		<*> switch
			(long "test"
			<> help "Run the integration tests")


