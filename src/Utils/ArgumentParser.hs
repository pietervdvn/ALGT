module Utils.ArgumentParser where

{-
This module defines 
-}

import Utils.Utils
import TypeSystem
import Options.Applicative
import Data.Monoid ((<>))
import Data.List (intercalate)
import System.Directory

import Control.Monad

descriptionText	= "ALGT (Automated Language Generation Tool)- automatically parse, interpret and proof properties of aribtrary languages.\n\n"++
			"This tool parses a 'typesystem'-file, where the syntax of your language is defined. With this, your target file is parsed."++
			"In the typesystem file, rewrite rules (or functions) can be defined - or better, rules defining properties can be defined."++
			"These can be applied to your target language to interpret, typecheck or proof some other property."
headerText (v, vId)
		= "Automated Language Generation Tool (version "++ (v |> show & intercalate ".") ++", "++show vId++" )"


data Args = Args 	{ tsFile		:: String
			, exampleFiles		:: [ExampleFile]
			, changeFile		:: [FilePath]
			, dumbTS		:: Bool
			, createHighlighting	:: Maybe String
			, autoSaveTo		:: Maybe String
			, rmConfig		:: Bool
			}
	deriving (Show)


data ExampleFile	= ExFileArgs
	{ fileName	:: String
	, parser	:: Name
	, lineByLine	:: Bool
	, symbol	:: Maybe Symbol
	, function	:: Maybe Name
	, stepByStep	:: Maybe Name
	} deriving (Show)
	

data AutoSyncHighlighting = ASH
	{ ashTsName	:: Name
	, ashRuleName	:: Name
	, ashTsHash	:: Int
	, ashSaveTo	:: FilePath
	} deriving (Show, Read, Eq)

data Config	= Config 
	{ autoSyntaxes	:: [AutoSyncHighlighting] }
		deriving (Show, Read, Eq)

emptyConfig
	= Config []


parseArgs	:: ([Int], String) -> [String] -> IO Args
parseArgs version strs	
	= do	let result	= execParserPure defaultPrefs (parserInfo version) strs
		handleParseResult result



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



parserInfo v	= info (helper <*> args)
			(fullDesc <> progDesc descriptionText <> header (headerText v))




targetFile	:: Parser ExampleFile
targetFile
	= ExFileArgs <$> 
		argument str
			(metavar "TARGET-PROGRAM-FILE"
			<> help "Filepath of the target programming language"
			<> action "file")
		<*> argument str
			(metavar "PARSER-RULE"
			<> help "Parse the target program with this bnf-parser")
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

args	:: Parser Args
args	= Args <$> argument str
			(metavar "TYPESYSTEM-FILE"
			<> help "FilePath of the typesystem-file"
			<> action "file")
		<*> many targetFile
		<*> many (strOption
			(metavar "TYPESYSTEM-CHANGES-FILE"
			<> long "changes"
			<> short 'c'
			<> help "Apply the given changes to the type-system file. These changes are applied before any other action"
			))
		<*> switch
			(long "dump-typesystem"
			<> long "dts"
			<> help "Dump the parsed type system, usefull for debugging purposes")
		<*> optional (strOption  
			(metavar "PARSER-RULE"
			<> long "create-highlighting"
			<> long "ch"
			<> help "Create a syntax highlighting file for your typesystem, dump it to the terminal, with given starting rule."))
		<*> optional (strOption
			(metavar "SYNTAX-HIGHLIGHTING-PATH"
			<> long "auto-sync-highlighting-to"
			<> long "ash"
			<> help "Saves the syntax highlighting to the specified path. This path is saved; whenever the language def changes, the syntax is updated. Use together with -ch"
			))
		<*> switch
			(long "rm-config"
			<> long "remove-config"
			<> long "rmc"
			<> help "Remove the config file, with auto-highlighting. Try this option if the tool doesn't work'")
		



