module ArgumentParser where

{-
This module defines 
-}

import Utils
import TypeSystem
import Options.Applicative
import Data.Monoid ((<>))
import Data.List (intercalate)

descriptionText	= "ALGT (Automated Language Generation Tool)- automatically parse, interpret and proof properties of aribtrary languages.\n\n"++
			"This tool parses a 'typesystem'-file, where the syntax of your language is defined. With this, your target file is parsed."++
			"In the typesystem file, rewrite rules (or functions) can be defined - or better, rules defining properties can be defined."++
			"These can be applied to your target language to interpret, typecheck or proof some other property."
headerText v	= "Automated Language Generation Tool (version "++ (v |> show & intercalate ".") ++" )"

data Args = Args 	{ ts_file	:: String
			, example_file	:: String
			, parser	:: Name
			, line_by_line	:: Bool
			, symbol	:: Maybe Symbol
			, function	:: Maybe Name
			, stepByStep	:: Maybe Name
			, dumbTS	:: Bool
			}
	deriving (Show)
	

parseArgs	:: [Int] -> [String] -> IO Args
parseArgs version strs	
	= do	let result	= execParserPure defaultPrefs (parserInfo version) strs
		handleParseResult result


parserInfo v	= info (helper <*> args)
			(fullDesc <> progDesc descriptionText <> header (headerText v))

args	:: Parser Args
args	= Args <$> argument str
			(metavar "TYPESYSTEM-FILE"
			<> help "FilePath of the typesystem-file"
			<> action "file")
		<*> argument str
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
		<*> ( optional $ strOption 
			 ( metavar "RELATION"
			 <> long "relation"
			 <> short 'r'
			 <> help "Proof that this relation is applicable to the example file" ))
		<*> ( optional $ strOption 
			( metavar "FUNCTION"
			 <> long "function"
			 <> short 'f'
			 <> help "Apply given function to the program"))
		<*> ( optional $ strOption
			(metavar "FUNCTION"
			<> long "step-by-step"
			<> short 's'
			<> help "Apply the given function, step by step, until the result of the function is the same as the input"
			))
		<*> switch
			(long "dump-typesystem"
			<> long "dts"
			<> help "Dump the parsed type system, usefull for debugging purposes")
