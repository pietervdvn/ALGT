 {-# LANGUAGE TemplateHaskell #-}
module Utils.PureIO where

import Utils.Utils

import Prelude hiding (writeFile, putStrLn, readFile)
import qualified Prelude as IO

import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Monoid
import Data.Either

import Control.Arrow ((&&&))
import Control.Monad

import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import Text.PrettyPrint.ANSI.Leijen (Doc)

import System.Random

import Lens.Micro hiding ((&))
import Lens.Micro.TH

class NeedsFiles a where
	filesNeeded	:: a -> [FilePath]
	



data Output = Output 
		{ _files	:: Map String (Bool, String)
		, _stdOut	:: [String]
		} deriving (Eq, Show, Read, Ord)
makeLenses ''Output

changedFiles	:: Output -> KnownFiles
changedFiles (Output f stdOut)
	= f & M.filter fst |> snd


type KnownFiles	= Map FilePath String
data PureIO' config state a
 			= PureIO ((config, Output, state) -> Either String (a, Output, state))

type PureIO a		= PureIO' () a



instance Monoid Output where
	mempty	= Output M.empty []
	mappend	(Output f1 o1) (Output f2 o2)
		= Output (M.union f2 f1 {-Reverse order: last write (f2) has priority-}) (o1 ++ o2) 

removeCarriageReturns	:: Output -> Output
removeCarriageReturns output
	= output & over stdOut (>>= lines) & over stdOut (|> (\l -> l & reverse & takeWhile (/='\r') & reverse)) 


removeUnchanged	:: Output -> Output
removeUnchanged
	= over files (M.filter fst)

runOutput	:: Output -> IO ()
runOutput output@(Output files stdOut)
	= do	(stdOut >>= lines) |+> IO.putStrLn
		let files'	= changedFiles output & M.toList
		unless (null files') $ IO.putStrLn $ fancyString' True "" files' (files & M.keys |> ("Rendering file "++))
		files' |+> uncurry IO.writeFile
		pass


runInput	:: (NeedsFiles a) => a -> IO KnownFiles
runInput fn	=  filesNeeded fn |> (id &&& IO.readFile) |+> sndEffect |> M.fromList


checkInput	:: (NeedsFiles a) => a -> PureIO' config state ()
checkInput fn
	= do	let needed	= filesNeeded fn & nub
		inp		<- getKnownFiles
		let found	= inp & M.keys
		let missing	= needed \\ found
		unless (null missing) $ fail $ "Missing files in input: "++ showComma missing


getKnownFiles :: PureIO' config state KnownFiles
getKnownFiles	= PureIO (\(_, output, state) -> return (get files output |> snd, output, state))

getConfig	:: PureIO' config state config
getConfig	= PureIO (\(c, output, s) -> return (c, output, s))

getConfig'	:: (config -> a) -> PureIO' config state a
getConfig' f	= getConfig |> f

getState	:: PureIO' config state state
getState	= PureIO (\(_, output, s) -> return (s, output, s))

withConfig	:: config -> PureIO' config state a -> PureIO' config state a
withConfig c (PureIO f)
		= PureIO (\(_, i, s) -> f (c, i, s))

withConfig'	:: (config' -> config) -> PureIO' config state a -> PureIO' config' state a
withConfig' f (PureIO fia)
		= do	c'	<- getConfig
			PureIO (\(_, i, s) -> fia (f c', i, s))

instance Functor (PureIO' config state) where
	fmap f (PureIO fi)
		= PureIO (\i -> fi i |> over _1 f)


instance Applicative (PureIO' config state) where
	pure a	= PureIO (\(_, out, state) -> Right (a, out, state))
	(<*>) (PureIO fia2b) (PureIO fia)
		= PureIO (\(c, out0, s0) -> 
			do 	(a2b, out1, s1)	<- fia2b (c, out0, s0)	-- :: Either String (a -> b, Output)
				(a, out1, s2)	<- fia (c, out1, s1)	-- :: Either String (a, Output)
				return (a2b a, out1, s2))


instance Monad (PureIO' config state) where
	return	= pure
	(>>=) (PureIO  fia) a2mb
		= PureIO (\(c, out0, s0) ->
			do	(a, out1, s1)		<- fia (c, out0, s0)
				let (PureIO fib)= a2mb a
				(b, out2, s2)	<- fib (c, out1, s1)
				return (b, out2, s2))
	fail msg	= PureIO $ \_ -> Left msg



runPure		:: config -> state -> KnownFiles -> PureIO' config state a -> Either String (a, Output, state)
runPure config state inp (PureIO i2a)
	= i2a (config, Output (inp |> (\c -> (False, c))) [],  state)

runPureOutput	:: config -> state -> KnownFiles -> PureIO' config state a -> Output
runPureOutput config state inp pureIO
	= runPure config state inp pureIO & either error id & snd3 


runIO		:: (NeedsFiles a) => config -> state -> a -> PureIO' config state x -> IO x
runIO c state	= runIOWith c state M.empty

runIOWith		:: (NeedsFiles a) => config -> state -> KnownFiles -> a -> PureIO' config state x -> IO x
runIOWith config state extraInput needsFiles f
	= do	inp	<- runInput needsFiles
		case runPure config state (M.union extraInput inp) f of
			Left msg		-> error msg
			Right (x, output, _)	-> runOutput output >> return x



runIO'		:: (NeedsFiles a) => config -> state -> a -> PureIO' config state x -> IO ()
runIO' config state nf f
		= runIO config state nf f & void








putStrLn	:: String -> PureIO' config state ()
putStrLn str
	= PureIO $ \(_, Output files stdOut, state)
			 -> Right ((), Output files (stdOut ++ [str]), state)

putDocLn	:: Doc -> PureIO' config state ()
putDocLn doc	= putStrLn $ show doc


writeFile	:: FilePath -> String -> PureIO' config state ()
writeFile fp contents
	= PureIO $ \(_, Output files stdOut, state)
			-> Right ((), Output (M.insert fp (True, contents) files) stdOut, state)

readFile	:: FilePath -> PureIO' config state String
readFile fp
	= PureIO $ \(c, output@(Output files _), state) -> 
		do	contents	<- checkExists fp files ("No file "++fp++" found") -- : try one of:\n"++indent (M.keys files & showComma))
			return (snd contents, output, state)


liftEith	:: Either String x -> PureIO' config state x
liftEith 	= either fail return


catch		:: (String -> PureIO' config state x) -> PureIO' config state x -> PureIO' config state x
catch handler (PureIO fix)
		= PureIO $ \i ->
			case fix i of
				Left msg	-> let PureIO fix'	= handler msg in fix' i
				right		-> right

ioIf	:: x -> (a -> Bool) -> PureIO' config state x -> a -> PureIO' config state x
ioIf x fb action a
	= if fb a then action else return x

ioIf'	= ioIf ()



ioIfJust	:: x -> (a -> Maybe b) -> (b -> PureIO' config state x) -> a -> PureIO' config state x
ioIfJust x fb action a
	= case fb a of
		(Just b)	-> action b
		Nothing		-> return x

ioIfJust'	= ioIfJust ()

onAll		:: (a -> b -> PureIO' config state ()) -> [b] -> a -> PureIO' config state ()
onAll f pts a
	= pts |+> f a & void

onAll'		:: (a -> b -> PureIO' config state ()) -> [b] -> a -> PureIO' config state ()
onAll' f pts a
	= pts |> f a |+> catch putStrLn & void
		



isolateFailure'	:: (String -> PureIO' config state x ) -> PureIO' config state x -> PureIO' config state x
isolateFailure' catchM (PureIO tested)
	= PureIO (\i@(c, input, state) -> case tested i of
				Left msg	-> let (PureIO catch)	= catchM msg in
							catch (c, input, state)
				Right (x, out, state)	
						-> Right (x, out, state) )	


isolateCheck	:: Either String () -> PureIO' config state ()
isolateCheck (Left msg)
		= fail msg & void & isolateFailure' putStrLn
isolateCheck _	= return ()


