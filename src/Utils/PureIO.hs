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



import Lens.Micro hiding ((&))
import Lens.Micro.TH

class NeedsFiles a where
	filesNeeded	:: a -> [FilePath]
	



data Output = Output 
		{ _files	:: Map String String
		, _stdOut	:: [String]
		} deriving (Show, Ord, Eq, Read)
makeLenses ''Output
instance Monoid Output where
	mempty	= emptyOutput
	mappend	(Output f1 o1) (Output f2 o2)
		= Output (M.union f2 f1 {-Reverse order: last write (f2) has priority-}) (o1 ++ o2) 


removeCarriageReturns	:: Output -> Output
removeCarriageReturns output
	= output & over stdOut (>>= lines) & over stdOut (|> (\l -> l & reverse & takeWhile (/='\r') & reverse)) 

emptyOutput	= Output M.empty []

runOutput	:: Output -> IO ()
runOutput (Output files stdOut)
	= do	(stdOut >>= lines) |+> IO.putStrLn
		let files'	= files & M.toList
		unless (null files') $ IO.putStrLn $ fancyString' True "" files' (files & M.keys |> ("Rendering file "++))
		files' |+> uncurry IO.writeFile
		pass

type Input	= Map FilePath String

runInput	:: (NeedsFiles a) => a -> IO Input
runInput fn	=  filesNeeded fn |> (id &&& IO.readFile) |+> sndEffect |> M.fromList


checkInput	:: (NeedsFiles a) => a -> PureIO ()
checkInput fn
	= do	let needed	= filesNeeded fn
		inp		<- getInputs
		let found	= inp & M.keys
		let missing	= needed \\ found
		unless (null missing) $ fail $ "Missing files in input: "++ showComma missing


getInputs	:: PureIO Input
getInputs	= PureIO (\i -> return (i, emptyOutput))



data PureIO a	= PureIO (Input -> Either String (a, Output))

instance Functor PureIO where
	fmap f (PureIO fi)
		= PureIO (\i -> fi i |> over _1 f)


instance Applicative PureIO where
	pure a	= PureIO (\_ -> Right (a, emptyOutput))
	(<*>) (PureIO fia2b) (PureIO fia)
		= PureIO (\i -> 
			do 	(a2b, out0)	<- fia2b i	-- :: Either String (a -> b, Output)
				(a, out1)	<- fia i	-- :: Either String (a, Output)
				return (a2b a, out0 <> out1))


instance Monad PureIO where
	return	= pure
	(>>=) (PureIO  fia) a2mb
		= PureIO (\i ->
			do	(a, out0)	<- fia i
				let (PureIO fib)= a2mb a
				(b, out1)	<- fib i
				return (b, out0 <> out1))
	fail msg	= PureIO $ \_ -> Left msg



runPure		:: Input -> PureIO a -> Either String (a, Output)
runPure inp (PureIO i2a)
	= i2a inp

runPureOutput	:: Input -> PureIO a -> Output
runPureOutput inp pureIO
	= runPure inp (pureIO & isolateFailure') & either (error "Bug") id & snd 




runIO		:: (NeedsFiles a) => a -> PureIO x -> IO x
runIO needsFiles f
	= do	inp	<- runInput needsFiles
		case runPure inp f of
			Left msg	-> error msg
			Right (x, output)	-> runOutput output >> return x



runIO'		:: (NeedsFiles a) => a -> PureIO x -> IO ()
runIO' nf f	= runIO nf f & void








putStrLn	:: String -> PureIO ()
putStrLn str
	= PureIO $ \_ -> Right ((), Output M.empty [str])


writeFile	:: FilePath -> String -> PureIO ()
writeFile fp contents
	= PureIO $ \_ -> Right ((), Output (M.singleton fp contents) [])

readFile	:: FilePath -> PureIO String
readFile fp
	= PureIO $ \i -> 
			do	contents	<- checkExists fp i ("No file "++fp++" found")
				return (contents, emptyOutput)


liftEith	:: Either String x -> PureIO x
liftEith 	= either fail return


catch		:: (String -> PureIO x) -> PureIO x -> PureIO x
catch handler (PureIO fix)
		= PureIO $ \i ->
			case fix i of
				Left msg	-> let PureIO fix'	= handler msg in fix' i
				right		-> right

ioIf	:: x -> (a -> Bool) -> PureIO x -> a -> PureIO x
ioIf x fb action a
	= if fb a then action else return x

ioIf'	= ioIf ()



ioIfJust	:: x -> (a -> Maybe b) -> (b -> PureIO x) -> a -> PureIO x
ioIfJust x fb action a
	= case fb a of
		(Just b)	-> action b
		Nothing		-> return x

ioIfJust'	= ioIfJust ()

onAll		:: (a -> b -> PureIO ()) -> [b] -> a -> PureIO ()
onAll f pts a
	= pts |+> f a & void

onAll'		:: (a -> b -> PureIO ()) -> [b] -> a -> PureIO ()
onAll' f pts a
	= pts |> f a |+> catch putStrLn & void
		



isolateFailure'	:: PureIO x -> PureIO ()
isolateFailure' (PureIO fix)
	= PureIO (\i -> case fix i of
				Left msg	-> Right ((), Output M.empty [msg])
				Right (x, out)	-> Right ((), out) )	

isolateFailure	:: x -> PureIO x -> PureIO x
isolateFailure x
	= catch (\msg -> putStrLn msg >> return x) 

isolateCheck	:: Either String () -> PureIO ()
isolateCheck (Left msg)
		= fail msg & isolateFailure'
isolateCheck _	= return ()


