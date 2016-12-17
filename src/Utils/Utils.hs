module Utils.Utils where

{-
Some utility functions, often used to program 'left to right'
-}

import Control.Monad

import Data.Either
import Data.Foldable
import Data.Maybe
import Data.List (intercalate)
import Data.Tuple

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set



-------------------- usefull types -------------------------------------------------------


type Name = String


------------------- Left to Right programming helpers -----------------------------------

(&)	= flip ($)

(|>)	:: Functor f => f a -> (a -> b) -> f b
(|>) 	= flip fmap

(||>>)	:: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(||>>) container f
	= container |> (|> f) 

(|+>)	:: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
(|+>)	= forM



inParens str	= "("++str++")"


showComma	:: Show a => [a] -> String
showComma as	= as |> show & commas


commas		= intercalate ", "

------------------- Maybe, Either and other Monad helpers ------------------------------------

cont		:: Monad m => m ()
cont		= return ()


sndEffect	:: Monad m => (a, m b) -> m (a, b)
sndEffect (a, mb)
		= do	b <- mb
			return (a, b)



firstJusts	:: [Maybe a] -> Maybe a
firstJusts maybes
	= let	as	= catMaybes maybes in
		if null as then Nothing else Just $ head as

-- Gives the first right value. If none of the values is Right, concats the 'error messages' (with newlines and indentation)
firstRight	:: [Either String b] -> Either String b
firstRight vals	= let r	= rights vals in
			if null r then 
				Left (pack $ lefts vals) else 
				Right (head r)
		  where pack vals	= vals |> lines ||>> ("  "++) |> unlines & unlines


-- Checks wether all are right, and returns those. Gives messages for failed values
allRight	:: Show b => [Either String b] -> Either String [b]
allRight eithers
 | all isRight eithers	= Right $ rights eithers
 | otherwise	= eithers |> either id ((++) "Successfull: " . show) & unlines & Left


allRight_	:: [Either String ()] -> Either String ()
allRight_ eithers
		= eithers & filter isLeft & allRight >> return ()


-- Stack message for Either Monad
inMsg		:: String -> Either String a -> Either String a
inMsg msg (Left msg')
		= let indMsg	= msg' & lines |> ("  "++) & unlines in
		 	Left (msg ++ ":\n"++ indMsg)
inMsg _ right	= right


ammendMsg	:: (e -> e) -> Either e a -> Either e a
ammendMsg f (Left e)
		= Left $ f e
ammendMsg _ a	= a


assert :: Monad m => (String -> m ()) -> Bool -> String -> m ()
assert c False msg	= c msg
assert c True _ 	= cont

----------------------- List tools --------------------

merge		:: Eq a => [(a,b)] -> [(a,[b])]
merge []	= []
merge ((a,b):ls)
		= let bs	= map snd $ filter ((==) a . fst) ls in
			(a,b:bs): merge (filter ((/=) a . fst) ls)
merge'		:: Eq b => [(a,b)] -> [([a],b)]
merge'		= map swap . merge . map swap

unmerge		:: [(a,[b])] -> [(a,b)]
unmerge 	=  concatMap (\(a,bs) -> [(a,b) | b <- bs])

dubbles		:: Eq a => [a] -> [a]
dubbles []	=  []
dubbles (a:as)	=  (if a `elem` as then (a:) else id) $ dubbles as


checkNoDuplicates	:: (Eq a) => [a] -> ([a] -> String) -> Either String ()
checkNoDuplicates as msg
	= do	let dups	= dubbles as
		unless (null dups) $ Left $ msg dups 

equalizeLength	:: a -> [[a]] -> [[a]]
equalizeLength a ass
	= let	longest		= ass |> length & maximum
		append as	= as ++ replicate (longest - length as) a
		in
		ass |> append

stitch		:: a -> [[a]] -> [[a]] -> [[a]]
stitch eq a b	= let	la	= length a
			lb	= length b
			longest	= max la lb
			a'	= replicate (longest - la) [] ++ a
			b'	= replicate (longest - lb) [] ++ b in
			zipWith (++) (equalizeLength eq a') b'



mapi	:: [a] -> [(Int, a)]
mapi	= zip [0..]


invertDict	:: (Eq b, Ord b, Ord a) => Map a (Set b) -> Map b (Set a)
invertDict	= fmap Set.fromList . Map.fromList . merge . map swap . unmerge . Map.toList . fmap Set.toList

invertDict'	:: (Ord a, Ord b, Eq b) => Map a b -> Map b [a]
invertDict'	=  Map.fromList .  merge . map swap . Map.toList