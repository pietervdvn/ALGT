 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module Utils.Utils where


{-
Some utility functions, often used to program 'left to right'
-}

import Control.Monad

import Data.Either
import Data.Foldable
import Data.Maybe
import Data.List (intercalate, isPrefixOf)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Lens.Micro.Extras as LME

get	= LME.view


-------------------- usefull types -------------------------------------------------------


type Name = String

-- Represents some path in some tree
type Path	= [Int]


class Check a where
	check	:: a -> Either String ()
	check a	= return ()

class Check' info a where
	check'	:: info -> a -> Either String ()
	check' info a
		= return ()

------------------- Left to Right programming helpers -----------------------------------

(&)	= flip ($)

(|>)	:: Functor f => f a -> (a -> b) -> f b
(|>) 	= flip fmap

(||>>)	:: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(||>>) container f
	= container |> (|> f) 

(|+>)	:: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
(|+>)	= forM


(|++>)	:: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
(|++>) mta f
	= do	ta	<- mta
		ta |+> f

inParens str	= "("++str++")"

inHeader prefix str chr msg
	= let	title	= " "++str++" "
		line	= title |> const chr
		in
		["", prefix ++ title, prefix ++ line, "", msg] & unlines

inHeader' s
	= inHeader "" s '='



showComma	:: Show a => [a] -> String
showComma as	= as |> show & commas


commas		= intercalate ", "

allCombinations	:: [[a]] -> [[a]]
allCombinations []	= [[]]
allCombinations (a:as)
	= do	tails	<- allCombinations as
		head	<- a
		return (head:tails)




------------------- Maybe, Either and other Monad helpers ------------------------------------

pass		:: Monad m => m ()
pass		= return ()


sndEffect	:: Monad m => (a, m b) -> m (a, b)
sndEffect (a, mb)
		= do	b <- mb
			return (a, b)

fstEffect	:: Monad m => (m a, b) -> m (a, b)
fstEffect (ma, b)
		= do	a <- ma
			return (a, b)
ifJust'		:: (a -> b -> IO ()) -> Maybe a -> b -> IO ()
ifJust' f Nothing b	= return ()
ifJust' f (Just a) b	= f a b


ifJust		:: (a -> IO ()) -> Maybe a -> IO ()
ifJust _ Nothing	= return ()
ifJust f (Just a)	= f a

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


indent []	= []
indent msg	= msg & lines |> ("  "++) & unlines & init

-- Stack message for Either Monad
inMsg		:: String -> Either String a -> Either String a
inMsg msg (Left msg')
		= Left (msg ++ ":\n"++ indent msg')
inMsg _ right	= right


ammendMsg	:: (e -> e) -> Either e a -> Either e a
ammendMsg f (Left e)
		= Left $ f e
ammendMsg _ a	= a


assert :: Monad m => (String -> m ()) -> Bool -> String -> m ()
assert c False msg	= c msg
assert c True _ 	= pass

----------------------- Tuple Tools ------------------

fst3		:: (a, b, c) -> a
fst3 (a, _, _)	= a

snd3		:: (a, b, c) -> b
snd3 (_, b, _)	= b

trd3		:: (a, b, c) -> c
trd3 (_, _, c)	= c


dropFst3		:: (a, b, c) -> (b, c)
dropFst3 (_, b, c)	= (b, c)

dropSnd3		:: (a, b, c) -> (a, c)
dropSnd3 (a, _, c)	= (a, c)

dropTrd3	:: (a, b, c) -> (a, b)
dropTrd3 (a, b, _)	= (a, b)

merge3l			:: (a, b, c) -> ((a, b), c)
merge3l (a, b, c)	=  ((a, b), c)

unmerge3r		:: (a, (b, c)) -> (a, b, c)
unmerge3r (a, (b, c))	= (a, b, c) 

mapBoth f (a, a')	= (f a, f a')

both f (a, a')		= f a && f a'

swap (a, b)		= (b, a)

----------------------- List tools --------------------

chain		:: (Traversable t) => t (a -> a) -> a -> a
chain fs a	= foldl (&) a fs 

validLines	:: String -> [String]
validLines fileConts
		= fileConts & lines & filter (not . null) & filter (not . ("#" `isPrefixOf`))

replaceN	:: Int -> a -> [a] -> [a]
replaceN 0 a (_:as)
		= a : as
replaceN i a (h:as)
		= h : replaceN (i-1) a as



replacePointwise	:: [a] -> [[a]] -> [[a]]
replacePointwise origs pointwise
	= do	i	<- [0..length pointwise -1]
		ai'	<- pointwise !! i
		return $ replaceN i ai' origs



length'		:: Int -> String -> Int
length' runningLength []
		= runningLength
length' runningLength ('\t':rest)
		= length' (runningLength & (+ 8) & (`div` 8) & (* 8) ) rest
length' runningLength (a:as)
		= length' (runningLength + 1) as

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


checkNoCommon		:: (Ord k) => Map k v -> Map k v -> ([(k, (v, v))] -> String) -> Either String ()
checkNoCommon dict1 dict2 errMsg
	= do	let common	= Map.intersection dict1 dict2 & Map.keys
		let v1s		= common |> (dict1 Map.!)
		let v2s		= common |> (dict2 Map.!)
		let msg		= errMsg $ zip common $ zip v1s v2s
		unless (null common) $ Left msg

checkNoExists		:: (Ord k) => k -> Map k v -> String -> Either String ()
checkNoExists k dict msg
 | k `Map.notMember` dict	= return ()
 | otherwise		= Left msg

checkExists		:: (Ord k) => k -> Map k v -> String -> Either String v
checkExists k dict msg
 | k `Map.member` dict
			= return $ dict Map.! k
 | otherwise		= Left msg

padL		:: Int -> a -> [a] -> [a]
padL i filler as
		= replicate (i - length as) filler ++ as

padR		:: Int -> a -> [a] -> [a]
padR i filler as
		= as ++ replicate (i - length as) filler

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

specialChars	= "+*?[]\\^$.()"

translateRegex	:: String -> String
translateRegex str
	= str >>= (\c -> if c `elem` specialChars then "\\"++[c] else [c])

