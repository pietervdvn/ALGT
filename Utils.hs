module Utils where

import Control.Monad
import Data.List (intercalate)
import Data.Maybe
import Data.Either
import Data.Foldable

type Name = String

-- Utility functions --
(|>)	:: Functor f => f a -> (a -> b) -> f b
(|>) 	= flip fmap

(||>>)	:: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(||>>) container f
	= container |> (|> f) 

(|+>)	:: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
(|+>)	= forM


(&)	= flip ($)

inParens str	= "("++str++")"


showComma	:: Show a => [a] -> String
showComma as	= as |> show & intercalate ", "

firstJusts	:: [Maybe a] -> Maybe a
firstJusts maybes
	= let	as	= catMaybes maybes in
		if null as then Nothing else Just $ head as


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


inMsg		:: String -> Either String a -> Either String a
inMsg msg (Left msg')
		= Left (msg ++ ":\n"++msg')
inMsg _ right	= right
