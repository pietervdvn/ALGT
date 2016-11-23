module Utils where

import Control.Monad
import Data.List (intercalate)
import Data.Maybe

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
