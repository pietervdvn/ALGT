module Utils where

import Control.Monad

type Name = String

-- Utility functions --
(|>)	:: Functor f => f a -> (a -> b) -> f b
(|>) 	= flip fmap

(|+>)	:: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
(|+>)	= forM


(&)	= flip ($)

inParens str	= "("++str++")"

