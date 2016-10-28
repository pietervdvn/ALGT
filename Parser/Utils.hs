module Utils where

type Name = String

-- Utility functions --
(|>)	:: Functor f => f a -> (a -> b) -> f b
(|>) 	= flip fmap


