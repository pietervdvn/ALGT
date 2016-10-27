module Utils where

-- Utility functions --
(|>)	:: Functor f => f a -> (a -> b) -> f b
(|>) 	= flip fmap


