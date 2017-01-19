 {-# LANGUAGE TemplateHaskell #-}
 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module TypeSystem.Relation where

import Utils.Utils
import Utils.ToString

import TypeSystem.Types

import Data.Maybe
import Data.List (find)

import Lens.Micro.TH
import Lens.Micro hiding ((&))




data Mode		= In | Out
	deriving (Ord, Eq)

instance Show Mode where
	show In		= "in"
	show Out	= "out"

-- A relation. Some relations might be able to produce values, given a few input variables (e.g. a evaluation or typing rule)
data Relation		= Relation {_relSymbol :: Symbol, _relTypesModes :: [(TypeName, Mode)], _relPronounce :: Maybe String }
	deriving (Show, Ord, Eq)

makeLenses ''Relation


-- Relation types
relType		:: Relation -> [TypeName]
relType r	= r & get relTypesModes |> fst


-- Relation modes
relModes	:: Relation -> [Mode]
relModes r	= r & get relTypesModes |> snd


-- filter a list according to the given mode
filterMode	:: Mode -> Relation -> [a] -> [a]
filterMode mode rel as
	= zip as (relModes rel) & filter ((==) mode . snd) |> fst


relTypesWith	:: Mode -> Relation -> [TypeName]
relTypesWith mode rel
		= filterMode mode rel (relType rel)


weaveMode	:: [Mode] -> [a] -> [a] -> [a]
weaveMode [] [] []	= []
weaveMode (In:modes) (inA:ins) outs
 	= inA : weaveMode modes ins outs
weaveMode (Out:modes) ins (out:outs)
	= out : weaveMode modes ins outs


findRelation	:: [Relation] -> Symbol -> Maybe Relation
findRelation rels s
	= find ((==) s . get relSymbol) rels

instance Refactorable TypeName Relation where
	refactor	= over (relTypesModes . each . _1)

instance Refactorable RelationSymbol Relation where
	refactor frs	= over relSymbol (unliftRelationSymbol frs)



instance FunctionlyTyped Relation where
	typesOf	= relType



instance ToString Relation where
	toParsable (Relation symbol tps pronounce)
		= let	sign	= inParens symbol ++ " \t: "++ tps |> (\(nm, mode) -> nm++" "++ inParens (show mode)) & commas
			pron	= pronounce |> show |> ("\tPronounced as "++) & fromMaybe "" in
			sign ++ pron
	toCoParsable (Relation symbol tps pronounce)
		= let	sign	= inParens symbol -- ++ " : "++ tps |> (\(nm, mode) -> nm++" "++ inParens (show mode)) & commas
			name	= maybe "" (\p -> show p++", with symbol ")  pronounce
			in
			name ++ sign
	debug		= show

