 {-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module TypeSystem.PropertyProof where

{-  -}

import Utils.Utils
import Utils.ToString

import TypeSystem.Proof
import TypeSystem.Rule

import Data.Either

import Lens.Micro hiding ((&))
import Lens.Micro.TH

data PropertyProof
	= PredicateFailed 
		{ _predicateProofs'	:: [Either String Proof]
		}	-- If a predicate failed, we don't have to actually proof
	| PropertyProof
		{ _predicateProofs	:: [Proof]
		, _conclusionProof	:: Proof}
	deriving (Show)

makeLenses ''PropertyProof


instance ToString' Property PropertyProof where
	toParsable'	= _propProofToString toParsable toParsable
	toCoParsable'	= _propProofToString toCoParsable toCoParsable
	debug'		= _propProofToString debug debug
	show'		= const show
	


_propProofToString		:: (Predicate -> String) -> (Proof -> String) -> Property -> PropertyProof -> String
_propProofToString sPred sProof prop (PredicateFailed predProofs)
	= _toStringFailed sPred sProof prop predProofs
_propProofToString sPred sProof prop (PropertyProof predProofs proof)
	= _showSuccess sPred sProof prop (predProofs, proof)



_showSuccess		:: (Predicate -> String) -> (Proof -> String) -> Property -> ([Proof], Proof) -> String
_showSuccess sPred sProof prop (predProofs, proof)
	= let	header	= "# Property "++get propName prop++" statisfied:"
		predsC	= get propPreds prop
		showPred (pred, proof)
			= "# Predicate satisfied: "++sPred pred++"\n"++sProof proof
		preamble= zip predsC predProofs |> showPred & unlines
		in
		unlines [header, preamble, "", "# Satisfies: ", sProof proof]

_toStringFailed 	:: (Predicate -> String) -> (Proof -> String) -> Property -> [Either String Proof] -> String
_toStringFailed sPred sProof prop preds
	= let	header	= "# Could not proof property "++ get propName prop++", predicate failed:"
		predsC	= get propPreds prop
		failOverview
			= preds |> either (const "# Failed: ") (const "# Success: ")
				& zipWith (++) (predsC |> sPred )
				|> indent
		fails	= preds |> either Right Left	
				& zip predsC
				|> sndEffect
				& rights
		showFail (pred, explanation)
			= "# "++sPred pred++" failed because: \n"++indent explanation
		failsDetail
			= fails |> showFail
		
		in
		unlines $ header:(failOverview ++ failsDetail)
