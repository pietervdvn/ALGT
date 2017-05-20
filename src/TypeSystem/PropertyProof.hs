 {-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module TypeSystem.PropertyProof where

{-  -}

import Utils.Utils
import Utils.ToString

import TypeSystem.Proof
import TypeSystem.Rule
import TypeSystem.Function (VariableAssignmentsA)
import TypeSystem.ParseTree

import Data.Either

import Lens.Micro hiding ((&))
import Lens.Micro.TH

data PropertyProof
	= PredicateFailed 
		{ _propProofAssgn	:: VariableAssignmentsA ParseTree	-- Just the input
		, _predicateProofs'	:: [Either String Proof]
		}	-- If a predicate failed, we don't have to actually proof
	| PropertyProof
		{ _propProofAssgn	:: VariableAssignmentsA ParseTree	-- The resulting assignment
		, _predicateProofs	:: [Proof]
		, _conclusionProvenWith	:: Int
		, _conclusionProof	:: Proof}
	deriving (Show)

makeLenses ''PropertyProof


instance ToString' (ProofOptions, Property) PropertyProof where
	toParsable'	= _propProofToString toParsable toParsable'
	toCoParsable'	= _propProofToString toCoParsable toCoParsable'
	debug'		= _propProofToString debug debug'
	show'		= const show
	


_propProofToString		:: (Predicate -> String) -> (ProofOptions -> Proof -> String) -> (ProofOptions, Property) -> PropertyProof -> String
_propProofToString sPred sProof (proofOptions, prop) (PropertyProof vars predProofs provenWith proof)
	= let	header		= "# Property "++get propName prop++" statisfied with assignment {"++ toParsable' ", " vars ++"}\n"
		predsC		= get propPreds prop
		showPred (pred, proof)
				= ["# Predicate satisfied:\n# "++sPred pred
					, ""
				  	, indent (sProof proofOptions proof)] & unlines
		preamble	= zip predsC predProofs |> showPred & unlines
		provenConcl	= get propConcl prop !! provenWith
		conclMsg	= ["# Satisfies a possible conclusion:\n# "++ sPred provenConcl
					, ""
					, sProof proofOptions proof] & unlines
		in
		header ++ indent (unlines [preamble, "", conclMsg])

_propProofToString sPred sProof (proofOptions, prop) (PredicateFailed vars predProofs)
	= let	header	= "# Property "++ get propName prop++" proven by failing predicate with assignment {"++ toParsable' ", " vars ++"}:"
		predsC	= get propPreds prop
		failOverview
			= predProofs |> either (const "# Failed: ") (const "# Success: ")
				& zipWith (flip (++)) (predsC |> sPred )
				|> indent
		fails	= predProofs |> either Right Left	
				& zip predsC
				|> sndEffect
				& rights
		showFail (pred, explanation)
			= "# "++sPred pred++" failed because: \n"++indent explanation
		failsDetail
			= fails |> showFail
		
		in
		unlines $ header:(failOverview ++ failsDetail)
