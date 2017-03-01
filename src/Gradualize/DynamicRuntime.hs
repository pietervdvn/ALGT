module Gradualize.DynamicRuntime where

import Utils.Utils

import TypeSystem


import Data.Char (toUpper)
import Data.Maybe

import Changer.Changes

import Control.Monad


dynamized	:: TypeSystem -> [Symbol] -> Either String Changes
dynamized ts proofsFor
	= do	proofRules	<- proofsFor |> generateRelationProof ts 
					& allRight |> concat
		let proofRules'	= proofRules 
					|> (\(n, bnf) -> New n $ (bnf, IgnoreWS, False))
		
		return $ Changes "Dynamic Runtimed" proofRules' [] [] []




generateTupleRule	:: (TypeName, TypeName) -> (TypeName, [BNF])
generateTupleRule (tnl, tnr)
	= ("tuple"++onHead toUpper tnl++onHead toUpper tnr, [BNFSeq [Literal "<", BNFRuleCall tnl, Literal ",", BNFRuleCall tnr, Literal ">"]])



generateRelationProof	:: TypeSystem -> Symbol -> Either String [(TypeName, [BNF])]
generateRelationProof ts nm
	= do	rel		<- findRelation' ts nm
		let types	= get relTypesModes rel |> fst
		unless (length types == 2) $ Left $ "Expected exactly two arguments to "++show nm++", this is not a tuple. Proofs can only be constructed for stuff like equality, is subtype of, ..."
		let [tnl, tnr]	= types
		let t		= generateTupleRule (tnl, tnr)
		let name	= get relPronounce rel |> ("proof "++) |> camelCase & fromMaybe nm
		let bnf		= [BNFRuleCall $ fst t]
		return [t, (name, bnf)]






		
