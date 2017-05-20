 {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module TypeSystem.Proof where


import Utils.Utils
import Utils.ToString

import TypeSystem.Types
import TypeSystem.ParseTree
import TypeSystem.Expression
import TypeSystem.Rule

import Lens.Micro hiding ((&))
import Lens.Micro.TH

{-
When a rule is applied to enough values (parsetrees) it generates a proof of this rule.
Use 'parseTreeInterpreter.RuleInterpreter' to do this
-}
data Proof	= Proof { _proofConcl	:: Conclusion'
			, _prover	:: Rule
			, _proofPreds	:: [Proof]	-- predicates for the rule
			}
		| ProofIsA ParseTree TypeName
		| ProofSame ParseTree Expression Expression
		 deriving (Show, Ord, Eq)

makeLenses ''Proof

isProof Proof{}	= True
isProof _	= False

{-Number of 'layers' in the proof-}
depth	:: Proof -> Int
depth proof@Proof{}
	= if null (get proofPreds proof) then 1
		else get proofPreds proof |> depth & maximum & (+1)
depth _	= 1


{-Number of proof elements-}
weight	:: Proof -> Int
weight proof@Proof{}
	 = 1 + (proof & get proofPreds |> weight & sum)
weight _ = 1



instance Refactorable TypeName Proof where
	refactor ftn proof
		= proof & over (proofPreds . each) (refactor ftn) 
			& over prover (refactor ftn)
			& over proofConcl (refactor ftn)

instance Refactorable FunctionName Proof where
	refactor ftn proof
		= proof & over (proofPreds . each) (refactor ftn) 
			& over prover (refactor ftn)


instance Refactorable RelationSymbol Proof where
	refactor ftn proof
		= proof & over (proofPreds . each) (refactor ftn) 
			& over prover (refactor ftn)
			& over proofConcl (refactor ftn)

instance Refactorable RuleName Proof where
	refactor ftn proof
		= proof & over (proofPreds . each) (refactor ftn) 
			& over prover (refactor ftn)


data ProofOptions	= PO {	nameParens		:: String -> String,
				showNames		:: Bool,
				showSatisfiesEquality	:: Bool,
				betweenPredicates	:: String }


defaultProofOptions	= PO (\s -> "["++s++"]") True True "    "


-- Extra options to print proofs
data ProofOptions'	= PO' {	opts'		:: ProofOptions,
				st		:: TypeName -> String,
				sp		:: ParseTree -> String,
				se		:: Expression -> String,
				sc 		:: Conclusion' -> String,
				sr		:: Rule -> String
				}

instance ToString Proof where
	toParsable	= toParsable' defaultProofOptions
	toCoParsable	= toCoParsable' defaultProofOptions
	debug		= debug' defaultProofOptions


instance ToString' ProofOptions Proof where
	show' po proof		= let opts	= PO' po show show show show show 				in showProofWith opts proof & unlines
	toParsable' po proof	= let opts	= PO' po id toParsable toParsable toParsable toCoParsable 	in showProofWith opts proof & unlines
	toCoParsable' po proof	= let opts	= PO' po id toCoParsable toParsable toCoParsable toParsable 	in showProofWith opts proof & unlines
	debug' po proof		= let opts	= PO' po id debug debug debug debug			 	in showProofWith opts proof & unlines
		

-- shows a proof part; returns lines 
showProofWith	:: ProofOptions' -> Proof -> [String]
showProofWith opts (ProofIsA expr typ)
		= [sp opts expr ++ " : "++ st opts typ]
showProofWith opts (ProofSame pt e1 e2)
 | showSatisfiesEquality (opts' opts)
	= [se opts e1 ++ " = "++ sp opts pt ++ " = "++ se opts e2]
 | otherwise
	= []
showProofWith opts (Proof concl proverRule predicates)
	= let	options	= opts' opts
		preds'	= predicates |> showProofWith opts
		preds''	= if null preds' then [] else init preds' ||>> (++ betweenPredicates options)  ++ [last preds']
		preds	= preds'' & foldl (stitch ' ') []	:: [String]
		predsW	= ("":preds) |> length & maximum	:: Int
		concl'	= sc opts concl
		lineL	= max predsW (length concl')
		name	= if showNames options then " " ++ nameParens options (get ruleName proverRule) else ""
		lineL'	= lineL - if 3 * length name <= lineL && predsW == lineL then length name else 0
		line	= replicate lineL' '-'		:: String
		line'	= line ++ name
		in
		(preds ++ [line', concl'])



showProofWithDepth		:: String -> Symbol -> ProofOptions -> Either String Proof -> String
showProofWithDepth input relation _ (Left str)	
	= ["# Could not apply relation "++relation++" to the input "++show input++", because:",str] & unlines
showProofWithDepth input relation options (Right proof)
	= ["# "++input++" applied to "++relation
		,"# Proof weight: "++show (weight proof)++", proof depth: "++ show (depth proof) 
		, ""
		, ""
		, toParsable' options proof, "", "", ""] & unlines
