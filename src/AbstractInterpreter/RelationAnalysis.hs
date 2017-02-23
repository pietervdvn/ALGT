 {-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module AbstractInterpreter.RelationAnalysis (RelationAnalysis(..), TypeNameSpec (..), 
		raSyntax, raIntroduced, raTrivial, analyzeRelations,
		tnsSuper) where

{- Analysises all Rules together -}

import TypeSystem

import Utils.Utils
import Utils.Image (terminalCS)
import Utils.ToString

import Graphs.Lattice

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.ASSubtract as AS
import AbstractInterpreter.RuleAnalysis

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Data.List as L
import Data.Maybe
import Control.Arrow ((&&&))
import Control.Monad

import Lens.Micro hiding ((&))
import Lens.Micro.TH


-------------------------------------------- TYPENAMESPEC ---------------------------------------------

data TypeNameSpec	= TypeNameSpec 
	{ _tnsSuper	:: TypeName
	, _tnsSymb	:: Symbol
	, _tnsMode	:: Mode
	, _tnsInt	:: Int	-- Numbering for input, e.g. REL : x (in), x (out), x(in), x (out) will be numbered (x)(REL)in0, (x)(REL)out0, (x)(REL)in1, (x)(REL)out1
	, _tnsPositive	:: Bool
	} deriving (Show, Ord, Eq)

makeLenses ''TypeNameSpec

invertSpec	= over tnsPositive not


ruleNameFor	:: TypeNameSpec -> String
ruleNameFor (TypeNameSpec tn symbol mode i isPositive) 
	= let baseName	= inParens tn ++ inParens symbol ++ show mode ++ show i
		in
		baseName & (if isPositive then id else ("!"++))




-------------------------------------------- RELATIONANALYSIS DATA DEF ---------------------------------------------

data RelationAnalysis	= RelationAnalysis
	{ _raSyntax	:: Syntax	-- New syntax, with somewhat changed subtyping relationships!
	{- New syntax types introduced (the type they were based on can be found via the spec)
		Mapping to every possible abstract set they can be
	-}
	, _raIntroduced	:: Map TypeNameSpec [AbstractSet]
	{- Syntax rules which were introduced originally, but turned out to be another type (+ the type they turned out to be)
	-}
	, _raTrivial	:: Map TypeNameSpec TypeName
	{- Empty, and thus useless rules -}
	, _raEmpty	:: [TypeName]
	} deriving (Show)

makeLenses ''RelationAnalysis

emptyRA syntax
		= RelationAnalysis syntax M.empty M.empty []


-------------------------------------------- ANALYSIS ---------------------------------------------


analyzeRelations	:: TypeSystem -> RelationAnalysis
analyzeRelations ts 	= createRuleSyntax ts 
				& calculateInverses ts

-------------------------------------------- INVERSION CALCULATION ---------------------------------------------


calculateInverses	:: TypeSystem -> RelationAnalysis -> RelationAnalysis
calculateInverses ts ra	
	=  let 	introduced	= get raIntroduced ra
		tnss		= M.keys introduced
		ra'		= ra	& prepForInverses ts tnss
		-- { e - (e)(→)in0 = !(e)(→)in0, ... }
		subtractions	= M.keys introduced & subtractsTo (get raSyntax ra')
				--	& M.toList |> (\((e, emin), v) -> e ++ "\t- "++ emin ++"\t= "++toParsable' "" v) & unlines & error 
		
		in ra'		& chain (tnss |> addInverseFor subtractions)
				& filterTrivial ts	-- includes refold
				& rebuildSubtypings' ts

prepForInverses		:: TypeSystem -> [TypeNameSpec] -> RelationAnalysis -> RelationAnalysis
prepForInverses ts tnss ra
	= let	tnss'	= tnss |> invertSpec |> (id &&& const []) & M.fromList in
		ra	& over raIntroduced (M.union tnss')
			& rebuildSubtypings' ts	-- also regenerates syntax




addInverseFor		:: Map (TypeName, TypeName) [AbstractSet] -> TypeNameSpec -> RelationAnalysis -> RelationAnalysis
addInverseFor subtractions tns ra 
	= let	inv@(newSpec, forms)
					= inverseFor subtractions ra tns
		tnss			= get raIntroduced ra & M.keys 
		in
		ra	& over raIntroduced (M.insert newSpec forms)


subtractsTo		:: Syntax -> [TypeNameSpec] -> Map (TypeName, TypeName) [AbstractSet]
subtractsTo syntax tnss
	= let	-- All rules from the form (e, (e)(→)in0)  --> [!(e)(→)in0]
		baseSubs	= tnss |> ((get tnsSuper &&& ruleNameFor) &&& (ruleNameFor . invertSpec)) & M.fromList
					|> generateAbstractSet syntax "" |> (:[])
		-- All rules of the form (eL, (e)(→)in0 -->  [] xor [!(eL)(→)in0]), where eL is every subtype of e
		-- If (eL)(→)in0 is an element of (e)(→)in0, then [!(eL)(→)in0] is given, [] otherwise
		subRules	= (tnss >>= subtractsToSubrule syntax) & M.fromList
		in M.union baseSubs subRules


subtractsToSubrule	:: Syntax -> TypeNameSpec -> [((TypeName, TypeName), [AbstractSet])]
subtractsToSubrule s tns
	= do	subtype		<- allSubsetsOf (get lattice s) (get tnsSuper tns) & S.toList
		guard (subtype /= bottomSymbol)
		let tnsSub	= tns & set tnsSuper subtype

		{-
		e	::= ...
			| eL

		(e)(→)	::= ...
			| (eL)(→)

		If this is the case, eL - (e)(→) = !(e)(→)
		If this is not the case, eL - (e)(→) = eL. It's thus no use adding it to the subtractsTo

		-}
		let isContained	= alwaysIsA s (ruleNameFor tnsSub) (ruleNameFor tns)
		guard isContained
		let tnsSubNeg	= invertSpec tnsSub & ruleNameFor & generateAbstractSet s ""
		return ((subtype, ruleNameFor tns), [tnsSubNeg])


inverseFor		:: Map (TypeName, TypeName) [AbstractSet] -> RelationAnalysis -> TypeNameSpec -> (TypeNameSpec, [AbstractSet])
inverseFor subtractions ra posNameSpec
	= let	s		= get raSyntax ra
		rec		= get raIntroduced ra & M.keys |> toParsable
		derivedFrom	= get tnsSuper posNameSpec
		negNameSpec	= invertSpec posNameSpec

		doesContainRec as
				= fromAsSeq' as |> fromEveryPossible 
						& catMaybes 
						& any (`elem` rec)		:: Bool

		all		= derivedFrom & generateAbstractSet s "" & unfold s
		posAll		= ruleNameFor posNameSpec & generateAbstractSet s "" & unfold s
		(posRec', posClass)
				= posAll & partition (\as -> doesContainRec as)
		(posRecPure, posRec) 
				= posRec' & partition isEveryPossible

		{- Positive, pure rulecalls are done as last, to avoid expansion of !(someform), as these will be empty for the moment -}

		negsClass	= subtractAll s all posClass
		negs'		= subtractAllWith s subtractions negsClass posRec
		negs		= subtractAllWith s subtractions negs' posRecPure

		test		= _subtractWith True s subtractions [all !! 1] (posClass !! 1)
		show'		= toParsable' "\n\t | "
		-- to use with Debug.trace
		debugMsg	= ["Calculating inverse "++ toParsable negNameSpec
					, "All:      "++show' all
					, "PosRec:   "++show' posRec
					, "PosClass: "++show' posClass
					, "NegClass: "++show' negsClass
					, "negs'     "++show' negs'
					, "negs:     "++show' negs
					, "test:     "++show' test
					] & unlines

		in
		-- trace debugMsg
		(negNameSpec, negs)





					
inverseForOld		:: [TypeNameSpec] -> RelationAnalysis -> TypeNameSpec -> (TypeNameSpec, [AbstractSet], [AbstractSet])
inverseForOld recursiveForms ra positiveNameSpec	
	= let	positiveName	= ruleNameFor positiveNameSpec
		syntax		= get raSyntax ra

		-- the names of rules introduced by recursive rule calling
		recursiveForms'	= recursiveForms |> ruleNameFor
		nameLookups	= recursiveForms |> (ruleNameFor &&& id)
						& M.fromList		:: Map TypeName TypeNameSpec

		positiveForms	= ra & get raIntroduced & (M.! positiveNameSpec)	:: [AbstractSet]
		
		doesContainRec as	= False
		
		(posRecursive, posClassical)
				= positiveForms & partition doesContainRec		:: ([AbstractSet], [AbstractSet])

		superT		= get tnsSuper positiveNameSpec

		{- consider
		(e)(→)in0	::= "(" (e)(→)in0 ")" | ...

		This means that !(e)(→)in0 should not contain "(" e ")". We transform every "(e)(→)in0" to "e" (thus "(" e ")" ) and subtract that from the superset
		
		-}

		factorAway	= recursiveForms |> (ruleNameFor &&& get tnsSuper)	:: [(TypeName, TypeName)]
		posAsClass	= posRecursive |> refactor' factorAway	-- rewritten forms, thus "(e)(→)in0 e" becomes "e e" here

		negClassical'	= subtractAll syntax 	[generateAbstractSet syntax "" superT]
							(posAsClass ++ posClassical)

		negRecursive	= posRecursive >>= invertRecAS nameLookups		
		negativeForms	= negClassical' ++ negRecursive				



		-- some extra inverted values, for debug/dynamization
		negClassicalExtra
				= subtractAll syntax	[generateAbstractSet syntax "" superT]
							posClassical
		negName		= invertSpec positiveNameSpec
		in
		(negName, negativeForms, refoldWithout syntax [ruleNameFor negName] negClassicalExtra)



{-
For each ruletype, there will be an inverse ruletype:

 (e)(→)in0	::= ...
!(e)(→)in0	::= ...

Note that !( x y)	::= !x y | x !y | !x !y
(if x and y *have* an inverse)

Especially the last "!x !y" is a bit surprising, but necessary:

consider:

x	::= "x"
!x	::= "a"
y	::= "y"
!y	::= "b"

!(x y)	::= "a" "y" | "x" "b" | "a" "b"
-}

invertRecAS	:: Map TypeName TypeNameSpec -> AbstractSet -> [AbstractSet]
invertRecAS nameSpecLookups as
	= do	let asSeq		= fromAsSeq' as & mapi
		let isRecCall as	= fromMaybe False (fromEveryPossible as |> (`M.member` nameSpecLookups))
		let getRecCall as	= (fromEveryPossible as >>= (`M.lookup` nameSpecLookups)) & fromJust
		let fromRecCall tn	= EveryPossible (ruleNameFor tn) "" (ruleNameFor tn)
		let (toFix, classical)	= L.partition (isRecCall . snd) asSeq
		toInvert		<- tail $ subsequences toFix	-- tail: we don't need the empty inverse
		let stayOver		= toFix & filter (`notElem` toInvert)
		let inverted		= toInvert	||>> getRecCall 
							||>> invertSpec
							||>> fromRecCall
		let invertedAsSeq	= (classical ++ stayOver ++ inverted)
						& sortOn fst
						|> snd
		if length invertedAsSeq == 1 then invertedAsSeq
			else return $ AsSeq (typeOf as) (error "Inverted sequences can't be subtracted with, convert them first to BNF") invertedAsSeq
















-------------------------------------------- PREPARE RA -------------------------------------------------------------

debugTrace	:: TypeSystem -> String -> RelationAnalysis -> String
debugTrace ts msg ra
	= let	ra'	= generateSyntax ts ra
		synt	= get raSyntax ra'
		in
		inHeader "" msg '#' (toParsable synt)
	 	++ inHeader "" (msg++inParens "lattice") '.' (debugLattice id $ get lattice synt)

createRuleSyntax	:: TypeSystem -> RelationAnalysis
createRuleSyntax ts
	= let	tr msg ra	= {--} ra  {-}  trace (debugTrace ts msg ra) ra --}
		ra	= prepareSyntax ts	& tr "prepped" 			-- Prepare the syntax, add empty rules for all possible forms
				& addSubchoices ts	& tr "subs added"	-- Add extra choices, e.g. add (eL)(→)in0 as choice to (e)(→)in0
				& rebuildSubtypings' ts	& tr "rebuild"
				& analyse ts 		& tr "analyzed"		-- actually analyse, thus add (origRule)(symbol)in0 ::= form1 | form2 | ...
				& refoldIntro ts	& tr "refolded"	
				& filterTrivial ts	& tr "filtered"	
				& rebuildSubtypings' ts	& tr "rebuild"		-- also regens the syntax
		in ra


rebuildSubtypings'	:: TypeSystem -> RelationAnalysis -> RelationAnalysis
rebuildSubtypings' ts ra
	= let	tnss		= get raIntroduced ra & M.keys
					|> (ruleNameFor &&& get tnsSuper)		:: [(TypeName, TypeName)]
		addRels		= tnss |> (& uncurry addRelation) & chain		:: Lattice TypeName -> Lattice TypeName
		fixSyntax syntax	= syntax & rebuildSubtypings
						& over lattice (removeTransitive' . addRels)
		in
		ra	& generateSyntax ts
			& over raSyntax fixSyntax


addSubchoices	:: TypeSystem -> RelationAnalysis -> RelationAnalysis
addSubchoices ts ra
	= let	syntax		= get raSyntax ra
		l		= get (tsSyntax . lattice) ts
		subsOf tns	= subsetsOf l (get tnsSuper tns) & S.toList
					& L.delete (get bottom l)	:: [TypeName]
		tnsSubsOf tns	= subsOf tns |> (\s -> set tnsSuper s tns)			:: [TypeNameSpec]
		tnsSubsOf' tns	= tnsSubsOf tns |> toParsable |> generateAbstractSet syntax ""	:: [AbstractSet]
		
		ra'	= ra & over raIntroduced (M.mapWithKey (\tns vals -> tnsSubsOf' tns ++ vals))
		in
		ra'


-- removes empty and trivial introduced forms
filterTrivial	:: TypeSystem -> RelationAnalysis -> RelationAnalysis
filterTrivial ts ra
	= let	isTrivial v	= length v == 1 && (v & head & toBNF & isRuleCall)
		(empties, (trivial, intro'))
				= get raIntroduced ra
					& M.partition null & over _1 M.keys
					|> M.partition isTrivial 
		empties'	= empties |> toParsable

		-- remove trivial rules, e.g. "a ::= b"
		trivialBNF	= trivial ||>> toBNF
		trivialMapsTo	= trivialBNF ||>> fromRuleCall |> catMaybes |> head 
		refactoring	= trivialBNF
					||>> fromRuleCall 
					|> catMaybes |> head 
					& M.toList |> over _1 ruleNameFor :: [(TypeName, TypeName)]

		raIntroduced'	= intro' |> L.filter (not . containsRuleAS empties')	-- remove choices calling empty rules
					||>> refactor' refactoring			-- refactor away trivial rules

		ra'	= ra	& over raTrivial (M.union trivialMapsTo)
				& over raEmpty (empties' ++)
				& set raIntroduced raIntroduced'
				& refoldIntro ts
		in
		-- iterate until all empties are removed (removing a choice might render a rule trivial the next step)
		if null trivialBNF && null empties then ra'
			else filterTrivial ts ra' 

refoldIntro	:: TypeSystem -> RelationAnalysis -> RelationAnalysis
refoldIntro ts ra
	= ra	& over raIntroduced (M.mapWithKey 
			(\k -> refoldWithout (get raSyntax ra) [ruleNameFor k]))



generateSyntax	:: TypeSystem -> RelationAnalysis -> RelationAnalysis
generateSyntax ts ra
	= let	possibleArgs	= get raIntroduced ra
					||>> toBNF	:: Map TypeNameSpec [BNF]
		synt		= possibleArgs & M.mapKeys toParsable
		tsSynt		= get tsSyntax ts
		raSynt		= get raSyntax ra
		wsModeOf tn	= M.findWithDefault (error $ "NO WSMODE FOR "++tn) tn (get wsModes raSynt)
		wsModes'	= possibleArgs & M.mapWithKey (\k _ -> wsModeOf $ get tnsSuper k)
					& M.mapKeys toParsable
		in
		ra	& set (raSyntax . bnf) (M.union synt $ get bnf tsSynt)
			& set (raSyntax . wsModes) (M.union (get wsModes tsSynt) wsModes')
		


{-
Actually adds which forms can be applied to the rules
-}
analyse		:: TypeSystem -> RelationAnalysis -> RelationAnalysis
analyse ts ra
	= let	
		findRel' symb	= fromJust $ findRelation ts symb	:: Relation
		relations	= get tsRules ts & M.toList 		:: [(Name, [Rule])]
		relations'	= relations |> over _1 findRel'		:: [(Relation, [Rule])]
		newTypeNames	= createHoleFillFor ts			:: Map Relation [TypeNameSpec]

		filterValid k ass	
				= ass & filter (\as -> toBNF as /= BNFRuleCall (ruleNameFor k))

		possibleArgs	= relations' |> possibleSets ts (get raSyntax ra) newTypeNames
					& M.unionsWith (++) |> nub
					-- remove direct cycles
					& M.mapWithKey filterValid
					:: Map TypeNameSpec [AbstractSet]

		in
		ra & over raIntroduced (M.unionWith (++) possibleArgs)






possibleSets		:: TypeSystem -> Syntax -> Map Relation [TypeNameSpec] -> (Relation, [Rule]) -> Map TypeNameSpec [AbstractSet]
possibleSets ts syntax holeFillers' (rel, rls)
	= let	holeFillers	= holeFillers' ||>> ruleNameFor
		symb		= get relSymbol rel
		inTps		= relTypesWith In rel
		possible	= rls	|> interpretRule' ts	-- interpret the rules abstractly
					& concat
					|> fillHole syntax	-- give fancy, recognizable names to recursive calls
					|> get possibleArgs 	:: [[AbstractSet]]
		possibleTyped	= possible ||>> (typeOf &&& id)
					|> mapi & concat	:: [(Int, (TypeName, AbstractSet))]
		possible'	= possibleTyped
					|> unmerge3r |> merge3l	
					|> over _1 (\(i, tn) -> TypeNameSpec tn symb In i True)	-- creation of the name
					& merge & M.fromList

		in
		possible'

{- Lets rename recursive stuff! 
e.g.	

 cond0 --> cond1
-------------------------------------------------------
 If cond0 then e1 else e2 --> If cond1 then e1 else e2

Gives as possible input args:

 "If e:0 then e:1 else e:2" where "e:0" is applicable

We replace "e:0" with (e)(-->)in0

-}

fillHole	:: Syntax -> RuleApplication -> RuleApplication
fillHole s rapp	= let	subs	= get predicates rapp |> fillHoleForPred s 
				& M.unionsWith (\v1 v2 -> if v1 == v2 then v1 else error $ "TODO/FIXME: common rules (relationanalysis): intersection of "++toParsable v1 ++ " and "++toParsable v2)
				-- TODO what if an argument is part of multiple rules!? Now it'll only have one form! We should use the lowest commond subgroup
			in rapp & applySubsSimple subs


fillHoleForPred	:: Syntax -> AbstractConclusion -> Map Name AbstractSet
fillHoleForPred s (RelationMet rel args)
	= let	inArgs	= filterMode In rel args & mapi
		in
		inArgs |>  fillHoleForArg s rel & catMaybes & M.fromList


fillHoleForArg	:: Syntax -> Relation -> (Int, AbstractSet) -> Maybe (Name, AbstractSet)
fillHoleForArg s r (i, as)
	= do	name	<- getAsName as
		let symb	= get relSymbol r
		let tns	= TypeNameSpec (typeOf as) symb In i True
				& toParsable
		return (name, generateAbstractSet s name tns)

{-
Creates the dictionary {Relation -> [These Rules are generated by it]}
-}
createHoleFillFor	:: TypeSystem -> Map Relation [TypeNameSpec]
createHoleFillFor ts
	= let	specsFor m (r, (symb, tps))
				= (r, tps & mapi |> (\(i, tp) -> TypeNameSpec tp symb m i True))	:: (Relation, [TypeNameSpec])
		specsForMode m	= get tsRelations ts 
					|> (id &&& (get relSymbol &&& relTypesWith m))
					|> specsFor m
		ins		= specsForMode In	:: [(Relation, [TypeNameSpec])]
		outs		= specsForMode Out	:: [(Relation, [TypeNameSpec])]
		in
		(ins ++ outs) & merge & M.fromList |> concat






{-
For each relation, we add a syntax rule representing input and output.

We 'cheat' a little by editting the embedded lattice, to force correct subtyping.

Note that relations with multiple arguments might contain wrong combinations.

E.g. 

(==)	: t (in), t (in)

will generate bnfs

(==)in0	::= t
(==)in1 ::= t

This does not mean every t1 == t2!

However, relations with just one input argument will always match

-}
prepareSyntax	:: TypeSystem -> RelationAnalysis 
prepareSyntax ts
	= let	s		= get tsSyntax ts
		(s', intro)	= foldl (prepRelation s) (s, []) (get tsRelations ts)
		introM		= intro |> (id &&& const []) & M.fromList
		in
		RelationAnalysis s' introM M.empty []

prepRelation	:: Syntax -> (Syntax, [TypeNameSpec]) -> Relation -> (Syntax, [TypeNameSpec])
prepRelation origSyntax s rel
	= let	symbol	= get relSymbol rel
		args	= relTypesWith In rel & mapi ||>> (id &&& const In) in
		foldl (addTypeFor origSyntax symbol rel) s args
		


addTypeFor	:: Syntax -> Symbol -> Relation -> (Syntax, [TypeNameSpec]) -> (Int, (TypeName, Mode)) -> (Syntax, [TypeNameSpec])
addTypeFor origSyntax symb rel (prepSyntax, newtypes) (i, (tn, mode))
	= let	l		= get lattice origSyntax
		subs		= allSubsetsOf l tn & S.toList
					& L.delete (get bottom l)	:: [TypeName]
		tnss		= (tn:subs) |> (\tn' -> TypeNameSpec tn' symb mode i True)
		in
		(foldr addTNS prepSyntax tnss, tnss ++ newtypes)

addTNS	:: TypeNameSpec -> Syntax -> Syntax
addTNS tns s
	= let	tn		= get tnsSuper tns
		newRuleN	= ruleNameFor tns
		wsMode		= get wsModes s M.! tn -- needed for printing and completeness
		s'		= s 	& over bnf (M.insert newRuleN [])
					& over wsModes (M.insert newRuleN wsMode)
		in
		s'



instance ToString' TypeSystem RelationAnalysis where
	toParsable'	= _toString toParsable
	toCoParsable'	= _toString toCoParsable
	debug'		= _toString debug
	show'		= const show

_toString	:: (Syntax -> String) -> TypeSystem -> RelationAnalysis -> String
_toString showSyntax ts ra
	= inHeader "" ("Relation analysis of "++get tsName ts) '=' $
			[ "Following new types were introduced:"
			, get raIntroduced ra & M.keys
				|> (\tns -> toParsable tns  ++ " \t(derived from "++get tnsSuper tns++")") & unlines & indent
			, ""
			, "Following types were ommitted, as they turned out to coincide with some other type:"
			, get raTrivial ra & M.toList |> (\(triv, super) -> toParsable triv ++ " \t== " ++ super) & unlines & indent
			, ""
			, "Following types were omitted, as they turned out to be empty. They might cause 'dissapearing' choices in other rules:"
			, get raEmpty ra & unlines & indent
			, ""
			, debugLattice id $ get (raSyntax . lattice) ra 
			, inHeader "" "Resulting Syntax" '-' $ showSyntax $ get raSyntax ra
			] & unlines

instance ToString TypeNameSpec where
	toParsable	= ruleNameFor



