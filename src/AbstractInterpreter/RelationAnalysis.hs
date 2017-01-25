 {-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
module AbstractInterpreter.RelationAnalysis where

{- Analysises all Rules together -}

import TypeSystem

import Utils.Utils
import Utils.Image (terminalCS)
import Utils.ToString

import Graphs.Lattice

import AbstractInterpreter.AbstractSet as AS
import AbstractInterpreter.RuleInterpreter

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Data.List as L
import Data.Maybe
import Control.Arrow ((&&&))

import Lens.Micro hiding ((&))
import Lens.Micro.TH

data TypeNameSpec	= TypeNameSpec 
	{ _tnsSuper	:: TypeName
	, _tnsSymb	:: Symbol
	, _tnsMode	:: Mode
	, _tnsInt	:: Int
	, _tnsPositive	:: Bool
	} deriving (Show, Ord, Eq)

makeLenses ''TypeNameSpec

invertSpec	= over tnsPositive not


ruleNameFor	:: TypeNameSpec -> String
ruleNameFor (TypeNameSpec tn symbol mode i isPositive) 
	= let baseName	= inParens tn ++ inParens symbol ++ show mode ++ show i
		in
		baseName & (if isPositive then id else ("!"++))



data RelationAnalysis	= RelationAnalysis
	{ _raSyntax	:: Syntax	-- New syntax, with somewhat changed subtyping relationships!
	{- New syntax types introduced (the type they were based on can be found via the spec)
		Mapping to every possible abstract set they can be
	-}
	, _raIntroduced	:: Map TypeNameSpec [AbstractSet]	
	{- Syntax rules which were introduced originally, but turned out to be another type (+ the type they turned out to be)
	-}
	, _raTrivial	:: Map TypeNameSpec TypeName
	-- Syntax rules which *don't* match an input, e.g. !(e)(→)in0
	-- , _raNegative	:: [TypeNameSpec]
	} deriving (Show)

makeLenses ''RelationAnalysis


analyzeRelations	:: TypeSystem -> RelationAnalysis
analyzeRelations 	=  calculateInverses . 
				createRuleSyntax



calculateInverses	:: RelationAnalysis -> RelationAnalysis
calculateInverses ra	
	=  let 	introduced	= get raIntroduced ra
		recursiveForms	= M.keys introduced
		addInversesFor	= M.keys introduced
		ra'		= foldr (addInverseFor recursiveForms) ra addInversesFor
		newForms	= ra' & get raIntroduced & M.keys
		in
		ra' & over raSyntax (rebuildSubtypings' newForms)


addInverseFor		:: [TypeNameSpec] -> TypeNameSpec -> RelationAnalysis -> RelationAnalysis
addInverseFor recursiveForms tns ra 
	= let	inv@(newSpec, forms)	= inverseFor recursiveForms ra tns
		tnss			= get raIntroduced ra & M.keys
		in
		ra	& over raIntroduced (M.insert newSpec forms)
			& over raSyntax (\syntax -> 
				syntax 	& (addTNS' newSpec)
					& over bnf  (M.insert (ruleNameFor newSpec) (forms |> toBNF))
				)
					

inverseFor		:: [TypeNameSpec] -> RelationAnalysis -> TypeNameSpec -> (TypeNameSpec, [AbstractSet])
inverseFor recursiveForms ra positiveNameSpec	
	= let	positiveName	= ruleNameFor positiveNameSpec
		syntax		= get raSyntax ra

		-- the names of rules introduced by recursive rule calling
		recursiveForms'	= recursiveForms |> ruleNameFor
		nameLookups	= recursiveForms |> (ruleNameFor &&& id)
						& M.fromList		:: Map TypeName TypeNameSpec

		positiveForms	= ra & get raIntroduced & (M.! positiveNameSpec)	:: [AbstractSet]
		

		-- From here on, stuff is broken
		doesContainRec as
				= fromAsSeq' as |> fromEveryPossible 
						& catMaybes 
						& any (`elem` recursiveForms')		:: Bool

		(posRecursive, posClassical)
				= positiveForms & partition doesContainRec		:: ([AbstractSet], [AbstractSet])

		superT		= get tnsSuper positiveNameSpec
		negClassical'	= subtractAll syntax 	[generateAbstractSet syntax "" superT]
							posClassical
		negRecursive	= posRecursive >>= invertRecAS nameLookups		
		negativeForms	= negClassical' ++ negRecursive				
		in
		(invertSpec positiveNameSpec, negativeForms)


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
		let isRecCall as	= fromMaybe False $ (fromEveryPossible as |> (`M.member` nameSpecLookups))
		let getRecCall as	= (fromEveryPossible as >>= (`M.lookup` nameSpecLookups)) & fromJust
		let fromRecCall tn	= EveryPossible (ruleNameFor tn, -1) "" (ruleNameFor tn)
		let (toFix, classical)	= L.partition (isRecCall . snd) asSeq
		toInvert		<- tail $ subsequences toFix	-- tail: we don't need the empty inverse
		let stayOver		= toFix & filter (`notElem` toInvert)
		let inverted		= toInvert	||>> getRecCall 
							||>> invertSpec
							||>> fromRecCall
		let invertedAsSeq	= (classical ++ stayOver ++ inverted)
						& sortOn fst
						|> snd
		return $ AsSeq (typeOf as, -1) invertedAsSeq

















-------------------------------------------- PREPARE RA -------------------------------------------------------------

rebuildSubtypings'	:: [TypeNameSpec] -> Syntax -> Syntax
rebuildSubtypings' tnss syntax
	= let	tnss'			= tnss 	|> (ruleNameFor &&& get tnsSuper)		:: [(TypeName, TypeName)]
		addRels			= tnss' |> (& uncurry addRelation) & chain		:: Lattice TypeName -> Lattice TypeName
		in
		syntax	& rebuildSubtypings
			& over lattice (removeTransitive' . addRels)


createRuleSyntax	:: TypeSystem -> RelationAnalysis
createRuleSyntax ts
	= let	-- add new, empty rules to the syntax, e.g. (origRule)(symbol)in0
		-- keep note of (newRuleName, origRule)
		(syntax, tnss)		= ts & prepareSyntax
		syntax'			= syntax & rebuildSubtypings' tnss
		
		-- actually analyse, thus add (origRule)(symbol)in0 ::= form1 | form2 | ...
		ra			= analyse ts syntax'
		
		-- rebuild subtyping-relations, for the new rules and add extra subtypings namely every (origRule)(symbol)in0 is also a origRule
		ra'			= ra & over raSyntax
						(rebuildSubtypings' tnss)
		
		-- filter out trivial rules, to neatly return
		-- a rule is trivial if it only contains a call to another rule, e.g. " a ::= b"
		trivialSpecs		= get raTrivial ra' & M.keys	:: [TypeNameSpec]
		ra''			= ra' & over raIntroduced
						(M.filterWithKey (const . not . (`elem` trivialSpecs)))
		in
		ra''




{-
Actually adds which forms can be applied to the rules
-}
analyse		:: TypeSystem -> Syntax -> RelationAnalysis
analyse ts synt
	= let	findRel' symb	= fromJust $ findRelation ts symb	:: Relation
		relations	= get tsRules ts & M.toList 		:: [(Name, [Rule])]
		relations'	= relations |> over _1 findRel'		:: [(Relation, [Rule])]
		newTypeNames	= createHoleFillFor ts			:: Map Relation [TypeNameSpec]

		filterValid k ass	
				= ass & filter (\as -> toBNF as /= BNFRuleCall (ruleNameFor k))

		possibleArgs	= relations' |> possibleSets ts synt newTypeNames
					& M.unionsWith (++) |> nub
					-- remove direct cycles
					& M.mapWithKey filterValid
					|> refold synt
					:: Map TypeNameSpec [AbstractSet]
		-- remove direct cycles, e.g. "a ::= ... | a | ..."
		-- remove trivial rules, e.g. "a ::= b"
		(trivial, possibleArgs')
				= possibleArgs & M.partition (\v -> length v == 1 && (v & head & toBNF & isRuleCall))
		trivialBNF	= trivial ||>> toBNF
		trivialMapsTo	= trivialBNF ||>> fromRuleCall |> catMaybes |> head 
		refactoring	= trivialBNF
					||>> fromRuleCall 
					|> catMaybes |> head 
					& M.toList |> over _1 ruleNameFor :: [(TypeName, TypeName)]
		
		possibleArgsNamed	= possibleArgs' & M.mapKeys ruleNameFor ||>> toBNF
		synt'		= synt	& over bnf (M.union possibleArgsNamed) 
					& refactor' refactoring
		possibleArgs''	= possibleArgs' ||>> refactor' refactoring
		in
		RelationAnalysis
			synt'
			possibleArgs''
			trivialMapsTo







possibleSets		:: TypeSystem -> Syntax -> Map Relation [TypeNameSpec] -> (Relation, [Rule]) -> Map TypeNameSpec [AbstractSet]
possibleSets ts syntax holeFillers' (rel, rls)
	= let	holeFillers	= holeFillers' ||>> ruleNameFor
		symb		= get relSymbol rel
		inTps		= relTypesWith In rel
		possible	= rls	|> interpretRule' ts 		-- interpret the rules abstractly
					& concat
					|> fillHoleWith' holeFillers	-- give fancy, recognizable names to recursive calls
					|> get possibleArgs 
					|> mapi |> zip inTps		-- housekeeping: add indices and types to the bnfs
					& concat
					|> unmerge3r |> merge3l	
					|> over _1 (\(tn, i) -> TypeNameSpec tn symb In i True)	-- creation of the name
					& merge & M.fromList 
		in
		possible



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
prepareSyntax	:: TypeSystem -> (Syntax, [TypeNameSpec])
prepareSyntax ts
	= let	s	= get tsSyntax ts in
		foldl (prepRelation s) (s, []) (get tsRelations ts)

prepRelation	:: Syntax -> (Syntax, [TypeNameSpec]) -> Relation -> (Syntax, [TypeNameSpec])
prepRelation origSyntax s rel
	= let	symbol	= get relSymbol rel
		args	= get relTypesModes rel & filter ((==) In . snd) & mapi in
		foldl (addTypeFor origSyntax symbol) s args
		


addTypeFor	:: Syntax -> Symbol -> (Syntax, [TypeNameSpec]) -> (Int, (TypeName, Mode)) -> (Syntax, [TypeNameSpec])
addTypeFor origSyntax symb prepSyntax (i, (tn, mode))
	= let	tns		= TypeNameSpec tn symb mode i True
		in
		addTNS tns prepSyntax

addTNS'		:: TypeNameSpec -> Syntax -> Syntax
addTNS'	tns s	= fst $ addTNS tns (s, [])

addTNS	:: TypeNameSpec -> (Syntax, [TypeNameSpec]) -> (Syntax, [TypeNameSpec])
addTNS tns (s, newTypes)
	= let	tn		= get tnsSuper tns
		newRuleN	= ruleNameFor tns
		wsMode		= get wsModes s M.! tn -- needed for printing and completeness
		s'		= s 	& over bnf (M.insert newRuleN [])
					& over wsModes (M.insert newRuleN wsMode)
		in
		(s', tns : newTypes)



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
			, inHeader "" "Resulting Syntax" '-' $ showSyntax $ get raSyntax ra
			] & unlines

instance ToString TypeNameSpec where
	toParsable	= ruleNameFor



