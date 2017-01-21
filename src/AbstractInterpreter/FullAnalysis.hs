module AbstractInterpreter.FullAnalysis where

{- Analysises all Rules together -}

import TypeSystem

import Utils.Utils
import Utils.Image (terminalCS)
import Utils.ToString

import Graphs.Lattice

import AbstractInterpreter.AbstractSet
import AbstractInterpreter.RuleInterpreter

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Data.List
import Data.Maybe
import Control.Arrow ((&&&))

import Lens.Micro hiding ((&))

type AnalysisSyntax	= Syntax


createRuleSyntax	:: TypeSystem -> Syntax
createRuleSyntax ts
	= let	-- add new, empty rules to the syntax, e.g. (origRule)(symbol)in0
		-- keep note of (newRuleName, origRule)
		(syntax, newRulesAndSuper)	= ts & prepareSyntax & over _1 addExtraSubtypings
		
		addExtraSubtypings s	= over lattice (\l -> foldr (uncurry addRelation) l newRulesAndSuper
									& removeTransitive') s	:: Syntax
		-- actually analyse, thus add (origRule)(symbol)in0 ::= form1 | form2 | ...
		syntax'			= analyse ts syntax 
		-- rebuild subtyping-relations, for the new rules
		syntax''		= syntax' & rebuildSubtypings  & addExtraSubtypings
		-- add extra subtypings, namely every (origRule)(symbol)in0 is also a origRule
		in
		syntax''

ruleNameFor	:: Symbol -> Int -> TypeName -> Mode -> String
ruleNameFor symbol i tn mode
	= inParens tn ++ inParens symbol ++ show mode ++ show i



createHoleFillFor	:: TypeSystem -> Map Relation [Name]
createHoleFillFor ts
	= let	namesForRelation m (r, (symb, tps))
				= (r, tps & mapi |> (\(i, tp) -> ruleNameFor symb i tp m))
		forMode m	= get tsRelations ts 
					|> (id &&& (get relSymbol &&& relTypesWith m))
					|> namesForRelation m
		ins		= forMode In	:: [(Relation, [Name])]
		outs		= forMode Out	:: [(Relation, [Name])]
		in
		(ins ++ outs) & merge ||>> concat & M.fromList


{-
Actually adds which forms can be applied to the rules
-}
analyse		:: TypeSystem -> AnalysisSyntax -> AnalysisSyntax
analyse ts synt
	= let	findRel' symb	= fromJust $ findRelation ts symb
		relations	= get tsRules' ts & get rules & M.toList 
		relations'	= relations
					|> over _1 (findRel')	:: [(Relation, [Rule])]
		holeFillers	= createHoleFillFor ts
		possibleArgs	= relations' |> possibleSets ts synt holeFillers
					& M.unionsWith (++) 
					|> refold synt
					||>> toBNF
		-- remove direct cycles, e.g. "a ::= ... | a | ..."
		possibleArgs'	= possibleArgs & M.mapWithKey (\k bnfs -> bnfs |> _validCall k & catMaybes & nub)
		-- remove trivial rules, e.g. "a ::= b"
		(invalidArgs, possibleArgs'')
				= possibleArgs'	& M.partition (\v -> length v == 1 && (v & head & isRuleCall))
		refactoring	= invalidArgs ||>> fromRuleCall |> catMaybes |> head & M.toList :: [(TypeName, TypeName)]
		in
		synt & over bnf (M.union possibleArgs'') & refactor' refactoring


_validCall	:: TypeName -> BNF -> Maybe BNF
_validCall ruleName bnf
 | fromRuleCall bnf == Just ruleName	= Nothing
 | otherwise			= Just bnf


possibleSets		:: TypeSystem -> Syntax -> Map Relation [Name] -> (Relation, [Rule]) -> Map String [AbstractSet]
possibleSets ts syntax holeFillers (rel, rls)
	= let	symb		= get relSymbol rel
		inTps		= relTypesWith In rel
		possible	= rls	|> interpretRule' ts 		-- interpret the rules abstractly
					& concat
					|> fillHoleWith' holeFillers	-- give fancy, recognizable names
					|> get possibleArgs 
					|> mapi |> zip inTps		-- housekeeping: add indices and types to the bnfs
					& concat
					|> unmerge3r |> merge3l	
					|> over _1 (\(tn, i) -> ruleNameFor symb i tn In)	-- creation of the name
					& merge & M.fromList
		in
		possible

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
prepareSyntax	:: TypeSystem -> (AnalysisSyntax, [(TypeName, TypeName)])
prepareSyntax ts
	= let	s	= get tsSyntax ts in
		foldl (prepRelation s) (s, []) (get tsRelations ts)

prepRelation	:: Syntax -> (Syntax, [(TypeName, TypeName)]) -> Relation -> (AnalysisSyntax, [(TypeName, TypeName)])
prepRelation origSyntax s rel
	= let	symbol	= get relSymbol rel
		args	= get relTypesModes rel & filter ((==) In . snd) & mapi in
		foldl (addTypeFor origSyntax symbol) s args
		


addTypeFor	:: Syntax -> Symbol -> (AnalysisSyntax, [(TypeName, TypeName)]) -> (Int, (TypeName, Mode)) -> (AnalysisSyntax, [(TypeName, TypeName)])
addTypeFor origSyntax symb (s, newTypes) (i, (tn, mode))
	= let	newRuleN	= ruleNameFor symb i tn mode
		wsMode		= get wsModes s M.! tn -- needed for printing and completeness
		s'		= s 	& over bnf (M.insert newRuleN [])
					& over wsModes (M.insert newRuleN wsMode)
		in
		(s', (newRuleN, tn) : newTypes)
