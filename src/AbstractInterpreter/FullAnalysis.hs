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
import Data.List
import Data.Maybe
import Control.Arrow ((&&&))

import Lens.Micro hiding ((&))

type AnalysisSyntax	= Syntax


createRuleSyntax	:: TypeSystem -> Syntax
createRuleSyntax ts
	= ts & prepareSyntax & analyse ts

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
	= let	findRel' symb	= fromJust $ findRelation (get tsRelations ts) symb
		relations	= get tsRules' ts & get rules & M.toList 
		relations'	= relations
					|> over _1 (findRel')	:: [(Relation, [Rule])]
		holeFillers	= createHoleFillFor ts
		possibleArgs	= relations' |> possibleSets ts synt holeFillers
					& M.unionsWith (++) 
					||>> toBNF
		in
		synt & over bnf (M.union possibleArgs)



possibleSets		:: TypeSystem -> Syntax -> Map Relation [Name] -> (Relation, [Rule]) -> Map String [AbstractSet]
possibleSets ts syntax holeFillers (rel, rls)
	= let	symb		= get relSymbol rel
		inTps		= relTypesWith In rel
		possible	= rls |> interpretRule' ts -- interpret the rules abstractly
					& concat
					|> fillHoleWith' holeFillers
					|> get possibleArgs 
					|> mapi |> zip inTps	-- housekeeping: add indices and types
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
prepareSyntax	:: TypeSystem -> AnalysisSyntax
prepareSyntax ts
	= foldl prepRelation (get tsSyntax ts) (get tsRelations ts)

prepRelation	:: Syntax -> Relation -> AnalysisSyntax
prepRelation s rel
	= let	symbol	= get relSymbol rel
		args	= get relTypesModes rel & filter ((==) In . snd) & mapi in
		foldl (addTypeFor symbol) s args
		


addTypeFor	:: Symbol -> AnalysisSyntax -> (Int, (TypeName, Mode)) -> AnalysisSyntax
addTypeFor symb s (i, (tn, mode))
	= let	newRuleN	= ruleNameFor symb i tn mode
		wsMode		= get wsModes s M.! tn -- needed for printing and completeness
				
		in
		s 	& over bnf (M.insert newRuleN [])
			& over wsModes (M.insert newRuleN wsMode)
			& over lattice (fst . addElement newRuleN [] [tn])
