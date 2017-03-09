 {-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module TypeSystem.Syntax where

{-
This module defines the 'syntax' - part of a typesystem
-}

import Utils.Utils
import Utils.ToString
import Utils.LatticeImage

import TypeSystem.Types
import TypeSystem.BNF

import Graphs.SearchCycles
import Graphs.Lattice
import qualified Graphs.MinDistance as Grph

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe
import Data.List as L
import Data.Bifunctor (first)

import Lens.Micro hiding ((&), both)
import Lens.Micro.TH

import Control.Monad
import Control.Arrow ((&&&))

{-Represents a syntax: the name of the rule + possible parseways -}
data Syntax	= BNFRules 
			{ _bnf 		:: Map TypeName [BNF]
			, _wsModes 	:: Map TypeName WSMode
			, _groupModes	:: Map TypeName Bool
			, _lattice 	:: Lattice TypeName
			, _minDistance	:: Map Name (Int, [Int])
			} deriving (Show)


makeLenses ''Syntax


--------------------------------------- ACCESSORS -----------------------------------



-- The sort is added to make sure the parser first tries "abc", before trying "a". Otherwise, "a" is parsed with an "abc" resting
bnfNames	:: Syntax -> [Name]
bnfNames r	= let	defined	= r & getBNF & M.keys & sortOn length & reverse
			builtin	= builtinSyntax |> fst |> fst
			in defined ++ builtin



getBNF	:: Syntax -> Map TypeName [BNF]
getBNF 	= get bnf


getWSMode	:: Syntax -> Map TypeName WSMode
getWSMode 	= get wsModes



getFullSyntax	:: Syntax -> Map TypeName ([BNF], WSMode)
getFullSyntax s
	= M.intersectionWith (,) (getBNF s) (getWSMode s)

getFullSyntax'	:: Syntax -> Map TypeName ([BNF], WSMode, Bool)
getFullSyntax' s
	= M.intersectionWith (\a (b, c) -> (a, b, c)) (getBNF s)
		(M.intersectionWith (,) (get wsModes s) (get groupModes s))


fromFullSyntax	:: Map TypeName ([BNF], WSMode) -> Syntax
fromFullSyntax dict
	= let syntax	= BNFRules (dict |> fst) (dict |> snd) M.empty (dict |> fst & asLattice') (distanceTillConcrete syntax) in
		syntax

fromFullSyntax' :: Map TypeName ([BNF], WSMode, Bool) -> Syntax
fromFullSyntax' dict
	= let syntax	= BNFRules (dict |> fst3) (dict |> snd3) (dict |> trd3) (dict |> fst3 & asLattice') (distanceTillConcrete syntax) in
		syntax



rebuildSubtypings	:: Syntax -> Syntax
rebuildSubtypings s
		= set lattice (get bnf s & asLattice') s

fullSyntax	:: Lens' Syntax (Map TypeName ([BNF], WSMode))
fullSyntax	= lens getFullSyntax (const fromFullSyntax)

fullSyntax'	:: Lens' Syntax (Map TypeName ([BNF], WSMode, Bool))
fullSyntax'	= lens getFullSyntax' (const fromFullSyntax')


instance Refactorable TypeName Syntax where
	refactor ftn (BNFRules bnfs ws group _ minDistance)
		= let	bnfs'	= bnfs ||>> refactor ftn & M.mapKeys ftn
			ws'	= M.mapKeys ftn ws
			group'	= M.mapKeys ftn group
			minDistance'	= M.mapKeys ftn minDistance
			lattice'	= asLattice' bnfs'
			in
			BNFRules bnfs' ws' group' lattice' minDistance'

---------------------------- ABOUT SUBTYPING ------------------------------

topSymbol	= ".*"
bottomSymbol	= "ɛ"


asLattice'	:: Map Name [BNF] -> Lattice TypeName
asLattice' s	= asLattice s & checkNoCycles & either error id & fst

asLattice	:: Map Name [BNF] -> Either [[TypeName]] (Lattice TypeName, [(TypeName, TypeName)])
asLattice syntax
	= let	relations	= syntax ||>> fromRuleCall |> catMaybes
					|> S.fromList & invertDict	:: Map Name (Set Name)
		-- let's make sure the 'disconnected' elems are known to the lattice too
		allElems	= syntax |> const S.empty		:: Map Name (Set Name)
		in makeLattice bottomSymbol topSymbol (M.union relations allElems)

latticeAsSVG	:: ColorScheme -> Syntax -> String
latticeAsSVG cs s
		= s & get lattice & asSVG id (`elem` [topSymbol, bottomSymbol]) 1 cs


ifNotBottom t	= if t `elem` [bottomSymbol, topSymbol] then Nothing else Just t

smallestCommonType	:: Syntax -> TypeName -> TypeName -> Maybe TypeName
smallestCommonType syntax t1 t2 
	= let 	l = get lattice syntax in
		ifNotBottom $ infimum l t1 t2

smallestCommonType'	:: Syntax -> [TypeName] -> Maybe TypeName
smallestCommonType' _ []	= Nothing
smallestCommonType' s ts
		= ifNotBottom $ infimums (get lattice s) ts


biggestCommonType	:: Syntax -> TypeName -> TypeName -> Maybe TypeName
biggestCommonType s t1 t2
		= ifNotBottom $ supremum (get lattice s) t1 t2

biggestCommonType'	:: Syntax -> [TypeName] -> Maybe TypeName
biggestCommonType' _ []	= Nothing
biggestCommonType' s ts
		= ifNotBottom $ supremums (get lattice s) ts



{-
Consider following BNF:
x ::= ... | y | ...
y ::= ...

This means that every 'y' also (and always) is an 'x'

alwaysIsA searches these relations:

alwaysIsA rules 'y' 'x'	--> True

-}
alwaysIsA	:: Syntax -> TypeName -> TypeName -> Bool
alwaysIsA syntax sub super
	= let	supr	= supremum (get lattice syntax) sub super
		in supr == super
				
alwaysIsA'	:: (SimplyTyped a) => Syntax -> a -> a -> Bool
alwaysIsA' syntax sub super
	= alwaysIsA syntax (typeOf sub) (typeOf super)


-- Either X is a Y, or Y is a X
equivalent	:: Syntax -> TypeName -> TypeName -> Bool
equivalent r x y
		= alwaysIsA r x y || alwaysIsA r y x


equivalent'	:: (SimplyTyped a) => Syntax -> a -> a -> Bool	
equivalent' s a b
		= equivalent s (typeOf a) (typeOf b)


equivalents r x y	= zip x y & all (uncurry $ equivalent r)
equivalents' r x y	= zip x y & all (uncurry $ equivalent' r)


alwaysAreA	:: Syntax -> Type -> Type -> Bool
alwaysAreA rules sub super
	= let	together	= zip sub super
		params		= init together |> uncurry (flip (alwaysIsA rules)) & and	-- contravariance
		result		= last together &  uncurry       (alwaysIsA rules)		-- covariance
		in		params && result


alwaysAreA'	:: (FunctionlyTyped a) => Syntax -> a -> a -> Bool
alwaysAreA' s sub super
		= alwaysAreA s (typesOf sub) (typesOf super)

---------------------------- Properties about rules ---------------------------


firstCalls	:: Syntax -> Map TypeName (Set TypeName)
firstCalls rules
	= rules & getBNF ||>> firstCall |> catMaybes |> S.fromList


leftRecursions	:: Syntax -> [[TypeName]]
leftRecursions	= cleanCycles . firstCalls



mightContainA	:: Syntax -> TypeName -> TypeName -> Bool
mightContainA syntax searched origin
	= reachableVia syntax origin & elem searched


mightContainA'	:: (SimplyTyped a) => Syntax -> a -> a -> Bool
mightContainA' s searched origin
	= mightContainA s (typeOf searched) (typeOf origin)

{- Deduces wether a certain value can be parsed as subtree of the given rule
e.g.

a	::= "A" b | d
...

reachableVia "a" --> [b, d]
Note that a is _not_ in this list

a	::= "A" b
b	::= "X" | a

reachableVia "a" --> [a, b]

-}
reachableVia	:: Syntax -> TypeName -> [TypeName]
reachableVia rules
	= _reachableVia rules []

_reachableVia	:: Syntax -> [TypeName] -> TypeName -> [TypeName]
_reachableVia syntax alreadyVisited root
 | isBuiltinName root	= [root]
 | otherwise
	= let	called	= get bnf syntax & M.findWithDefault (error $ root ++ " not found in syntax") root
				>>= calledRules	:: [TypeName]
		new	= called & filter (`notElem` alreadyVisited)	:: [TypeName]
		visitd'	= root:alreadyVisited
		new'	= new >>= _reachableVia syntax visitd'
		in
		nub (visitd' ++ new')



---------------------------- Transforming regexes ------------------------------

{-
A rule containing a sequence of Literals/Builtins is concrete and has distance 0:

x ::= "X"

a rule calling this x has distance (x + 1):

y ::= x		-> 1

A rule calling both, has the minimum of them, + 1

z ::= x | y	-> 1

-}
distanceTillConcrete	:: Syntax -> Map Name (Int, [Int])
distanceTillConcrete s
	= let	connected	= s & get bnf ||>> calledRules'	:: Map Name [[Name]]
		initial		= connected |> (\crs -> if any null crs then Just 0 else Nothing)
		minDist		= Grph.minDistance (connected |> S.fromList) initial
		choicesCost	= connected ||>> (|> (minDist M.!)) 
					||>> (\choices -> if null choices then 0::Int else maximum choices)
					:: Map Name [Int]
		in
		M.intersectionWith (,) minDist choicesCost




-- Used for syntax highlightingx
consumeOne	:: BNF -> Maybe BNF
consumeOne (BNFSeq (_:bnf:bnfs))
	= Just $ BNFSeq (bnf:bnfs)
consumeOne _
	= Nothing

consumeOne'	:: (String -> String) -> Syntax -> Maybe Syntax
consumeOne' nameEdit syntax
	= let	bnfs'	= syntax & getBNF & M.toList 
				||>> (|> consumeOne) ||>> catMaybes 	-- consume ones
				& L.filter (not . L.null . snd)		-- remove empty rulesm
				|> over _1 nameEdit			-- rename stuff
				& M.fromList
		in if M.null bnfs' then Nothing else Just $ set bnf bnfs' syntax


-- If the 'choice' is a single rule call, inline it
inline		:: Syntax -> BNF -> [BNF]
inline syntax (BNFRuleCall nm)
	= syntax & getBNF & (M.! nm)
inline _ bnf
	= [bnf]


inline'		:: Syntax -> Syntax
inline' syntax
	= let	bnfs'	= syntax & getBNF & M.toList ||>> (>>= inline syntax) & M.fromList in
		set bnf bnfs' syntax



---------------------------- Checks and constructors -------------------------------





-- constructor, with checks
makeSyntax	:: [(Name, ([BNF], WSMode, Bool))] -> Either String Syntax
makeSyntax vals
	= do	let bnfs	= vals & M.fromList |> fst3 ||>> normalize
		let supertypings	= asLattice' bnfs
		let bnfr	= BNFRules bnfs 
					(vals ||>> snd3 & M.fromList) 
					(vals ||>> trd3 & M.fromList)
					supertypings
					(distanceTillConcrete bnfr)	-- yeah, we're tying a knot here, no probs
		checkNoDuplicates (vals |> fst) (\duplicates -> "The rule "++showComma duplicates++"is defined multiple times")
		return bnfr


instance Check' Syntax (Name, ([BNF], WSMode, Bool)) where
	check' s rule@(n, bnfs)
	      = [checkUnknownRuleCall s
		, checkNoDuplicateChoices
		, checkTrivial] |> (rule &) & allRight_
			& inMsg ("While checking the syntax rule "++show n)


checkTrivial	:: (Name, ([BNF], WSMode, Bool)) -> Either String ()
checkTrivial (nm, ([BNFRuleCall n], _, False))
 | not (isBuiltinName n)
	= inMsg "While checking for triviality" $
		Left $ "The rule "++show nm++" only calls another rule and is trivial. Please remove it"
checkTrivial	_	= pass

checkNoDuplicateChoices	:: (Name, ([BNF], a, b)) -> Either String ()
checkNoDuplicateChoices (n, (asts, _, _))
	= inMsg ("While checking for duplicate choices in rule "++n) $
	  checkNoDuplicates asts (\dups -> "The choice "++showComma dups++" appears multiple times")

checkUnknownRuleCall	:: Syntax -> (Name, ([BNF], a, b)) -> Either String ()
checkUnknownRuleCall bnfs' (n, (asts, _, _))
	= inMsg "While checking for unknowns" $
	  do	let bnfs	= getBNF bnfs'
		mapi asts |> (\(i, ast) ->
			inMsg ("While checking choice "++show i++", namely '"++toParsable ast++"'") $
			do	let unknowns = calledRules ast & filter (not . isBuiltinName)
						& filter (`M.notMember` bnfs) 
				assert Left (null unknowns) $ "Unknown type "++showComma unknowns
			) & allRight_ & ammendMsg (++"Known rules are "++ showComma (bnfNames bnfs'))
		pass		
	


instance Check Syntax where
	check syntax	= inMsg "While checking the syntax" $ do
		  		allRight_ $
					[checkLeftRecursion syntax
					, checkAllUnique syntax
					, checkNoCommonSubsets syntax
					, checkUnneededTransitive syntax
					] ++ (syntax & get fullSyntax' & M.toList |> check' syntax)
				-- we check dead choices afterwards, as common subsets might cause this test to hang
				checkDeadChoices syntax 


checkLeftRecursion	:: Syntax -> Either String ()
checkLeftRecursion bnfs
	= do	let cycles	= leftRecursions bnfs
		let msg cycle	= cycle & intercalate " -> "
		let msgs	= cycles |> msg |> ("    "++) & unlines
		assert Left (null cycles) ("Potential infinite left recursion detected in the syntax.\nLeft cycles are:\n"++msgs)


checkAllUnique		:: Syntax -> Either String ()
checkAllUnique syntax
	= do	let lookupT	= syntax & get bnf & M.toList 
					& unmerge 
					|> swap
					& merge 
					& filter (not . isRuleCall . fst)
						:: [(BNF, [TypeName])]
		let duplicates	= lookupT & filter ((<) 1 . length . snd)
		
		let msg bnf tns	="The bnf sequence "++toParsable bnf ++ " is presented as a choice in multiple rule declarations, "
					++"namely "++showComma tns++". Please, separate them of into a new rule"
		duplicates |> uncurry msg |> Left & allRight_	


getBNFSFor		:: Syntax -> Name -> Either String [BNF]
getBNFSFor s n	= checkExists n (get bnf s) $ "No rule with name "++n++" found"


checkNoCommonSubsets	:: Syntax -> Either String ()
checkNoCommonSubsets s
	= do	common	<- commonSubsets s (get bnf s & M.keys)
		let msg n0 n1 common
			= "Syntactic forms "++n0 ++" and "++n1++" both have a common subset of choices.\n"
				++ "Please, separate these into a new rule.\n"
				++ "Common choices are: "++toParsable' " | " common
		common |> uncurry3 msg |> Left & allRight_


commonSubsets	:: Syntax -> [Name] -> Either String [(TypeName, TypeName, [BNF])]
commonSubsets _ []	= return []
commonSubsets _ [nm]	= return []
commonSubsets s (nm:nms)
	= do	nmComm	<- nms |+> commonSubsetsBetween s nm |> catMaybes
		rest	<- commonSubsets s nms
		return $ nmComm ++ rest
		

commonSubsetsBetween	:: Syntax -> TypeName -> TypeName -> Either String (Maybe (TypeName, TypeName, [BNF]))
commonSubsetsBetween s ruleToCheck against
	= do	bnfs	<- getBNFSFor s ruleToCheck 
		bnfs'	<- getBNFSFor s against
		-- we check that this rule (bnfs) do not have a set in common (with more then one element)
		let common	=  bnfs `intersect` bnfs'
		if length common > 1 then return $ Just (ruleToCheck, against, common)
			else return Nothing

checkUnneededTransitive	:: Syntax -> Either String ()
checkUnneededTransitive s
	= inMsg "While checking for unneeded transitivity in the subtyping relationship" $
	  do	(_, unneeded')	<- s & get bnf & asLattice 
					& checkNoCycles
		let unneeded	= unneeded' & filter (both (not . (`elem` [topSymbol, bottomSymbol])))
		let msg		= unneeded |> (\(tsup, tsub) -> 
					"Every "++show tsub ++" is a "++show tsup++", but this is already known.\nPlease remove this choice.") 
					& unlines
		unless (null unneeded) $ Left msg


checkNoCycles	:: Either [[TypeName]] a -> Either String a
checkNoCycles (Left cycles)
	= cycles |> intercalate " ⊂ " 
		& unlines 
		& indent 
		& ("Cycles detected in the supertyping relationship:\n"++)
		& Left
checkNoCycles (Right a)
	= Right a


checkDeadChoices	:: Syntax -> Either String ()
checkDeadChoices s
	= get bnf s & M.toList |> checkDeadChoice s & allRight_


checkDeadChoice	:: Syntax -> (Name, [BNF]) -> Either String ()
checkDeadChoice s (nm, bnfs)
	= inMsg ("While checking for dead choices in "++nm) $ do
		let dead	= deadChoices s bnfs
		let msgFor dead killer
				= [ "The choice '"++toParsable dead++"' will never be parsed."
					, "The previous choice '"++toParsable killer++"' will already consume a part of it."
					, "Swap them and you'll be fine." ] & unlines
		let msg		= dead |> uncurry msgFor
					& unlines
		unless (null dead) $ Left msg 


deadChoices	:: Syntax -> [BNF] -> [(BNF, BNF)]
deadChoices s []	= []
deadChoices s [bnf]	= []
deadChoices s bnfs
		= let	victim		= last bnfs
			killers		= bnfs & init
						|> (getsKilledBy s victim &&& id)
						& filter fst |> snd
			rest		= deadChoices s $ init bnfs
			in
			zip (repeat victim) killers ++ rest


getsKilledBy	:: Syntax -> BNF -> BNF -> Bool
getsKilledBy s victim killer
	= let	victim'	= fromSeq' victim	-- e.g. value eL e
		killer'	= fromSeq' killer	-- e.g. eL e
		
		prefixIsSub	= zip victim' killer'
					|> uncurry (bnfAlwaysIsA s)
					& and
		in
		length victim' >= length killer' && prefixIsSub


bnfAlwaysIsA	:: Syntax -> BNF -> BNF -> Bool
bnfAlwaysIsA s (BNFRuleCall tpSub) (BNFRuleCall tpSup)
		= alwaysIsA s tpSub tpSup
bnfAlwaysIsA _ bnf0 bnf1
		= bnf0 == bnf1


---------------------------- TO STRING and other helpers ------------------------------



instance ToString Syntax where
	toParsable (BNFRules rules wsModes group _ _)
		= let 	width	= rules & M.keys |> length & maximum
			merged	= M.intersectionWith (,) rules (M.intersectionWith (,) wsModes group) in
			merged & M.toList |> (\(n, (r, (ws, group))) -> (n, width, ws, group, "", r)) 
					& toParsable' "\n" 
	debug		= show


