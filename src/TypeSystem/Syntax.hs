 {-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

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

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe
import Data.List as L

import Lens.Micro hiding ((&))
import Lens.Micro.TH

import Control.Monad

{-Represents a syntax: the name of the rule + possible parseways -}
data Syntax	= BNFRules 
			{ _bnf :: Map TypeName [BNF]
			, _wsModes :: Map TypeName WSMode
			, _lattice :: Lattice TypeName
			} deriving (Show)


makeLenses ''Syntax


--------------------------------------- ACCESSORS -----------------------------------



-- The sort is added to make sure the parser first tries "abc", before trying "a". Otherwise, "a" is parsed with an "abc" resting
bnfNames	:: Syntax -> [Name]
bnfNames r	=  r & getBNF & M.keys & sortOn length & reverse



getBNF	:: Syntax -> Map TypeName [BNF]
getBNF 	= get bnf


getWSMode	:: Syntax -> Map TypeName WSMode
getWSMode 	= get wsModes



getFullSyntax	:: Syntax -> Map TypeName ([BNF], WSMode)
getFullSyntax s
	= M.intersectionWith (,) (getBNF s) (getWSMode s)


fromFullSyntax	:: Map TypeName ([BNF], WSMode) -> Syntax
fromFullSyntax dict
	= BNFRules (dict |> fst) (dict |> snd) (dict |> fst & asLattice)


rebuildSubtypings	:: Syntax -> Syntax
rebuildSubtypings s
		= set lattice (get bnf s & asLattice) s

fullSyntax	:: Lens' Syntax (Map TypeName ([BNF], WSMode))
fullSyntax	= lens getFullSyntax (const fromFullSyntax)

instance Refactorable TypeName Syntax where
	refactor ftn (BNFRules bnfs ws _)
		= let	bnfs'	= bnfs ||>> refactor ftn & M.mapKeys ftn
			ws'	= M.mapKeys ftn ws
			lattice'	= asLattice bnfs'
			in
			BNFRules bnfs' ws' lattice'

---------------------------- ABOUT SUBTYPING ------------------------------

topSymbol	= ".*"
bottomSymbol	= "ɛ"

asLattice	:: Map Name [BNF] -> Lattice TypeName
asLattice syntax
	= let	relations	= syntax ||>> fromRuleCall |> catMaybes
					|> S.fromList & invertDict	:: Map Name (Set Name)
		nms		= syntax & M.keys
		topBottom	= M.fromList ((bottomSymbol, S.fromList nms): zip nms (repeat $ S.singleton topSymbol))
		relations'	= M.unionWith S.union relations topBottom
		in fst $ makeLattice bottomSymbol topSymbol relations' 

latticeAsSVG	:: ColorScheme -> Syntax -> String
latticeAsSVG cs s
		= s & get lattice & asSVG id (`elem` [topSymbol, bottomSymbol]) 1 cs


ifNotBottom t	= if t `elem` [bottomSymbol, topSymbol] then Nothing else Just t

smallestCommonType	:: Syntax -> TypeName -> TypeName -> Maybe TypeName
smallestCommonType syntax t1 t2 
	= let 	l = get lattice syntax in
		ifNotBottom $ infimum l t1 t2

smallestCommonType'	:: Syntax -> [TypeName] -> Maybe TypeName
smallestCommonType' s ts
		= ifNotBottom $ infimums (get lattice s) ts


biggestCommonType	:: Syntax -> TypeName -> TypeName -> Maybe TypeName
biggestCommonType s t1 t2
		= ifNotBottom $ supremum (get lattice s) t1 t2

biggestCommonType'	:: Syntax -> [TypeName] -> Maybe TypeName
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
	= biggestCommonType syntax sub super |> (== super) & fromMaybe False
				
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
	= let	called	= get bnf syntax & M.findWithDefault (error $ root ++ " not found in syntax") root
				>>= calledRules	:: [TypeName]
		new	= called & filter (`notElem` alreadyVisited)	:: [TypeName]
		visitd'	= root:alreadyVisited
		new'	= new >>= _reachableVia syntax visitd'
		in
		nub (visitd' ++ new')



---------------------------- Transforming regexes ------------------------------



startRegex	:: Syntax -> BNF -> String
startRegex _ (Literal s)	= translateRegex s
startRegex _ Identifier	=  "[a-z][a-zA-Z0-9]*"
startRegex _ Number	= "-?[0-9]*"
startRegex syntax (BNFRuleCall c)
	= syntax & getBNF & (M.! c) |> startRegex syntax & intercalate "|"
startRegex syntax (BNFSeq (bnf:_))
	= startRegex syntax bnf



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
makeSyntax	:: [(Name, ([BNF], WSMode))] -> Either String Syntax
makeSyntax vals
	= do	let bnfs	= vals & M.fromList |> fst ||>> normalize
		let bnfr	= BNFRules bnfs (M.fromList $ vals ||>> snd) (asLattice bnfs)
		[checkNoDuplicates (vals |> fst) (\duplicates -> "The rule "++showComma duplicates++"is defined multiple times"),
			check bnfr] & allRight_
		return bnfr


instance Check' Syntax (Name, [BNF]) where
	check' s rule@(n, bnfs)
		= inMsg ("While checking the syntax rule "++show n) $ do
			[checkUnknownRuleCall s, checkNoDuplicateChoices, checkTrivial] 
				|> (rule &) & allRight_


checkTrivial	:: (Name, [BNF]) -> Either String ()
checkTrivial (nm, [BNFRuleCall _])
	= inMsg "While checking for triviality" $
		Left $ "The rule "++show nm++" only calls another rule and is trivial. Please remove it"
checkTrivial	_	= pass

checkNoDuplicateChoices	:: (Name, [BNF]) -> Either String ()
checkNoDuplicateChoices (n, asts)
	= inMsg "While checking for duplicate choices" $
	  checkNoDuplicates asts (\dups -> "The choice "++showComma dups++" appears multiple times")

checkUnknownRuleCall	:: Syntax -> (Name, [BNF]) -> Either String ()
checkUnknownRuleCall bnfs' (n, asts)
	= inMsg "While checking for unknowns" $
	  do	let bnfs	= getBNF bnfs'
		mapi asts |> (\(i, ast) ->
			inMsg ("While checking choice "++show i++", namely "++show ast) $
			do	let unknowns = calledRules ast & filter (`M.notMember` bnfs) 
				assert Left (null unknowns) $ "Unknown type "++showComma unknowns
			) & allRight_ & ammendMsg (++"Known rules are "++ showComma (bnfNames bnfs'))
		pass		
			

instance Check Syntax where
	check syntax	= inMsg "While checking the syntax:" $
		  		allRight_ (checkLeftRecursion syntax:checkAllUnique syntax:(syntax & getBNF & M.toList |> check' syntax))


checkLeftRecursion	:: Syntax -> Either String ()
checkLeftRecursion bnfs
	= do	let cycles	= leftRecursions bnfs
		let msg cycle	= cycle & intercalate " -> "
		let msgs	= cycles |> msg |> ("    "++) & unlines
		assert Left (null cycles) ("Potential infinite left recursion detected in the syntax. Left cycles are:\n"++msgs)


checkAllUnique		:: Syntax -> Either String ()
checkAllUnique syntax
	= do	let lookupT	= syntax & get bnf & M.toList 
					& unmerge 
					|> swap
					& merge 
					& filter (not . isRuleCall . fst)
						:: [(BNF, [TypeName])]
		let duplicates	= lookupT & filter ((<) 1 . length . snd)
		
		let msg bnf tns	="The bnf sequence "++toParsable bnf ++ " is presented as a choice in multiple rule declarations, namely "++showComma tns++". Please, separate them of into a new rule"
		duplicates |> uncurry msg |> Left & allRight_		 




---------------------------- TO STRING and other helpers ------------------------------



instance ToString Syntax where
	toParsable (BNFRules rules wsModes _)
		= let 	width	= rules & M.keys |> length & maximum
			merged	= M.intersectionWith (,) rules wsModes in
			merged & M.toList |> (\(n, (r, ws)) -> (n, width, ws, "", r)) 
					& toParsable' "\n" 
	debug		= show


