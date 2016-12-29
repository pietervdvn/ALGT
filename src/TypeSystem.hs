 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module TypeSystem where

{-
This module defines all the important data structures, used throughout the entire program.

This document is thus an excellent starting point to grasping everything.

Note: often I code 'left to right'. Herefore, a few operators are defined in Utils.
Most prominents are:
a & f 	=== f a
ls |> f	=== map f a
ls |+> f	=== mapM f a

-}

import Utils.Utils

import Graphs.SearchCycles

import Data.Maybe
import Data.Either

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List as L

import Data.Bifunctor

import Control.Arrow ((&&&))
import Control.Monad (foldM, when)


------------------------ Syntax -------------------------



{- Syntax is described in a Backus-Naur format, a simple naive parser is constructed from it. -}

data BNF 	= Literal String	-- Literally parse 'String'
		| Identifier		-- Parse an identifier
		| Number		-- Parse a number
		| BNFRuleCall Name	-- Parse the rule with the given name
		| BNFSeq [BNF]	-- Sequence of parts
	deriving (Show, Eq)





fromSingle	:: BNF -> Maybe BNF
fromSingle (BNFSeq [bnf])	= Just bnf
fromSingle (BNFSeq _)		= Nothing
fromSingle bnf			= Just bnf


fromRuleCall	:: BNF -> Maybe Name
fromRuleCall (BNFRuleCall nm)	= Just nm
fromRuleCall _			= Nothing

isRuleCall	:: BNF -> Bool
isRuleCall (BNFRuleCall _)	= True
isRuleCall _			= False


calledRules	:: BNF -> [TypeName]
calledRules (BNFRuleCall nm)	= [nm]
calledRules (BNFSeq bnfs)	= bnfs >>= calledRules
calledRules _			= []


-- First call, without consumption of a character
firstCall	:: BNF -> Maybe TypeName
firstCall (BNFRuleCall nm)	= Just nm
firstCall (BNFSeq (ast:_))	= firstCall ast
firstCall _			= Nothing







data WSMode	= IgnoreWS | StrictWS | StrictWSRecursive
	deriving (Show)

enterRule	:: WSMode -> WSMode
enterRule StrictWS	= IgnoreWS
enterRule wsMode	= wsMode


strictest		:: WSMode -> WSMode -> WSMode
strictest StrictWSRecursive _	= StrictWSRecursive
strictest StrictWS IgnoreWS	= StrictWS
strictest IgnoreWS IgnoreWS	= IgnoreWS
strictest a b			= strictest b a

{-Represents a syntax: the name of the rule + possible parseways -}
data Syntax	= BNFRules { getBNF :: Map TypeName [BNF], getWSMode :: Map TypeName WSMode}
	deriving (Show)

getFullSyntax	:: Syntax -> Map TypeName ([BNF], WSMode)
getFullSyntax s
	= M.intersectionWith (,) (getBNF s) (getWSMode s)

startRegex	:: Syntax -> BNF -> String
startRegex _ (Literal s)	= translateRegex s
startRegex _ Identifier	=  "[a-z][a-zA-Z0-9]*"
startRegex _ Number	= "-?[0-9]*"
startRegex syntax (BNFRuleCall c)
	= syntax & getBNF & (M.! c) |> startRegex syntax & intercalate "|"
startRegex syntax (BNFSeq (bnf:_))
	= startRegex syntax bnf



-- Used for syntax highlighting
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
				|> first nameEdit			-- rename stuff
				& M.fromList
		in if M.null bnfs' then Nothing else Just $ syntax{getBNF = bnfs'}


-- If the 'choice' is a single rule call, inline it
inline		:: Syntax -> BNF -> [BNF]
inline syntax (BNFRuleCall nm)
	= syntax & getBNF & (M.! nm)
inline _ bnf
	= [bnf]


inline'		:: Syntax -> Syntax
inline' syntax
	= let	bnfs'	= syntax & getBNF & M.toList ||>> (>>= inline syntax) & M.fromList in
		syntax{getBNF = bnfs'}


renameRule	:: (String -> String) -> BNF -> BNF
renameRule f (BNFRuleCall s)
		= BNFRuleCall $ f s
renameRule f (BNFSeq as)
		= as |> renameRule f & BNFSeq
renameRule _ bnf
		= bnf



-- constructor, with checks
makeSyntax	:: [(Name, ([BNF], WSMode))] -> Either String Syntax
makeSyntax vals
	= do	let bnfr	= BNFRules (M.fromList $ vals ||>> fst) (M.fromList $ vals ||>> snd)
		[checkNoDuplicates (vals |> fst) (\duplicates -> "The rule "++showComma duplicates++"is defined multiple times"),
			checkBNF bnfr] & allRight_
		return bnfr




checkBNF	:: Syntax -> Either String ()
checkBNF bnfs	= inMsg "While checking the syntax:" $
		  	allRight_ (checkLeftRecursion bnfs:(bnfs & getBNF & M.toList |> checkUnknownRuleCall bnfs))

checkUnknownRuleCall	:: Syntax -> (Name, [BNF]) -> Either String ()
checkUnknownRuleCall bnfs' (n, asts)
	= inMsg ("While checking rule "++n++" for unknowns") $
	  do	let bnfs	= getBNF bnfs'
		mapi asts |> (\(i, ast) ->
			inMsg ("While checking choice "++show i++", namely "++show ast) $
			do	let unknowns = calledRules ast & filter (`M.notMember` bnfs) 
				assert Left (null unknowns) $ "Unknown type "++showComma unknowns
			) & allRight_ & ammendMsg (++"Known rules are "++ showComma (bnfNames bnfs')) >> return ()
		
			

checkLeftRecursion	:: Syntax -> Either String ()
checkLeftRecursion bnfs
	= do	let cycles	= leftRecursions bnfs
		let msg cycle	= cycle & intercalate " -> "
		let msgs	= cycles |> msg |> ("    "++) & unlines
		assert Left (null cycles) ("Potential infinite left recursion detected in the syntax. Left cycles are:\n"++msgs)





-- The sort is added to make sure the parser first tries "abc", before trying "a". Otherwise, "a" is parsed with an "abc" resting
bnfNames	:: Syntax -> [Name]
bnfNames r	=  r & getBNF & M.keys & sortOn length & reverse


mightContainA	:: Syntax -> TypeName -> TypeName -> Bool
mightContainA syntax searched origin
	= reachableVia syntax origin & elem searched

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
_reachableVia r alreadyVisited root
	= let	called	= r & getBNF & (M.! root) >>= calledRules	:: [TypeName]
		new	= called & filter (`notElem` alreadyVisited)	:: [TypeName]
		visitd'	= root:alreadyVisited
		new'	= new >>= _reachableVia r visitd'
		in
		nub (visitd' ++ new')


firstCalls	:: Syntax -> Map TypeName (Set TypeName)
firstCalls rules
	= rules & getBNF ||>> firstCall |> catMaybes |> S.fromList

leftRecursions	:: Syntax -> [[TypeName]]
leftRecursions	= cleanCycles . firstCalls


smallestCommonType	:: Syntax -> TypeName -> TypeName -> Maybe TypeName
smallestCommonType syntax t1 t2 
 | t1 == t2			= Just t1
 | alwaysIsA syntax t1 t2	= Just t2
 | alwaysIsA syntax t2 t1	= Just t1
 | otherwise {- Unrelated types -}
				= Nothing

smallestCommonType'	:: Syntax -> [TypeName] -> Maybe TypeName
smallestCommonType' _ []	= Nothing
smallestCommonType' s [t]	= Just t
smallestCommonType' s (t:ts)
			= do	t'	<- smallestCommonType' s ts
				smallestCommonType s t t'

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
 | super == "" || sub == ""
		= True	-- The empty string is used in dynamic cases, thus is equivalent to everything
 | sub == super	= True
 | super `M.notMember` getBNF syntax
	= error $ "Unknwown super name: "++show super
 | sub `M.notMember` getBNF syntax
	= error $ "Unknwown sub name: "++show sub
 | otherwise	-- super-rule should contain a single occurence, sub or another rule
	= let	rules	= getBNF syntax
		superR	= (rules M.! super) |> fromSingle & catMaybes
		-- this single element should be a BNFRuleCall
		superR'	= superR |> fromRuleCall & catMaybes
		-- either sub is an element from superR', or it has a rule which is a super for sub
		-- we don't have to worry about loops; this is left recursion and is checked against

		-- in one special case, " sub ::= super ", they are equals too
		-- we lookup the subRule, check if it has one call...
		subR	= (rules M.! sub)
		-- and has exactly one choice
		equalRules	= length subR == 1 && head subR == BNFRuleCall super
		in equalRules || sub `elem` superR' || or (superR' |> alwaysIsA syntax sub)
				
-- Either X is a Y, or Y is a X
equivalent	:: Syntax -> TypeName -> TypeName -> Bool
equivalent r x y
		= alwaysIsA r x y || alwaysIsA r y x

equivalents r x y	= zip x y & all (uncurry $ equivalent r)

alwaysAreA	:: Syntax -> Type -> Type -> Bool
alwaysAreA rules sub super
	= let	together	= zip sub super
		params		= init together |> uncurry (flip (alwaysIsA rules)) & and	-- contravariance
		result		= last together &  uncurry       (alwaysIsA rules)		-- covariance
		in		params && result





-- same as mergeContext, but on a list
mergeContexts	:: Syntax -> [Map Name TypeName] -> Either String (Map Name TypeName)
mergeContexts bnfs
		= foldM (mergeContext bnfs) M.empty



-- Merges two contexts (variable names --> expected types) according to the subtype relationsship defined in the given bnf-rules
mergeContext	:: Syntax -> Map Name TypeName -> Map Name TypeName -> Either String (Map Name TypeName)
mergeContext bnfs ctx1 ctx2
	= let msg v t1 t2 	= v ++ " is typed as both "++show t1++" and "++show t2 in
		mergeContextWith msg (equivalent bnfs) ctx1 ctx2

checkPatterns		:: Syntax -> Map Name TypeName -> Map Name TypeName -> Either String (Map Name TypeName)
checkPatterns bnfs pats usages
	= let msg v t1 t2	= v ++ " is deduced a "++show t1++" by its usage in the patterns, but used as a "++show t2 in
		mergeContextWith msg (alwaysIsA bnfs) pats usages



-- Merges two contexts, according to valid combination. 
mergeContextWith	:: (Name -> TypeName -> TypeName -> String) -> (TypeName -> TypeName -> Bool) -> 
				Map Name TypeName -> Map Name TypeName -> Either String (Map Name TypeName)
mergeContextWith msg' validCombo ctx1 ctx2
		= do	let	common		= (ctx1 `M.intersection` ctx2) & M.keys	:: [Name]
			let ctx1'	= common |> (ctx1 M.!)
			let ctx2'	= common |> (ctx2 M.!)
			-- for each common key, we see wether they are equivalent
			let conflicts	= zip common (zip ctx1' ctx2')
						||>> uncurry validCombo
						& filter (not . snd) |> fst
			let msg n	= msg' n (ctx1 M.! n) (ctx2 M.! n)
			if null conflicts then return (M.union ctx1 ctx2) else Left ("Conflicts for variables "++show conflicts++":\n"++unlines (conflicts |> msg))



------------------------ Syntax Highlighting --------------

-- A little extra, cause I like fancy colors

data SyntaxStyle = SyntaxStyle
	{ baseStyles	:: Map TypeName Name
	, extraStyles	:: Map (TypeName, Int) Name
	, styleRemaps	:: Map Name Name	-- e.g. noise maps to comment
	} deriving (Show)


------------------------ functions -------------------------

-- functions do transform syntax trees (by rewrite rules) and are often used in typechecking and evaluation


type TypeName	= Name
type Type	= [TypeName]	

class SimplyTyped a where
	typeOf	:: a -> TypeName

class FunctionlyTyped a where
	typesOf	:: a -> Type


{- A Expression is always based on a corresponding syntacic rule.
 It can be both for deconstructing a parsetree or constructing one (depending wether it is used as a pattern or not)
-}
type Builtin	= Bool
-- info about which BNF-rule was used constructing the ParseTree
type MInfo	= (TypeName, Int)


-- Represents values that can only come from target language
data ParseTree
	= MLiteral MInfo String			-- Generated by a literal expression
	| MIdentifier MInfo Name		-- identifier, generated by 'Identifier'
	| MInt MInfo Int			-- number, generated by 'Number'
	| PtSeq MInfo [ParseTree]		-- Sequence of stuff
	deriving (Show, Ord, Eq)

instance SimplyTyped ParseTree where
	typeOf pt	= typeInfoOf' pt & either id fst

typeInfoOf'		:: ParseTree -> Either TypeName (TypeName, Int)
typeInfoOf' (MLiteral tp _)		= Right tp
typeInfoOf' (MInt tp _) 		= Right tp
typeInfoOf' (MIdentifier tp _)		= Right tp
typeInfoOf' (PtSeq tp _)		= Right tp


replace	:: ParseTree -> [Int] -> ParseTree -> ParseTree
replace _ [] toPlace	= toPlace
replace (PtSeq tp orig) (i:rest) toPlace
 | length orig <= i
	= error $ "Invalid substitution path: index "++show i++" to big for " ++show orig
 | otherwise
	= let	(init, head:tail)	= splitAt i orig
		head'		= replace head rest toPlace in
		(init ++ (head':tail)) & PtSeq tp
replace rest path toReplace
	= error $ "Invalid substitution path: not a sequence, but trying to execute the path "++show path++" on " ++show rest

isMInt'	:: ParseTree -> Bool
isMInt' (MInt _ _)	= True
isMInt' _		= False

isPtSeq	:: ParseTree -> Bool
isPtSeq (PtSeq _ _)	= True
isPtSeq _		= False

usedIdentifiers'	:: ParseTree -> [Name]
usedIdentifiers' (MIdentifier _ nm)	= [nm]
usedIdentifiers' (PtSeq _ pts)		= pts >>= usedIdentifiers'




{-
Advanced expressions with evaluation contexts and variables and such
-}
data Expression
	= MParseTree ParseTree				-- a 'value'
	| MVar TypeName Name				-- a variable
	| MSeq MInfo [Expression]			
	| MCall TypeName Name Builtin [Expression]	-- function call; not allowed in pattern matching
	| MAscription TypeName Expression 		-- checks wether the expression is built by this smaller rule.
	| MEvalContext TypeName Name Expression	-- describes a pattern that searches a context
	deriving (Show, Ord, Eq)

instance SimplyTyped Expression where
	typeOf e	= typeInfoOf e & either id fst

-- returns as much typeinfo as possible, thus also the parsing rule choice (index of the option) if possible
typeInfoOf	:: Expression -> Either TypeName (TypeName, Int)
typeInfoOf (MVar tp _)			= Left tp
typeInfoOf (MSeq tp _)			= Right tp
typeInfoOf (MCall tp _ _ _)		= Left tp
typeInfoOf (MAscription tp _)		= Left tp
typeInfoOf (MEvalContext tp _ _)	= Left tp
typeInfoOf (MParseTree pt)		= typeInfoOf' pt


isMInt	:: Expression -> Bool
isMInt (MParseTree pt)	= isMInt' pt
isMInt _		= False


usedIdentifiers	:: Expression -> [Name]
usedIdentifiers (MParseTree pt)		= usedIdentifiers' pt
usedIdentifiers (MSeq _ exprs)		= exprs >>= usedIdentifiers
usedIdentifiers (MCall _ _ _ exprs)	= exprs >>= usedIdentifiers
usedIdentifiers (MAscription _ expr)	= usedIdentifiers expr
usedIdentifiers (MEvalContext _ fnm hole)
					= fnm : usedIdentifiers hole
usedIdentifiers	_			= []

-- generates an MVar, with a name that does not occurr in the given expression
unusedIdentifier	:: Expression -> Maybe Name -> TypeName -> ParseTree
unusedIdentifier noOverlap baseName productionType 
	= let	name	= fromMaybe "x" baseName
		alreadyUsed = name: usedIdentifiers noOverlap
		varName	= [0..] |> show |> (name++) & filter (`notElem` alreadyUsed) & head
		in
		MIdentifier (productionType, -2) varName



-- walks a  expression, gives which variables have what types
expectedTyping	:: Syntax -> Expression -> Either String (Map Name TypeName)
expectedTyping _ (MVar mt nm)	= return $ M.singleton nm mt
expectedTyping r (MSeq _ mes)		= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping r (MCall _ _ _ mes)	= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping r (MAscription _ e)		= expectedTyping r e
expectedTyping r (MEvalContext tp fnm hole)
					= expectedTyping r hole >>= mergeContext r (M.singleton fnm tp)
expectedTyping _ (MParseTree _)	= return M.empty












-- Patterns, used to deconstruct values (parsetrees) and capture variables, to calculate the end expression
data Clause	= MClause {mecPatterns :: [Expression], mecExpr :: Expression}
	deriving (Show, Ord, Eq)

instance FunctionlyTyped Clause where
	typesOf (MClause pats e)	= (pats |> typeOf) ++ [typeOf e]

-- a function; pattern matching goes from first to last clause
data Function	= MFunction Type [Clause]
	deriving (Show, Ord, Eq)

instance FunctionlyTyped Function where
	typesOf (MFunction t _)	= t


type Functions	= Map Name Function








----------------------- Relations and Rules ------------------------


type Symbol		= Name
data Mode		= In | Out
	deriving (Ord, Eq)

instance Show Mode where
	show In		= "in"
	show Out	= "out"

-- A relation. Some relations might be able to produce values, given a few input variables (e.g. a evaluation or typing rule)
data Relation		= Relation {relSymbol :: Symbol, relTypesModes :: [(TypeName, Mode)], relPronounce :: Maybe String }
	deriving (Show, Ord, Eq)


-- Relation types
relType		:: Relation -> [TypeName]
relType r	= r & relTypesModes |> fst


-- Relation modes
relModes	:: Relation -> [Mode]
relModes r	= r & relTypesModes |> snd


-- filter a list according to the given mode
filterMode	:: Mode -> Relation -> [a] -> [a]
filterMode mode rel as
	= zip as (relModes rel) & filter ((==) mode . snd) |> fst


{- Predicates for a rule -}
data Predicate		= TermIsA Expression TypeName
			| Same Expression Expression
			| Needed Conclusion
	deriving (Show, Ord, Eq)

{- A generic conclusion that can be drawn. E.g. given these parsetrees, this relation holds. -}
data ConclusionA a	= RelationMet 	{ conclusionRel 	:: Relation
					, conclusionArgs 	:: [a]
					} deriving (Show, Ord, Eq)

-- Proof for a conclusion; it should be valid using giving parsetrees
type Conclusion'	= ConclusionA ParseTree
-- Prototype for a conclusion. Might be valid, given a specific parsetree as value.
type Conclusion		= ConclusionA Expression


{-
A rule is an expression, often of the form:
If these predicates are met, then this relation is valid.
Note that these can be transformed to also produce values, giving the modes of the relations used
-}
data Rule		= Rule 	{ ruleName 	:: Name
				, rulePreds 	:: [Predicate]
				, ruleConcl	:: Conclusion
				} deriving (Show, Ord, Eq)


newtype Rules	= Rules {getRules :: Map Name [Rule]}
			deriving (Show)



makeRules	:: Syntax -> [Rule] -> Either String Rules
makeRules s rules
	= do 	checkNoDuplicates (rules |> ruleName) (\dups -> "Multiple rules have the name "++showComma dups)
		let sortedRules = rules |> ((\r -> r & ruleConcl & conclusionRel & relSymbol) &&& id) & merge
		let rules'	= Rules $ M.fromList sortedRules		
		check' s rules'
		return rules'
		

instance Check' Syntax Rules where
	check' syntax (Rules rules)
		= rules & M.elems & concat |> check' syntax & allRight_
		

instance Check' Syntax Rule where
	check' syntax (Rule nm preds concl)
		= inMsg ("While typechecking the rule "++show nm) $
		  do	predTypings	<- mapi preds |> (\(i, p) -> inMsg ("In predicate "++show i) $ typeCheckPredicate syntax p) & allRight
			predTyping	<- inMsg "In the combination of typings generated by all the predicates" $ mergeContexts syntax predTypings
			conclTyping	<- inMsg "In the conclusion" $ typeCheckConclusion syntax concl
			finalTyping	<- inMsg "While matching the predicate typing and the conclusion typing" $ mergeContext syntax predTyping conclTyping
			return ()



typeCheckPredicate	:: Syntax -> Predicate -> Either String (Map Name TypeName)
typeCheckPredicate syntax (TermIsA e tp)
	= expectedTyping syntax e
typeCheckPredicate syntax (Same e1 e2)
	= do	t1	<- expectedTyping syntax e1
		t2	<- expectedTyping syntax e2
		mergeContext syntax t1 t2
typeCheckPredicate syntax (Needed concl)
	= typeCheckConclusion syntax concl


typeCheckConclusion	:: Syntax -> Conclusion -> Either String (Map Name TypeName)
typeCheckConclusion syntax (RelationMet relation exprs)
	= do	let types 	= relation & relType		-- Types of the relation
		let modes	= relation & relModes
		assert Left (length types == length exprs) $
			"Expected "++show (length types)++" expressions as arguments to the relation "++
			show (relSymbol relation)++" : "++show types++", but only got "++show (length exprs)++" arguments"
		
		let usagesForMode mode	
			= filterMode mode relation exprs	-- we get the expressions that are used for INput or OUTput
			  |> expectedTyping syntax & allRight	-- how are these typed? Either String [Map Name TypeName]
			  >>= mergeContexts syntax			-- merge these, crash for input/output contradictions
		pats	<- usagesForMode In			
		usages 	<- usagesForMode Out
		checkPatterns syntax pats usages



----------------------- Proofs -------------------------------------

{-
When a rule is applied to enough values (parse-trees) it generates a proof of this rule.
Use 'parseTreeInterpreter.RuleInterpreter'
-}
data Proof	= Proof { proofConcl	:: Conclusion'
			, prover	:: Rule
			, proofPreds	:: [Proof]	-- predicates for the rule
			}
		| ProofIsA ParseTree TypeName
		| ProofSame ParseTree Expression Expression
		 deriving (Ord, Eq)

isProof Proof{}	= True
isProof _	= False

{-Number of 'layers' in the proof-}
depth	:: Proof -> Int
depth proof@Proof{}
	= if null (proofPreds proof) then 1
		else proofPreds proof |> depth & maximum & (+1)
depth _	= 1


{-Number of proof elements-}
weight	:: Proof -> Int
weight proof@Proof{}
	 = 1 + (proof & proofPreds |> weight & sum)
weight _ = 1






------------------------ Typesystemfile ------------------------

{-Represents a full typesystem file-}
data TypeSystem 	
	= TypeSystem 	{ tsName 	:: Name	-- what is this typesystem's name?
			, tsSyntax	:: Syntax	-- synax of the language
			, tsStyle	:: SyntaxStyle
			, tsFunctions 	:: Functions	-- syntax functions of the TS 
			, tsRelations	:: [Relation]
			-- predicates and inference rules of the type system, most often used for evaluation and/or typing rules; sorted by conclusion relation
			, tsRules' 	:: Rules
			} deriving (Show)


instance Check TypeSystem where
	check ts
		=	[ checkBNF (tsSyntax ts)
			, check' (tsSyntax ts) (tsRules' ts)
			, checkNoDuplicates (tsRelations ts |> relSymbol) (\dups -> "Multiple relations declared with the symbol "++commas dups)
			] & allRight_


tsRules		:: TypeSystem -> Map Name [Rule]
tsRules	ts	= tsRules' ts & (\(Rules dict) -> dict)






