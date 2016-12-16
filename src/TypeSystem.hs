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

import Utils

import Data.List (intersperse, intercalate)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Data.List
import Data.Either

import Control.Arrow ((&&&))
import Control.Monad (foldM, when)

import Graphs.SearchCycles

------------------------ Syntax -------------------------



{- Syntax is described in a Backus-Naur format, a simple naive parser is constructed from it. -}

data BNFAST 	= Literal String	-- Literally parse 'String'
		| Identifier		-- Parse an identifier
		| Number		-- Parse a number
		| BNFRuleCall Name	-- Parse the rule with the given name
		| BNFSeq [BNFAST]	-- Sequence of parts
	deriving (Show, Eq)


fromSingle	:: BNFAST -> Maybe BNFAST
fromSingle (BNFSeq [bnf])	= Just bnf
fromSingle (BNFSeq _)		= Nothing
fromSingle bnf			= Just bnf


fromRuleCall	:: BNFAST -> Maybe Name
fromRuleCall (BNFRuleCall nm)	= Just nm
fromRuleCall _			= Nothing

{-Represents a syntax: the name of the rule + possible parseways -}
type BNFRules	= Map TypeName [BNFAST]


-- The sort is added to make sure the parser first tries "abc", before trying "a". Otherwise, "a" is parsed with an "abc" resting
bnfNames	:: BNFRules -> [Name]
bnfNames r	=  M.keys r & sortOn length & reverse



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
reachableVia	:: BNFRules -> TypeName -> [TypeName]
reachableVia rules root
	= _reachableVia rules [] root

_reachableVia	:: BNFRules -> [TypeName] -> TypeName -> [TypeName]
_reachableVia r alreadyVisited root
	= let	called	= r M.! root >>= calledRules	:: [TypeName]
		new	= called & filter (`notElem` alreadyVisited)	:: [TypeName]
		visitd'	= root:alreadyVisited
		new'	= new >>= _reachableVia r visitd'
		in
		nub (visitd' ++ new')


calledRules	:: BNFAST -> [TypeName]
calledRules (BNFRuleCall nm)	= [nm]
calledRules (BNFSeq bnfs)	= bnfs >>= calledRules
calledRules _			= []


-- First call, without consumption of a character
firstCall	:: BNFAST -> Maybe TypeName
firstCall (BNFRuleCall nm)	= Just nm
firstCall (BNFSeq (ast:_))	= firstCall ast
firstCall _			= Nothing

firstCalls	:: BNFRules -> Map TypeName (Set TypeName)
firstCalls rules
	= rules ||>> firstCall |> catMaybes |> S.fromList

leftRecursions	:: BNFRules -> [[TypeName]]
leftRecursions	= cleanCycles . firstCalls


makeBNFRules	:: [(Name, [BNFAST])] -> Either String BNFRules
makeBNFRules vals
	= do	let bnfr	= M.fromList vals
	
		[checkNoDuplicates (vals |> fst) (\duplicates -> "The rule "++showComma duplicates++"is defined multiple times"),
			checkBNF bnfr] & allRight_
		return bnfr


checkBNF	:: BNFRules -> Either String ()
checkBNF bnfs	= inMsg ("While checking the syntax:") $
		  	allRight_ (checkLeftRecursion bnfs:(bnfs & M.toList |> checkUnknownRuleCall bnfs))

checkUnknownRuleCall	:: BNFRules -> (Name, [BNFAST]) -> Either String ()
checkUnknownRuleCall bnfs (n, asts)
	= inMsg ("While checking rule "++n++" for unknowns") $
	  mapi asts |> (\(i, ast) ->
		inMsg ("While checking choice "++show i++", namely "++show ast) $
		do	let unknowns = calledRules ast & filter (flip M.notMember bnfs) 
			assert Left (null unknowns) $ "Unknown type "++showComma unknowns
		) & allRight_ & ammendMsg (++"Known rules are "++ showComma (bnfNames bnfs)) >> return ()
		
			

checkLeftRecursion	:: BNFRules -> Either String ()
checkLeftRecursion bnfs
	= do	let cycles	= leftRecursions bnfs
		let msg cycle	= cycle & intercalate " -> "
		let msgs	= cycles |> msg |> ("    "++) & unlines
		assert Left (null cycles) ("Potential infinite left recursion detected in the syntax. Left cycles are:\n"++msgs)

{-
Consider following BNF:
x ::= ... | y | ...
y ::= ...

This means that every 'y' also (and always) is an 'x'

alwaysIsA searches these relations:

alwaysIsA rules 'y' 'x'	--> True

-}
alwaysIsA	:: BNFRules -> TypeName -> TypeName -> Bool
alwaysIsA rules sub super
 | super == "" || sub == ""
		= True	-- The empty string is used in dynamic cases, thus is equivalent to everything
 | sub == super	= True
 | super `M.notMember` rules
	= error $ "Unknwown super name: "++show super
 | sub `M.notMember` rules
	= error $ "Unknwown sub name: "++show sub
 | otherwise	-- super-rule should contain a single occurence, sub or another rule
	= let	superR	= (rules M.! super) |> fromSingle & catMaybes
		-- this single element should be a BNFRuleCall
		superR'	= superR |> fromRuleCall & catMaybes
		-- either sub is an element from superR', or it has a rule which is a super for sub
		-- we don't have to worry about loops; this is left recursion and is checked against

		-- in one special case, " sub ::= super ", they are equals too
		-- we lookup the subRule, check if it has one call...
		subR	= (rules M.! sub)
		-- and has exactly one choice
		equalRules	= length subR == 1 && head subR == BNFRuleCall super
		in equalRules || sub `elem` superR' || or (superR' |> alwaysIsA rules sub)
				
-- Either X is a Y, or Y is a X
equivalent	:: BNFRules -> TypeName -> TypeName -> Bool
equivalent r x y
		= alwaysIsA r x y || alwaysIsA r y x

equivalents r x y	= zip x y & all (uncurry $ equivalent r)

alwaysAreA	:: BNFRules -> Type -> Type -> Bool
alwaysAreA rules sub super
	= let	together	= zip sub super
		params		= init together |> uncurry (flip (alwaysIsA rules)) & and	-- contravariance
		result		= last together &  uncurry       (alwaysIsA rules)		-- covariance
		in		params && result





-- same as mergeContext, but on a list
mergeContexts	:: BNFRules -> [Map Name TypeName] -> Either String (Map Name TypeName)
mergeContexts bnfs ctxs
		= foldM (mergeContext bnfs) M.empty ctxs



-- Merges two contexts (variable names --> expected types) according to the subtype relationsship defined in the given bnf-rules
mergeContext	:: BNFRules -> Map Name TypeName -> Map Name TypeName -> Either String (Map Name TypeName)
mergeContext bnfs ctx1 ctx2
	= let msg v t1 t2 	= v ++ " is typed as both "++show t1++" and "++show t2 in
		mergeContextWith msg (equivalent bnfs) ctx1 ctx2

checkPatterns		:: BNFRules -> Map Name TypeName -> Map Name TypeName -> Either String (Map Name TypeName)
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
	deriving (Ord, Eq)

instance SimplyTyped ParseTree where
	typeOf pt	= typeInfoOf' pt & either id fst

replace	:: ParseTree -> [Int] -> ParseTree -> ParseTree
replace _ [] toPlace	= toPlace
replace (PtSeq tp orig) (i:rest) toPlace
 | length orig <= i
	= error $ "Invalid substitution path: index "++show i++" to big for "++show orig
 | otherwise
	= let	(init, head:tail)	= splitAt i orig
		head'		= replace head rest toPlace in
		(init ++ (head':tail)) & PtSeq tp
replace rest path toReplace
	= error $ "Invalid substitution path: not a sequence, but trying to execute the path "++show path++" on "++showPt' rest


{-
Advanced expressions with evaluation contexts and variables and such
-}
data Expression
	= MParseTree ParseTree				-- a 'value'
	| MVar TypeName Name				-- a variable
	| MSeq MInfo [Expression]			
	| MCall TypeName Name Builtin [Expression]	-- function call; not allowed in pattern matching
	| MAscription TypeName Expression 		-- checks wether the expression is built by this smaller rule.
	| MEvalContext {evalCtx_fullType::TypeName, evalCtx_fullName::Name, evalCtx_hole::Expression}	-- describes a pattern that searches a context
	deriving (Ord, Eq)



isMInt'	:: ParseTree -> Bool
isMInt' (MInt _ _)	= True
isMInt' _		= False

isPtSeq	:: ParseTree -> Bool
isPtSeq (PtSeq _ _)	= True
isPtSeq _		= False

isMInt	:: Expression -> Bool
isMInt (MParseTree pt)	= isMInt' pt
isMInt _		= False

usedIdentifiers'	:: ParseTree -> [Name]
usedIdentifiers' (MIdentifier _ nm)	= [nm]
usedIdentifiers' (PtSeq _ pts)		= pts >>= usedIdentifiers'


usedIdentifiers	:: Expression -> [Name]
usedIdentifiers (MParseTree pt)		= usedIdentifiers' pt
usedIdentifiers (MSeq _ exprs)		= exprs >>= usedIdentifiers
usedIdentifiers (MCall _ _ _ exprs)	= exprs >>= usedIdentifiers
usedIdentifiers (MAscription _ expr)	= usedIdentifiers expr
usedIdentifiers (MEvalContext _ fnm hole)
					= fnm : usedIdentifiers hole
usedIdentifiers	_			= []

-- generates an MVar, with a name that does not occurr in the given expression
unusedIdentifier	:: Expression -> (Maybe Name) -> TypeName -> ParseTree
unusedIdentifier noOverlap baseName productionType 
	= let	name	= fromMaybe "x" baseName
		alreadyUsed = name: usedIdentifiers noOverlap
		varName	= [0..] |> show |> (name++) & filter (`notElem` alreadyUsed) & head
		in
		MIdentifier (productionType, -2) varName



-- walks a  expression, gives which variables have what types
expectedTyping	:: BNFRules -> Expression -> Either String (Map Name TypeName)
expectedTyping _ (MVar mt nm)	= return $ M.singleton nm mt
expectedTyping r (MSeq _ mes)		= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping r (MCall _ _ _ mes)	= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping r (MAscription _ e)		= expectedTyping r e
expectedTyping r (MEvalContext tp fnm hole)
					= expectedTyping r hole >>= mergeContext r (M.singleton fnm tp)
expectedTyping _ (MParseTree _)	= return M.empty








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

typeInfoOf' (MLiteral tp _)		= Right tp
typeInfoOf' (MInt tp _) 		= Right tp
typeInfoOf' (MIdentifier tp _)		= Right tp
typeInfoOf' (PtSeq tp _)		= Right tp

-- Patterns, used to deconstruct values (parsetrees) and capture variables, to calculate the end expression
data Clause	= MClause {mecPatterns :: [Expression], mecExpr :: Expression}
	deriving (Ord, Eq)

instance FunctionlyTyped Clause where
	typesOf (MClause pats e)	= (pats |> typeOf) ++ [typeOf e]

-- a function; pattern matching goes from first to last clause
data Function	= MFunction Type [Clause]
	deriving (Ord, Eq)

instance FunctionlyTyped Function where
	typesOf (MFunction t _)	= t


type Functions	= Map Name Function








----------------------- Proof Rules ------------------------


type Symbol		= Name
data Mode		= In | Out
	deriving (Show, Ord, Eq)

-- A relation. Some relations might be able to produce values, given a few input variables (e.g. a evaluation or typing rule)
data Relation		= Relation {relSymbol :: Symbol, relTypesModes :: [(TypeName, Mode)], relPronounce :: (Maybe String) }
	deriving (Ord, Eq)

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

{- A generic conclusion that can be drawn. E.g. given these parsetrees, this relation holds.
-}

data ConclusionA a	= RelationMet 	{ conclusionRel 	:: Relation
					, conclusionArgs 	:: [a]
					, showArgs		:: a -> String
					}

-- Proof for a conclusion; it should be valid using giving parsetrees
type Conclusion'	= ConclusionA ParseTree
-- Prototype for a conclusion. Might be valid, given a specific parsetree as value.
type Conclusion		= ConclusionA Expression

-- constructor synonym
relationMet	:: Relation -> [Expression] -> Conclusion
relationMet r es	= RelationMet r es show

-- constructor synonym
relationMet'		:: Relation -> [ParseTree] -> Conclusion'
relationMet' r es	= RelationMet r es show

instance Eq a => Eq (ConclusionA a) where
	(==) (RelationMet r as _) (RelationMet r' as' _)	
		= (r, as) == (r', as')


instance Ord a => Ord (ConclusionA a) where
	(<=) (RelationMet r as _) (RelationMet r' as' _)	
		= (r, as) <= (r', as')

{- Predicates for a rule -}
data Predicate		= TermIsA Expression TypeName
			| Same Expression Expression
			| Needed Conclusion
	deriving (Ord, Eq)

{-
A rule is an expression, often of the form:
If these predicates are met, then this relation is valid.
Note that these can be transformed to also produce values, giving the modes of the relations used
-}
data Rule		= Rule 	{ ruleName 	:: Name
				, rulePreds 	:: [Predicate]
				, ruleConcl	:: Conclusion
				} deriving (Ord, Eq)


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

isProof (Proof {})	= True
isProof _		= False

{-Number of 'layers' in the proof-}
depth	:: Proof -> Int
depth proof@(Proof _ _ _)
	= if null (proofPreds proof) then 1
		else proofPreds proof |> depth & maximum & (+1)
depth _	= 1


{-Number of proof elements-}
weight	:: Proof -> Int
weight proof@(Proof _ _ _)
	 = 1 + (proof & proofPreds |> weight & sum)
weight _ = 1




typeCheckRule		:: BNFRules -> Rule -> Either String ()
typeCheckRule bnfs (Rule nm preds concl)
	= inMsg ("While typechecking the rule "++show nm) $
	  do	predTypings	<- mapi preds |> (\(i, p) -> inMsg ("In predicate "++show i) $ typeCheckPredicate bnfs p) & allRight
		predTyping	<- inMsg "In the combination of typings generated by all the predicates" $ mergeContexts bnfs predTypings
		conclTyping	<- inMsg "In the conclusion" $ typeCheckConclusion bnfs concl
		finalTyping	<- inMsg "While matching the predicate typing and the conclusion typing" $ mergeContext bnfs predTyping conclTyping
		return ()



typeCheckPredicate	:: BNFRules -> Predicate -> Either String (Map Name TypeName)
typeCheckPredicate bnfs (TermIsA e tp)
	= expectedTyping bnfs e
typeCheckPredicate bnfs (Same e1 e2)
	= do	t1	<- expectedTyping bnfs e1
		t2	<- expectedTyping bnfs e2
		mergeContext bnfs t1 t2
typeCheckPredicate bnfs (Needed concl)
	= typeCheckConclusion bnfs concl


typeCheckConclusion	:: BNFRules -> Conclusion -> Either String (Map Name TypeName)
typeCheckConclusion bnfs (RelationMet relation exprs _)
	= do	let types 	= relation & relType		-- Types of the relation
		let modes	= relation & relModes
		assert Left (length types == length exprs) $
			"Expected "++show (length types)++" expressions as arguments to the relation "++
			show (relSymbol relation)++" : "++show types++", but only got "++show (length exprs)++" arguments"
		
		let usagesForMode mode	
			= (filterMode mode relation exprs	-- we get the expressions that are used for INput or OUTput
			  |> expectedTyping bnfs & allRight	-- how are these typed? Either String [Map Name TypeName]
			  >>= mergeContexts bnfs)			-- merge these, crash for input/output contradictions
		pats	<- usagesForMode In			
		usages 	<- usagesForMode Out
		checkPatterns bnfs pats usages





------------------------ Typesystemfile ------------------------

{-Represents a full typesystem file-}
data TypeSystem 	
	= TypeSystem {	tsName :: Name, 	-- what is this typesystem's name?
			tsSyntax	:: BNFRules,	-- synax of the language
			tsFunctions 	:: Functions,	-- syntax functions of the TS 
			tsRelations	:: [Relation],
			-- predicates and inference rules of the type system, most often used for evaluation and/or typing rules; sorted by conclusion relation
			tsRules 	:: Map Symbol [Rule]	
			} deriving (Show)



checkTypeSystem	:: TypeSystem -> Either String ()
checkTypeSystem ts
	= do	let	checks	= checkBNF (tsSyntax ts) : (tsRules ts & M.elems & concat |> typeCheckRule (tsSyntax ts))
		checks & allRight_









---------------------------------------------------------------------------
------------------------------ UTILITIES ----------------------------------
---------------------------------------------------------------------------

{- Only boring show functions below -}

-- Show as if this was target language
instance Show ParseTree where
	show (MLiteral _ s)	= s
	show (MIdentifier _ i)	= i
	show (MInt _ i)		= show i
	show (PtSeq _ exprs)	= exprs |> show & unwords & inParens 


-- show as if this parsetree was an expression in the declaring file
showPt'	:: ParseTree -> String
showPt' (MLiteral _ s)	= show s
showPt' (MIdentifier _ i)	= show i
showPt' (MInt _ i)	= show i
showPt' (PtSeq _ exprs)	= exprs |> showPt' & unwords & inParens 


-- Show as if this was an expression in the typesystem file
instance Show Expression where
	show (MParseTree pt)	= showPt' pt
	show (MVar _ n)		= n
	show (MSeq _ exprs)	= exprs |> show & unwords & inParens 
	show (MCall _ nm builtin args)
				= let args'	= args & showComma & inParens
				  in (if builtin then "!" else "") ++ nm ++ args'
	show (MAscription nm expr)	= (show expr ++ ":" ++ nm) & inParens
	show (MEvalContext tp fullName hole)
				= let 	hole'	= show hole ++ if show hole /= typeOf hole then " : "++typeOf hole else ""
					in
					fullName ++ "["++ hole' ++"]" ++ if fullName /= tp then " : " ++ tp else ""


showTI ("", _)	= ""
showTI (mt, -1) = ": "++mt
showTI (mt, i)	= ": "++mt++"."++show i

-- Show as if this was target language code
show' (MVar mt n)	= show n
show' (MParseTree pt)	= show pt
show' (MSeq mt exprs)	= exprs |> show' & unwords
show' (MCall mt nm builtin args)
			= let args'	= args & showComma & inParens
			  in (if builtin then "!" else "") ++ nm ++ args' ++ ": "++show mt
show' (MAscription _ expr)	= show' expr & inParens
show' ctx@(MEvalContext _ _ _)
			= show $ show ctx



instance Show Function where
	show (MFunction tp clauses)
		= let	sign	= ": "++show tp
			clss	= clauses |> show in
			(sign:clss) & intercalate "\n"


instance Show Clause where
	show (MClause patterns expr)
		= inParens (patterns |> show & intercalate ", ") ++ " = "++show expr


instance Show Relation where
	show (Relation symbol tps pronounce)
		= let	sign	= inParens symbol ++ " : "++ (show tps)	:: String
			pron	= pronounce |> show |> ("\tPronounced as "++) & fromMaybe "" 	:: String in
			sign ++ pron

instance Show (ConclusionA a) where
	show (RelationMet rel [arg] showArg)
		= inParens (relSymbol rel) ++ " " ++ showArg arg
	show (RelationMet rel (arg1:args) showArg)	
		= showArg arg1 ++ " " ++ relSymbol rel ++ " " ++ (args |> showArg & intercalate ", ")




instance Show Predicate	where
	show (TermIsA e typ)	= show e  ++ ": "  ++ typ
	show (Same e1 e2)	= show e1 ++ " = " ++ show e2
	show (Needed concl)	= show concl

instance Show Rule where
	show (Rule nm predicates conclusion)
		= let	predicates'	= predicates |> show & intercalate "    "
			conclusion'	= show conclusion
			nm'	= "[" ++ nm ++ "]"
			line	= replicate (2 + max (length predicates') (length conclusion')) '-'
			in
			["", " " ++ predicates', line ++ " " ++ nm', " "++ conclusion'] & unlines


instance Show Proof where
	show proof = showProof True proof & unlines
	


showProof	:: Bool -> Proof -> [String]
showProof _ (ProofIsA expr typ)	= [showPt' expr ++ " : "++show typ]
showProof _ (ProofSame pt e1 e2)= [showPt' pt ++ " satisfies "++show e1 ++" = "++show e2]
showProof showName (Proof concl proverRule predicates)
	= let	preds'	= predicates |> showProof showName
		preds''	= if null preds' then [] else init preds' ||>> (++"   ")  ++ [last preds']
		preds	= preds'' & foldl (stitch ' ') []	:: [String]
		predsW	= ("":preds) |> length & maximum	:: Int
		concl'	= show concl
		line	= replicate (max predsW (length concl')) '-'		:: String
		line'	= line ++ if showName then " " ++ inParens (ruleName proverRule) else ""
		in
		(preds ++ [line', concl'])

