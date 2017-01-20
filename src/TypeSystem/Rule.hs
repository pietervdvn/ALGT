 {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, FlexibleContexts #-}
module TypeSystem.Rule where

import Utils.Utils
import Utils.ToString

import TypeSystem.Types
import TypeSystem.Syntax
import TypeSystem.ParseTree
import TypeSystem.Expression
import TypeSystem.Relation

import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)
import Data.List as L

import Control.Arrow ((&&&))

import Lens.Micro hiding ((&))
import Lens.Micro.TH


{- Predicates for a rule -}
data Predicate		= TermIsA Name TypeName
			| Same Expression Expression
			| Needed Conclusion
	deriving (Show, Ord, Eq)


fromTermIsA			:: Predicate -> Maybe (Name, TypeName)
fromTermIsA (TermIsA n tn)	= Just (n, tn)
fromTermIsA _			= Nothing

fromSame			:: Predicate -> Maybe (Expression, Expression)
fromSame (Same e1 e2)		= Just (e1, e2)
fromSame _			= Nothing

fromNeeded			:: Predicate -> Maybe Conclusion
fromNeeded (Needed c)		= Just c
fromNeeded _			= Nothing

instance Refactorable TypeName Predicate where
	refactor ftn (TermIsA n tn)	= TermIsA n (ftn tn)
	refactor ftn (Same e1 e2)	= Same (refactor ftn e1) (refactor ftn e2)
	refactor ftn (Needed concl)	= Needed $ refactor ftn concl


instance Refactorable FunctionName Predicate where
	refactor _   isA@TermIsA{}	= isA
	refactor ffn (Same e1 e2)	= Same (refactor ffn e1) (refactor ffn e2)
	refactor ffn (Needed concl)	= Needed $ refactor ffn concl

instance Refactorable RelationSymbol Predicate where
	refactor _   isA@TermIsA{}	= isA
	refactor _   same@Same{}	= same
	refactor frs (Needed concl)	= Needed $ refactor frs concl






{- A generic conclusion that can be drawn. E.g. given these parsetrees, this relation holds. -}
data ConclusionA a	= RelationMet 	{ conclusionRel 	:: Relation
					, conclusionArgs 	:: [a]
					} deriving (Show, Ord, Eq)

instance Functor ConclusionA where
	fmap f (RelationMet r args)
		= RelationMet r (args |> f)


instance (Refactorable TypeName a) => Refactorable TypeName (ConclusionA a) where
	refactor ftn (RelationMet rel args)
			= RelationMet (refactor ftn rel) (args |> refactor ftn)

instance (Refactorable FunctionName a) => Refactorable FunctionName (ConclusionA a) where
	refactor ffn (RelationMet rel args)
			= RelationMet rel (args |> refactor ffn)

instance Refactorable RelationSymbol (ConclusionA a) where
	refactor frs (RelationMet rel args)
			= RelationMet (refactor frs rel) args



-- Proof for a conclusion; it should be valid using giving parsetrees
type Conclusion'	= ConclusionA ParseTree
-- Prototype for a conclusion. Might be valid, given a specific parsetree as value.
type Conclusion		= ConclusionA Expression


{-
A rule is an expression, often of the form:
If these predicates are met, then this relation is valid.
Note that these can be transformed to also produce values, giving the modes of the relations used
-}
data Rule		= Rule 	{ _ruleName 	:: Name
				, _rulePreds 	:: [Predicate]
				, _ruleConcl	:: Conclusion
				} deriving (Show, Ord, Eq)
makeLenses ''Rule



newtype Rules	= Rules {_rules :: Map Symbol [Rule]}
			deriving (Show)
makeLenses ''Rules

getRulesOnName	:: Rules -> Map Name Rule
getRulesOnName rls
	= rls & get rules & M.elems & concat |> (get ruleName &&& id) & M.fromList

fromRulesOnName :: Syntax -> Map k Rule -> Either String Rules
fromRulesOnName syntax rules
	= rules & M.elems & makeRules syntax

rulesOnName	:: Lens' Rules (Map Name Rule)
rulesOnName	= lens getRulesOnName (\_ rulesDict -> _makeRules $ M.elems rulesDict)


_makeRules	:: [Rule] -> Rules
_makeRules rules
	= let sortedRules = rules |> ((\r -> r & get ruleConcl & conclusionRel & get relSymbol) &&& id) & merge
		in Rules $ M.fromList sortedRules		

makeRules	:: Syntax -> [Rule] -> Either String Rules
makeRules s rules
	= do 	checkNoDuplicates (rules |> get ruleName) (\dups -> "Multiple rules have the name "++showComma dups)
		let rules'	= _makeRules rules
		check' s rules'
		return rules'





instance Refactorable TypeName Rule where
	refactor ftn rule
		= rule 	& over (rulePreds . each) (refactor ftn) 
			& over ruleConcl (refactor ftn)

instance Refactorable FunctionName Rule where
	refactor ffn rule
		= rule 	& over (rulePreds . each) (refactor ffn) 
			& over ruleConcl (refactor ffn)


instance Refactorable RelationSymbol Rule where
	refactor ffn rule
		= rule 	& over (rulePreds . each) (refactor ffn) 
			& over ruleConcl (refactor ffn)

instance Refactorable RuleName Rule where
	refactor frn rule
		= rule	& over ruleName (unliftRuleName frn)


instance Refactorable TypeName Rules where
	refactor f rls	= rls & over rules (||>> refactor f)


instance Refactorable FunctionName Rules where
	refactor f rls	= rls & over rules (||>> refactor f)


instance Refactorable RelationSymbol Rules where
	-- rules are ordered over symbols, so we map the keys too
	refactor f rls	= rls 	& over rules (||>> refactor f)
				& over rules (M.mapKeys $ unliftRelationSymbol f)


instance Refactorable RuleName Rules where
	-- rules are ordered on symbol, so no changes here
	refactor f rls	= rls & over rules (||>> refactor f)



------------------------------ CHECKS --------------------------------
		

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
	= return $ M.singleton e tp
typeCheckPredicate syntax (Same e1 e2)
	= do	t1	<- expectedTyping syntax e1
		t2	<- expectedTyping syntax e2
		mergeContext syntax t1 t2
typeCheckPredicate syntax (Needed concl)
	= typeCheckConclusion syntax concl


typeCheckConclusion	:: Syntax -> Conclusion -> Either String (Map Name TypeName)
typeCheckConclusion syntax (RelationMet relation exprs)
	= inMsg "While typechecking the conclusion" $	
	  do	let types 	= relation & relType		-- Types of the relation
		let modes	= relation & relModes
		assert Left (length types == length exprs) $
			"Expected "++show (length types)++" expressions as arguments to the relation "++
			show (get relSymbol relation)++" : "++show types++", but only got "++show (length exprs)++" arguments"
		
		let usagesForMode mode	
			= filterMode mode relation exprs	-- we get the expressions that are used for INput or OUTput
			  |> expectedTyping syntax & allRight	-- how are these typed? Either String [Map Name TypeName]
			  >>= mergeContexts syntax			-- merge these, crash for input/output contradictions
		pats	<- usagesForMode In			
		usages 	<- usagesForMode Out
		mergeContext syntax pats usages






------------------------------- TO STRING ------------------------------

instance ToString Predicate where
	toParsable	= showPredicateWith toParsable toParsable
	toCoParsable	= showPredicateWith toCoParsable toCoParsable
	debug		= showPredicateWith debug debug


showPredicateWith 	:: (Expression -> String) -> (Conclusion -> String) -> Predicate -> String
showPredicateWith se sc (TermIsA e tp)
	= se (MVar tp e) ++ ": "++ tp
showPredicateWith se sc (Same e1 e2)
	= se e1 ++ " = "++ se e2++" : "++ typeOf e1
showPredicateWith se sc (Needed concl)
	= sc concl


instance ToString Rule where
	toParsable	= showRuleWith toParsable toParsable
	toCoParsable	= showRuleWith toCoParsable toCoParsable
	debug		= showRuleWith debug debug



showRuleWith	:: (Predicate -> String) -> (Conclusion -> String) -> Rule -> String
showRuleWith sp sc (Rule nm predicates conclusion)
	= let	predicates'	= predicates |> sp & intercalate "\t"
		conclusion'	= sc conclusion
		nm'	= " \t[" ++ nm ++ "]"
		line	= replicate (2 + max (length' 1 predicates') (length conclusion')) '-'
		in
		["", " " ++ predicates', line ++ " " ++ nm', " "++ conclusion'] & unlines



instance ToString' [Relation] Rules where
	show'		= const show
	toParsable'	= showRulesWith toParsable
	toCoParsable'	= showRulesWith toCoParsable
	debug'		= showRulesWith debug

showRulesWith	:: (Rule -> String) -> [Relation] -> Rules -> String
showRulesWith sr relations (Rules rules)
	= let	relationOf nm	= relations & filter ((==) nm . get relSymbol) & head
	  	relationOrder symbol	= fromMaybe (length relations) (elemIndex symbol (relations |> get relSymbol))
	  in
	  rules & M.toList & sortOn (relationOrder . fst) |> (\(symbol, rules) ->
		"\n" ++ inHeader "# " ("Rules about "++toCoParsable (relationOf symbol)) '-'
			(rules |> sr & unlines)
		)
		& intercalate "\n\n"

instance (ToString a) => ToString (ConclusionA a) where
	toParsable	= showConclusionWith toParsable
	toCoParsable	= showConclusionWith toCoParsable
	debug		= showConclusionWith debug


showConclusionWith showArg (RelationMet rel [arg])
		= inParens (get relSymbol rel) ++ " " ++ showArg arg
showConclusionWith showArg (RelationMet rel (arg1:args))	
		= showArg arg1 ++ " " ++ get relSymbol rel ++ " " ++ (args |> showArg & commas)



