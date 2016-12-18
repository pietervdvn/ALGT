module Changer.Changes where

{-
This module defines the main data structures, representing a '.typesystem-changes'. It contains the transformations that will be performed
-}

import TypeSystem
import Utils.TypeSystemToString
import Utils.ToString
import Utils.Utils

import Data.Map hiding (null, filter, lookup)
import qualified Data.Map as M
import Data.List hiding (union)
import Data.Maybe

import Control.Monad
import Control.Arrow ((&&&))


data DefaultChange a
	= Rename a a
	| Copy a a
	| Delete a
	deriving (Show)

-- apply a generic change to a generic map
applyChangeTo	:: (Show k, Ord k) => DefaultChange k -> Map k v -> Either String (Map k v)
applyChangeTo (Rename ok nk) dict
	= do	dict'	<- applyChangeTo (Copy ok nk) dict
		applyChangeTo (Delete ok) dict'
applyChangeTo (Delete ok) dict
	= do	v	<- checkExists ok dict $ "You want delete "++show ok++", but it does not exist."
		return $ M.delete ok dict
applyChangeTo (Copy ok nk) dict
	= do	v	<- checkExists ok dict $ "You want to rename/copy "++show ok++" as "++show nk++", but it does not exist."
		checkNoExists nk dict $ "You want to rename/copy "++show ok++" to "++show nk++", but the new name already exists. Try deleting it first."
		return $ M.insert nk v dict

type Changes' a b	= [Either (DefaultChange a) b]





data SyntaxChange	= AddOption TypeName [BNF] | OverrideBNFRule TypeName [BNF]
	deriving (Show)

data FunctionChange	= AddClauses Name Function
			| OverrideFunc Name Function
	deriving (Show)


data RelationChangeInfo	
	= RCI 	{ name		:: Name
		, pronounced	:: Maybe String
		, ruleRename	:: Maybe (String, String)}
		deriving (Show)

data RelationChange
	= RDelete Name
	| RCopy   Name RelationChangeInfo
	| RRename Name RelationChangeInfo
		deriving (Show)

data Changes = Changes
			{ changesName	:: Name
			, newSyntax	:: Syntax
			, changedSyntax	:: Changes' TypeName SyntaxChange
			, newFunctions	:: Functions
			, changedFuncs	:: Changes' Name FunctionChange
			, newRelations	:: [Relation] 
			, changedRels	:: [RelationChange]
			, newRules	:: [Rule]
			, ruleChanges	:: [DefaultChange Name]
			} deriving (Show)


checkNoCommon' old new rule section
	= checkNoCommon old new
			(\kv1v2s -> "You want to introduce the "++rule++" "++ showComma (kv1v2s |> fst) ++", but it already exists in the original definition.\n"++
			            "If you want to override the "++rule++", put in in the '"++section++" Changes' section")



addSyntax	:: Syntax -> Syntax -> Either String Syntax
addSyntax newSyntax oldSyntax
	= 	inMsg "While updating the syntax" $
	  do	let oldBNF	= oldSyntax & getBNF
		let newBNF	= newSyntax & getBNF
		checkNoCommon' oldBNF newBNF "bnf-type" "Syntax"
		makeSyntax $ M.toList $ M.union oldBNF newBNF

rewriteSyntax	:: Changes' TypeName SyntaxChange -> Syntax -> Either String Syntax
rewriteSyntax changes oldSyntax
	= inMsg "While updating the syntax" $
	  do	let oldBNF	= oldSyntax & getBNF
		bnf'	<- changes |> either applyChangeTo applySyntaxChange
					& foldM (&) oldBNF
		makeSyntax $ M.toList bnf'


applySyntaxChange	:: SyntaxChange -> Map TypeName [BNF] -> Either String (Map TypeName [BNF])
applySyntaxChange (OverrideBNFRule tn choices) bnfs
	= inMsg ("While overriding bnfrule "++show tn) $ 
	  do	checkExists tn bnfs $ show tn++" does not exist in the original definition. Add it to the 'New Syntax'-section if you want to introduce it"
		let bnfs'	= M.insert tn choices bnfs
		return bnfs'
applySyntaxChange (AddOption tn newChoices) bnfs
	= inMsg ("While adding choices bnfrule "++show tn) $ 
	  do	checkExists tn bnfs $ show tn++" does not exist. Perhaps you meant to introduce this type?"
		let bnfs'	= M.adjust ( ++ newChoices) tn bnfs
		return bnfs'




addFunctions	:: Functions -> Functions -> Either String Functions
addFunctions newFuncs oldFuncs
	= inMsg "While updating the functions" $
	  do	checkNoCommon' newFuncs oldFuncs "function" "Functions"
		let functions	= M.union newFuncs oldFuncs
		return functions

rewriteFunctions	:: Changes' Name FunctionChange -> Functions -> Either String Functions
rewriteFunctions changes oldFuncs
	= inMsg "While updating the functions" $
	  changes |> either applyChangeTo applyFunctionChange
		& foldM (&) oldFuncs
		



applyFunctionChange	:: FunctionChange -> Functions -> Either String Functions
applyFunctionChange (OverrideFunc nm func) fs
	= inMsg ("While overriding function "++show nm) $
	  do	checkFuncOVerride nm func fs
		return $ M.insert nm func fs
applyFunctionChange (AddClauses nm func@(MFunction _ newClauses)) fs
	= inMsg ("While adding clauses to function "++show nm) $ 
	  do	(MFunction tp clauses)	<- checkFuncOVerride nm func fs
		let newFunc	= MFunction tp (init clauses ++ newClauses)	-- throw away the falltrhough; newClauses has it anyway
		return $ M.insert nm newFunc fs

checkFuncOVerride	:: Name -> Function -> Map Name Function -> Either String Function
checkFuncOVerride nm func fs
	= do	oldFunc	<- checkExists nm fs $ show nm++", but it does not exist. Add it to the 'New Functions'-section if you want to introduce it."
		unless (typesOf oldFunc == typesOf func) $ Left $ "The new version of "++show nm++" has the type "++(typesOf func & intercalate " -> ")++
			", expeced "++(typesOf oldFunc & intercalate " -> ")
		return oldFunc






addRelations	:: [Relation] -> [Relation] -> Either String [Relation]
addRelations newRels oldRels
	= inMsg "While updating the relations" $
          do	let asDict ls	= ls |> (relSymbol &&& id) & M.fromList
		checkNoCommon' (asDict newRels) (asDict oldRels) "relation" "Relations"
		return $ oldRels ++ newRels


rewriteRelations	:: [RelationChange] -> TypeSystem -> Either String TypeSystem
rewriteRelations changes ts
	= changes |> applyRelationChange & foldM (&) ts

applyRelationChange	:: RelationChange -> TypeSystem -> Either String TypeSystem
applyRelationChange (RDelete symbol) ts
	= do	unless (symbol `elem` (tsRelations ts |> relSymbol)) $ Left $
			"You want to remove relation "++show symbol++", but it does not exist"
		let rules'	= tsRules ts & M.delete symbol
		let rels'	= tsRelations ts & filter ((/=) symbol . relSymbol)
		return ts{ tsRules' = Rules rules', tsRelations = rels'}	
applyRelationChange (RCopy symbol rci@(RCI newSymb _ ruleRename)) ts
	= inMsg ("While copying "++show symbol++" to "++show newSymb) $ 
	  do	(ts', oldRelation, newRelation)	<- createRelation (symbol, rci) ts
		let existingRules	= ts & tsRules & findWithDefault [] symbol
		let rewrittenRules	= existingRules |> _rewriteRule (M.singleton oldRelation newRelation) ruleRename
		return ts' {tsRules' = Rules $ M.insert newSymb rewrittenRules (tsRules ts)}
applyRelationChange (RRename oldSymbol rci@(RCI newSymb _ ruleRename)) ts
	= inMsg ("While renaming "++show oldSymbol++" to "++show newSymb) $ 
	  do	(ts', oldRelation, newRelation)	<- createRelation (oldSymbol, rci) ts
		let rules'	= tsRules ts ||>> _rewriteRule (M.singleton oldRelation newRelation) ruleRename
		let rulesAboutOldSymbol
				= rules' M.! oldSymbol
		let rules''	= rules' & M.delete oldSymbol 
					 & M.insert newSymb rulesAboutOldSymbol
		let relations'	= tsRelations ts' & filter ((/=) oldSymbol . relSymbol)
		return ts' {tsRules' = Rules rules'' , tsRelations = relations'}


createRelation	:: (Symbol, RelationChangeInfo) -> TypeSystem -> Either String (TypeSystem, Relation, Relation)
createRelation	(oldSymbol, RCI newSymbol pronounce ruleRename) ts
	= do	let msg		= "You want to add an relation based on"++show oldSymbol++", but it does not exist"
		oldRelation@(Relation oldN tp oldPronounce)	
				<- fromMaybe (Left msg) (tsRelations ts |> (relSymbol &&& Right) & lookup oldSymbol)
		let newRelation 
			= Relation newSymbol tp (firstJusts [pronounce, oldPronounce])
		relations'	<- addRelations [newRelation] $ tsRelations ts
		return (ts{tsRelations = relations'}, oldRelation, newRelation)


_rewriteConclusion	:: Map Relation Relation -> ConclusionA a -> ConclusionA a
_rewriteConclusion dict (RelationMet rel args)
		= RelationMet (findWithDefault rel rel dict) args

_rewritePredicate	:: Map Relation Relation -> Predicate -> Predicate
_rewritePredicate dict (Needed concl)
		= Needed $ _rewriteConclusion dict concl
_rewritePredicate _ p	= p

_rewriteRule		:: Map Relation Relation -> Maybe (String, String) -> Rule -> Rule
_rewriteRule dict rename (Rule nm preds concl)
		= Rule (_renameRule nm rename) (preds |> _rewritePredicate dict) (_rewriteConclusion dict concl)

_renameRule	:: Name -> Maybe (String, String) -> Name
_renameRule nm rewrite
    = fromMaybe nm $ do	(prefix, replacement)	<- rewrite
			guard (prefix `isPrefixOf` nm)
			return $ replacement ++ drop (length prefix) nm



_rewrDict d	= d & M.elems & concat |> (ruleName &&& id) & merge & M.fromList 

addRules	:: Syntax -> [Rule] -> Rules -> Either String Rules
addRules syntax newRules (Rules oldRules)
	= inMsg "While updating the rules" $
	  do	(Rules newRules)	<- makeRules syntax newRules
		checkNoCommon' (_rewrDict oldRules) (_rewrDict newRules) "rule" "Rules"
		return $ Rules $ M.unionWith (++) oldRules newRules


rewriteRules	:: Syntax -> [DefaultChange Name] -> Rules -> Either String Rules
rewriteRules syntax changes (Rules rules)
	= inMsg "While updating the rules" $
	  do	let rulesOnName	= _rewrDict rules
		rulesOnName'	<- changes |> applyChangeTo & foldM (&) rulesOnName
		makeRules syntax (rulesOnName' & M.elems & concat)
		
