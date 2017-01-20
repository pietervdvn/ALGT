{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TemplateHaskell #-}
module Changer.Changes where

{-
This module defines the main data structures, representing a '.typesystem-changes'. It contains the transformations that will be performed
-}

import TypeSystem
import Utils.ToString
import Utils.Utils

import Data.Map hiding (null, filter, lookup)
import qualified Data.Map as M
import Data.List hiding (union)
import Data.List as L
import Data.Maybe
import Data.Bifunctor
import Data.Either

import Control.Monad
import Control.Arrow ((&&&))

import Lens.Micro hiding ((&))
import Lens.Micro.TH

data DefaultChange k v
	= Rename k k	-- simply rename the entries
	| Copy k k
	| Delete k
	| New k v
	| Override k v		-- take the entry and edit it slightly
	| OverrideAs k k v	-- take the entry, save it slightly, save it as k2 (and remove the original entry)
	| Edit k v
	| EditAs k k v		-- take the old entry, edit it with the edit function, save it as the new key
	deriving (Show)




type EditFunction k v	= (Bool -> k -> v -> v -> Either String v)

noEditAllowed isOverride k origV oldV
	= Left $ "You want to "++if isOverride then "override" else "edit" ++ " a value, but this is not supported. This is a bug."



applyChangeTo	:: (Show k, Ord k, Eq k) => DefaultChange k v -> Map k v -> Either String (Map k v)
applyChangeTo	= applyChangeTo' noEditAllowed



-- apply a generic change to a generic map
applyChangeTo'	:: (Show k, Ord k, Eq k) =>  EditFunction k v -> DefaultChange k v -> Map k v -> Either String (Map k v)
applyChangeTo' f (Rename ok nk) dict
	= do	dict'	<- applyChangeTo' f (Copy ok nk) dict
		applyChangeTo' f (Delete ok) dict'
applyChangeTo' _ (Delete ok) dict
	= do	v	<- checkExists ok dict $ "You want delete "++show ok++", but it does not exist."
		return $ M.delete ok dict
applyChangeTo' _ (Copy ok nk) dict
	= do	v	<- checkExists ok dict $ "You want to rename/copy "++show ok++" as "++show nk++", but it does not exist."
		_checkTargetNoExists "copy" ok nk dict
		return $ M.insert nk v dict
applyChangeTo' _ (New k v) dict
	= do	checkNoExists k dict $ "You want to introduce "++show k++", but it already exists. Try overriding it in the 'Changes'-section instead."
		return $ M.insert k v dict
applyChangeTo' edit (Override k vNew) dict
	= applyChangeTo' edit (OverrideAs k k vNew) dict
applyChangeTo' edit (OverrideAs k kNew vNew) dict
	= do	vOld	<- checkExists k dict $ "You want to override "++show k++ (if k /= kNew then " as "++show kNew else "") ++", but it does not exist. If you want to introduce this, add it in the 'new' section"
		_checkTargetNoExists "copy" k kNew dict
		v	<- edit True k vOld vNew
		return $ M.insert kNew v $ M.delete k dict
applyChangeTo' edit (Edit k vNew) dict
	= applyChangeTo' edit (EditAs k k vNew) dict
applyChangeTo' edit (EditAs k kNew vNew) dict
	= do	vOld	<- checkExists k dict $ "You want to edit "++show k++(if k /= kNew then " as "++show kNew else "") ++", but it does not exist. If you want to introduce this, add it in the 'new' section"
		_checkTargetNoExists "copy" k kNew dict
		v	<- edit False k vOld vNew
		return $ M.insert kNew v dict

_checkTargetNoExists action kOld kNew dict
	= do	let msg		= "You want to "++action++" "++show kOld++" to "++show kNew++", but the new name already exists. Try deleting it first."
		unless (kOld == kNew) $ checkNoExists kNew dict msg

isNew		:: DefaultChange k v -> Bool
isNew New{}	= True
isNew _		= False



applyAllChanges	:: (Show k, Ord k, Eq k) => EditFunction k v-> [DefaultChange k v] -> Map k v -> Either String (Map k v)
applyAllChanges f changes dict
		= changes & foldM (flip $ applyChangeTo' f) dict


refactorFunc		:: (Eq k) => [DefaultChange k v] -> k -> k
refactorFunc []	k 	= k
refactorFunc (Rename ok nk:rest) k
	| ok == k	= nk
	| otherwise	= refactorFunc rest k
refactorFunc (OverrideAs ok nk _:rest) k
	| ok == k	= nk
	| otherwise	= refactorFunc rest k
refactorFunc (_:rest) k	= refactorFunc rest k









data Changes = Changes
			{ _changesName		:: Name
			, _changedSyntax	:: [DefaultChange TypeName ([BNF], WSMode)]
			, _changedFuncs		:: [DefaultChange Name Function]
			, _changedRels		:: [DefaultChange Symbol Relation]
			, _changedRules		:: [DefaultChange Name Rule]
			} deriving (Show)
makeLenses ''Changes

checkNoCommon' old new rule section
	= checkNoCommon old new
			(\kv1v2s -> "You want to introduce the "++rule++" "++ showComma (kv1v2s |> fst) ++", but it already exists in the original definition.\n"++
			            "If you want to override the "++rule++", put in in the '"++section++" Changes' section")





editSyntax	:: Bool -> TypeName -> ([BNF], WSMode) -> ([BNF], WSMode) -> Either String ([BNF], WSMode)
-- Case for overwriting
editSyntax True _ old new
		= return new
-- Case for editing (thus adding cases)
editSyntax False n (oldBNFs, oldWS) (newBNF, newWS)
	= inMsg ("While updating syntax rule "++show n) $
	  do	unless (oldWS == newWS) $ Left $ "Whitespace-mode doesn't match. Expected mode (by the original definition): "++toParsable oldWS++", but got "++toParsable newWS
		return (oldBNFs ++ newBNF, newWS)




editFunction	:: Syntax -> Bool -> Name -> Function -> Function -> Either String Function
-- We overwrite. The new function type is a *subtype* of the old type
editFunction syntax True nm old new
	= do	-- check for *subtyping*
		let sType x	= intercalate " -> " (typesOf x)
		unless (alwaysAreA' syntax new old ) $ Left $ "The original function has a type "++ sType old++
			", but the new function has type "++ sType new++" which is not a subtype"
		return new
-- We add a function clauses. Types should be exactly the same
editFunction syntax False nm old@(MFunction tOld clauses) new@(MFunction tNew clauses')	-- edit case
	= do	-- check for *type equality*
		unless (tOld == tNew) $ Left $ "Types don't match. Expected type: "++ intercalate " -> " tOld ++
			", actual type: "++ intercalate " -> " tNew
		return $ MFunction tNew $ clauses ++ clauses'
		


editRelation	:: Bool -> Symbol -> Relation -> Relation -> Either String Relation
-- OverrideAs, thus 'renaming' (with a new symbol and name)
-- Both renaming and copying do the same here
editRelation _ symb (Relation _ oldTps oldPronounce) (Relation newSymb tps newPronounce)
	= do	unless (oldTps == tps) $ Left $ "You want to copy/rename the relation "++symb++" as "++newSymb++" but the types don't match"
		return $ Relation newSymb tps $ firstJusts[newPronounce, oldPronounce]


	
editRule	:: Syntax -> Bool -> Name -> Rule -> Rule -> Either String Rule
-- Case for overwriting
editRule syntax True nm old new
	= return new
editRule syntax false nm old new
	= Left "Editing rules is not implemented. How did you end here?"




--------------------------------------- ToString -------------------------------------------------


instance ToString' (k -> String, Bool -> k -> v -> String) (DefaultChange k v) where
	toParsable' (sk, _) (Rename k0 k1)	= "Rename "++sk k0++" to "++sk k1
	toParsable' (sk, _) (Copy k0 k1)	= "Copy "  ++sk k0++" as "++sk k1
	toParsable' (sk, _) (Delete k)		= "Delete "++sk k
	toParsable' (_, sv) (New k v)		= sv True k v
	toParsable' (sk, sv) (Override k v)	= sv True k v
	toParsable' (sk, sv) (Edit k v)		= sv False k v
	toParsable' (sk, sv) (EditAs k _ v)	= sv False k v
	toParsable' (sk, sv) (OverrideAs k _ v)	= sv False k v
	
	toCoParsable'	= toParsable'
	debug'		= toParsable'
	show'		= toParsable'



instance ToString' (k -> String, Bool -> k -> v -> String, Bool -> String) [DefaultChange k v] where
	toParsable' (sk, sv, section) changes	
			= let	(new, changed)	= L.partition isNew changes
				new'	= new |> toParsable' (sk, sv) 
						& intercalate "\n"
						& inHeader' ("New "++ section True)

				changed' = changed |> toParsable' (sk, sv )
						& intercalate "\n"
					 	& inHeader' (section False ++" Changes")

				guard ls v	= [ v | not $ null ls]
				in
				[guard new new', guard changed changed'] & concat & unlines
				

	toCoParsable'	= toParsable'
	debug'		= toParsable'
	show'		= toParsable'
		




instance ToString' Int Changes where
	toParsable' i (Changes
			 changesName	
			 changedSyntax
			 changedFuncs
			 changedRels
			 changedRules)
		= let	plural s doPluralize	= s ++ if doPluralize then "s" else ""	:: String
			
			showBNFRule newOverride tn (bnfs, ws)
				=  toParsable (tn :: TypeName
					, i
					, ws :: WSMode
					, if newOverride then "" else "... | "
					, bnfs :: [BNF])
			syntaxExtras	= (id :: TypeName -> String
						, showBNFRule
						, const "Syntax" :: Bool -> String)
			changedSyntax'	= toParsable' syntaxExtras changedSyntax



			showFunc newOverride n f
					= funcWith (if newOverride then "" else "\n...") (toParsable' (n, i) :: Clause -> String) (n, i) f ++ "\n"
			functionExtras	= (id :: Name -> String
						, showFunc
						, plural "Function")
			changedFuncs'	= toParsable' functionExtras changedFuncs

			showRelCh newOverride nm (Relation newSymb _ pronounced)
					= [if newOverride then "Copy" else "Rename"
						, nm & inParens
						, if newOverride then "as" else "to"
						, newSymb & inParens
						  ++ maybe "" (\p -> ", pronounced as "++ show p) pronounced
						] & unwords

			relExtras	= (inParens	:: Symbol -> String
						, showRelCh
						, plural "Relation")
			changedRels'	= toParsable' relExtras changedRels

			ruleExtras	= (show :: Name -> String
						, const $ const toParsable :: Bool -> Name -> Rule -> String
						, plural "Rule")
			changedRules'	= toParsable' ruleExtras changedRules

			in
			inHeader "" changesName '*' $ unlines
				[ changedSyntax'
				, changedFuncs'
				, changedRels'
				, changedRules'
				]

	toCoParsable'	= toParsable'
	debug' _	= show
	show' _		= show

