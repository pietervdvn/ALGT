module Gradualization.Gradualization where

{-
This module defines the main data structures, representing a '.gradualization'. It contains the transformations that will be performed
-}

import TypeSystem
import Utils.TypeSystemToString
import Utils.ToString
import Utils.Utils

import Data.Map hiding (null, filter, lookup)
import qualified Data.Map as M
import Data.List hiding (union)

import Control.Monad
import Control.Arrow ((&&&))

data Gradualization = Gradualization
			{ gradName	:: Name
			, typeToEnhance	:: TypeName
			, symbolToAdd	:: Symbol
			, extraFunctions	:: Functions
			, overridenFuncs	:: Functions
			, newRelations		:: [Relation]
			, relationRenames	:: [(Symbol, (Symbol, Maybe String))]
			} deriving (Show)




gradualize	:: Gradualization -> TypeSystem -> Either String TypeSystem
gradualize grad ts
	= inMsg ("While gradualizing the grammer "++show (tsName ts)++" with the gradualization "++show (gradName grad)) $
	  do	-- add symbol to target rule
		syntax'		<- rewriteSyntax (typeToEnhance grad) (symbolToAdd grad) (tsSyntax ts)
		

		-- add the extra functions
		functions'	<- addFunctions (extraFunctions grad) (overridenFuncs grad) (tsFunctions ts)


		-- rename relations
		let neededNames		= relationRenames grad |> fst
		let namedTo		= relationRenames grad |> snd |> fst
		let knownRelations	= tsRelations ts |> relSymbol 
		let namedTo'		= namedTo & filter (`elem` knownRelations)
		let missing		= neededNames & filter (`notElem` knownRelations)
		unless (null missing) $ Left ("Some relations should be renamed, but these don't exist: "++showComma missing)
		checkNoDuplicates neededNames $ (\dups -> "A relation will be renamed multiple times: "++showComma dups)
		checkNoDuplicates namedTo $ (\dups -> "Multiple relations will be renamed to the same new name "++showComma dups)
		unless (null namedTo') $ Left ("A relation will be renamed, but it's new name is already in use in the original file: "++showComma namedTo')
	

		let newRelation rel@(Relation oldSymb typing oldPronounce)
			= lookup oldSymb (relationRenames grad) &
				maybe rel (\(newSymb, newPronounce) -> Relation newSymb typing (firstJusts [newPronounce, oldPronounce]))
		let relationMapping
			= tsRelations ts |> (id &&& newRelation) & fromList	:: Map Relation Relation
		let symbolMapping
			= relationRenames grad ||>> fst & fromList :: Map Symbol Symbol
		let relations'	= tsRelations ts |> (\rel -> findWithDefault rel rel relationMapping) 

		-- rename relation in rules
		let rules'	= tsRules ts & toList 
					|> (\(nm, rs) -> ( findWithDefault nm nm symbolMapping , rs |> rewriteRule relationMapping) )
					& fromList



		let ts' = ts{tsSyntax = syntax', tsRelations = relations', tsRules' = Rules rules', tsFunctions = functions' }
		checkTypeSystem ts'
		return ts




addFunctions	:: Map Name Function -> Map Name Function -> Map Name Function -> Either String (Map Name Function)
addFunctions newFuncs overridenFuncs origFuncs
	= do	let commonFunctionsNew	= intersection origFuncs newFuncs & keys
		unless (null commonFunctionsNew) $ 
					Left $ "You defined a new function (in the 'Function'-section), but this function already exists in the original file.\n"++
						"If you want to override this function, place it in the 'Overridden Functions'-section.\n"++
						"Function which already exists is "++showComma commonFunctionsNew
		let functions'	= union origFuncs newFuncs

		

		let commonFunctionsOver	= intersection functions' overridenFuncs & keys
		commonFunctionsOver |> (\nm -> do
			let origType	= origFuncs ! nm & typesOf
			let newType	= overridenFuncs ! nm & typesOf
			unless (origType == newType) $ Left $ "The new version of function "++show nm++" has type \""++ (newType & intercalate " -> ")++
				"\", the original version has type \""++(origType & intercalate " -> ") ++ "\". Doe da keer nie, thx."
			) & allRight_ & inMsg "While checking if type of the overriding function is the same as the function it replaces"
		-- replace the functions. M.union prefers values from the left dictionary if the key is present in both
		let functions''	= M.union overridenFuncs functions'	


		return functions''



rewriteSyntax	:: TypeName -> Symbol -> Syntax -> Either String Syntax
rewriteSyntax ruleName symbol syntaxWrapped
	= do	let syntax	= syntaxWrapped & getBNF
		unless (ruleName `member` syntax) $ 
					Left $ "The rule "++show ruleName ++ ", that should be gradualized, does not exist..."
		let syntax'	= adjust (++ [ Literal symbol ]) ruleName syntax
		return $ BNFRules syntax'



rewriteConclusion	:: Map Relation Relation -> ConclusionA a -> ConclusionA a
rewriteConclusion dict (RelationMet rel args)
		= RelationMet (findWithDefault rel rel dict) args

rewritePredicate	:: Map Relation Relation -> Predicate -> Predicate
rewritePredicate dict (Needed concl)
		= Needed $ rewriteConclusion dict concl
rewritePredicate _ p	= p

rewriteRule		:: Map Relation Relation -> Rule -> Rule
rewriteRule dict (Rule nm preds concl)
		= Rule nm (preds |> rewritePredicate dict) (rewriteConclusion dict concl)
