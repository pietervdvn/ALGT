 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module TypeSystem.Expression where

{-
This module defines expressions for functions
-}


import Utils.Utils
import Utils.ToString
import Utils.ToStringExtra

import TypeSystem.Types
import TypeSystem.ParseTree
import TypeSystem.Syntax

import qualified Data.Map as M
import Data.Map (Map, singleton, empty)
import Data.Maybe

import Control.Monad
import Control.Arrow ((&&&))


type Builtin	= Bool

{-
 An expression is always based on a corresponding syntacic rule.
 It can be both for deconstructing a parsetree or constructing one (depending wether it is used as a pattern or not)

 It might contain evaluation contexts, ascriptions, ...
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


-- Searches for identifiers *in the target language*
usedIdentifiers	:: Expression -> [Name]
usedIdentifiers (MParseTree pt)		= usedIdentifiers' pt
usedIdentifiers (MSeq _ exprs)		= exprs >>= usedIdentifiers
usedIdentifiers (MCall _ _ _ exprs)	= exprs >>= usedIdentifiers
usedIdentifiers (MAscription _ expr)	= usedIdentifiers expr
usedIdentifiers (MEvalContext _ _ hole)
					= usedIdentifiers hole
usedIdentifiers	_			= []


-- Searches for variable names *in the meta expression*. If a variable is used twice, it'll appear twice in the list
usedVariables	:: Expression -> [(Name, TypeName)]
usedVariables (MVar tp nm)	= [(nm, tp)]
usedVariables (MSeq _ es)	= es >>= usedVariables
usedVariables (MCall _ _ _ es)	= es >>= usedVariables
usedVariables (MAscription _ e)	= usedVariables e
usedVariables (MEvalContext tp n hole)
				= (n, tp) : usedVariables hole
usedVariables (MParseTree _)	= []



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




-------------------------------------------------- HELPERS TO MERGE CONTEXTS ACCORDING TO A SYNTAX --------------------------------------------

-- same as mergeContext, but on a list
mergeContexts	:: Syntax -> [Map Name TypeName] -> Either String (Map Name TypeName)
mergeContexts syntax
		= foldM (mergeContext syntax) M.empty

-- Merges two contexts (variable names --> expected types) according to the subtype relationsship defined in the given bnf-rules
mergeContext	:: Syntax -> Map Name TypeName -> Map Name TypeName -> Either String (Map Name TypeName)
mergeContext syntax
	= mergeContextWith (mergeTypes syntax)


-- Selects the biggest of two types, or gives an error msg if it doesn't exist
mergeTypes	:: Syntax -> Name -> TypeName -> TypeName -> Either String TypeName
mergeTypes syntax varName t1 t2
 | t1 == t2	= Right t1
 | otherwise
		= do	let msg	= varName ++ " is typed as both "++show t1++" and "++show t2++", which don't have a common supertype"
			maybe (Left msg) Right $ biggestCommonType syntax t1 t2


-- Merges two contexts, according to valid combination. 
mergeContextWith	:: (Name -> TypeName -> TypeName -> Either String TypeName) ->
				Map Name TypeName -> Map Name TypeName -> Either String (Map Name TypeName)
mergeContextWith validCombo ctx1 ctx2
		= inMsg "While merging contexts" $
		  do	let common	= (ctx1 `M.intersection` ctx2) & M.keys	:: [Name]
			let vals	= common |> ((ctx1 M.!) &&& (ctx2 M.!))
			-- for each common key, we see wether they are equivalent (handled by validCombo). If that is the case, we select the supremum of both types
			common'		<- zip common vals |> uncurry (uncurry . validCombo) & allRight
			return $ M.union (M.fromList $ zip common common') $ M.union ctx1 ctx2



instance Refactorable TypeName Expression where
	refactor ftn (MParseTree pt)	= refactor ftn pt & MParseTree
	refactor ftn (MVar tn n)	= MVar (ftn tn) n
	refactor ftn (MSeq mi es)	= es |> refactor ftn & MSeq (refactor ftn mi)
	refactor ftn (MCall tn nm bi es)
					= MCall (ftn tn) nm bi (es |> refactor ftn)
	refactor ftn (MAscription tn e)	= MAscription (ftn tn) (e & refactor ftn)
	refactor ftn (MEvalContext tn n e)	
					= MEvalContext (ftn tn) n (e & refactor ftn)


instance Refactorable FunctionName Expression where
	refactor ffn (MCall tn nm bi es)
					= MCall tn (unliftFunctionName ffn nm) bi (es |> refactor ffn)
	refactor ffn (MSeq mi es)	= es |> refactor ffn & MSeq mi
	refactor ffn (MAscription tn e)	= MAscription tn (refactor ffn e)
	refactor ffn (MEvalContext tn n e)
					= MEvalContext tn n (refactor ffn e)

	refactor ffn pt@MParseTree{}	= pt
	refactor ffn var@MVar{}		= var


instance ToString' ShowParens Expression where
	show'		= const show
	debug'		= show'

	-- Show as if this was an expression in the typesystem file
	toParsable' p (MParseTree pt)	= toCoParsable' p pt
	toParsable' _ (MVar _ n)	= n
	toParsable' p (MSeq _ exprs)	= exprs |> toParsable' (deepen p) & unwords & inParens' p
	toParsable' p (MCall _ nm builtin args)
				= let args'	= args |> toParsable' (least NotOnRoot p) & commas & inParens
				  in (if builtin then "!" else "") ++ nm ++ args'
	toParsable' p (MAscription nm expr)	= (toParsable' (least NotOnRoot p) expr ++ ":" ++ nm) & inParens
	toParsable' p (MEvalContext tp fullName hole)
				= let 	hole'	= toParsable' (least NotOnRoot p) hole
					in
					fullName ++ "["++ hole' ++"]"



	-- Show as if this was target language code. This is not always possible
	toCoParsable' p (MParseTree pt)	= toParsable' p pt
	toCoParsable' p (MSeq mt exprs)	= exprs |> toCoParsable' (deepen p) & unwords
	toCoParsable' p (MVar mt n)	= n & isMeta
	toCoParsable' p expr@MCall{}
				= toParsable' p expr & isMeta
	toCoParsable' p (MAscription as expr)	= toCoParsable' p expr & inParens & (++ isMeta (": "++as))
	toCoParsable' p ctx@MEvalContext{}
				= isMeta $ toParsable' p ctx



instance ToString Expression where
	toParsable	= toParsable' NotOnRoot
	toCoParsable	= toCoParsable' NotOnRoot
	debug		= debug' NotOnRoot	

isMeta str	= "{-#" ++ str ++ "#-}"


