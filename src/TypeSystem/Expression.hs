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
import TypeSystem.BNF

import qualified Data.Map as M
import Data.Map (Map, singleton, empty)
import Data.Maybe

import Control.Monad
import Control.Arrow ((&&&))

import Control.Monad.State as ST

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



-- Searches for variable names *in the meta expression*. If a variable is used twice, it'll appear twice in the list
usedVariables			:: Expression -> [(Name, TypeName)]
usedVariables (MVar tp nm)	= [(nm, tp)]
usedVariables (MSeq _ es)	= es >>= usedVariables
usedVariables (MCall _ _ _ es)	= es >>= usedVariables
usedVariables (MAscription _ e)	= usedVariables e
usedVariables (MEvalContext tp n hole)
				= (n, tp) : usedVariables hole
usedVariables (MParseTree _)	= []


usedFunctions			:: Expression -> [(Name, Bool)]
usedFunctions (MCall _ nm bi es)
				= (nm, bi):(es >>= usedFunctions)
usedFunctions (MSeq _ es)	= es >>= usedFunctions
usedFunctions (MAscription _ e)	= usedFunctions e
usedFunctions (MEvalContext _ _ hole)
				= usedFunctions hole
usedFunctions _			= []


-- walks a  expression, gives which variables have what types
expectedTyping	:: Syntax -> Expression -> Either String (Map Name TypeName)
expectedTyping _ (MVar mt nm)	= return $ M.singleton nm mt
expectedTyping r (MSeq _ mes)	= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping r (MEvalContext tp fnm hole)
				= expectedTyping r hole >>= mergeContext r (M.singleton fnm tp)
expectedTyping r (MCall _ _ _ mes)
				= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping _ (MParseTree _)	= return M.empty
expectedTyping r (MAscription _ e)		
				= expectedTyping r e

bnfAsExpr	:: BNF -> Expression
bnfAsExpr bnf	= evalState (_bnfAsExpr bnf) (negate 1::Int)

_bnfAsExpr	:: BNF -> State Int Expression
_bnfAsExpr (Literal str)
		= return $ MParseTree (MLiteral _mi str)
_bnfAsExpr bnf@(BNFRuleCall r)
 | isBuiltin bnf
		= return $ MParseTree (MLiteral _mi r)
 | otherwise	= do	i	<- getIndex
			return $ MAscription r $ MVar r (r ++ i)
_bnfAsExpr (BNFSeq bnfs)
		= bnfs |+> _bnfAsExpr |> MSeq _mi
_mi	= ("", -1)


getIndex		:: State Int String
getIndex	= do	i	<- ST.get
			modify (+1)
			return $ if i < 0 then ""
				else show i


-------------------------------------------------- HELPERS TO MERGE CONTEXTS ACCORDING TO A SYNTAX --------------------------------------------


-- same as mergeContext, but on a list
mergeContexts	:: Syntax -> [Map Name TypeName] -> Either String (Map Name TypeName)
mergeContexts syntax
		= foldM (mergeContext syntax) M.empty

-- Merges two contexts (variable names --> expected types) according to the subtype relationsship defined in the given bnf-rules
mergeContext	:: Syntax -> Map Name TypeName -> Map Name TypeName -> Either String (Map Name TypeName)
mergeContext syntax
	= mergeContextWith (mergeTypes syntax)


-- Selects the biggest of two types, or gives an error msg if it doesn't exist (thus is top).
mergeTypes	:: Syntax -> Name -> TypeName -> TypeName -> Either String TypeName
mergeTypes syntax varName t1 t2
 | t1 == t2	= Right t1
 | t1 == topSymbol
		= Right t2	-- T1 is used as input argument to a builtin function able to handle everything. We ignore the typing here
 | t2 == topSymbol
		= Right t1
 | otherwise
		= do	let msg	= varName ++ " is typed as both "++show t1++" and "++show t2++", which don't have a common supertype"
			let bct	= 		biggestCommonType syntax t1 t2	
			maybe (Left msg) Right $ bct


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
	toParsable' p (MCall tp nm builtin args)
				= let args'	= args |> toParsable' (least NotOnRoot p) & commas & inParens
				  in if builtin then
					"!" ++ nm ++ ":" ++ tp ++ args' 
					else nm ++ args'
	toParsable' p (MAscription nm expr)	= (toParsable' (least NotOnRoot p) expr ++ ":" ++ nm) & inParens
	toParsable' p (MEvalContext tp fullName hole)
				= let 	hole'	= toParsable' (least NotOnRoot p) hole
					in
					fullName ++ "["++ hole' ++"]"



	-- Show as if this was target language code. This is not always possible
	toCoParsable' p (MParseTree pt)	= "("++toCoParsable' p pt++":"++typeOf pt++")"
	toCoParsable' p (MSeq mt exprs)	= exprs |> toCoParsable' (deepen p) & unwords
	toCoParsable' p (MVar tp n)	= "("++n++":"++tp++")"
	toCoParsable' p expr@MCall{}
				= toParsable' p expr
	toCoParsable' p (MAscription as expr)	= toCoParsable' p expr & inParens & (++": "++as)
	toCoParsable' p ctx@MEvalContext{}
				= isMeta $ toParsable' p ctx



instance ToString Expression where
	toParsable	= toParsable' NotOnRoot
	toCoParsable	= toCoParsable' NotOnRoot
	debug		= debug' NotOnRoot	

isMeta str	= "{-#" ++ str ++ "#-}"



expressionExamples	:: [(Expression, String, String, String)]
expressionExamples
      = [ (MVar "syntactic_rule" "x", 
		"Variable","Captures the argument as the name. If multiple are used in the same pattern, the captured arguments should be the same or the match fails.", 
		"Recalls the parsetree associated with this variable")
	, (MVar "" "_", "Wildcard", "Captures the argument and ignores it", "_Not defined_")
	, (_int 42, "Number", "Argument should be exactly this number", "This number")
	, (_lit "Token", "Literal", "Argument should be exactly this string", "This string")
	, (MCall "resultType" "func" False [MVar "sr" "arg0", MVar "sr" "arg1", MVar "sr" "..."],
		"Function call", "Evaluates the function, matches if the argument equals the result. Can only use variables which are declared left of this pattern", "Evaluate this function")
	, (MCall "type" "func" True [MVar "sr" "arg0", MVar "sr" "..."],
		"Builtin function call", "Evaluates the builtin function, matches if the argument equals the result. Can only use variables which are declared left of this pattern", "Evaluate this builtin function, let it return a `type`")
	, (MAscription "type" $ MVar "" "expr or pattern", "Ascription", "Check that the argument is an element of `type`", "Checks that an expression is of a type. Bit useless")
	, (MEvalContext "tn" "e" $ MVar "" "expr or pattern", "Evaluation context",
		"Matches the parsetree with `e`, searches a subpart in it matching `pattern`", "Replugs `expr` at the same place in `e`. Only works if `e` was created with an evaluation context")
	, (MSeq ("",0) [MVar "" "a", _lit "b", MSeq ("", 0) [MVar "" "nested"]],
		"Sequence", "Splits the parse tree in the appropriate parts, pattern matches the subparts", "Builds the parse tree")
	]

_lit str	= MLiteral ("sr", 0) str & MParseTree
_int i		= MInt ("sr", 0) i & MParseTree

