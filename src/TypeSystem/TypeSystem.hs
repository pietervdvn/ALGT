 {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module TypeSystem.TypeSystem where

{-
-- Finally, it all comes together
-}


import Utils.Utils
import Utils.ToString

import TypeSystem.Types
import TypeSystem.BNF
import TypeSystem.Syntax
import TypeSystem.SyntaxStyle
import TypeSystem.Function
import TypeSystem.Relation
import TypeSystem.Rule

import Data.Map as M
import Data.List (intercalate, find)

import Control.Arrow ((&&&))

import Lens.Micro hiding ((&))
import Lens.Micro.TH


{-Represents a full typesystem file-}
data TypeSystem 	
	= TypeSystem 	{ _tsName 	:: Name	-- what is this typesystem's name?
			, _tsSyntax	:: Syntax	-- synax of the language
			, _tsStyle	:: SyntaxStyle
			, _tsFunctions 	:: Functions	-- syntax functions of the TS 
			, _tsRelations	:: [Relation]
			-- predicates and inference rules of the type system, most often used for evaluation and/or typing rules; sorted by conclusion relation
			, _tsRules' 	:: Rules
			, _tsProps	:: [Property]
			} deriving (Show, Eq)
makeLenses ''TypeSystem


instance Check TypeSystem where
	check ts
		=	[ check (get tsSyntax ts)
			, check' (get tsSyntax ts) (get tsRules' ts)
			, checkNoDuplicates (get tsRelations ts |> get relSymbol) (\dups -> "Multiple relations declared with the symbol "++commas dups)
			] & allRight_


tsRules		:: Lens' TypeSystem (Map Name [Rule])
tsRules		= tsRules' . rules

tsRelations'	:: Lens' TypeSystem (Map Name Relation)
tsRelations'	=  lens (\ts -> get tsRelations ts |> (get relSymbol &&& id) & M.fromList)
			(\ts rels -> set tsRelations (M.elems rels) ts)

tsRulesOnName	:: Lens' TypeSystem (Map Name Rule)
tsRulesOnName	=  tsRules' . rulesOnName
			
findRelation	:: TypeSystem -> Symbol -> Maybe Relation
findRelation rels s
	= find ((==) s . get relSymbol) $ get tsRelations rels

_checkExists	:: (TypeSystem -> Map Name v) -> String -> TypeSystem -> Name -> Either String v
_checkExists getter category ts n
	= do	let msg		= category ++" "++ show n ++ " not found"
		checkExistsSugg show n (getter ts) msg

checkSyntaxExists	:: TypeSystem -> Name -> Either String ([BNF], WSMode, Bool)
checkSyntaxExists
	= _checkExists (get (tsSyntax . fullSyntax')) "Syntactic form"
		

checkRelationExists	:: TypeSystem -> Symbol -> Either String Relation
checkRelationExists
	= _checkExists (get tsRelations') "Relation"


checkRelationRulesExists		:: TypeSystem -> Name -> Either String [Rule]
checkRelationRulesExists
	= _checkExists (get tsRules) "Rule"


checkFunctionExists	:: TypeSystem -> Name -> Either String Function
checkFunctionExists
	= _checkExists (get tsFunctions) "Function"

checkPropertyExists	:: TypeSystem -> Name -> Either String Property
checkPropertyExists ts nm
	= do	let props ts	= get tsProps ts |> (get propName &&& id) & M.fromList
		_checkExists props "Property" ts nm


instance Refactorable TypeName TypeSystem where
	refactor f ts	= ts	& over tsSyntax (refactor f)
				& over tsStyle (refactor f)
				& over tsFunctions (refactor f)
				& over tsRelations (|> refactor f)
				& over tsRules' (refactor f)
		
instance Refactorable FunctionName TypeSystem where
	refactor f ts	= ts	& over tsFunctions (refactor f)
				& over tsRules' (refactor f)
		

instance Refactorable RelationSymbol TypeSystem where
	refactor f ts	= ts	& over tsRelations (|> refactor f)
				& over tsRules' (refactor f)


instance Refactorable RuleName TypeSystem where
	refactor f ts	= ts	& over tsRules' (refactor f)




instance ToString' Int TypeSystem where
	toParsable' i ts@(TypeSystem name syntax syntaxStyle functions relations rules properties)
		=  inHeader " " name '*' $ intercalate "\n\n"
			[ inHeader' "Syntax" $ toParsable syntax
			, inHeader' "Syntax Style" $ toParsable syntaxStyle
			, inHeader' "Functions"
				(functions & M.toList |> (\(nm, func) -> toParsable' (nm, i) func) & intercalate "\n\n")
			, inHeader' "Relations" $ toParsable' "\n" relations
			, inHeader' "Rules" $ toParsable' relations rules
			, inHeader' "Properties" $ toParsable' "\n" properties
			]
	
	toCoParsable'	= toParsable'
	debug' _	= show
	show' _		= show




