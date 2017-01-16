 {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module TypeSystem.TypeSystem where

{-
-- Finally, it all comes together
-}


import Utils.Utils
import Utils.ToString

import TypeSystem.Types
import TypeSystem.Syntax
import TypeSystem.SyntaxStyle
import TypeSystem.Function
import TypeSystem.Relation
import TypeSystem.Rule

import Data.Map as M
import Data.List (intercalate)

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
			} deriving (Show)
makeLenses ''TypeSystem


instance Check TypeSystem where
	check ts
		=	[ check (get tsSyntax ts)
			, check' (get tsSyntax ts) (get tsRules' ts)
			, checkNoDuplicates (get tsRelations ts |> get relSymbol) (\dups -> "Multiple relations declared with the symbol "++commas dups)
			] & allRight_


tsRules		:: TypeSystem -> Map Name [Rule]
tsRules	ts	= get tsRules' ts & (\(Rules dict) -> dict)

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
	toParsable' i ts@(TypeSystem name syntax syntaxStyle functions relations rules)
		=  inHeader " " name '*' $ intercalate "\n\n"
			[ inHeader' "Syntax" $ toParsable syntax
			, inHeader' "Syntax Highlighting" $ toParsable syntaxStyle
			, inHeader' "Functions"
				(functions & M.toList |> (\(nm, func) -> toParsable' (nm, i) func) & intercalate "\n\n")
			, inHeader' "Relations" $ toParsable' "\n" relations
			, inHeader' "Rules" $ toParsable' relations rules]
	
	toCoParsable'	= toParsable'
	debug' _	= show
	show' _		= show



