 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module Utils.TypeSystemToString where

import TypeSystem

import Utils.Utils
import Utils.ToString

import Data.List (intercalate, sortOn, elemIndex, sort)
import Data.Map as M hiding (filter, null, foldl)
import Data.Maybe
import Data.Bifunctor

{-
This module defines multiple 'ToString's for all type-system data structures
-}

-------------------------------------- SYNTAX ------------------------------------------------

instance ToString BNF where
	toParsable	= toStr toParsable
	toCoParsable	= toParsable

	debug (BNFSeq asts)
			= asts |> toStr debug & unwords & inParens
	debug ast	= toStr debug ast

toStr			:: (BNF -> String) -> BNF -> String
toStr _ (Literal str)	= show str
toStr _ Identifier	= "Identifier"
toStr _ Number		= "Number"
toStr _ (BNFRuleCall n)	= n
toStr f (BNFSeq asts)	= asts |> f & unwords


instance ToString Syntax where
	toParsable (BNFRules rules wsModes)
		= let 	width	= rules & M.keys |> length & maximum
			merged	= M.intersectionWith ( (,) ) rules wsModes in
			merged & M.toList |> uncurry (toParsableBNFRule width) & unlines
	debug		= show

instance ToString WSMode where
	toParsable IgnoreWS	= "::="
	toParsable StrictWS	= "~~="
	toParsable StrictWSRecursive	= "//="

toParsableBNFRule	:: Int -> Name -> ([BNF], WSMode) -> String
toParsableBNFRule w n (choices, wsMode)
	= n++ replicate (w - length n) ' ' ++ "\t "++ toParsable wsMode ++ " " ++ (choices |> toParsable & intercalate " | ")

------------------------------------ Syntax Highlighting --------------------------------------------

instance ToString SyntaxStyle where
	toParsable (SyntaxStyle baseStyles extraStyles styleRemaps)
		= let	styles	= M.toList baseStyles ++
				  (M.toList extraStyles |> first (\(nm, i) -> nm++"."++show i)) ++
				  (M.toList styleRemaps |> first show)
			styles'	= styles ||>> show |> (\(key, val) -> key ++ "\t -> "++val)
			in
			styles' & sort & unlines


------------------------------------ EXPRESSIONS --------------------------------------------


-- Show MInfo typles
showTI ("", _)	= ""
showTI (mt, -1) = ": "++mt
showTI (mt, i)	= ": "++mt++"."++show i

data ShowParens	= Parens | NoParens | NotOnRoot
	deriving (Show)

deepen NotOnRoot	= Parens
deepen p		= p

least Parens p		= p
least NotOnRoot	Parens	= NotOnRoot
least NotOnRoot p	= p
least NoParens _	= NoParens

inParens' Parens	= inParens
inParens' _		= id

instance ToString' ShowParens ParseTree where
	show'		= const show

	-- show as if this parsetree was an expression in the declaring file
	toParsable' _ (MLiteral _ s)	= s
	toParsable' _ (MIdentifier _ i)	= i
	toParsable' _ (MInt _ i)	= show i
	toParsable' p (PtSeq _ exprs)	= exprs |> toParsable' (deepen p) & unwords & inParens' p

	-- show as if this parsetree was an expression in the typesystem file
	toCoParsable' _ (MLiteral _ s)	= show s
	toCoParsable' _ (MIdentifier _ i)= show i
	toCoParsable' _ (MInt _ i)	= show i
	toCoParsable' p (PtSeq _ exprs)	= exprs |> toCoParsable' (deepen p) & unwords & inParens' p

	debug' _ (MLiteral ti s)	= show s ++ showTI ti
	debug' _ (MIdentifier ti i)	= show i ++ showTI ti
	debug' _ (MInt ti i)		= show i ++ showTI ti
	debug' p (PtSeq ti exprs)	= (exprs |> debug' p & unwords & inParens) ++ showTI ti

instance ToString ParseTree where
	toParsable	= toParsable' NotOnRoot
	toCoParsable	= toCoParsable' NotOnRoot
	debug		= debug' NotOnRoot		

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



instance ToString' (Name, Int) Clause where
	show' 	= clauseWith show
	toParsable'
		= clauseWith toParsable
	toCoParsable'	= clauseWith toCoParsable
	debug'		= clauseWith debug

clauseWith exprShow (fname, i) (MClause pats expr)
	= let	head	= fname ++ inParens (pats |> exprShow & commas)
		spacing	= replicate (i - length head) ' ' ++ (if length head > i then "\n"++replicate i ' ' else "") ++ " = "
		tail	= exprShow expr in
		head ++ spacing ++ tail



instance ToString' (Name, Int) Function where
	show' nmi	= funcWith (show' nmi) nmi
	toParsable' nmi	= funcWith (toParsable' nmi) nmi
	toCoParsable' nmi
			= funcWith (toCoParsable' nmi) nmi
	debug' nmi	= funcWith (debug' nmi) nmi


funcWith showClause (name, int) (MFunction tp clauses)
	= let	sign	= name ++ replicate (int - length name) ' ' ++ " : "++ intercalate " -> " tp
		-- we drop the last clause, as it is an automatically added error clause for non exhaustive patterns
		clauses'	= init clauses
		clss	= clauses' |> showClause in
		(sign:clss) & unlines


------------------------------------- RELATIONS AND RULES -----------------------------------

instance ToString Relation where
	toParsable (Relation symbol tps pronounce)
		= let	sign	= inParens symbol ++ " \t: "++ tps |> (\(nm, mode) -> nm++" "++ inParens (show mode)) & commas
			pron	= pronounce |> show |> ("\tPronounced as "++) & fromMaybe "" in
			sign ++ pron
	toCoParsable (Relation symbol tps pronounce)
		= let	sign	= inParens symbol -- ++ " : "++ tps |> (\(nm, mode) -> nm++" "++ inParens (show mode)) & commas
			name	= maybe "" (\p -> show p++", with symbol ")  pronounce
			in
			name ++ sign
	debug		= show



instance (ToString a) => ToString (ConclusionA a) where
	toParsable	= showConclusionWith toParsable
	toCoParsable	= showConclusionWith toCoParsable
	debug		= showConclusionWith debug


showConclusionWith showArg (RelationMet rel [arg])
		= inParens (relSymbol rel) ++ " " ++ showArg arg
showConclusionWith showArg (RelationMet rel (arg1:args))	
		= showArg arg1 ++ " " ++ relSymbol rel ++ " " ++ (args |> showArg & commas)




instance ToString Predicate where
	toParsable	= showPredicateWith toParsable toParsable
	toCoParsable	= showPredicateWith toCoParsable toCoParsable
	debug		= showPredicateWith debug debug


showPredicateWith 	:: (Expression -> String) -> (Conclusion -> String) -> Predicate -> String
showPredicateWith se sc (TermIsA e tp)
	= se e ++ ": "++ tp
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



instance ToString' TypeSystem Rules where
	show'		= const show
	toParsable'	= showRulesWith toParsable
	toCoParsable'	= showRulesWith toCoParsable
	debug'		= showRulesWith debug

showRulesWith	:: (Rule -> String) -> TypeSystem -> Rules -> String
showRulesWith sr ts (Rules rules)
	= let	relations	= tsRelations ts
		relationOf nm	= relations & filter ((==) nm . relSymbol) & head
	  	relationOrder symbol	= fromMaybe (length relations) (elemIndex symbol (relations |> relSymbol))
	  in
	  rules & M.toList & sortOn (relationOrder . fst) |> (\(symbol, rules) ->
		"\n" ++ header "# " ("Rules about "++toCoParsable (relationOf symbol)) '-' ++ "\n"++
			rules |> sr & unlines
		)
		& intercalate "\n\n"


------------------------------------ PROOFS ------------------------------------------------

data ProofOptions	= PO {	nameParens		:: String -> String,
				showNames		:: Bool,
				showSatisfiesEquality	:: Bool,
				betweenPredicates	:: String }


defaultProofOptions	= PO (\s -> "["++s++"]") True True "    "


data ProofOptions'	= PO' {	opts'		:: ProofOptions,
				st		:: TypeName -> String,
				sp		:: ParseTree -> String,
				se		:: Expression -> String,
				sc 		:: Conclusion' -> String,
				sr		:: Rule -> String
				}

instance ToString Proof where
	toParsable	= toParsable' defaultProofOptions
	toCoParsable	= toCoParsable' defaultProofOptions
	debug		= debug' defaultProofOptions


instance ToString' ProofOptions Proof where
	show' po proof		= let opts	= PO' po show show show show show 				in showProofWith opts proof & unlines
	toParsable' po proof	= let opts	= PO' po id toParsable toParsable toParsable toCoParsable 	in showProofWith opts proof & unlines
	toCoParsable' po proof	= let opts	= PO' po id toCoParsable toParsable toCoParsable toParsable 	in showProofWith opts proof & unlines
	debug' po proof		= let opts	= PO' po id debug debug debug debug			 	in showProofWith opts proof & unlines
		

-- shows a proof part; returns lines 
showProofWith	:: ProofOptions' -> Proof -> [String]
showProofWith opts (ProofIsA expr typ)
		= [sp opts expr ++ " : "++ st opts typ]
showProofWith opts (ProofSame pt e1 e2)
 | showSatisfiesEquality (opts' opts)
	= [se opts e1 ++ " = "++ sp opts pt ++ " = "++ se opts e2]
 | otherwise
	= []
showProofWith opts (Proof concl proverRule predicates)
	= let	options	= opts' opts
		preds'	= predicates |> showProofWith opts
		preds''	= if null preds' then [] else init preds' ||>> (++ betweenPredicates options)  ++ [last preds']
		preds	= preds'' & foldl (stitch ' ') []	:: [String]
		predsW	= ("":preds) |> length & maximum	:: Int
		concl'	= sc opts concl
		lineL	= max predsW (length concl')
		name	= if showNames options then " " ++ nameParens options (ruleName proverRule) else ""
		lineL'	= lineL - if 3 * length name <= lineL && predsW == lineL then length name else 0
		line	= replicate lineL' '-'		:: String
		line'	= line ++ name
		in
		(preds ++ [line', concl'])

------------------------------------- FULL TYPESYSTEM FILE -----------------------------------

header	:: String -> String -> Char -> String
header prefix str chr
	= let	title	= " "++str++" "
		line	= title |> const chr
		in
		prefix++title++"\n"++prefix++line

header' s
	= header "" s '='


instance ToString TypeSystem where
	toParsable ts@(TypeSystem name syntax syntaxStyle functions relations rules)
		= 	[ header " " ("# "++name++" #") '#'
			, header' "Syntax", toParsable syntax
			, header' "Syntax Highlighting", toParsable syntaxStyle
			, header' "Functions", 
				functions & M.toList |> (\(nm, func) -> toParsable' (nm, 24 :: Int) func) & intercalate "\n\n"
			, header' "Relations" , toParsable' "\n" relations
			, header' "Rules", toParsable' ts rules]
			& intercalate "\n\n"
	toCoParsable 
		= toParsable 
	debug		= show







