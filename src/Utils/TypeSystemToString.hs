 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
module Utils.TypeSystemToString where

import TypeSystem

import Utils.Utils
import Utils.ToString

import Data.List (intercalate)
import Data.Map as M

{-
This module defines multiple 'ToString's for all type-system data structures
-}

-------------------------------------- SYNTAX ------------------------------------------------

instance ToString BNF where
	toParsable	= toStr toParsable

	debug (BNFSeq asts)
			= asts |> toStr debug & unwords & inParens
	debug ast	= toStr debug ast

toStr			:: (BNF -> String) -> BNF -> String
toStr _ (Literal str)	= show str
toStr _ Identifier		= "Identifier"
toStr _ Number		= "Number"
toStr _ (BNFRuleCall n)	= n
toStr f (BNFSeq asts)	= asts |> f & unwords


instance ToString Syntax where
	toParsable (BNFRules rules)
		= let width	= rules & M.keys |> length & maximum in
			rules & M.toList |> uncurry (toParsableBNFRule width) & unlines


toParsableBNFRule	:: Int -> Name -> [BNF] -> String
toParsableBNFRule w n choices
	= n++ replicate (w - length n) ' ' ++ "   ::= "++ (choices |> toParsable & intercalate " | ")



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
	toParsable' p (MSeq _ exprs)	= exprs |> toParsable' (deepen p) & unwords
	toParsable' p (MCall _ nm builtin args)
				= let args'	= args |> toParsable' (deepen p) & commas & inParens
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
	toCoParsable' p expr@(MCall _ _ _ _)
				= toParsable' p expr & isMeta
	toCoParsable' p (MAscription as expr)	= toCoParsable' p expr & inParens & (++ isMeta (": "++as))
	toCoParsable' p ctx@(MEvalContext _ _ _)
				= isMeta $ toParsable' p ctx



instance ToString Expression where
	toParsable	= toParsable' NotOnRoot
	toCoParsable	= toCoParsable' NotOnRoot
	debug		= debug' NotOnRoot	

isMeta str	= "{-#" ++ str ++ "#-}"




------------------------------------- FULL TYPESYSTEM FILE -----------------------------------

instance ToString TypeSystem where
	toParsable (TypeSystem name syntax functions relations rules)
		= [ "# "++name, " SYNTAX \n========", toParsable syntax, show functions, show relations, show rules]
			& intercalate "\n\n"




