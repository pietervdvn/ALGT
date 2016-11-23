module Parser.MetaExpressionParser where

{-
This module defines a parser for meta expressions, take 2

In this approach, we tokenize first to a tree, and then try to match a rule with it

-}

import Parser.ParsingUtils
import Parser.BNFParser
import TypeSystem
import Utils

import Text.Parsec
import Data.Maybe
import Data.Char
import Text.Read (readMaybe)
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad
import Control.Arrow ((&&&))

data MEParseTree	= MePtToken String | MePtSeq [MEParseTree] | MePtVar Name | MePtCall Name Builtin [MEParseTree]
	deriving (Show, Ord, Eq)



{- Given a context (knwon function typings + bnf syntax), given a bnf rule (as type),
the parsetree is interpreted/typed following the bnf syntax.
-}
typeAs		:: Map Name MetaType -> BNFRules -> Name -> MEParseTree -> Maybe MetaExpression
typeAs functions rules ruleName pt
 | ruleName `M.member` rules
		= do	let bnfASTs	= rules M.! ruleName
			zip [0..] bnfASTs |> (\(i, bnf) -> matchTyping functions rules bnf (MType ruleName, i) pt) & firstJusts
 | otherwise	= error $ "No bnf rule with name " ++ ruleName


-- walks a meta expression, gives which variables have what types
expectedTyping	:: BNFRules -> MetaExpression -> Either String (Map Name MetaType)
expectedTyping _ (MVar (mt, _) nm)	= return $ M.singleton nm mt
expectedTyping r (MSeq _ mes)		= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping r (MCall _ _ _ mes)	= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping _ _			= return M.empty



mergeContexts	:: BNFRules -> [Map Name MetaType] -> Either String (Map Name MetaType)
mergeContexts bnfs ctxs
		= foldM (mergeContext bnfs) M.empty ctxs


mergeContext	:: BNFRules -> Map Name MetaType -> Map Name MetaType -> Either String (Map Name MetaType)
mergeContext bnfs ctx1 ctx2
		= do	let	common		= (ctx1 `M.intersection` ctx2) & M.keys	:: [Name]
			ctx1'	<- common |> (ctx1 M.!) |+> toSimpleType'
			ctx2'	<- common |> (ctx2 M.!) |+> toSimpleType'
			-- for each common key, we see wether they are equivalent
			let conflicts	= zip common (zip ctx1' ctx2')
						||>> uncurry (equivalent bnfs)
						& filter (not . snd) |> fst
			if null conflicts then return (M.union ctx1 ctx2) else Left ("Conflicts for variables "++show conflicts)


matchTyping	:: Map Name MetaType -> BNFRules -> BNFAST -> (MetaType, Int) -> MEParseTree -> Maybe MetaExpression
matchTyping _ _ (BNFRuleCall ruleCall) tp (MePtVar nm)
			= return $ MVar (MType ruleCall, -1) nm
matchTyping _ _ _ tp (MePtVar nm)	= return $ MVar tp nm
matchTyping f r bnf tp pt@(MePtCall _ _ _)	= matchCall f r bnf pt
matchTyping _ _ (Literal s) _ (MePtToken s')
 | s == s'		= return $ MLiteral s
 | otherwise		= Nothing
matchTyping _ _ Identifier _ (MePtToken s)
 | isIdentifier s	= return $ MLiteral s
 | otherwise		= Nothing
matchTyping _ _ Number tp (MePtToken s)
 | otherwise 		= readMaybe s |> MInt
matchTyping f r (Seq bnfs) tp (MePtSeq pts)
 | length bnfs == length pts
			= zip bnfs pts |+> (\(bnf, pt) -> matchTyping f r bnf tp pt) |> MSeq tp
 | otherwise		= Nothing
matchTyping f r (BNFRuleCall nm) tp pt
 | otherwise		= typeAs f r nm pt
matchTyping _ _ _ _ _	= Nothing


isIdentifier	:: String -> Bool
isIdentifier (c:chrs)
		= isLower c && all isAlphaNum chrs


{-
Typechecks calls
-}
matchCall	:: Map Name MetaType -> BNFRules -> BNFAST -> MEParseTree -> Maybe MetaExpression
matchCall functions bnfRules _ (MePtCall fNm True args)
 = return $ MCall (MType "") fNm True (args |> dynamicTranslate)

matchCall functions bnfRules (BNFRuleCall bnfNm) (MePtCall fNm False args)
 | fNm `M.notMember` functions	= Nothing
 | bnfNm `M.notMember` bnfRules	= Nothing
 | otherwise		
	= do	let fType		= flatten (functions M.! fNm)
		let argTypes		= init fType
		let returnTyp		= last fType
		guard (returnTyp == bnfNm)
		args'			<- zip args argTypes |+> (\(arg, tp) -> typeAs functions bnfRules tp arg)
		return $ MCall (MType returnTyp) fNm False args'
matchCall _ _ _ _ 		= Nothing



dynamicTranslate	:: MEParseTree -> MetaExpression
dynamicTranslate (MePtToken s)	= MLiteral s
dynamicTranslate (MePtSeq pts)	= pts |> dynamicTranslate & MSeq (MType "", -1) 
dynamicTranslate (MePtVar nm)	= MVar (MType "", -1) nm
dynamicTranslate (MePtCall _ _ _)
				= error "For now, no calls within a builtin are allowed"


parseMetaExpression	:: Parser u MEParseTree
parseMetaExpression	= mePt

mePt	= many1 (ws *> mePtPart <* ws) |> mePtSeq
		where 	mePtSeq [a]	= a
			mePtSeq as	= MePtSeq as

mePtPart	= try mePtToken <|> try mePtCall <|> mePtVar

mePtToken	= bnfLiteral |> MePtToken
mePtVar		= identifier |> MePtVar
mePtCall	= do	builtin	<- try (char '!' >> return True) <|> return False
			nm	<- identifier
			char '('
			args	<- (ws *> mePt <* ws) `sepBy` char ','
			char ')'
			return $ MePtCall nm builtin args
