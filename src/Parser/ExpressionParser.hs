module Parser.ExpressionParser where

{-
This module defines a parser for  expressions, take 2

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
import Data.List (intercalate)
import Control.Monad
import Control.Arrow ((&&&))

data MEParseTree	= MePtToken String 
			| MePtSeq [MEParseTree] 
			| MePtVar Name 
			| MePtCall Name Builtin [MEParseTree]
			| MePtAscription Name MEParseTree
	deriving (Ord, Eq)


instance Show MEParseTree where
	show (MePtToken s)	= show s
	show (MePtSeq pts)	= pts |> show & unwords & inParens
	show (MePtVar v)	= v
	show (MePtCall n bi args)
				= (if bi then "!" else "") ++ n ++ inParens (args |> show & intercalate ", ")
	show (MePtAscription n pt)	= inParens (show pt++" : "++n)

-- walks a  expression, gives which variables have what types
expectedTyping	:: BNFRules -> Expression -> Either String (Map Name TypeName)
expectedTyping _ (MVar (mt, _) nm)	= return $ M.singleton nm mt
expectedTyping r (MSeq _ mes)		= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping r (MCall _ _ _ mes)	= mes |+> expectedTyping r >>= mergeContexts r
expectedTyping r (MAscription _ e)		= expectedTyping r e
expectedTyping _ _			= return M.empty



mergeContexts	:: BNFRules -> [Map Name TypeName] -> Either String (Map Name TypeName)
mergeContexts bnfs ctxs
		= foldM (mergeContext bnfs) M.empty ctxs


mergeContext	:: BNFRules -> Map Name TypeName -> Map Name TypeName -> Either String (Map Name TypeName)
mergeContext bnfs ctx1 ctx2
		= do	let	common		= (ctx1 `M.intersection` ctx2) & M.keys	:: [Name]
			let ctx1'	= common |> (ctx1 M.!)
			let ctx2'	= common |> (ctx2 M.!)
			-- for each common key, we see wether they are equivalent
			let conflicts	= zip common (zip ctx1' ctx2')
						||>> uncurry (equivalent bnfs)
						& filter (not . snd) |> fst
			let msg n	= n++" is typed as both "++(ctx1 M.! n)++" and "++(ctx2 M.! n)
			if null conflicts then return (M.union ctx1 ctx2) else Left ("Conflicts for variables "++show conflicts++":\n"++unlines (conflicts |> msg))



{- Given a context (knwon function typings + bnf syntax), given a bnf rule (as type),
the parsetree is interpreted/typed following the bnf syntax.
-}
typeAs		:: Map Name Type -> BNFRules -> TypeName -> MEParseTree -> Either String Expression
typeAs functions rules ruleName pt
	= inMsg ("While typing "++show pt++" against "++ruleName) $ 
		matchTyping functions rules (BNFRuleCall ruleName) (error "Should not be used", error "Should not be used") pt
 



matchTyping	:: Map Name Type -> BNFRules -> BNFAST -> (TypeName, Int) -> MEParseTree -> Either String Expression
matchTyping f r (BNFRuleCall ruleCall) tp (MePtAscription as expr)
 | not (alwaysIsA r as ruleCall)	
			= Left $ "Invalid cast: "++as++" is not a "++ruleCall
 | otherwise 		= typeAs f r as expr |> MAscription as
matchTyping f r bnf tp c@(MePtAscription as expr)
 | otherwise		= Left $ "Invalid cast: "++show c++" could not be matched with "++show bnf
matchTyping _ _ (BNFRuleCall ruleCall) tp (MePtVar nm)
						= return $ MVar (ruleCall, -1) nm
matchTyping _ _ _ tp (MePtVar nm)		= return $ MVar tp nm
matchTyping f r bnf tp pt@(MePtCall _ _ _)	= matchCall f r bnf pt

matchTyping _ _ (Literal s) tp (MePtToken s')
 | s == s'		= return $ MLiteral tp s
 | otherwise		= Left $ "Not the right literal: "++show s++" ~ "++show s'
matchTyping _ _ Identifier tp (MePtToken s)
 | isIdentifier s	= return $ MIdentifier tp s
 | otherwise		= Left $ s ++ " is not an identifier"
matchTyping _ _ Number tp (MePtToken s)
 | otherwise 		= readMaybe s & maybe (Left $ "Not a valid int: "++s) return |> MInt tp
matchTyping f r (Seq bnfs) tp (MePtSeq pts)
 | length bnfs == length pts
			= do	let joined	= zip bnfs pts |> (\(bnf, pt) -> matchTyping f r bnf tp pt) 
				joined & allRight |> MSeq tp
 | otherwise		= Left $ "Seq could not match " ++ show bnfs ++ " ~ " ++ show pts 
matchTyping f r (BNFRuleCall nm) _ pt
 | nm `M.member` r
		= do	let bnfASTs	= r M.! nm
			let oneOption i bnf	= inMsg ("Trying to match "++nm++"." ++ show i++" ("++show bnf++")") $ 
							matchTyping f r bnf (nm, i) pt
			zip [0..] bnfASTs |> uncurry oneOption & firstRight
 | otherwise	= Left $ "No bnf rule with name " ++ nm
matchTyping _ _ bnf _ pt
		= Left $ "Could not match "++show bnf++" ~ "++show pt


isIdentifier	:: String -> Bool
isIdentifier (c:chrs)
		= isLower c && all isAlphaNum chrs


{-
Typechecks calls
-}
matchCall	:: Map Name Type -> BNFRules -> BNFAST -> MEParseTree -> Either String Expression
matchCall functions bnfRules _ (MePtCall fNm True args)
 = return $ MCall "" fNm True (args |> dynamicTranslate "")

matchCall functions bnfRules (BNFRuleCall bnfNm) (MePtCall fNm False args)
 | fNm `M.notMember` functions	= Left $ "Unknwown function: "++fNm
 | bnfNm `M.notMember` bnfRules	= Left $ "Unknwown type/bnfrule: "++bnfNm
 | otherwise		
	= do	let fType		= flatten (functions M.! fNm)
		let argTypes		= init fType
		let returnTyp		= last fType
		if not (equivalent bnfRules returnTyp bnfNm)
			then Left ("Actual type "++show returnTyp ++" does not match expected type "++show bnfNm)
			else return ()
		args'			<- zip args argTypes |> (\(arg, tp) -> typeAs functions bnfRules tp arg) & allRight
		return $ MCall returnTyp fNm False args'
matchCall _ _ bnf pt 		= Left $ "Could not match " ++ show bnf ++ " ~ " ++ show pt


-- only used for builtin functions
dynamicTranslate	:: TypeName -> MEParseTree -> Expression
dynamicTranslate tp (MePtToken s)	= MLiteral (tp, -1) s
dynamicTranslate tp (MePtSeq pts)	= pts |> dynamicTranslate tp & MSeq (tp, -1) 
dynamicTranslate tp (MePtVar nm)	= MVar (tp, -1) nm
dynamicTranslate _ (MePtAscription tp e)	= dynamicTranslate tp e
dynamicTranslate tp (MePtCall _ _ _)
				= error "For now, no calls within a builtin are allowed"


---------------------- PARSING ---------------------------


parseExpression	:: Parser u MEParseTree
parseExpression	= mePt

mePt	= many1 (ws *> mePtPart <* ws) |> mePtSeq
		where 	mePtSeq [a]	= a
			mePtSeq as	= MePtSeq as

mePtPart	= try mePtToken
			<|> try mePtCall 
			<|> try meAscription 
			<|> try meNested 
			<|> mePtVar

meNested	= char '(' *> ws *> mePt <* ws <* char ')'
mePtToken	= bnfLiteral |> MePtToken
mePtVar		= identifier |> MePtVar
mePtCall	= do	builtin	<- try (char '!' >> return True) <|> return False
			nm	<- identifier
			char '('
			args	<- (ws *> mePt <* ws) `sepBy` char ','
			char ')'
			return $ MePtCall nm builtin args
meAscription	= do	char '('
			ws
			expr	<- mePt
			ws
			char ':'
			ws
			nm	<- identifier
			ws
			char ')'
			return $ MePtAscription nm expr
			
