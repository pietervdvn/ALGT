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
			| MePtEvalContext Name MEParseTree TypeName	-- The type of the EvaluationContext is derived... from, well the context :p
	deriving (Ord, Eq)


instance Show MEParseTree where
	show (MePtToken s)	= show s
	show (MePtSeq pts)	= pts |> show & unwords & inParens
	show (MePtVar v)	= v
	show (MePtCall n bi args)
				= (if bi then "!" else "") ++ n ++ inParens (args |> show & intercalate ", ")
	show (MePtAscription n pt)	= inParens (show pt++" : "++n)
	show (MePtEvalContext n e t)	= n++"["++show e++":"++t ++"]"





{- Given a context (knwon function typings + bnf syntax), given a bnf rule (as type),
the parsetree is interpreted/typed following the bnf syntax.
-}
typeAs		:: Map Name Type -> BNFRules -> TypeName -> MEParseTree -> Either String Expression
typeAs functions rules ruleName pt
	= inMsg ("While typing "++show pt++" against "++ruleName) $ 
		matchTyping functions rules (BNFRuleCall ruleName) (ruleName, error "Should not be used") pt
 




-- we compare the expected parse type (known via the BNF) and the expression we got
matchTyping	:: Map Name Type -> BNFRules -> BNFAST -> (TypeName, Int) -> MEParseTree -> Either String Expression
matchTyping f r (BNFRuleCall ruleCall) tp (MePtAscription as expr)
 | not (alwaysIsA r as ruleCall)	
			= Left $ "Invalid cast: "++as++" is not a "++ruleCall
 | otherwise 		= typeAs f r as expr |> MAscription as
matchTyping f r bnf tp c@(MePtAscription as expr)
 | otherwise		= Left $ "Invalid cast: "++show c++" could not be matched with "++show bnf


matchTyping _ _ (BNFRuleCall ruleCall) tp (MePtVar nm)
			= return $ MVar ruleCall nm
matchTyping _ _ exp tp (MePtVar nm)		
			= Left $ "Non-rulecall (expected: "++show exp++") with a var "++ nm-- return $ MVar tp nm -- TODO

matchTyping f r bnf (tp, _) (MePtEvalContext nm hole holeT)
			= do	hole'	<- typeAs f r holeT hole
				let holeAsc	= MAscription holeT hole'
				return $ MEvalContext tp nm hole'



matchTyping _ _ _ _ (MePtCall fNm True args)
 = return $ MCall "" fNm True (args |> dynamicTranslate "")
matchTyping functions bnfRules (BNFRuleCall bnfNm) _ (MePtCall fNm False args)
 | fNm `M.notMember` functions	= Left $ "Unknwown function: "++fNm
 | bnfNm `M.notMember` bnfRules	= Left $ "Unknwown type/bnfrule: "++bnfNm
 | otherwise		
			= do	let fType		= functions M.! fNm
				let argTypes		= init fType
				let returnTyp		= last fType
				if not (equivalent bnfRules returnTyp bnfNm)
					then Left ("Actual type "++show returnTyp ++" does not match expected type "++show bnfNm)
					else return ()
				args'			<- zip args argTypes |> (\(arg, tp) -> typeAs functions bnfRules tp arg) & allRight
				return $ MCall returnTyp fNm False args'
matchTyping _ _ bnf _ pt@(MePtCall _ _ _) 
			= Left $ "Could not match " ++ show bnf ++ " ~ " ++ show pt


matchTyping _ _ (Literal s) tp (MePtToken s')
 | s == s'		= MLiteral tp s & MParseTree & return
 | otherwise		= Left $ "Not the right literal: "++show s++" ~ "++show s'
matchTyping _ _ Identifier tp (MePtToken s)
 | isIdentifier s	= MIdentifier tp s & MParseTree & return
 | otherwise		= Left $ s ++ " is not an identifier"
matchTyping _ _ Number tp (MePtToken s)
 | otherwise 		= readMaybe s & maybe (Left $ "Not a valid int: "++s) return |> MInt tp |> MParseTree


matchTyping f r (BNFSeq bnfs) tp (MePtSeq pts)
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


-- only used for builtin functions
dynamicTranslate	:: TypeName -> MEParseTree -> Expression
dynamicTranslate tp (MePtToken s)	= MParseTree $ MLiteral (tp, -1) s
dynamicTranslate tp (MePtSeq pts)	= pts |> dynamicTranslate tp & MSeq (tp, -1) 
dynamicTranslate tp (MePtVar nm)	= MVar tp nm
dynamicTranslate _ (MePtAscription tp e)	= dynamicTranslate tp e
dynamicTranslate tp (MePtCall _ _ _)
				= error "For now, no calls within a builtin are allowed"
dynamicTranslate tp (MePtEvalContext name hole mType)
				= error "For now, no contexts within a builtin are allowed"

---------------------- PARSING ---------------------------


parseExpression	:: Parser u MEParseTree
parseExpression	= mePt

mePt	= many1 (ws' *> mePtPart <* ws') |> mePtSeq
		where 	mePtSeq [a]	= a
			mePtSeq as	= MePtSeq as

mePtPart	= try mePtToken
			<|> meContext
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


meContext	= do	name <- 	try (identifier <* char '[')
			-- char '[' ---------------------------- ^
			ws
			(hole, holeT)	<- try (do	hole	<- parseExpression
							ws
							char ':'
							holeT	<- identifier
							return (hole, holeT))
					   <|> (identifier |> (MePtVar &&& id))
			ws			
			char ']'
			return $ MePtEvalContext name hole holeT
			



			
