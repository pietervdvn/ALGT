module TypeSystem.Parser.ExpressionParser where

{-
This module defines a parser for expressions.
In this approach, we tokenize first to a tree, and then try to match a rule with it by typing the rule.
-}

import TypeSystem.Parser.ParsingUtils
import TypeSystem.Parser.BNFParser
import TypeSystem
import Utils.Utils

import Text.Parsec
import Data.Maybe
import Data.Char
import Text.Read (readMaybe)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Either
import Control.Monad
import Control.Arrow ((&&&))

-- Simple parsetree, only knowing of tokens. Mirrors 'expressions' from typesystem, but with less metainfo
data MEParseTree	= MePtToken String 
			| MePtSeq [MEParseTree] 
			| MePtVar Name 
			| MePtInt Int
			| MePtCall Name (Maybe TypeName) [MEParseTree]	-- builtin if a type is provided
			| MePtAscription Name MEParseTree
			| MePtEvalContext Name MEParseTree	-- The type of the EvaluationContext is derived... from, well the context :p
	deriving (Ord, Eq)


instance Show MEParseTree where
	show (MePtToken s)	= show s
	show (MePtSeq pts)	= pts |> show & unwords & inParens
	show (MePtVar v)	= v
	show (MePtInt i)	= show i
	show (MePtCall n bi args)
				= (if isJust bi then "!" else "") ++ n ++ maybe "" (":"++) bi ++ inParens (args |> show & intercalate ", ")
	show (MePtAscription n pt)	= inParens (show pt++" : "++n)
	show (MePtEvalContext n expr)	= n++"["++ show expr ++"]"





{- Given a context (knwon function typings + bnf syntax), given a bnf rule (as type),
the parsetree is interpreted/typed following the bnf syntax.
-}
typeAs		:: Map Name Type -> Syntax -> TypeName -> MEParseTree -> Either String Expression
typeAs functions rules ruleName pt
	= inMsg ("While typing "++show pt++" against "++ruleName) $ 
		matchTyping functions rules (BNFRuleCall ruleName) (ruleName, error "Should not be used") pt
 




-- we compare the expected parse type (known via the BNF) and the expression we got
matchTyping	:: Map Name Type -> Syntax -> BNF -> (TypeName, Int) -> MEParseTree -> Either String Expression
matchTyping f r (BNFRuleCall ruleCall) tp (MePtAscription as expr)
 | not (alwaysIsA r as ruleCall)	
			= Left $ "Invalid cast: "++as++" is not a "++ruleCall
 | otherwise 		= typeAs f r as expr |> MAscription as
matchTyping f r bnf tp c@(MePtAscription as expr)
			= Left $ "Invalid cast: "++show c++" could not be matched with "++show bnf

matchTyping _ _ (BNFRuleCall ruleCall) tp (MePtVar nm)
			= return $ MVar ruleCall nm
matchTyping _ _ exp tp (MePtVar nm)		
			= Left $ "Non-rulecall (expected: "++show exp++") with a var "++ nm

matchTyping f r bnf (tp, _) ctx@(MePtEvalContext nm hole@(MePtVar someName))
			= inMsg ("While typing the evalution context (with only an identifier as hole)"++ show ctx) $
			  do	let types	= bnfNames r
				let options	= types & filter (`isPrefixOf` someName)
				let actualType	= head options

				when (null options) $ Left ("The type of the lifted-out expression is not found: "++someName
					++"\nAn identifier should start with its type(or BNF-rulename)."
					++"\nAvailable types are: "++showComma types)

				hole'	<- typeAs f r actualType hole
				let holeAsc	= MAscription actualType hole'
				return $ MEvalContext tp nm hole'
matchTyping f r bnf (tp, _) ctx@(MePtEvalContext nm expr)
			= inMsg ("While typing the evaluation context (which has an expression as hole)"++show ctx) $
			  do	let possibleTypes	= bnf & calledRules >>= reachableVia r		:: [TypeName]
				let possibleTypings	= possibleTypes |> flip (typeAs f r) expr	:: [Either String Expression]
				let successfull		= zip possibleTypes possibleTypings & filter (isRight . snd ) |> fst & nub
				when ((<) 1 $ length successfull) $ Left $ 
					"Trying a possible typing for the expression "++show expr++" is ambiguous, as it can be typed as "++showComma successfull
					++"\nAdd a type annotation to resolve this: "++nm++"[ ("++show expr ++" : someType) ]"
				expr'	<- firstRight possibleTypings
				return $ MEvalContext tp nm expr'


-- Builtin function
matchTyping f s _ _ (MePtCall fNm (Just returnTyp) args)
 	= do	args'	<- inMsg "While typing arguments in a builtin function (flying blind)" $
			args |+> dynamicTranslate f s bottomSymbol
		return $ MCall returnTyp fNm True args'
-- 'Normal function'
matchTyping functions syntax (BNFRuleCall ruleName) _ (MePtCall fNm Nothing args)
 | fNm `M.notMember` functions	= Left $ "Unknwown function: "++fNm
 | ruleName `M.notMember` get bnf syntax
			= Left $ "Unknwown type/bnfrule: "++ruleName
 | otherwise		= do	let fType		= functions M.! fNm
				let argTypes		= init fType
				let returnTyp		= last fType
				assert Left (equivalent syntax returnTyp ruleName) $
					"Actual type "++show returnTyp ++" does not match expected type "++show ruleName
				args'			<- zip args argTypes |> (\(arg, tp) -> typeAs functions syntax tp arg) 
								& allRight
				return $ MCall returnTyp fNm False args'
matchTyping _ _ bnf _ pt@MePtCall{}
			= Left $ "Could not match " ++ show bnf ++ " ~ " ++ show pt

matchTyping _ _ (Literal s) tp (MePtToken s')
 | s == s'		= MLiteral tp s & MParseTree & return
 | otherwise		= Left $ "Not the right literal: "++show s++" ~ "++show s'
matchTyping _ _ Identifier tp (MePtToken s)
 | isIdentifier s	= MIdentifier tp s & MParseTree & return
 | otherwise		= Left $ s ++ " is not an identifier"
matchTyping _ _ Number tp (MePtToken s)
			= readMaybe s & maybe (Left $ "Not a valid int: "++s) return |> MInt tp |> MParseTree
matchTyping _ _ Number tp (MePtInt i)
			= return $ MParseTree $ MInt tp i

matchTyping f s (BNFSeq bnfs) tp (MePtSeq pts)
 | length bnfs == length pts
			= do	let joined	= zip bnfs pts |> (\(bnf, pt) -> matchTyping f s bnf tp pt) 
				joined & allRight 
				|> MSeq tp
 | otherwise		= Left $ "Seq could not match " ++ show bnfs ++ " ~ " ++ show pts 


matchTyping f syntax (BNFRuleCall nm) _ pt
 | nm `M.member` get bnf syntax
		= do	let bnfASTs	= get bnf syntax M.! nm
			let oneOption i bnf	= inMsg ("Trying to match "++nm++"." ++ show i++" ("++show bnf++")") $ 
							matchTyping f syntax bnf (nm, i) pt
			zip [0..] bnfASTs |> uncurry oneOption & firstRight
 | otherwise	= Left $ "No bnf rule with name " ++ nm
matchTyping _ _ bnf _ pt
		= Left $ "Could not match "++show bnf++" ~ "++show pt






isIdentifier	:: String -> Bool
isIdentifier (c:chrs)
		= isLower c && all isAlphaNum chrs


-- only used for builtin functions, as it's arguments do not have a typing. (If this were so, we wouldn't be ably to handle arbitrary syntaxes)
dynamicTranslate	:: Map Name Type -> Syntax -> TypeName -> MEParseTree -> Either String Expression
dynamicTranslate _ _ tp (MePtToken s)	= MLiteral (tp, -1) s & MParseTree & return
dynamicTranslate f s tp (MePtSeq pts)	= pts |+> dynamicTranslate f s tp |> MSeq (tp, -1) 
dynamicTranslate _ _ tp (MePtVar nm)	= MVar tp nm & return
dynamicTranslate _ _ tp (MePtInt i)	= MInt (tp, -1) i & MParseTree & return
dynamicTranslate f s _ ascr@(MePtAscription tp e)	
					= typeAs f s tp e
dynamicTranslate f s tp call@(MePtCall nm bi _)
	= do	tp'	<- maybe (Left $ "Could not find function "++nm) return $ firstJusts [bi, M.lookup nm f |> last]
		typeAs f s tp' call
dynamicTranslate f s tp (MePtEvalContext name hole)
				= Left "Evaluation contexts can not be typed blind/in a dynamic context. Add a type ascription around it"

---------------------- PARSING ---------------------------


parseExpression	:: Parser u MEParseTree
parseExpression	= parseExpression' (identifier <|> iDentifier)

-- we allow expression with some injected 'identifier' parser
parseExpression'	:: Parser u String -> Parser u MEParseTree
parseExpression'	= mePt

mePt ident
	= many1 (ws' *> mePtPart ident <* ws') |> mePtSeq
		where 	mePtSeq [a]	= a
			mePtSeq as	= MePtSeq as

mePtPart ident	= try mePtToken
			<|> meContext ident
			<|> try (mePtCall ident)
			<|> try (meAscription ident)
			<|> try (meNested ident)
			<|> try mePtInt
			<|> mePtVar ident

meNested ident	= char '(' *> ws *> mePt ident <* ws <* char ')'
mePtToken	= bnfLiteral	|> MePtToken
mePtVar ident	= try ident 	|> MePtVar
mePtInt		= negNumber	|> MePtInt
mePtCall ident	= do	builtin	<- try (char '!' >> return True) <|> return False
			nm	<- identifier	-- here we use the normal identifier, not the injected one
			biType	<- if builtin then char ':' >> identifier |> Just else return Nothing
			char '('
			args	<- (ws *> mePt ident <* ws) `sepBy` char ','
			char ')'
			return $ MePtCall nm biType args
meAscription ident
		= do	char '('
			ws
			expr	<- mePt ident
			ws
			char ':'
			ws
			nm	<- identifier	-- this is a bnf-syntax rule; the normal identifier too
			ws
			char ')'
			return $ MePtAscription nm expr


meContext ident	= do	name <- 	try (ident <* char '[')	
			-- char '[' ---------------------------- ^
			ws
			hole	<- parseExpression' ident
			ws			
			char ']'
			return $ MePtEvalContext name hole
			



			