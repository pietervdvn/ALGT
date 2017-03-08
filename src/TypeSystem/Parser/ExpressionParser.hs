module TypeSystem.Parser.ExpressionParser where

{-
This module defines a parser for expressions.
In this approach, we tokenize first to a tree, and then try to match a rule with it by typing the rule.
-}

import TypeSystem.Parser.ParsingUtils
import TypeSystem.Parser.BNFParser
import TypeSystem.Parser.TargetLanguageParser
import TypeSystem.TypeSystemData
import Utils.Utils
import Utils.ToString

import Graphs.Lattice

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
			| MePtCall Name Bool (Maybe TypeName) [MEParseTree]	-- builtin if the bool is True, might provide a type in that case
			| MePtAscription Name MEParseTree
			| MePtEvalContext Name MEParseTree	-- The type of the EvaluationContext is derived... from, well the context :p
	deriving (Show, Ord, Eq)


fromMePtToken (MePtToken s)	= Just s
fromMePtToken _			= Nothing

instance ToString MEParseTree where
	toParsable	= showMEPT

showMEPT (MePtToken s)	= show s
showMEPT (MePtSeq pts)	= pts |> showMEPT & unwords & inParens
showMEPT (MePtVar v)	= v
showMEPT (MePtInt i)	= show i
showMEPT (MePtCall n bi biTp args)
			= (if bi then "!" else "") ++ n ++ maybe "" (":"++) biTp ++ inParens (args |> showMEPT & intercalate ", ")
showMEPT (MePtAscription n pt)	= inParens (showMEPT pt++" : "++n)
showMEPT (MePtEvalContext n expr)	= n++"["++ showMEPT expr ++"]"




{- Given a context (knwon function typings + bnf syntax), given a bnf rule (as type),
the parsetree is interpreted/typed following the bnf syntax.
-}
typeAs		:: Map Name Type -> Syntax -> TypeName -> MEParseTree -> Either String Expression
typeAs functions rules ruleName pt
	= inMsg ("While typing "++toParsable pt++" against "++ruleName) $ 
		matchTyping functions rules (BNFRuleCall ruleName) (ruleName, error "Should not be used") pt
 




-- we compare the expected parse type (known via the BNF) and the expression we got
matchTyping	:: Map Name Type -> Syntax -> BNF -> (TypeName, Int) -> MEParseTree -> Either String Expression
matchTyping f r (BNFRuleCall ruleCall) tp (MePtAscription as expr)
 | not (alwaysIsA r as ruleCall)	
			= Left $ "Invalid cast: "++ruleCall++" is not a "++as
 | otherwise 		= typeAs f r as expr |> MAscription as
matchTyping f r bnf tp c@(MePtAscription as expr)
			= Left $ "Invalid cast: '"++toParsable c++"' could not be matched with '"++toParsable bnf++"'"

matchTyping _ _ (BNFRuleCall ruleCall) tp (MePtVar nm)
 | nm == "_"		= return $ MVar topSymbol "_"
 | otherwise		= return $ MVar ruleCall nm
matchTyping _ _ exp tp (MePtVar nm)
 | nm == "_"		= return $ MVar topSymbol "_"
 | otherwise		
			= Left $ "Non-rulecall (expected: "++toParsable exp++") with a var "++ nm

matchTyping f r bnf (tp, _) ctx@(MePtEvalContext nm hole@(MePtVar someName))
			= inMsg ("While typing the evalution context (with only an identifier as hole)"++ toParsable ctx) $
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
			= inMsg ("While typing the evaluation context (which has an expression as hole)"++toParsable ctx) $
			  do	let possibleTypes	= bnf & calledRules >>= reachableVia r		:: [TypeName]
				let possibleTypings	= possibleTypes |> flip (typeAs f r) expr	:: [Either String Expression]
				let successfullTypings	= zip possibleTypes possibleTypings |> sndEffect & rights :: [(TypeName, Expression)]
				let successfull		= successfullTypings |> fst & nub		:: [TypeName]
				let successfull'	= successfull & smallestCommonType' r		:: Maybe TypeName
				let ambiguousTypingErr	= Left $
					"Trying a possible typing for the expression "++toParsable expr++" is ambiguous, as it can be typed as "++showComma successfull
					++"\nAdd a type annotation to resolve this: "++nm++"[ ("++toParsable expr ++" : someType) ]"
				inMsg "No typing for the hole can be found" $ when (null successfull) $ Left (possibleTypings & lefts & unlines & indent)
				when (isNothing successfull') ambiguousTypingErr
				
				let expr'	= successfullTypings & filter ((== fromJust successfull') . fst) & head & snd
				return $ MEvalContext tp nm expr'


-- Builtin function
matchTyping f s (BNFRuleCall ruleName) _ (MePtCall fNm True returnTypAct args)
 	= do	let fNm'	= show ('!':fNm)
		let notFoundMsg	= "Builtin function "++fNm'++" is not defined. Consult the reference manual for a documentation about builtin functions."
					++"\nKnown builtins are:\n"++(builtinFunctions' & M.keys & unlines & indent)
		funcDef	<- checkExists fNm builtinFunctions' notFoundMsg
		let inTps	= get bifInArgs funcDef

		let outTpKnown	= get bifResultType funcDef
		let outTpAct	= maybe outTpKnown (infimum (get lattice s) outTpKnown) returnTypAct
		unless (alwaysIsA s outTpAct ruleName) $ Left $
			"The builtin function returns a "++outTpAct++", but a "++ruleName++" is expected. Add (or change) an explicit typing:\n"
			++ indent (toParsable $ MePtCall fNm True (Just ruleName) args)

		args'	<- case inTps of
			(Right knownTypes)	-> 
				do	let nonMatchingMsg	= "Expected "++show (length knownTypes)++" arguments to "++fNm'++", but got "++show (length args)
					unless (length args == length knownTypes) $ Left nonMatchingMsg
					inMsg ("While typing arguments to the builtin function "++fNm') $
							zip args knownTypes |> (\(arg, tp) -> typeAs f s tp arg) & allRight
			(Left (shouldBe, atLeast)) ->
				do	unless (length args >= atLeast) $ Left $ 
						"Expected at least "++show atLeast++" arguments of type "++shouldBe++" to the bulitin function "++fNm'
					inMsg ("While typing arguments to the builtin function "++fNm') $
						args |> typeAs f s shouldBe & allRight
		return $ MCall outTpAct fNm True args'

-- Normal function
matchTyping functions syntax (BNFRuleCall ruleName) _ (MePtCall fNm False _ args)
 | fNm `M.notMember` functions	= Left $ "Unknown function: "++fNm
 | ruleName `M.notMember` get bnf syntax && not (isBuiltinName ruleName)
			= Left $ "Unknown type/syntactic form: "++ruleName
 | otherwise		= do	let fType		= functions M.! fNm
				let argTypes		= init fType
				let returnTyp		= last fType
				assert Left (equivalent syntax returnTyp ruleName) $
					"Actual type "++show returnTyp ++" does not match expected type "++show ruleName
				let lengthMsg		= "Expected "++show (length argTypes)++" arguments to "++show fNm++", but got "++show (length args)++" arguments"
				unless (length args == length argTypes) $ Left lengthMsg
				args'		<- zip args argTypes
							|> (\(arg, tp) -> typeAs functions syntax tp arg) 
							& allRight'
				return $ MCall returnTyp fNm False args'
matchTyping _ _ bnf _ pt@MePtCall{}
			= Left $ "Could not match " ++ toParsable bnf ++ " ~ " ++ toParsable pt

matchTyping f s (BNFSeq bnfs) tp (MePtSeq pts)
 | length bnfs == length pts
			= do	let joined	= zip bnfs pts |> (\(bnf, pt) -> matchTyping f s bnf tp pt) 
				joined & allRight 
				|> MSeq tp
 | otherwise		= Left $ "Seq could not match " ++ toParsable' " " bnfs ++ " ~ " ++ toParsable' " " pts 


matchTyping _ _ (BNFRuleCall "Number") tp (MePtToken s) -- TODO Dehardcode this
			= readMaybe s & maybe (Left $ "Not a valid int: "++s) return |> MInt tp |> MParseTree
matchTyping _ _ (BNFRuleCall "Number") tp (MePtInt i)	-- TODO dehardcode this
			= return $ MParseTree $ MInt tp i
matchTyping f syntax (BNFRuleCall nm) _ pt
 | isBuiltinName nm
		= do	contents	<- maybe (Left "Builtin with no token matched") return $
						fromMePtToken pt
			unless (isValidBuiltin (BNFRuleCall nm) contents) $  Left $ contents ++" is not a "++nm 
			MLiteral (nm, 0) contents & MParseTree & return
 | nm == topSymbol
		= dynamicTranslate f syntax nm pt
 | nm `M.member` get bnf syntax
		= do	let bnfASTs	= get bnf syntax M.! nm
			let grouped	= get groupModes syntax M.! nm
			if not grouped then do
				let oneOption i bnf	= inMsg ("Trying to match "++nm++"." ++ show i++" ("++toParsable bnf++")") $ 
								matchTyping f syntax bnf (nm, i) pt
				let options'	= zip [0..] bnfASTs |> uncurry oneOption		
				let options	= options' & rights & nub
				when (null options) $ Left $ "No clauses matched:\n"++(options' & lefts & unlines & indent)
				when (length options > 1) $ Left $ "Multiple matches: "++toCoParsable' " | " options
				return $ head options
			 else do
					let noTokenMsg	= Left $ "Trying to decipher a grouped rule "++show nm++", but '"++toParsable pt++"' is not a token"
					str		<- fromMePtToken pt |> return & fromMaybe noTokenMsg
					let groupedMsg	= "While parsing grouped token "++show str++" against "++show nm
					pt'	<- inMsg groupedMsg $ parseTargetLang syntax nm "Literal of a grouped value" str
					return $ MParseTree pt'

				
 | otherwise	= Left $ "No bnf rule with name " ++ nm

-- Simpler cases
matchTyping _ _ (Literal s) tp (MePtToken s')
 | s == s'		= MLiteral tp s & MParseTree & return
 | otherwise		= Left $ "Not the right literal: "++show s++" ~ "++show s'

matchTyping _ _ bnf _ pt
	= Left $ "Could not match "++toParsable bnf++" ~ "++toParsable pt




-- only used for builtin functions, as it's arguments do not have a typing. (If this were so, we wouldn't be ably to handle arbitrary syntaxes)
dynamicTranslate	:: Map Name Type -> Syntax -> TypeName -> MEParseTree -> Either String Expression
dynamicTranslate _ _ tp (MePtToken s)	= MLiteral (tp, -1) s & MParseTree & return
dynamicTranslate f s tp (MePtSeq pts)	= pts |+> dynamicTranslate f s tp |> MSeq (tp, -1) 
dynamicTranslate _ _ tp (MePtVar nm)	= MVar tp nm & return
dynamicTranslate _ _ tp (MePtInt i)	= MInt (tp, -1) i & MParseTree & return
dynamicTranslate f s _ ascr@(MePtAscription tp e)	
					= typeAs f s tp e
dynamicTranslate f s tp call@(MePtCall nm _ bi _)
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
mePtVar		:: Parser u Name-> Parser u MEParseTree
mePtVar ident	= do	nm	<- try ident 
					<|> (char '_' |> (:[]))
			return $ MePtVar nm
mePtInt		= negNumber	|> MePtInt
mePtCall ident	= do	builtin	<- try (char '!' >> return True) <|> return False
			nm	<- identifier	-- here we use the normal identifier, not the injected one
			biType	<- if builtin then (char ':' >> identifier |> Just) <|> return Nothing else return Nothing
			char '('
			args	<- (ws *> mePt ident <* ws) `sepBy` char ','
			char ')'
			return $ MePtCall nm builtin biType args
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
			



			
