module Parser.RuleParser where

import Utils.Utils
import Parser.ParsingUtils
import qualified Parser.ExpressionParser as EP
import Parser.ExpressionParser (MEParseTree, typeAs)



import Control.Monad

import TypeSystem

import Text.Parsec
import Data.List
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M

import Control.Arrow ((&&&))

data Ctx	= Ctx 	{ syntax	:: Syntax
			, rels		:: [Relation]
			, relTypes	:: Map Symbol Relation
			, funcs		:: Functions
			}

bnfNames'	= bnfNames . syntax
funcTypes ctx	= ctx & funcs |> typesOf

relSymbols ctx	= ctx & rels |> relSymbol & sort & reverse

parseRules	:: (Syntax, [Relation], Functions) -> Parser u [Rule]
parseRules (bnfs, rels, funcs)
	= do	let ctx	= Ctx bnfs rels (rels |> (relSymbol &&& id) & M.fromList) funcs
		many $ try (nls *> rule ctx <* nls)



-- expressions in rules can also use unicode and other weird stuff as identifier



rule	:: Ctx -> Parser u Rule
rule ctx
	= do	ws
		preds	<- try (predicate ctx `sepBy` many1 (char '\t') <* (ws >> char '\n')) 
				<|> (ws >> return [])
		ws
		nm	<- line
		ws
		char '\n'
		ws
		con	<- conclusion ctx
		let rule	= Rule nm preds con
		typeCheckRule (syntax ctx) rule & either error return
		return rule


parseExpr	:: Ctx -> Parser u MEParseTree
parseExpr ctx	= do	let notSymbols	= relSymbols ctx
			EP.parseExpression' $ identifier' notSymbols



conclusion 	:: Ctx -> Parser u Conclusion
conclusion ctx
		= try (conclusionPre ctx) <|> conclusionIn ctx

conclusionIn	:: Ctx -> Parser u Conclusion
conclusionIn ctx
	= do	expr1	<- parseExpr ctx
		ws
		relation	<- choose (relSymbols ctx) 
		ws
		let types	= (relTypes ctx M.! relation) & relType
		exprs	<- replicate (length types - 1) (ws *> parseExpr ctx)
				& intersperseM (ws *> char ',')
		typeAsRelation ctx relation (expr1:exprs)
		
		
		

conclusionPre	:: Ctx -> Parser u Conclusion
conclusionPre ctx
	= do	char '('
		relationSymb	<- choose $ relSymbols ctx 
		char ')'
		ws
		let types 	= (relTypes ctx M.! relationSymb) & relType
		sExprs		<- replicate (length types) (ws *> parseExpr ctx)
					& intersperseM (ws *> char ',')

		typeAsRelation ctx relationSymb sExprs

typeAsRelation	:: Ctx -> String -> [MEParseTree] -> Parser u Conclusion
typeAsRelation ctx symbol sExprs
	= do	let relation	= relTypes ctx M.! symbol	-- possible relation symbols
		let bnfs	= syntax ctx
		
		pos		<- sourcePos
		exprs		<- zip (relType relation) sExprs
					|> uncurry (typeAs (funcTypes ctx) bnfs) 
					|> inMsg ("While typing a predicate/conclusion about "++symbol++", somewhere around "++show pos)
					& allRight
					& either error return
		return $ RelationMet relation exprs
		
 
predicate	:: Ctx -> Parser u Predicate
predicate ctx	= try (predicateIsA ctx) 
			<|> try (predicateSame ctx)
			<|> try (predicateConcl ctx)
			<|> (char '(' *> ws *> predicate ctx <* ws <* char ')')



predicateConcl	:: Ctx -> Parser u Predicate
predicateConcl ctx
	= do	concl <- conclusion ctx
		return $ Needed concl




predicateIsA	:: Ctx -> Parser u Predicate
predicateIsA ctx	
	= do	ws
		nm	<- identifier' (relSymbols ctx)
		colon
		t	<- choose $ bnfNames' ctx
		return $ TermIsA (MVar t nm) t


predicateSame	:: Ctx -> Parser u Predicate
predicateSame ctx
	= do	ws
		e1	<- parseExpr ctx
		ws
		char '='
		ws
		e2	<- parseExpr ctx
		colon
		t	<- choose $ bnfNames' ctx

		let bnfs	= syntax ctx
		pos	<- sourcePos
		let typeExpr e	= typeAs (funcTypes ctx) bnfs t e & inMsg ("While typing the predicate around " ++ show pos) & either fail return
		e1'	<- typeExpr e1
		e2'	<- typeExpr e2
		
		return $ Same e1' e2'

lineName	:: Parser u String
lineName
	= do	char '['
		ws
		nm	<- many $ noneOf "]"
		char ']'
		return nm

line	:: Parser u String
line	= do	ws
		many $ char '-'
		ws
		nm	<- lineName
		ws
		return nm
