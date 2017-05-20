module TypeSystem.Parser.RuleParser where

import Utils.Utils
import TypeSystem.Parser.ParsingUtils
import qualified TypeSystem.Parser.ExpressionParser as EP
import TypeSystem.Parser.ExpressionParser (MEParseTree, typeAs)



import Control.Monad

import TypeSystem.TypeSystemData hiding (relTypes)

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

relSymbols ctx	= ctx & rels |> get relSymbol & sort & reverse

_makeCtx (bnfs, rels, funcs)
	= Ctx bnfs rels (rels |> (get relSymbol &&& id) & M.fromList) funcs

parseRules	:: (Syntax, [Relation], Functions) -> Parser u [Rule]
parseRules env
	= many $ try (nls *> rule (_makeCtx env) <* nls)


parseProperties	:: (Syntax, [Relation], Functions) -> Parser u [Property]
parseProperties env
	= many $ try (nls *> property (_makeCtx env) <* nls)



parseRule	:: (Syntax, [Relation], Functions) -> Parser u Rule
parseRule env
	= rule $ _makeCtx env

parseProperty	:: (Syntax, [Relation], Functions) -> Parser u Property
parseProperty env
	= property $ _makeCtx env

-- expressions in rules can also use unicode and other weird stuff as identifier


rule		:: Ctx -> Parser u Rule
rule ctx
	= do	(Property n p concls)	<- property ctx
		let msg	= "In rule "++ n ++":\n   Only a single conclusion is allowed. You can't make a choice here!"
		unless (length concls == 1) $ fail msg
		concl	<- case head concls of
				(Needed concl) -> return concl
				_	-> fail $ "Only a conclusion containing a relation is allowed here"
		return (Rule n p concl)

property	:: Ctx -> Parser u Property
property ctx
	= do	ws
		preds	<- try (predicate ctx `sepBy` many1 (char '\t') <* (ws >> char '\n')) 
				<|> (ws >> return [])
		ws
		nm	<- line
		ws
		char '\n'
		ws
		concls	<- predicate ctx `sepBy` (ws >> char '|' >> ws)
		let rule	= Property nm preds concls
		check' (syntax ctx) rule & either error return
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
		inWs $ char ':'
		t	<- choose $ bnfNames' ctx
		return $ TermIsA nm t


predicateSame	:: Ctx -> Parser u Predicate
predicateSame ctx
	= do	ws
		e1	<- parseExpr ctx
		ws
		char '='
		ws
		e2	<- parseExpr ctx
		inWs $ char ':'
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
