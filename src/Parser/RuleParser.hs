module Parser.RuleParser where

import Utils
import Parser.ParsingUtils
import Parser.ExpressionParser

import Control.Monad

import TypeSystem

import Text.Parsec
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Control.Arrow ((&&&))

data Ctx	= Ctx 	{ bnfRules	:: BNFRules
			, rels		:: [Relation]
			, relTypes	:: Map Symbol Relation
			, funcs		:: Functions
			}

bnfNames'	= bnfNames . bnfRules

relSymbols ctx	= ctx & rels |> relSymbol & sort & reverse

parseRules	:: (BNFRules, [Relation], Functions) -> Parser u [Rule]
parseRules (bnfs, rels, funcs)
	= do	let ctx	= Ctx bnfs rels (rels |> (relSymbol &&& id) & M.fromList) funcs
		many $ try (nls *> rule ctx <* nls)



rule	:: Ctx -> Parser u Rule
rule ctx
	= do	ws
		preds	<- try (predicate ctx `sepBy` char '\t' <* (ws >> char '\n')) 
				<|> (ws >> return [])
		ws
		nm	<- line
		ws
		char '\n'
		ws
		con	<- conclusion ctx
		return $ Rule nm preds con

conclusion ctx	= try (conclusionPre ctx) <|> conclusionIn ctx

conclusionIn	:: Ctx -> Parser u Conclusion
conclusionIn ctx
	= do	expr1	<- parseExpression
		ws
		relation	<- choose (relSymbols ctx) 
		ws
		expr2	<- parseExpression
		typeAsRelation ctx relation [expr1, expr2]
		
		
		

conclusionPre	:: Ctx -> Parser u Conclusion
conclusionPre ctx
	= do	char '('
		relationSymb	<- choose $ relSymbols ctx 
		char ')'
		ws
		let types 	= (relTypes ctx M.! relationSymb) & relType
		sExprs		<- replicate (length types) (ws *> parseExpression <* ws)
					& intersperseM (char ',')

		typeAsRelation ctx relationSymb sExprs


typeAsRelation ctx symbol sExprs
	= do	let relation	= relTypes ctx M.! symbol
		let funcTps	= funcs ctx |> typeOfF
		let bnfs	= bnfRules ctx
		let types 	= relation & relType
		if length types /= length sExprs then 
			fail ("Expected "++show (length types)++" expressions as arguments to the relation "++show (relSymbol relation)++" : "++show types) 
			else return ()
		
		exprs		<- zip types sExprs
					|> uncurry (typeAs funcTps bnfs) 
					& allRight
					& either fail return

		return $ RelationMet relation exprs
		
 

predicate ctx	= try (predicateIsA ctx) <|> predicateConcl ctx



predicateConcl	:: Ctx -> Parser u Predicate
predicateConcl ctx
	= do	concl <- conclusion ctx
		return $ Needed concl




predicateIsA	:: Ctx -> Parser u Predicate
predicateIsA ctx	
	= do	char '('
		ws
		nm	<- identifier
		ws
		char ':'
		ws
		t	<- choose $ bnfNames' ctx
		ws
		char ')'
		return $ TermIsA (MVar (t, -1) nm) t

line	:: Parser u String
line	= do	ws
		char '('
		ws
		nm	<- many $ noneOf ")"
		char ')'
		ws
		many $ char '-'
		ws
		return nm
