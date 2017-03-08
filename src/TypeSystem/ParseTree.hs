 {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module TypeSystem.ParseTree where

import Utils.Utils
import Utils.ToString
import Utils.ToStringExtra

import TypeSystem.Types

import Lens.Micro hiding ((&))
import Lens.Micro.TH

-- info about which BNF-rule was used constructing the ParseTree
type MInfo	= (TypeName, Int)

instance Refactorable TypeName MInfo where
	refactor ftn (tn, i)	= (ftn tn, i)

data LocationInfo = LocationInfo
	{ _liTokenStartLine	:: Int
	, _liTokenStartColumn	:: Int
	, _liTokenStopLine	:: Int
	, _liTokenLineColumn	:: Int
	} deriving (Show, Ord, Eq)
makeLenses ''LocationInfo


-- Represents values that can only come from target language
type ParseTree	= ParseTreeA ()



-- Annotated parse tree
data ParseTreeA a
	= MLiteral 	{_ptAnnot :: a, _ptaInf :: MInfo, _ptaContents :: String}
	| MInt		{_ptAnnot :: a, _ptaInf :: MInfo, _ptaInt :: Int}	
	| PtSeq	{_ptAnnot :: a, _ptaInf :: MInfo, _ptaPts :: [ParseTreeA a]}	
	deriving (Show, Ord, Eq)
makeLenses ''ParseTreeA

instance Functor ParseTreeA where
	fmap f (MLiteral a minf str)	= MLiteral (f a) minf str
	fmap f (MInt a minf i)		= MInt (f a) minf i
	fmap f (PtSeq a minf pts)	= pts |> fmap f & PtSeq (f a) minf

type ParseTreeLi	= ParseTreeA LocationInfo

deAnnot	:: ParseTreeA a -> ParseTree
deAnnot (MLiteral a minf str)	= MLiteral () minf str
deAnnot (MInt a minf i)		= MInt () minf i
deAnnot (PtSeq a minf pts)	= pts |> deAnnot & PtSeq () minf

annot	:: a -> ParseTree -> ParseTreeA a
annot a (MLiteral _ minf str)	= MLiteral a minf str
annot a (MInt _ minf i)		= MInt a minf i
annot a (PtSeq _ minf pts)	= pts |> annot a & PtSeq a minf 

flatten	:: ParseTreeA a -> (a, MInfo, String)
flatten (MLiteral a minf str)	= (a, minf, str)
flatten (MInt a minf i)	= (a, minf, show i)
flatten (PtSeq a minf pts)	= (a, minf, (pts |> flatten) >>= trd3)


removeEmptyTokens	:: ParseTreeA a -> ParseTreeA a
removeEmptyTokens (PtSeq a i seq)
	= let seq'	= seq |> removeEmptyTokens & filter (not . isEmpty) in
		case seq' of
			[pt]	-> pt
			_	-> PtSeq a i seq'
removeEmptyTokens pt	= pt		


isEmpty		:: ParseTreeA a -> Bool
isEmpty (MLiteral _ _ "")
		= True
isEmpty (PtSeq _ _ [])
		= True
isEmpty _	= False

instance SimplyTyped ParseTree where
	typeOf pt	= typeInfoOf' pt & either id fst

typeInfoOf'		:: ParseTreeA a -> Either TypeName (TypeName, Int)
typeInfoOf' (MLiteral _ tp _)		= Right tp
typeInfoOf' (MInt _ tp _) 		= Right tp
typeInfoOf' (PtSeq _ tp _)		= Right tp


replaceAll	:: ParseTreeA a -> [Path] -> ParseTreeA a -> ParseTreeA a
replaceAll _ [] pt = pt
replaceAll r (p:pths) pt
		= replaceAll r pths $ replace r p pt

replace	:: ParseTreeA a -> Path -> ParseTreeA a -> ParseTreeA a
replace _ [] toPlace	= toPlace
replace (PtSeq a tp orig) (i:rest) toPlace
 | length orig <= i
	= error $ "Invalid substitution path: index "++show i++" to big for " ++show (orig |> deAnnot)
 | otherwise
	= let	(init, head:tail)	= splitAt i orig
		head'		= replace head rest toPlace in
		(init ++ (head':tail)) & PtSeq a tp
replace rest path toReplace
	= error $ "Invalid substitution path: not a sequence, but trying to execute the path "++show path++" on " ++show (rest & deAnnot)


search	:: (ParseTreeA a -> Bool) -> ParseTreeA a -> [Path]
search pred pt@(PtSeq _ _ pts)
 | pred pt	= [ [] ]
 | otherwise	= do	(i, pt') 	<- mapi pts
			path		<- search pred pt'
			return $ i:path
search pred pt
 | pred pt	= [ [] ]
 | otherwise	= []


isMInt'	:: ParseTreeA a -> Bool
isMInt' MInt{}	= True
isMInt' _		= False

isPtSeq	:: ParseTreeA a -> Bool
isPtSeq PtSeq{}		= True
isPtSeq _		= False


fromPtToken	:: ParseTreeA a -> Maybe Name
fromPtToken (MLiteral _ _ s)	= Just s
fromPtToken _			= Nothing


instance Refactorable TypeName (ParseTreeA a) where
	refactor ftn (MLiteral a mi s)	= MLiteral a (refactor ftn mi) s
	refactor ftn (MInt a mi i)	= MInt a (refactor ftn mi) i
	refactor ftn (PtSeq a mi seq)	= seq |> refactor ftn & PtSeq a (refactor ftn mi)



instance (Show a) => ToString' ShowParens (ParseTreeA a) where
	show'		= const show

	-- show as if this parsetree was an expression in the declaring file
	toParsable' _ (MLiteral _ _ s)	= s
	toParsable' _ (MInt _ _ i)	= show i
	toParsable' p (PtSeq _ _ exprs)	= exprs |> toParsable' (deepen p) & unwords & inParens' p

	-- show as if this parsetree was an expression in the typesystem file
	toCoParsable' _ (MLiteral _ _ s)	= show s
	toCoParsable' _ (MInt _ _ i)	= show i
	toCoParsable' p (PtSeq _ _ exprs)	= exprs |> toCoParsable' (deepen p) & unwords & inParens' p

	debug' _ (MLiteral _ ti s)	= show s ++ showTI ti
	debug' _ (MInt _ ti i)		= show i ++ showTI ti
	debug' p (PtSeq _ ti exprs)	= "+ " ++ tail (showTI ti) ++ "\n" ++ (exprs |> debug' p & unlines & indentWith "|  ")

instance (Show a) => ToString (ParseTreeA a) where
	toParsable	= toParsable' NoParens
	toCoParsable	= toCoParsable' NotOnRoot
	debug		= debug' NotOnRoot		


instance ToString LocationInfo where
	toParsable (LocationInfo sl sc el ec)	= show sl ++ ","++show sc++";"++show el++","++show ec


