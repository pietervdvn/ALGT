{-# LANGUAGE MultiParamTypeClasses #-}
module SyntaxHighlighting.Style where

{- 
Builds a syntax highlighting file, based on a typesystem
-}

import Utils.Utils
import Utils.ToString
import Utils.TypeSystemToString
import Utils.XML
import Assets
import TypeSystem

import Data.Map as M
import Data.List as L
import Data.Maybe

import Control.Monad

import Parser.TypeSystemParser

data SyntaxHighlighting	
	= SH	{ name		:: Name
		, humanName	:: Name
		, desc		:: String
		, commentInfo	:: (Maybe String, Maybe String, Maybe String)
		-- extra style mappings, e.g. noise --> comment
		, extraStyle	:: Map Name Name
		, mainRule	:: Name
		, context	:: [StyleContext]
		} deriving (Show)
		

type SyntaxStyle	= (Map TypeName Name, Map (TypeName, Int) Name)

data StyleContext
	= Match Name String (Maybe Name)
	| Ref Name 
	| Container
		{ containerName		:: Name
		, start			:: Maybe String
		, end			:: Maybe String
		, style			:: Maybe Name
		, includes		:: [StyleContext]
		}
	deriving (Show)





createFullStyles	:: SyntaxStyle -> Syntax -> [StyleContext]
createFullStyles styling syntax
	= syntax & getBNF & M.toList
				>>= uncurry (createStyleForRule syntax styling)
		

createStyleForRule	:: Syntax -> SyntaxStyle -> TypeName -> [BNF] -> [StyleContext]
createStyleForRule syntax styling@(defs, _) tp choices
	= let	choiceStylings	= mapi choices >>= uncurry (createStyleFor syntax styling tp)	:: [StyleContext]
		choiceRefs	= [0 .. length choices - 1] |> show |> ((tp++"-") ++) |> Ref
		container	= Container tp Nothing Nothing Nothing choiceRefs
		in 
		container:choiceStylings




_genMatch :: SyntaxStyle -> String -> Int -> String -> [StyleContext]
_genMatch styling tp i regex
	= [Match (tp++"-"++show i) (inParens regex) $ styleFor styling (tp, i) ]



onHead		:: (a -> a) -> [a] -> [a]
onHead f []	= []
onHead f (a:as)	= f a : as

createStyleFor	:: Syntax -> SyntaxStyle -> TypeName -> Int -> BNF -> [StyleContext]
createStyleFor syntax styling tp i (BNFRuleCall nm)
	= [Container (tp++"-"++show i) Nothing Nothing (styleFor styling (tp, i)) [Ref nm]]
createStyleFor syntax styling tp i (BNFSeq bnfs)
	= let 	style		= styleFor styling (tp, i)
		(SeqState decls refs hardEnd _)		
				= createSeqChain style syntax (tp, i) 0 bnfs

				  -- apply the hard-end (or soft-end) on the first element of the chain
		decls'	 	= onHead (\container -> container{end = firstJusts [hardEnd, end container]}) decls

	  	-- filter references to the element itself, to avoid an infinite loop
		-- The parent context will relaunch the instance anyway
		refs'		= refs & L.filter ((/=) tp)
		
		final		= Container (tp ++ "-" ++ show i) Nothing Nothing style (refs'|> Ref)
		in
		final : decls'

		



createStyleFor syntax styling tp i bnf
	= _genMatch styling tp i $ startRegex syntax bnf


data SeqState	= SeqState 
			{ declared	:: [StyleContext]
			, refs		:: [Name]
			, hardEnd	:: Maybe String	-- If "Just ")", we can end with a regex on the head"
			, softlyEnded	:: Bool		-- Indicates if the last element of the chain has a "end=()" tag
			}

emptySeqState	= SeqState [] [] Nothing True

createSeqChain	:: Maybe Name -> Syntax -> (TypeName, Int) -> Int -> [BNF] -> SeqState
createSeqChain _ _ _ _ []
	= emptySeqState
createSeqChain style syntax (tp, i) counter [BNFRuleCall call]
	= emptySeqState {refs=[call], softlyEnded=False}
createSeqChain style syntax (tp, i) counter [bnf]
	= emptySeqState {hardEnd=Just (startRegex syntax bnf), softlyEnded=False}
createSeqChain style syntax (tp, i) counter (BNFRuleCall call:bnfs)
	= let	seqState	= createSeqChain style syntax (tp, i) (counter + 1) bnfs
		in
		seqState{refs = call:refs seqState, softlyEnded=softlyEnded seqState}
createSeqChain style syntax (tp, i) counter (bnf:bnfs)
	= let	nm	= tp ++ "-" ++ show i ++ "-" ++ show counter
		seqState	= createSeqChain style syntax (tp, i) (counter + 1) bnfs
		
		end	= case (hardEnd seqState, softlyEnded seqState) of
				(Nothing, False)	-> Just ""
				_			-> Nothing

		decl	= Container nm (Just $ startRegex syntax bnf) end style (refs seqState |> Ref)
		in
		emptySeqState{refs = [nm], hardEnd = hardEnd seqState, declared = decl:declared seqState}





styleFor	:: SyntaxStyle -> (Name, Int) -> Maybe Name
styleFor style (tp, i)
	= styleFor' style (fst $ break (== '-') tp, i)

styleFor'	:: SyntaxStyle -> (Name, Int) -> Maybe Name
styleFor' (fallBack, specific) full@(name, _)
 | full `M.member` specific
		= full `M.lookup` specific
 | otherwise	= name `M.lookup` fallBack







instance ToString SyntaxHighlighting where
	toParsable sh	= xmlHeader ++ render sh


render	:: SyntaxHighlighting -> XML
render (SH name humanName desc (lineComment, blockCommentStart, blockCommentEnd) extraStyle mainRule ctxs)
	= let	inProperty propName = maybe "" $ (inLT' "property" [SA "name" propName]) 
		metadata	=	inT "metadata" $ unlines
						[inLT' "property" [SA "name" "globs"] ("*."++name)
						, inProperty "line-comment-start" lineComment
						, inProperty "block-comment-start" blockCommentStart
						, inProperty "block-comment-end" blockCommentEnd]	:: XML
		styles	= inT "styles" $ unlines
				[defStyles |> renderDefStyle & unlines
				, ""
				, extraStyle & M.toList |> (\(source, target) -> mapStyleTo source (name++":"++target))& unlines] :: XML
		defs	= inT "definitions" $ unlines 
					[toParsable' "\n" ctxs
					, ""
					, renderMain name mainRule ]	:: XML
		in 
		  inT' "language" [SA "id" name, SA "_name" humanName, SA "version" "2.0", SA "_section" "Source"] $ unlines
			[ metadata
			, ""
			, styles
			, ""
			, defs 
			]



renderDefStyle	:: String -> XML
renderDefStyle style
	= mapStyleTo style $ "def:"++style

renderMain	:: Name -> TypeName -> XML
renderMain styleName mainRule
	= inT' "context" [SA "id" styleName] $ inT "include" $ toParsable $ Ref mainRule

mapStyleTo	:: String -> String -> XML
mapStyleTo s0 s1
	= attrTag "style" [SA "id" s0, SA "\t_name" s0, SA "\tmap-to" s1]

defStyles	:: [String]
defStyles
	= Assets._GTKSourceViewOptions_DefaultStyleElems & validLines >>= words






instance Check' SyntaxHighlighting StyleContext where
	check' sh sc
		= checkStyles sh sc


checkStyles	:: SyntaxHighlighting -> StyleContext -> Either String ()
checkStyles sh (Match id _ (Just nm))
	= inMsg ("In context "++show id) $ checkHasStyle nm sh
checkStyles sh container@Container{}
	= inMsg ("In context "++show (containerName container)) $ 
	  do	checkHasStyle (fromMaybe "statement" $ style container) sh
		allRight_ (includes container |> checkStyles sh)
checkStyles _ _
	= return ()


instance Check SyntaxHighlighting where
	check sh	= return ()


checkHasStyle	:: Name -> SyntaxHighlighting -> Either String ()
checkHasStyle style sh
	= let	isDef	= style `elem` defStyles
		isContained	 = sh & extraStyle & member style in
		unless (isDef || isContained) $ Left $ "Style "++show style++" is not defined"




styleAttr	:: Name -> Maybe Name -> [Attr]
styleAttr nm Nothing
	= [SA "id" nm]
styleAttr nm (Just style)
	= [SA "id" nm, SA "style-ref" style]


subPattern	:: Maybe a -> String -> Maybe String -> XML
subPattern condition where_ style
 | isJust condition
	= attrTag "context" $ [SA "sub-pattern" "0", SA "where" where_] ++ maybe [] ((:[]) . SA "style-ref") style
 | otherwise
	= []

instance ToString StyleContext where
	toParsable (Match nm match style)
		= inT' "context" (styleAttr nm style) $ inLT "match" match
	toParsable (Ref n)
		= attrTag "context" $ [SA "ref" n]
	toParsable (Container nm start end style includes)
		= inT' "context" [SA "id" nm] $ unlines'
			[ maybe "" (inLT "start" . inParens) start
			, maybe "" (inLT "end" . inParens) end
			, inT "include" $ unlines'
				[subPattern start "start" style
				, subPattern end "end" style
				, (toParsable' "\n" includes)]
			] 
			




unlines' strs	= strs & L.filter (not . L.null) & unlines


----------------------- TEST CODE ------------------------------------------



t	= writeFile "/home/pietervdvn/.local/share/gtksourceview-3.0/language-specs/STFL.lang" tstyle
		

tstyle	= toParsable $ SH "stfl"
			"Staticly Typed Functional Language"
			"Syntax highligting for expressions of STFL"
			(Nothing, Nothing, Nothing)
			(M.singleton "noise" "comment")
			"e"
			(createFullStyles ts $ stfl_syntax)
	
ts	:: SyntaxStyle
ts	= let	base	= Assets._Test_Highlighting 
			  & validLines |> words |> (\[k, v] -> (k,v))	:: [(Name, String)]
		(dotted, normal) = base & L.partition (elem '.' . fst)
		(specKeys, specVals)	= unzip dotted
		specKeys'	= specKeys |> break (== '.') ||>> tail ||>> read
		in (normal & M.fromList, zip specKeys' specVals & M.fromList)


stfl_syntax
	= parseTypeSystem Assets._Test_STFL_typesystem (Just "Test asset STFL")
		& either (error . show) id
		& tsSyntax
