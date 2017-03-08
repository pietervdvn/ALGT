module Assets where


import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (toStrict)


-- Automatically generated
-- This file contains all assets, loaded via 'unsafePerformIO' or hardcoded as string, not to need IO for assets


allAssets = [("White.style", _White_style)
			, ("White.style1", _White_style1)
			, ("WhiteFlat.style", _WhiteFlat_style)
			, ("language-changes.lang", _language_changes_lang)
			, ("Style.language", _Style_language)
			, ("Terminal.style", _Terminal_style)
			, ("WhiteFlat.style1", _WhiteFlat_style1)
			, ("language.lang", _language_lang)
			, ("MinimalStyles.txt", _MinimalStyles_txt)
			, ("Manual/TypeTrees1.svg", _Manual_TypeTrees1_svg)
			, ("Manual/2.3Tut-Relations.md", _Manual_2_3Tut_Relations_md)
			, ("Manual/2.0Tut-Intro.md", _Manual_2_0Tut_Intro_md)
			, ("Manual/6Thanks.md", _Manual_6Thanks_md)
			, ("Manual/TypeTrees2.svg", _Manual_TypeTrees2_svg)
			, ("Manual/2Tutorial.md", _Manual_2Tutorial_md)
			, ("Manual/Manual.genPaused", _Manual_Manual_genPaused)
			, ("Manual/2.1Tut-Syntax.md", _Manual_2_1Tut_Syntax_md)
			, ("Manual/Focus.generate", _Manual_Focus_generate)
			, ("Manual/TypeTrees0.svg", _Manual_TypeTrees0_svg)
			, ("Manual/TypeTrees1annot.svg", _Manual_TypeTrees1annot_svg)
			, ("Manual/Main.tex", _Manual_Main_tex)
			, ("Manual/TypeTrees2annot1.svg", _Manual_TypeTrees2annot1_svg)
			, ("Manual/3ReferenceManual.md", _Manual_3ReferenceManual_md)
			, ("Manual/4Concepts.md", _Manual_4Concepts_md)
			, ("Manual/TypeTrees2annot.svg", _Manual_TypeTrees2annot_svg)
			, ("Manual/5Gradualization.md", _Manual_5Gradualization_md)
			, ("Manual/Manual.generate", _Manual_Manual_generate)
			, ("Manual/Options.language", _Manual_Options_language)
			, ("Manual/build.sh", _Manual_build_sh)
			, ("Manual/1Overview.md", _Manual_1Overview_md)
			, ("Manual/TypeTrees0annot.svg", _Manual_TypeTrees0annot_svg)
			, ("Manual/2.2Tut-Functions.md", _Manual_2_2Tut_Functions_md)
			, ("Manual/0Introduction.md", _Manual_0Introduction_md)
			, ("Manual/Files/examples.stfl", _Manual_Files_examples_stfl)
			, ("Manual/Files/typeExamples.stfl", _Manual_Files_typeExamples_stfl)
			, ("Manual/Files/STFLForSlides.language", _Manual_Files_STFLForSlides_language)
			, ("Manual/Files/STFLBool.language", _Manual_Files_STFLBool_language)
			, ("Manual/Files/STFLBoolSimpleExpr.language", _Manual_Files_STFLBoolSimpleExpr_language)
			, ("Manual/Files/STFLrec.language", _Manual_Files_STFLrec_language)
			, ("Manual/Files/STFLInt.language", _Manual_Files_STFLInt_language)
			, ("Manual/Files/STFLWrongOrder.language", _Manual_Files_STFLWrongOrder_language)
			, ("Manual/Files/STFL.language", _Manual_Files_STFL_language)
			, ("Manual/Output/ALGT_Manual.html", _Manual_Output_ALGT_Manual_html)
			, ("Manual/Output/ALGT_Focus.html", _Manual_Output_ALGT_Focus_html)
			, ("GTKSourceViewOptions/AppearanceElems", _GTKSourceViewOptions_AppearanceElems)
			, ("GTKSourceViewOptions/ColorOptions", _GTKSourceViewOptions_ColorOptions)
			, ("GTKSourceViewOptions/DefaultStyleElems", _GTKSourceViewOptions_DefaultStyleElems)
			, ("GTKSourceViewOptions/FloatOptions", _GTKSourceViewOptions_FloatOptions)
			, ("GTKSourceViewOptions/Readme.md", _GTKSourceViewOptions_Readme_md)
			, ("GTKSourceViewOptions/BoolOptions.txt", _GTKSourceViewOptions_BoolOptions_txt)
			, ("Test/CommonSubset.language", _Test_CommonSubset_language)
			, ("Test/examples.stfl", _Test_examples_stfl)
			, ("Test/Recursive.language", _Test_Recursive_language)
			, ("Test/FuncTypeErr.language", _Test_FuncTypeErr_language)
			, ("Test/LiveCheck.language", _Test_LiveCheck_language)
			, ("Test/DynamizeSTFL.language-changes", _Test_DynamizeSTFL_language_changes)
			, ("Test/GradualizeSTFL.language-changes", _Test_GradualizeSTFL_language_changes)
			, ("Test/STFL.language", _Test_STFL_language)
			, ("IntegrationTests/_log___Test_STFL_language__dlf__plain", _IntegrationTests__log___Test_STFL_language__dlf__plain)
			, ("IntegrationTests/Parsetrees_13.svg", _IntegrationTests_Parsetrees_13_svg)
			, ("IntegrationTests/Parsetrees_12.svg", _IntegrationTests_Parsetrees_12_svg)
			, ("IntegrationTests/_Test_STFL_language__irdouble_pointdouble_point__plain", _IntegrationTests__Test_STFL_language__irdouble_pointdouble_point__plain)
			, ("IntegrationTests/_Test_STFL_language__ifaequate__plain", _IntegrationTests__Test_STFL_language__ifaequate__plain)
			, ("IntegrationTests/_log___Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain", _IntegrationTests__log___Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain)
			, ("IntegrationTests/Parsetrees_6.svg", _IntegrationTests_Parsetrees_6_svg)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l_rarrow__plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l_rarrow__plain)
			, ("IntegrationTests/Parsetrees_1.svg", _IntegrationTests_Parsetrees_1_svg)
			, ("IntegrationTests/Parsetrees_0.svg", _IntegrationTests_Parsetrees_0_svg)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l_r____plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l_r____plain)
			, ("IntegrationTests/Parsetrees_9.svg", _IntegrationTests_Parsetrees_9_svg)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__tpProgress__plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpProgress__plain)
			, ("IntegrationTests/Parsetrees_2.svg", _IntegrationTests_Parsetrees_2_svg)
			, ("IntegrationTests/_log___Test_STFL_language__ifadom__plain", _IntegrationTests__log___Test_STFL_language__ifadom__plain)
			, ("IntegrationTests/_Test_STFL_language__ifadom__plain", _IntegrationTests__Test_STFL_language__ifadom__plain)
			, ("IntegrationTests/_Test_STFL_language__ifaeval__plain", _IntegrationTests__Test_STFL_language__ifaeval__plain)
			, ("IntegrationTests/_Test_CommonSubset_language__dlf__plain", _IntegrationTests__Test_CommonSubset_language__dlf__plain)
			, ("IntegrationTests/Parsetrees_17.svg", _IntegrationTests_Parsetrees_17_svg)
			, ("IntegrationTests/_Test_STFL_language__dlf__plain", _IntegrationTests__Test_STFL_language__dlf__plain)
			, ("IntegrationTests/_log___Test_STFL_language__irarrow__plain", _IntegrationTests__log___Test_STFL_language__irarrow__plain)
			, ("IntegrationTests/Parsetrees_20.svg", _IntegrationTests_Parsetrees_20_svg)
			, ("IntegrationTests/_log___Test_STFL_language__ifaequate__plain", _IntegrationTests__log___Test_STFL_language__ifaequate__plain)
			, ("IntegrationTests/_Test_STFL_language__ir___plain", _IntegrationTests__Test_STFL_language__ir___plain)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain)
			, ("IntegrationTests/Parsetrees_23.svg", _IntegrationTests_Parsetrees_23_svg)
			, ("IntegrationTests/_log___Test_STFL_language__plain", _IntegrationTests__log___Test_STFL_language__plain)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain)
			, ("IntegrationTests/_log___Test_Recursive_language__dlf__plain", _IntegrationTests__log___Test_Recursive_language__dlf__plain)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__plain)
			, ("IntegrationTests/Parsetrees_4.svg", _IntegrationTests_Parsetrees_4_svg)
			, ("IntegrationTests/_Style_languageTerminal_stylestyleFile__html__plain", _IntegrationTests__Style_languageTerminal_stylestyleFile__html__plain)
			, ("IntegrationTests/_log___Style_languageTerminal_stylestyleFile__html__plain", _IntegrationTests__log___Style_languageTerminal_stylestyleFile__html__plain)
			, ("IntegrationTests/Parsetrees_14.svg", _IntegrationTests_Parsetrees_14_svg)
			, ("IntegrationTests/_log___Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain", _IntegrationTests__log___Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain)
			, ("IntegrationTests/_Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain", _IntegrationTests__Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain)
			, ("IntegrationTests/_Test_STFL_language__ia__plain", _IntegrationTests__Test_STFL_language__ia__plain)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__tpa__plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpa__plain)
			, ("IntegrationTests/_Test_FuncTypeErr_language__dlf__plain", _IntegrationTests__Test_FuncTypeErr_language__dlf__plain)
			, ("IntegrationTests/_log___Test_STFL_language__lsvgSyntax_svg__plain", _IntegrationTests__log___Test_STFL_language__lsvgSyntax_svg__plain)
			, ("IntegrationTests/_Test_STFL_language__irEvalCtx__plain", _IntegrationTests__Test_STFL_language__irEvalCtx__plain)
			, ("IntegrationTests/_Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain", _IntegrationTests__Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__plain)
			, ("IntegrationTests/_log___Test_TypeErrFunc_language__dlf__plain", _IntegrationTests__log___Test_TypeErrFunc_language__dlf__plain)
			, ("IntegrationTests/_log___Test_STFL_language__irEvalCtx__plain", _IntegrationTests__log___Test_STFL_language__irEvalCtx__plain)
			, ("IntegrationTests/Parsetrees_16.svg", _IntegrationTests_Parsetrees_16_svg)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l_r___plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l_r___plain)
			, ("IntegrationTests/Parsetrees_22.svg", _IntegrationTests_Parsetrees_22_svg)
			, ("IntegrationTests/_log___Test_STFL_language__ir___plain", _IntegrationTests__log___Test_STFL_language__ir___plain)
			, ("IntegrationTests/_Test_STFL_language__lsvgSyntax_svg__plain", _IntegrationTests__Test_STFL_language__lsvgSyntax_svg__plain)
			, ("IntegrationTests/Parsetrees_5.svg", _IntegrationTests_Parsetrees_5_svg)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain)
			, ("IntegrationTests/_log___Test_STFL_language__irarrowstart__plain", _IntegrationTests__log___Test_STFL_language__irarrowstart__plain)
			, ("IntegrationTests/_log___Test_STFL_language__ifacod__plain", _IntegrationTests__log___Test_STFL_language__ifacod__plain)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__tpa__plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpa__plain)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l_rarrow__plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_rarrow__plain)
			, ("IntegrationTests/_log___Test_STFL_language__ir____plain", _IntegrationTests__log___Test_STFL_language__ir____plain)
			, ("IntegrationTests/_log___Test_STFL_language__ia__plain", _IntegrationTests__log___Test_STFL_language__ia__plain)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain)
			, ("IntegrationTests/_log___Test_CommonSubset_language__dlf__plain", _IntegrationTests__log___Test_CommonSubset_language__dlf__plain)
			, ("IntegrationTests/_Test_Recursive_language__dlf__plain", _IntegrationTests__Test_Recursive_language__dlf__plain)
			, ("IntegrationTests/_Test_STFL_language__ir____plain", _IntegrationTests__Test_STFL_language__ir____plain)
			, ("IntegrationTests/Parsetrees_11.svg", _IntegrationTests_Parsetrees_11_svg)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain)
			, ("IntegrationTests/Parsetrees_8.svg", _IntegrationTests_Parsetrees_8_svg)
			, ("IntegrationTests/Syntax.svg", _IntegrationTests_Syntax_svg)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain)
			, ("IntegrationTests/Parsetrees_10.svg", _IntegrationTests_Parsetrees_10_svg)
			, ("IntegrationTests/_Test_STFL_language__irarrow__plain", _IntegrationTests__Test_STFL_language__irarrow__plain)
			, ("IntegrationTests/_Test_STFL_language__irarrowstart__plain", _IntegrationTests__Test_STFL_language__irarrowstart__plain)
			, ("IntegrationTests/Parsetrees_18.svg", _IntegrationTests_Parsetrees_18_svg)
			, ("IntegrationTests/_log___Test_STFL_language__ifaeval__plain", _IntegrationTests__log___Test_STFL_language__ifaeval__plain)
			, ("IntegrationTests/_Test_STFL_language__plain", _IntegrationTests__Test_STFL_language__plain)
			, ("IntegrationTests/_Test_TypeErrFunc_language__dlf__plain", _IntegrationTests__Test_TypeErrFunc_language__dlf__plain)
			, ("IntegrationTests/Parsetrees_3.svg", _IntegrationTests_Parsetrees_3_svg)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l_r___plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_r___plain)
			, ("IntegrationTests/Parsetrees_15.svg", _IntegrationTests_Parsetrees_15_svg)
			, ("IntegrationTests/_Test_STFL_language__ifacod__plain", _IntegrationTests__Test_STFL_language__ifacod__plain)
			, ("IntegrationTests/_log___Test_STFL_language__irdouble_pointdouble_point__plain", _IntegrationTests__log___Test_STFL_language__irdouble_pointdouble_point__plain)
			, ("IntegrationTests/Parsetrees_7.svg", _IntegrationTests_Parsetrees_7_svg)
			, ("IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__tpProgress__plain", _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpProgress__plain)
			, ("IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l_r____plain", _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_r____plain)
			, ("IntegrationTests/_log___Test_FuncTypeErr_language__dlf__plain", _IntegrationTests__log___Test_FuncTypeErr_language__dlf__plain)
			, ("IntegrationTests/Parsetrees_21.svg", _IntegrationTests_Parsetrees_21_svg)
			, ("IntegrationTests/Parsetrees_19.svg", _IntegrationTests_Parsetrees_19_svg)
			]

{-# NOINLINE _White_style #-}
_White_style
	 = unsafePerformIO $ readFile "src/Assets/White.style"

{-# NOINLINE _White_style1 #-}
_White_style1
	 = unsafePerformIO $ readFile "src/Assets/White.style1"

{-# NOINLINE _WhiteFlat_style #-}
_WhiteFlat_style
	 = unsafePerformIO $ readFile "src/Assets/WhiteFlat.style"

{-# NOINLINE _language_changes_lang #-}
_language_changes_lang
	 = unsafePerformIO $ readFile "src/Assets/language-changes.lang"

{-# NOINLINE _Style_language #-}
_Style_language
	 = unsafePerformIO $ readFile "src/Assets/Style.language"

{-# NOINLINE _Terminal_style #-}
_Terminal_style
	 = unsafePerformIO $ readFile "src/Assets/Terminal.style"

{-# NOINLINE _WhiteFlat_style1 #-}
_WhiteFlat_style1
	 = unsafePerformIO $ readFile "src/Assets/WhiteFlat.style1"

{-# NOINLINE _language_lang #-}
_language_lang
	 = unsafePerformIO $ readFile "src/Assets/language.lang"

{-# NOINLINE _MinimalStyles_txt #-}
_MinimalStyles_txt
	 = unsafePerformIO $ readFile "src/Assets/MinimalStyles.txt"

{-# NOINLINE _Manual_TypeTrees1_svg #-}
_Manual_TypeTrees1_svg
	 = unsafePerformIO $ readFile "src/Assets/Manual/TypeTrees1.svg"

{-# NOINLINE _Manual_2_3Tut_Relations_md #-}
_Manual_2_3Tut_Relations_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/2.3Tut-Relations.md"

{-# NOINLINE _Manual_2_0Tut_Intro_md #-}
_Manual_2_0Tut_Intro_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/2.0Tut-Intro.md"

{-# NOINLINE _Manual_6Thanks_md #-}
_Manual_6Thanks_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/6Thanks.md"

{-# NOINLINE _Manual_TypeTrees2_svg #-}
_Manual_TypeTrees2_svg
	 = unsafePerformIO $ readFile "src/Assets/Manual/TypeTrees2.svg"

{-# NOINLINE _Manual_2Tutorial_md #-}
_Manual_2Tutorial_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/2Tutorial.md"

{-# NOINLINE _Manual_hofstadter_png #-}
_Manual_hofstadter_png
	 = unsafePerformIO $ B.readFile "src/Assets/Manual/hofstadter.png"

{-# NOINLINE _Manual_Manual_genPaused #-}
_Manual_Manual_genPaused
	 = unsafePerformIO $ readFile "src/Assets/Manual/Manual.genPaused"

{-# NOINLINE _Manual_2_1Tut_Syntax_md #-}
_Manual_2_1Tut_Syntax_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/2.1Tut-Syntax.md"

{-# NOINLINE _Manual_Rules_png #-}
_Manual_Rules_png
	 = unsafePerformIO $ B.readFile "src/Assets/Manual/Rules.png"

{-# NOINLINE _Manual_Focus_generate #-}
_Manual_Focus_generate
	 = unsafePerformIO $ readFile "src/Assets/Manual/Focus.generate"

{-# NOINLINE _Manual_TypeTrees0_svg #-}
_Manual_TypeTrees0_svg
	 = unsafePerformIO $ readFile "src/Assets/Manual/TypeTrees0.svg"

{-# NOINLINE _Manual_TypeTrees1annot_svg #-}
_Manual_TypeTrees1annot_svg
	 = unsafePerformIO $ readFile "src/Assets/Manual/TypeTrees1annot.svg"

{-# NOINLINE _Manual_Main_tex #-}
_Manual_Main_tex
	 = unsafePerformIO $ readFile "src/Assets/Manual/Main.tex"

{-# NOINLINE _Manual_TypeTrees2annot1_svg #-}
_Manual_TypeTrees2annot1_svg
	 = unsafePerformIO $ readFile "src/Assets/Manual/TypeTrees2annot1.svg"

{-# NOINLINE _Manual_3ReferenceManual_md #-}
_Manual_3ReferenceManual_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/3ReferenceManual.md"

{-# NOINLINE _Manual_4Concepts_md #-}
_Manual_4Concepts_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/4Concepts.md"

{-# NOINLINE _Manual_TypeTrees2annot_svg #-}
_Manual_TypeTrees2annot_svg
	 = unsafePerformIO $ readFile "src/Assets/Manual/TypeTrees2annot.svg"

{-# NOINLINE _Manual_ConsTrans_png #-}
_Manual_ConsTrans_png
	 = unsafePerformIO $ B.readFile "src/Assets/Manual/ConsTrans.png"

{-# NOINLINE _Manual_5Gradualization_md #-}
_Manual_5Gradualization_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/5Gradualization.md"

{-# NOINLINE _Manual_Manual_generate #-}
_Manual_Manual_generate
	 = unsafePerformIO $ readFile "src/Assets/Manual/Manual.generate"

{-# NOINLINE _Manual_Options_language #-}
_Manual_Options_language
	 = unsafePerformIO $ readFile "src/Assets/Manual/Options.language"

{-# NOINLINE _Manual_build_sh #-}
_Manual_build_sh
	 = unsafePerformIO $ readFile "src/Assets/Manual/build.sh"

{-# NOINLINE _Manual_1Overview_md #-}
_Manual_1Overview_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/1Overview.md"

{-# NOINLINE _Manual_TypeTrees0annot_svg #-}
_Manual_TypeTrees0annot_svg
	 = unsafePerformIO $ readFile "src/Assets/Manual/TypeTrees0annot.svg"

{-# NOINLINE _Manual_2_2Tut_Functions_md #-}
_Manual_2_2Tut_Functions_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/2.2Tut-Functions.md"

{-# NOINLINE _Manual_0Introduction_md #-}
_Manual_0Introduction_md
	 = unsafePerformIO $ readFile "src/Assets/Manual/0Introduction.md"

{-# NOINLINE _Manual_Files_examples_stfl #-}
_Manual_Files_examples_stfl
	 = unsafePerformIO $ readFile "src/Assets/Manual/Files/examples.stfl"

{-# NOINLINE _Manual_Files_typeExamples_stfl #-}
_Manual_Files_typeExamples_stfl
	 = unsafePerformIO $ readFile "src/Assets/Manual/Files/typeExamples.stfl"

{-# NOINLINE _Manual_Files_STFLForSlides_language #-}
_Manual_Files_STFLForSlides_language
	 = unsafePerformIO $ readFile "src/Assets/Manual/Files/STFLForSlides.language"

{-# NOINLINE _Manual_Files_STFLBool_language #-}
_Manual_Files_STFLBool_language
	 = unsafePerformIO $ readFile "src/Assets/Manual/Files/STFLBool.language"

{-# NOINLINE _Manual_Files_STFLBoolSimpleExpr_language #-}
_Manual_Files_STFLBoolSimpleExpr_language
	 = unsafePerformIO $ readFile "src/Assets/Manual/Files/STFLBoolSimpleExpr.language"

{-# NOINLINE _Manual_Files_STFLrec_language #-}
_Manual_Files_STFLrec_language
	 = unsafePerformIO $ readFile "src/Assets/Manual/Files/STFLrec.language"

{-# NOINLINE _Manual_Files_STFLInt_language #-}
_Manual_Files_STFLInt_language
	 = unsafePerformIO $ readFile "src/Assets/Manual/Files/STFLInt.language"

{-# NOINLINE _Manual_Files_STFLWrongOrder_language #-}
_Manual_Files_STFLWrongOrder_language
	 = unsafePerformIO $ readFile "src/Assets/Manual/Files/STFLWrongOrder.language"

{-# NOINLINE _Manual_Files_STFL_language #-}
_Manual_Files_STFL_language
	 = unsafePerformIO $ readFile "src/Assets/Manual/Files/STFL.language"

{-# NOINLINE _Manual_Output_ALGT_Focus_pdf #-}
_Manual_Output_ALGT_Focus_pdf
	 = unsafePerformIO $ B.readFile "src/Assets/Manual/Output/ALGT_Focus.pdf"

{-# NOINLINE _Manual_Output_ALGT_Manual_html #-}
_Manual_Output_ALGT_Manual_html
	 = unsafePerformIO $ readFile "src/Assets/Manual/Output/ALGT_Manual.html"

{-# NOINLINE _Manual_Output_ALGT_Focus_html #-}
_Manual_Output_ALGT_Focus_html
	 = unsafePerformIO $ readFile "src/Assets/Manual/Output/ALGT_Focus.html"

{-# NOINLINE _Manual_Output_ALGT_Manual_pdf #-}
_Manual_Output_ALGT_Manual_pdf
	 = unsafePerformIO $ B.readFile "src/Assets/Manual/Output/ALGT_Manual.pdf"

{-# NOINLINE _GTKSourceViewOptions_AppearanceElems #-}
_GTKSourceViewOptions_AppearanceElems
	 = unsafePerformIO $ readFile "src/Assets/GTKSourceViewOptions/AppearanceElems"

{-# NOINLINE _GTKSourceViewOptions_ColorOptions #-}
_GTKSourceViewOptions_ColorOptions
	 = unsafePerformIO $ readFile "src/Assets/GTKSourceViewOptions/ColorOptions"

{-# NOINLINE _GTKSourceViewOptions_DefaultStyleElems #-}
_GTKSourceViewOptions_DefaultStyleElems
	 = unsafePerformIO $ readFile "src/Assets/GTKSourceViewOptions/DefaultStyleElems"

{-# NOINLINE _GTKSourceViewOptions_FloatOptions #-}
_GTKSourceViewOptions_FloatOptions
	 = unsafePerformIO $ readFile "src/Assets/GTKSourceViewOptions/FloatOptions"

{-# NOINLINE _GTKSourceViewOptions_Readme_md #-}
_GTKSourceViewOptions_Readme_md
	 = unsafePerformIO $ readFile "src/Assets/GTKSourceViewOptions/Readme.md"

{-# NOINLINE _GTKSourceViewOptions_BoolOptions_txt #-}
_GTKSourceViewOptions_BoolOptions_txt
	 = unsafePerformIO $ readFile "src/Assets/GTKSourceViewOptions/BoolOptions.txt"

{-# NOINLINE _Test_CommonSubset_language #-}
_Test_CommonSubset_language
	 = unsafePerformIO $ readFile "src/Assets/Test/CommonSubset.language"

{-# NOINLINE _Test_examples_stfl #-}
_Test_examples_stfl
	 = unsafePerformIO $ readFile "src/Assets/Test/examples.stfl"

{-# NOINLINE _Test_Recursive_language #-}
_Test_Recursive_language
	 = unsafePerformIO $ readFile "src/Assets/Test/Recursive.language"

{-# NOINLINE _Test_FuncTypeErr_language #-}
_Test_FuncTypeErr_language
	 = unsafePerformIO $ readFile "src/Assets/Test/FuncTypeErr.language"

{-# NOINLINE _Test_LiveCheck_language #-}
_Test_LiveCheck_language
	 = unsafePerformIO $ readFile "src/Assets/Test/LiveCheck.language"

{-# NOINLINE _Test_DynamizeSTFL_language_changes #-}
_Test_DynamizeSTFL_language_changes
	 = unsafePerformIO $ readFile "src/Assets/Test/DynamizeSTFL.language-changes"

{-# NOINLINE _Test_GradualizeSTFL_language_changes #-}
_Test_GradualizeSTFL_language_changes
	 = unsafePerformIO $ readFile "src/Assets/Test/GradualizeSTFL.language-changes"

{-# NOINLINE _Test_STFL_language #-}
_Test_STFL_language
	 = unsafePerformIO $ readFile "src/Assets/Test/STFL.language"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__dlf__plain #-}
_IntegrationTests__log___Test_STFL_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__dlf__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_13_svg #-}
_IntegrationTests_Parsetrees_13_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_13.svg"

{-# NOINLINE _IntegrationTests_Parsetrees_12_svg #-}
_IntegrationTests_Parsetrees_12_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_12.svg"

{-# NOINLINE _IntegrationTests__Test_STFL_language__irdouble_pointdouble_point__plain #-}
_IntegrationTests__Test_STFL_language__irdouble_pointdouble_point__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__irdouble_pointdouble_point__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__ifaequate__plain #-}
_IntegrationTests__Test_STFL_language__ifaequate__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__ifaequate__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain #-}
_IntegrationTests__log___Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_6_svg #-}
_IntegrationTests_Parsetrees_6_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_6.svg"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l_rarrow__plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l_rarrow__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l_rarrow__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_1_svg #-}
_IntegrationTests_Parsetrees_1_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_1.svg"

{-# NOINLINE _IntegrationTests_Parsetrees_0_svg #-}
_IntegrationTests_Parsetrees_0_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_0.svg"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l_r____plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l_r____plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l_r____plain"

{-# NOINLINE _IntegrationTests_Parsetrees_9_svg #-}
_IntegrationTests_Parsetrees_9_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_9.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpProgress__plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpProgress__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__tpProgress__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_2_svg #-}
_IntegrationTests_Parsetrees_2_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_2.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__ifadom__plain #-}
_IntegrationTests__log___Test_STFL_language__ifadom__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__ifadom__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__ifadom__plain #-}
_IntegrationTests__Test_STFL_language__ifadom__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__ifadom__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__ifaeval__plain #-}
_IntegrationTests__Test_STFL_language__ifaeval__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__ifaeval__plain"

{-# NOINLINE _IntegrationTests__Test_CommonSubset_language__dlf__plain #-}
_IntegrationTests__Test_CommonSubset_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_CommonSubset_language__dlf__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_17_svg #-}
_IntegrationTests_Parsetrees_17_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_17.svg"

{-# NOINLINE _IntegrationTests__Test_STFL_language__dlf__plain #-}
_IntegrationTests__Test_STFL_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__dlf__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__irarrow__plain #-}
_IntegrationTests__log___Test_STFL_language__irarrow__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__irarrow__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_20_svg #-}
_IntegrationTests_Parsetrees_20_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_20.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__ifaequate__plain #-}
_IntegrationTests__log___Test_STFL_language__ifaequate__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__ifaequate__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__ir___plain #-}
_IntegrationTests__Test_STFL_language__ir___plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__ir___plain"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_23_svg #-}
_IntegrationTests_Parsetrees_23_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_23.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__plain #-}
_IntegrationTests__log___Test_STFL_language__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain"

{-# NOINLINE _IntegrationTests__log___Test_Recursive_language__dlf__plain #-}
_IntegrationTests__log___Test_Recursive_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_Recursive_language__dlf__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_4_svg #-}
_IntegrationTests_Parsetrees_4_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_4.svg"

{-# NOINLINE _IntegrationTests__Style_languageTerminal_stylestyleFile__html__plain #-}
_IntegrationTests__Style_languageTerminal_stylestyleFile__html__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Style_languageTerminal_stylestyleFile__html__plain"

{-# NOINLINE _IntegrationTests__log___Style_languageTerminal_stylestyleFile__html__plain #-}
_IntegrationTests__log___Style_languageTerminal_stylestyleFile__html__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Style_languageTerminal_stylestyleFile__html__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_14_svg #-}
_IntegrationTests_Parsetrees_14_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_14.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain #-}
_IntegrationTests__log___Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain #-}
_IntegrationTests__Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language_cTest_DynamizeSTFL_language_changes_cTest_GradualizeSTFL_language_changes__dlf__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__ia__plain #-}
_IntegrationTests__Test_STFL_language__ia__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__ia__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpa__plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpa__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__tpa__plain"

{-# NOINLINE _IntegrationTests__Test_FuncTypeErr_language__dlf__plain #-}
_IntegrationTests__Test_FuncTypeErr_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_FuncTypeErr_language__dlf__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__lsvgSyntax_svg__plain #-}
_IntegrationTests__log___Test_STFL_language__lsvgSyntax_svg__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__lsvgSyntax_svg__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__irEvalCtx__plain #-}
_IntegrationTests__Test_STFL_language__irEvalCtx__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__irEvalCtx__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain #-}
_IntegrationTests__Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language_cTest_DynamizeSTFL_language_changes__dlf__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__plain"

{-# NOINLINE _IntegrationTests__log___Test_TypeErrFunc_language__dlf__plain #-}
_IntegrationTests__log___Test_TypeErrFunc_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_TypeErrFunc_language__dlf__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__irEvalCtx__plain #-}
_IntegrationTests__log___Test_STFL_language__irEvalCtx__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__irEvalCtx__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_16_svg #-}
_IntegrationTests_Parsetrees_16_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_16.svg"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l_r___plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l_r___plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l_r___plain"

{-# NOINLINE _IntegrationTests_Parsetrees_22_svg #-}
_IntegrationTests_Parsetrees_22_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_22.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__ir___plain #-}
_IntegrationTests__log___Test_STFL_language__ir___plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__ir___plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__lsvgSyntax_svg__plain #-}
_IntegrationTests__Test_STFL_language__lsvgSyntax_svg__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__lsvgSyntax_svg__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_5_svg #-}
_IntegrationTests_Parsetrees_5_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_5.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__ptsvgParsetrees__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__irarrowstart__plain #-}
_IntegrationTests__log___Test_STFL_language__irarrowstart__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__irarrowstart__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__ifacod__plain #-}
_IntegrationTests__log___Test_STFL_language__ifacod__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__ifacod__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpa__plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpa__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__tpa__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_rarrow__plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_rarrow__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l_rarrow__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__ir____plain #-}
_IntegrationTests__log___Test_STFL_language__ir____plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__ir____plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__ia__plain #-}
_IntegrationTests__log___Test_STFL_language__ia__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__ia__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain"

{-# NOINLINE _IntegrationTests__log___Test_CommonSubset_language__dlf__plain #-}
_IntegrationTests__log___Test_CommonSubset_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_CommonSubset_language__dlf__plain"

{-# NOINLINE _IntegrationTests__Test_Recursive_language__dlf__plain #-}
_IntegrationTests__Test_Recursive_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_Recursive_language__dlf__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__ir____plain #-}
_IntegrationTests__Test_STFL_language__ir____plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__ir____plain"

{-# NOINLINE _IntegrationTests_Parsetrees_11_svg #-}
_IntegrationTests_Parsetrees_11_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_11.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_8_svg #-}
_IntegrationTests_Parsetrees_8_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_8.svg"

{-# NOINLINE _IntegrationTests_Syntax_svg #-}
_IntegrationTests_Syntax_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Syntax.svg"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l_rdouble_pointdouble_point__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__tpProgress__ppp__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l__tpa__ppp__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_10_svg #-}
_IntegrationTests_Parsetrees_10_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_10.svg"

{-# NOINLINE _IntegrationTests__Test_STFL_language__irarrow__plain #-}
_IntegrationTests__Test_STFL_language__irarrow__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__irarrow__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__irarrowstart__plain #-}
_IntegrationTests__Test_STFL_language__irarrowstart__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__irarrowstart__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_18_svg #-}
_IntegrationTests_Parsetrees_18_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_18.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__ifaeval__plain #-}
_IntegrationTests__log___Test_STFL_language__ifaeval__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__ifaeval__plain"

{-# NOINLINE _IntegrationTests__Test_STFL_language__plain #-}
_IntegrationTests__Test_STFL_language__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__plain"

{-# NOINLINE _IntegrationTests__Test_TypeErrFunc_language__dlf__plain #-}
_IntegrationTests__Test_TypeErrFunc_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_TypeErrFunc_language__dlf__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_3_svg #-}
_IntegrationTests_Parsetrees_3_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_3.svg"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_r___plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_r___plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l_r___plain"

{-# NOINLINE _IntegrationTests_Parsetrees_15_svg #-}
_IntegrationTests_Parsetrees_15_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_15.svg"

{-# NOINLINE _IntegrationTests__Test_STFL_language__ifacod__plain #-}
_IntegrationTests__Test_STFL_language__ifacod__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_language__ifacod__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_language__irdouble_pointdouble_point__plain #-}
_IntegrationTests__log___Test_STFL_language__irdouble_pointdouble_point__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_language__irdouble_pointdouble_point__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_7_svg #-}
_IntegrationTests_Parsetrees_7_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_7.svg"

{-# NOINLINE _IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpProgress__plain #-}
_IntegrationTests__Test_STFL_languageTest_examples_stfle_l__tpProgress__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_Test_STFL_languageTest_examples_stfle_l__tpProgress__plain"

{-# NOINLINE _IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_r____plain #-}
_IntegrationTests__log___Test_STFL_languageTest_examples_stfle_l_r____plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_STFL_languageTest_examples_stfle_l_r____plain"

{-# NOINLINE _IntegrationTests__log___Test_FuncTypeErr_language__dlf__plain #-}
_IntegrationTests__log___Test_FuncTypeErr_language__dlf__plain
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/_log___Test_FuncTypeErr_language__dlf__plain"

{-# NOINLINE _IntegrationTests_Parsetrees_21_svg #-}
_IntegrationTests_Parsetrees_21_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_21.svg"

{-# NOINLINE _IntegrationTests_Parsetrees_19_svg #-}
_IntegrationTests_Parsetrees_19_svg
	 = unsafePerformIO $ readFile "src/Assets/IntegrationTests/Parsetrees_19.svg"
