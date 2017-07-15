
Appendixes
==========


STFL-sourcecode
---------------

$$$stfl.language![1..7],[14..15],9,[19..33],38,39,[61..73],[105..108],112,116,118,120,[123..161],[188..268]!file


GradualingSTFL.language-changes
--------------------------------

$$$GradualizeSTFL.language-changes!file


GTFL
----

The result of applying GradualingSTFL.language-changes on STFL


$$$!($$$stfl.language -c $$$GradualizeSTFL.language-changes --dlf --nc)!file



Analysis of the domain-function
--------------------------------

For reference, the full analysis of the domain function: 

$$$($$$stfl.language --ifa dom  --nc)


Some informal thoughts about the Futamura projections
------------------------------------------------------

ALGT is, in constellation with a Haskell compiler, a universal compiler, as stated by the Futamura-projections \cite{FutY:1983}: ALGT functions as universal interpreter, whereas GHC functions as specializer.

Simply put, the ALGT binary takes three inputs: the program language description, the target program and the input for this program[^noInput] and acts as interpreter yielding the output of the program.

	algt	:: Language -> Program -> Input -> Output

However, GHC can compile the ALGT-sourcecode, with the language definition and target program embedded at compiletime (which could be regarded as partial application). This yields an executable of the target program only:

	compile (algt someLanguage someProgram) :: Executable (Input -> Output)


If a description of the ALGT-metalanguage should be given to ALGT, the program should be a `.language`; yielding the following types, exactly the same as the `algt`-function. The matching type signature is no coincidence, as `algt algtLanguage` is exactly the `algt`-function. 

	algt algtLanguage	:: Language  {- The 'program' -}
					-> Program  {- The 'input' -}
					-> (Input -> Output) {- The 'Output' -}


	


[^noInput]: No input/output system exists for the moment, so target programs are totally isolated from the outside. As workaround, input should be embedded within the program. This is but a technical problem without consequences for the reasoning above. 
