
Appendixes
==========


STFL-sourcecode
---------------

$$$stfl.language![1..7],[14..15],9,[19..33],38,39,[61..73],[105..108],112,116,118,120,[123..161],[188..268]!file


GradualizeSTFL.language-changes
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


\clearpage

Some informal thoughts about the Futamura projections
------------------------------------------------------

In constellation with a Haskell compiler is ALGT a universal compiler. This is a result of the Futamura-projections \cite{FutY:1983}: ALGT functions as the universal interpreter, whereas GHC functions as the specializer.

Simply put, the ALGT binary takes three inputs: the program language description, the target program and the input for this program[^noInput]. ALGT acts as interpreter yielding the output of the program:

	algt	:: Language -> Program -> Input -> Output

However, GHC can compile the ALGT-sourcecode together with the language definition and target program embedded at compiletime - the equivalent of a partial application of ALGT. This yields an executable, only containing the target program:

	compile (algt someLanguage someProgram) :: Executable (Input -> Output)


If a description of the ALGT-metalanguage is given to ALGT itself, the program needs to be a `.language`, the input should be a program, giving a function as output; this partial application yields exactly the same types as the `algt`-function:

	algt algtLanguage	:: Language  {- The 'Program' -}
					-> Program  {- The 'Input' -}
					-> (Input -> Output) {- The 'Output' -}


This matching type signature is no coincidence, as `algt algtLanguage` is exactly the `algt`-function itself, showcasing the power of ALGT as tool to create arbitrary programming languages and even compile programs with fast runtime characteristics.
	
[^noInput]: No input/output system exists withing ALGT for the moment, so target programs are totally isolated from the outside. As workaround, input should be embedded within the program. This is but a technical problem without consequences for the reasoning above. 
