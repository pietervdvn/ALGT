
 Building and Gradualizing programming languages
=================================================


Computers are complicated machines. A modern CPU (anno 2017) contains over _2 billion_ transistors and flips states over _3 billion_ times a second \cite{Intel}. Controlling these machines is hard; controlling them with low-level assembly has been an impossible task for decades. Luckily, higher level programming languages have been created to ease this task.


However, creating such programming languages is a hard task too. Aside from the technical aspect of executing a program in such a language on a specific machine, languages should be formally correct and strive to minimize errors made by the human programmer, preferably without hindering creating usefull programs. This is a huge task; several approaches to solve this complex problem have been tried, all with their own trade-offs - such as usage of typecheckers, amongst other choices.
Another hindrance for the programming language field is the lack of common jargon and tools supporting programming language design. 


 Representing arbitrary semantics
----------------------------------

_Program Language Design_ is a vast and intriguing field. As this field starts to mature, a common jargon is starting to emerge among researchers to formally pin down programming languages and concepts. This process was started by John Backus and Peter Naur in 1960, by introducing _BNF_ in the famous ALGOL60 report \cite{BackusNaur}, where the __syntax__ of the ALGOL60 language was formally specified. Due to its simplicity and ease to use, BNF has become a standard tool for any language designer and has been used throughout of the field of computer science.

The following breakthrough on formalization of program languages was made in 1968. The ALGOL68-report \cite{ALGOL68} was the first to give a full formal definition of the semantics of the ALGOL-language, pioneering the mathematical framework that is known today as _operational semantics_.

Another mathematical framework to describe semantics was introduced a year later by C.A.R. Hoare \cite{HoareAxiomatic}, in which he introduces an __axiomatic framework__ to describe programming languages.

The last approach capturing semantics still widely used today was introduced in 1971 by Dana Scott and Christopher Strachey. They introduce the technique to translate the program to a mathematical object (such as a function, transforming the input to the output), giving rise to __denotational semantics__ \cite{ScottStrachey}.

Sadly, little tools are availabe that reason about the semantics of a programming language in a mechanized way. Researchers often use one of the above frameworks to denote semantics, but in an informal way: the semantics are denoted in \LaTeX for publication - often typeset as a natural deduction rule. This is error-prone, as no mechanical checks are performed on these rules. 

This is changing lately, as programming language researchers are starting to two categories of tools to automate this process. On one hand, theorem provers are used to automate the reasoning about the semantics: tools such as COQ and Isabelle are gaining popularity in the field, checking correctness proofs of the languages. 
 On the other hand are lightweight tools, optimized for language design. These tools help with typesetting, translation to the theorem provers or other smaller tasks, in a simple and easy metalanguage.


 Static versus dynamic languages
---------------------------------

A tradeoff that all programming languages make is the choice between static and dynamic typing, thus whether a typechecker is used or not. 

For example, consider the erronous expression `0.5 + True`. 

A programming language with __static typing__, such as Java, will point out this error to the developer, even before running the program. A dynamic programming language, such as Python, will happily start executing the program, only crashing when it attempts to calculate the value.

This dynamic behaviour can cause bugs to go undedected for a long time, such as the bug hidden in the following Python snippet. Can you spot it [^pythonBug]?


	if some_rare_condition:
		list = list.sort()
	x	= list[0]


[^pythonBug]: `list.sort()` will sort the list in memory and return `void`. `list = list.sort()` thus results in `list` being `void`. The correct code is either `list = list.sorted()` or `list.sort()` (without assignment).

Python will happily execute this snippet, until `some_rare_condition` is met and the bug is triggered - perhaps after months in production.

Java, on the other hand, will quickly surface this bug with a compiler error and even refuse to start the code altogether:


	List<Integer> list = ...
	if(someRareCondition){
		// Error: Type mismatch: cannot convert from void to List
		list = list.sort(intComparator);
	}
	int x	= list.get(0)


The strongest guarantee the typechecker gives is that code will not crash due to type errors. Furthermore, having precise type information gives other benefits, such as compiler optimizations, code suggestions, ease of refactoring, ...

However, this typechecker has a cost to the programmer. First, types should be stated explicitly and slows down programming. Second, some valid programs can't be written anymore. While typechecking is a good tool in the long run to maintain large programs, it is a burden when creating small programs.

Per result, programs often start their life as a small _proof of concept_ in a dynamic language. When more features are requested, the program steadily grows beyond the point it can do without static typechecker - but when it's already to cumbersome and expensive to rewrite it in a statically typed language.

 Gradual typesystems
----------------------


However, static or dynamic typing shouldn't be a binary choice. By using a _gradual_ type system, some parts of the code might be statically typed - giving all the guarantees and checks of a static programming language; while other parts can dynamically typed - giving more freedom and speed to development. A program where all type annotations are given will offer the same guarantees as a static language, a program without type annotations is just as free as a dynamic program.
This means that the developer has the best of both worlds and can migrate the codebase either way as needed:

	
	// This is statically typed
	List<Integer> list = new ArrayList<>()
	if(someRareCondition){
		// Error: Type mismatch: cannot convert from void to List
		list = list.sort(intComparator);
	}

	// This is dynamic
	? x	= list.get(0)
	
	x	= "Some string"
	System.out.println(x + True)
	

Very little gradual programming languages exist, because creating a gradual type system is a hard.

Gradual typing is a new research domain, not widely known nor well understood. Based on the paper of Ronald Garcia, __Abstracting Gradual Typing__ \cite{GarciaAGT}, we attempt to _automate gradual typing_ of arbitrary programming languages, based on the tool above.


Our contribution
----------------

In this master dissertaion, we try to solve both problems at once. First, we propose a tool which allows for a lightweight denotation of arbitrary programming languages, capturing both the syntax and semantics of any chosen language. The metalanguage is optimized for ease of use, without compromising the formal correctness.

This information is in turn used to build both an parser and interpreter - offering the possibility to execute the programming language based on the specification.
On top of that, properties can be stated and automatically tested within the tool.

At last, we aim to assist in the gradualization of the given programming language. By offering a symbolic, analytical framework, large parts of gradualizing the language can be automated.

