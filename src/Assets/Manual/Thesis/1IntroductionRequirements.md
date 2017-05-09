
 Building and Gradualizing programming languages
=================================================


Computers are complicated machines. A modern CPU (anno 2017) contains around _2 billion_ transistors and flips states around _4 billion_ times a second. Controlling these machines is hard; controlling them with low-level assembly has been an impossible task for decades. Luckily, higher level programming languages have been created to ease this task.


However, creating such programming languages is a hard task too. Aside from the technical details of executing a language on a specific machine, languages should be formally correct and strive to minimize errors made by the human programmer, preferably without hindering creating usefull programs. This is a huge task; several approaches to solve this complex problem have been tried, all with their own trade-offs - such as usage of typecheckers, amongst other choices. Another hindrance is the lack of common jargon and tools supporting programming language design. 


 Representing arbitrary semantics
----------------------------------

_Program Language Design_ is a vast and intriguing field. As this field starts to mature, a common jargon is starting to emerge among researchers to formally pin down programming languages and concepts. This process was started by John Backus Naur in 1963, by introducing _BNF_, where the __syntax__ of a language can be formally declared. Due to its simplicity and ease to use, it has become a standard tool for any language designer and has been used throughout of the field of computer science.

Sadly, no such formal language is availabe to reason about the __semantics__ of a programming language. Researchers often use _natural deduction_ to denote semantics, but in an informal way. We crystallize this by introducing a tool which allows the direct input of such rules -allowing manipulation directly on the parsetrees- giving rise to __parsetree oriented programming__ and providing an intuitive interface to formally create programming languages, reason about them and execute them.

By explicitly stating the semantics of a programming language as formal rules, these rules can be automatically transformed and programming languages can be automatically changed. 


In this master dissertation, we present a tool which:

 - Allows an easy notation for both the syntax and semantics of arbitrary programming languages
 - Which interprets these languages
 - Provides ways to automatically reason about certain aspects and properties of the semantics
 - And helps rewriting parts of the typesystem to gradualize them


The tool should help with easily creating and testing programming languages; it should help analyzing the various choices that can be made.


 Static versus dynamic languages
---------------------------------

One of those choices that programming languages make, is wheter a typechecker is used or not. 

For example, consider the erronous expression `0.5 + True`. 

A programming language with __static typing__, such as Java, will point out this error to developer, even before running the program. A dynamic programming language, such as Python, will happily start executing of the program, only crashing when it attempts to calculate the value.

This dynamic behaviour can cause bugs to go by undedected for a long time. For example, a bug is hidden in the following Python snippet. Can you spot it [^pythonBug]?


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


The typechecker thus gives a lot of guarantees about your code, even before running a single line of it. The strongest guarantee the typechecker gives is that code will not crash due to type errors. Furthermore, having precise type information gives other benefits, such as compiler optimizations, code suggestions, ease of refactoring, ...

However, this typechecker has a cost to the programmer. First, types should be stated explicitly and slows down programming. Second, some valid programs can't be written anymore. While typechecking is a good tool in the long run to maintain larger programs, it is a burden when creating small programs.

Per result, programs often start their life as a small _proof of concept_ in a dynamic language. When more features are requested, the program steadily grows beyond the point it can do without static typechecker - but when it's already to cumbersome and expensive to rewrite it in a statically typed language.

 Gradual typesystems
----------------------


However, static or dynamic typing shouldn't be a binary choice. By using a _gradual_ type system, some parts of the code might be statically typed - giving all the guarantees and checks of a static programming language; while other parts can dynamically typed - giving more freedom and speed to development. A program where all type annotations are given will offer the same guarantees as a static language, a program without type annotations is just as free as a dynamic program.
This means that the developer has the best of both worlds and can migrate the codebase either way as needed:

	
	// Here we type statically
	List<Integer> list = new ArrayList<>()
	if(someRareCondition){
		// Error: Type mismatch: cannot convert from void to List
		list = list.sort(intComparator);
	}

	// Here, we work dynamically
	? x	= list.get(0)
	
	x	= "Some string"
	System.out.println(x + True)
	

Very little gradual programming languages exist - for an obvious reason: creating a gradual type system is a hard.

%% TODO bibliography

Gradual typing is a new research domain. It is not widly known nor well understood. Based on the paper of Ronald Garcia, __Abstracting Gradual Typing__, we attempt to _automate gradual typing_ of arbitrary programming languages, based on the tool above.




