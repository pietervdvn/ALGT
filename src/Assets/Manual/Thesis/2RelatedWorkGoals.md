
 Related Work and Goals
========================

As _Programming Language Design_ starts to take of as a major field within computer sciences, tools are surfacing to formally define programming languages. But what should the ideal tool do, if we were to create it? And what does already exist?



Goals and nongoals
------------------

The ultimate goal should be to create a common language for language design, as this would increase formalization of language design. To gain widespread adoption, there should be as little barriers as possible, in installation, usage and documentation. It should be easy for a newcomer to use, without hindering the expressive power or available tools used by the expert.

### Tooling

Practical aspects are important - even the greatest tools lose users over these unecassry barriers.

The first potential barrier is **installation** - which should be as smooth as possible. New users are easily scared by a difficult installation process, fleeing to other tools hindering adoption. 
Preferably, the tool should be availabe in the package repos. If not, installation should be as easy as downloading and running a single binary. Dependencies should be avoided, as these are often hard to deploy on the dev machine - as they might be hard to get, to install, having version conflicts with other tools on the machine, not being supported on the operating system of choice...

The second important feature is **documentation**. Documentation should be easy to find, and preferably be distrubeted alongside the binary.

Thirdly, we'll also want to be **cross-platform**.
While most of the PL community uses a Unix-machine, other widely used, non-free operating systems should be supported as well.

As last, extra features like **syntax highlighting**, **automated tests** or having editor support for the target language is a nice touch.


### Metalanguage

The most important part is the metalangue itself - as that is the major interface the language designer will use.

As language design itself is already difficult to grasp, we want our language to be **easy**. There are some aspects which help to achieve this.

An easy language should be:

- as **focused** as possible, with little boilerplate. The core concepts of language design should have a central place.
- **expressive** enough to be usefull
- as **simple** as possible, thus having as little elements and special constructs which should be learned and considered when doing automatic translations.
- **checked** as much as possible for big and small errors and report these errors with a clear error message.

#### Embedded in another programming language?

Should the tool be designed as library or domain specific language embedded in anohter programming language? Or should a totally new programming language be created?

Using an embedded language gives us a headstart, as all of the builtin functionality and optimazations can be used. However, The toll later on the road is high. Starting with a fresh language has quite some benefits:

First, the user does not have to deal with the host language at all. The user is forced to make the choice between learning the new programming language - which is quite an investment- or ignoring the native bits, and never having a full grasp over the definition. 

Second, by creating a fresh language, this language can be streamlined on what is needed - it is easy to cut out any boilerplate, making the language more fun to use.

By not using a host language, analysises on metafunctions become possible, as the metalanguage is small and well understood. This would be hard to do in a host language, where compilers span over 100'000 lines of code. 

As last, we don't have to deal with installing a host compiler, skipping another dependency. 

### Parsing the target language

The first step in defining our target language is declaring its syntax. The standard practice to do so has been _BNF_ since its introduction in the ALGOL-report in 1960. As BNF is the de facto standard, it is already well-known to language designers and thus both the theoritical and practical aspects are well understood.
Furthermore, it is easy to port existing languages, as often a BNF is already available for this language. 

A drawback is that many variants of BNF exist, each with their own superficial syntactic differences. This is only a minor drawback though: as the underlying structure is the same, a simple search-and-replace can easily convert one dialect into another. On the other hand, we want the BNF syntax to be as light and boilerplate-free as possible, eventually introducing a new dialect.

No other lower-level details should be exposed to the language designer, such as tokenizing or the internal definition of the parsetree - the language designer should be working directly with the parsetree.


### Executing the target language

Of course, we'll want to execute our target programming language in one way or another. What are important aspects? As this is a tool to develop and prototype programming language, we're firstly concerned with debugging the programming language, no matter how the runtime semantics were declared.

- **Immediate feedback**: when starting the program, we want to see output as soon as possible
- **Write once, run anywhere**: running target programs should behave the same on all systems
- **traceable**: it should be possible to see how program execution went exactly, step by step, as to easily debug.

While compilation to a target architecture and target program optimazations are nice, they are not priorities - as it only complicates the implementation.


### Typechecking the target language

Just like the operational semantics, we should be able to build and run a typechecker for the target language. Here, the same constraints apply. We should strive to make the declaration of the typesystem consistent with the operational semantics, preferably it should use the same kind of logic.

We should also strive to **automatically test** the correctness of the typechecker, in an automated way, in conjunction with the operational semantics.


Related Work
------------

With these requirements in mind, we investigate what tools already exist and how these tools evolved.


### Yacc

__Yacc__ (Yet Another Compiler Compiler), designed in 1975 was the first tool designed to automatically generate parsers from a given _BNF_-syntax. Depending on the parsed rule, a certain action could be specified - such as constructing a parse tree. 

It was a major step to formally define the syntax of a programming language, but carries a clear legacy of its inception era: you are supposed to include raw `c`-statements, compile to `c` and then compile the generated `c`-code. Furthermore, lexing and parsing are two different steps, requiring two different declarations. Thus, quite some low-level work is needed.

Furthermore, only the parser itself is generated, the parsetree itself should be designed by the language designer.

As this was the first widely available tool for this purpose, it has been tremendously popular, specifically within unix systems, albeit as reimplementation  _GNU bison_. Implementations in other programming languages are widely available. Yacc gets an honorable mention here for its historical importance, but is outdated.


### ANTLR

__ANTLR__ (ANother Tool for Language Recognition) is a more modern _parser generator_. This tool has been widely used as well, as it is compatible with many programming languages, such as Java, C#, Javascript, Python, ...

Just as _YACC_, for each rule defined, a programmer specified action is performed; most often the construction of a parsetree.

_ANTLR_ is used to parse the syntax of projects as Groovy, Jython, Hibernate, OpenJDK Compiler Grammer Project, Twitter's search query language, Cassandra and Processing.

An example grammer is:


	options {
		language="Cpp";
	}

	class MyExprParser extends Parser;

	options {
		k = 2;
		exportVocab=MyExpr;
		buildAST = true;
	}


	exprlist
	  : ( assignment_statement )* EOF!
	  ;

	assignment_statement
	  : assignment SEMICOLON!
	  ;

	assignment
	  : (IDENT ASSIGN )? expr
	  ;

	primary_expr
	  : IDENT 
	  | constant 
	  | (LPAREN! expr RPAREN! ) 
	  ;

	sign_expr
	  : (MINUS)? primary_expr
	  ;

	mul_expr
	  : sign_expr (( TIMES | DIVIDE | MOD ) sign_expr)*
	  ;

	expr
	  : mul_expr (( PLUS | MINUS ) mul_expr)*
	  ;

	constant
	  : (ICON | CHCON)
	  ;


%% TODO Source: http://www.bearcave.com/software/antlr/antlr_examples.html

### XText

__XText__ is a modern tool to define grammers and associated tooling support. It is heavily tied-in into the Java Virtual Machine, as grammers are compiled to _Java Artifacts_.
https://www.eclipse.org/Xtext/documentation/102_domainmodelwalkthrough.html
_XText_ focuses majorly on tooling support. Once a language is defined, an plugin for the _Eclipse IDE_ can give code suggestions, syntax highlighting, hover information, ...

This is usefull for day to day programming, but not as usefull for formal language design.

It might be noted that _XText_ uses _ANTLR_ for parsetree generation.


	grammar org.example.domainmodel.Domainmodel with
				org.eclipse.xtext.common.Terminals
	 
	generate domainmodel "http://www.example.org/model/Domainmodel"
	 
	Domainmodel :
		(elements+=Type)*;
	  
	Type:
		DataType | Entity;
	  
	DataType:
		'datatype' name=ID;
	 
	Entity:
		'entity' name=ID ('extends' superType=[Entity])? '{'
		(features+=Feature)*
		'}';
	 
	Feature:
		(many?='many')? name=ID ':' type=[Type];

%%SOURCE: https://www.eclipse.org/Xtext/documentation/102_domainmodelwalkthrough.html

### LLVM

__LLVM__ focusses mainly on the technical aspect of running programs on specific, real world machines. It contains an excellent intermediate _intermediate representation_ of imperative programs, which can be optimized and compiled for all major computer architectures. LLVM is thus an excellent compiler backend.

As it focuses on the backend, _LLVM_ is less suited for easily defining a programming language and thus for researching Language Design. As seen in [their own tutorial](http://llvm.org/docs/tutorial/LangImpl02.html#full-code-listing), declaring a parser for a simple programming language takes _nearly 500 lines_ of imperative C-code.

LLVM is not usefull as tool to design programming languages.

_LLVM_ is thus a production tool, made to compile day-to-day programming languages in an efficient way. It would usefull to hook this as backend to _ALGT_, as to further automate the process of creating programming languages. This is however out of scope for this master dissertation.


	define i32 @mul_add(i32 %x, i32 %y, i32 %z) {
	entry:
	  %tmp = mul i32 %x, %y
	  %tmp2 = add i32 %tmp, %z
	  ret i32 %tmp2
	}

%% TODO Source http://releases.llvm.org/2.6/docs/tutorial/JITTutorial1.html




PLT-Redex
---------

__PLT-Redex__ is a DSL implemented in Racket. It allows the declaration of a syntax as BNF and the definition of arbitrary relations, such as reduction or typing. _PLT-REdex_ also features an automated checker, which generates random examples and tests arbitrary properties on them.

In other words, _PLT-redex_ is another major step to formally define languages and was thus a major inspiration to ALGT.

As __PLT-Redex__ is a DSL, it assumes knowledge of the host language, _Racket_ . On one hand, it is easy to escape to the host language and use features otherwise not available. On the other hand, this is a practical barrier to aspiring Designers and hobbyists. A new language has to be learned -Racket is far from popular- and installed, which brings its own problems.

Furthermore, by allowing specification parts to be a full-fledged programming language, it hinders automatic reasoning about several aspects of the definition.

Thirdly, being a DSL brings syntax overhead of the host language. A fresh programming language, specifically for this task, allows to focus on a clean and to the point syntax.


	#lang racket
	(require redex)

	(define-language L
	  (e (e e)
	     (λ (x t) e)
	     x
	     (amb e ...)
	     number
	     (+ e ...)
	     (if0 e e e)
	     (fix e))
	  (t (→ t t) num)
	  (x variable-not-otherwise-mentioned))

%% TODO Source of the example


 MAUDE
-------

__Maude System__ is a high-level programming language based on rewriting and equational logic. It allows a broad range of applications, in a logic-programming driven way. It might be used as a tool to get explore the semantics of programming, it does not meet our needs to easily define programming languages - notably because a lot of overhead is introduced in the tool, both cognitive and syntactic. 


	 fmod NAT is
	   sort Nat .

	   op 0 : -> Nat [ctor] .
	   op s : Nat -> Nat [ctor] .
	 endfm

%% source: wikipedia

ALGT
-----

__ALGT__, which we present in this dissertation, tries to be a generic _compiler front-end_ for arbitrary languages. It should be easy to set up and use, for both hobbyists wanting to create a language and academic researchers trying to create a formally correct language. 

_ALGT_ should handle *all* aspects of Programming Language Design, which is the Syntax, the runtime semantics, the typechecker (if wanted) and the associated properties (such as _Progress_ and _Preservation_) with automatic tests.

By defining runtime semantics, an interpreter is automatically defined and operational as well. This means that no additional effort has to be done to immediatly _run_ a target program.

To maximize ease of use, a build consists of a single binary, containing all that is needed, including the tutorial and Manual.

_ALGT_ is written entirely in Haskell. However, the user of ALGT does not have to leave the _ALGT_-language for any task, so no knowledge of Haskell is needed.

It can be easily extended with additional features. Some of these are already added, such as automatic syntax highlighting, rendering of parsetrees as HTML and LaTeX; but also more advanced features, such as calculation of which syntactic forms are applicable to certain rules or totality and liveability checks of meta functions.


Feature comparison
------------------

