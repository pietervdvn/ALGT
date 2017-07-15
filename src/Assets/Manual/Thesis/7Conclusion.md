
ALGT as tool for language design
=================================

In this master dissertation we presented ALGT; a tool which tries to solve two problems:

- ALGT helps with the creation of arbitrary programming languages, from syntax to semantics.
- ALGT aims to automate gradualization of the typesystem.

 Representing arbitrary programming languages
----------------------------------------------

The first problem tackled is the problem of language design, especially the lack of a tool that mechanically checks and executes language specifications. 
The presented tool has a powerfull metalanguage, which allows all aspects of a programming language to be described in a high-level way. All aspects of formal language design are covered by the tool:

- The syntax is given as BNF
- The language semantics can be given in a framework of choice
- Properties can be stated and tested

Based on this input, an interpreter for the language can be constructed fully automatically: 

- a parser is constructed automatically (see section \ref{target-language-parsing});
- the parsed program (together with the input) can be automatically interpreted, by proving that its reduction relation holds for the given program. This proof contains the execution of the program as side effect, including the output.

With all these steps automated, language prototyping becomes easy. The two essential ingredients are declared easily, after which it is possible to test and execute them.

We tried to strike the right balance between the formal and lightweight tools:

- The language itself contains enough features to be practical, yet not too many, which might hinder formal proving of some properties. 
- All the tools available are modeled after well-known mathematical objects, such as BNF to capture the syntax and natural deduction to capture semantics and typecheckers
- The semantics can be mechanically tested, increasing the formality and practicality of the tool
- The language specification is tested for many common bugs, by the typechecker, liveness- and completenesschecker, ...


In order to evaluate the metalanguage, STFL was created within the tool, resulting in a clean and readable specification - as can be seen in chapter \ref{algt-in-a-nutshell}. This specification is successfully used as interpreter, resulting in a working programming language.

A workshop was given to members of Zeus WPI, where uniniated members gave their try to create a programming language. Most were able to create the syntax of their language, but as none were familiar with natural deduction, implementation of the semantics was somewhat difficult.

In conclusion, ALGT is suited for formal language development, with plenty tools to assist the designer: running and testing the language, catching bugs, ...


 Gradualization
----------------

The other major goal of ALGT is to assist in the gradualization of a typesystem - where the typesystem of a language is modified so that some parts are typed and some are not.

As seen in section \ref{simplifying-the-runtime}, the components needed are:

- the language syntax;
- a runtime supporting dynamic features;
- a gradual typesystem.

Especially gradualization of the typechecker is automated. As noted in section \ref{gradualizing-domain-and-codomain}, the metafunctions over types should suddenly work on _sets of types_. For this, the necessary framework -abstract interpretation- is constructed and implemented (chapter \ref{syntax-driven-abstract-interpretation}), which gives guidance on how to implement the gradual counterparts of metafunctions. As a bonus are some extra checks for general metafunctions possible with the framework, as described in section \ref{combining-clauses}.

The needed function modifications can be passed into a file describing a refactoring of the language, automating the boring and error-prone changes. 

To evaluate this approach, STFL was gradualized in section \ref{automating-gradualization}, yielding GTFL. This gradualization was successfull, although some care should be given to the exact implementation of the dynamic runtime.

We might thus conclude that ALGT can also assist in the gradualization of programming languages, by having built-in support for abstract interpretation and refactoring. 

