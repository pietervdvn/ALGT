
ALGT as tool for Language Design
=================================

In this concluding chapter, we reflect on ALGT and its position next to other tools in the language design community. We also reflect on the difficulties of gradual typing and the advancements made.

Context
-------

Recall that two typing approaches exist in day-to-day programming: static and dynamic typing.  

Static typing checks the program rigorously before executing the program for possible typing errors, whereas the dynamic approach crashes at runtime on such faulty expressions.

As is clear from the Tiobe Index \cite{TiobeIndex}, static programming languages are the most popular as the top 4 is filled with the static languages -_Java_, _C_, _C++_ and _C#_. Nontheless, dynamic languages have their fair share in the top ten as well, as four spots are filled by the dynamic languages _Python_, _PHP_, _Javascript_ and _Perl_.

Hybrid systems, such as gradual typing, are starting to emerge as these offer the benefits of both approaches. With gradual typing, programmers can choose, with a very fine grain, which parts of the code are statically or dynamically typed. On a larger timeframe can codebases be migrated from one approach to the other, depending on the needs of the program.

Some dynamic languages have an experimental, gradual typechecker available. Notable examples are MyPy for the Python language\cite{mypy} or TypeScript, a superset with optional typechecker for Javascript\cite{TypeScript}. Little is available, due to a lack of tools designing gradual programming languages.


### Related work

As seen in section \ref{related-work}, some tools exist within the sphere of language design. Especially _PLT-redex_ is a mature and advanced tool for language design, but has no support for gradualization. On the other hand, one highly specialized tool exists: _The Gradualizer_ by Cimini. This tool gradualizes some languages, but is difficult to use due to lack of documentation and an arcane input method.

In conclusion, no tool exists which allows both the easy creation of a programming language and supports gradualization afterwards.

Main contribution
-----------------


To fill this niche, a new tool ALGT was created. ALGT has a powerfull metalanguage for creating arbitrary programming languages. Secondly does ALGT help with the gradualization of a programming language.

### Tool for language design

The first problem tackled by ALGT is the problem of easy and formal language design. The presented tool has a powerfull metalanguage, which allows all aspects of a programming language to be described in a high-level way. In order to evaluate the metalanguage, STFL was created within the tool, resulting in a clean and readable specification - as can be seen in chapter \ref{algt-in-a-nutshell}.

The syntax specification for a language can be given in the well-known _BNF_ notation. 
The sytnax of STFL was constructed in BNF, as explained in section \ref{syntax}, resulting in a small and clean implementation.

This syntax definition is reused as data structure, on which transformations are based. These transformations can be given in two practical styles, one based on functions and one based on natural deduction.

The functional style consists of straightforward functions with little overhead. This functional approach is an excellent tool to create many smaller helper functions,  as explained in \ref{metafunctions}.

The other style for transformations are in the form of relations, where the implementation is given by one or more natural deduction rules; as explained in \ref{natural-deduction}. While having a little overhead, the natural deduction rules exhibit search behaviour making them an excellent tool to implement typecheckers or semantics. 


Using this natural deduction style, semantics are given for STFL. Although any mathematical framework as described \ref{describing-semantics} is available, operational semantics were chosen over axiomatic or denotational semantics. Operational semantics are given for STFL in \ref{defining-smallstep}, again yielding a small and elegant specification.

A typechecker is constructed with the same tools in \ref{typechecker}. Mirroring the operational semantics, the typechecker is small and elegant as well.

At last, important properties for the language can be stated and tested automatically as natural deduction rules. For STFL, this consisted of Progress and Preservation. Together with many of the other checks, this offers strong guarantees about the correctness of the designed language.

\medskip

Based on the specification, an interpreter for the target language is fully automatically constructed. The BNF is enough to parse a target language creating an AST without needing any interaction of the language designer, as described in \ref{target-language-parsing}.

This AST is in turn transformed by a relation of choice. If a typechecking-relation is available, the target program can be typechecked; if semantics are availabe, the target program can be executed by proving that its reduction relation holds. The proof contains the execution trace and end result of the program as side effect, giving the output of the program.

With all these steps automated, language prototyping becomes easy. The two essential ingredients are declared easily and elegantly; using the automatic interpreter executing and testing the language is possible.

\medskip

We tried to strike the right balance between the formal and lightweight tools.

- The language itself contains enough features to be practical, yet not too many, which might hinder formal proving of some properties. 
- All the tools available are modeled after well-known mathematical objects, such as BNF to capture the syntax and natural deduction to capture semantics and typecheckers
- The semantics can be mechanically tested, increasing the formality and practicality of the tool
- A language specification is tested for many common bugs, by the typechecker, liveness- and completenesschecker, ...

A workshop was given to members of Zeus WPI, where uniniated programmers tried to create a programming language. Most were able to create the syntax of their language, but as none were familiar with natural deduction, implementation of the semantics was somewhat difficult.

In conclusion, ALGT is well suited for formal language development, with plenty tools to assist the designer: running and testing the language, catching bugs, ...


### Tool for gradualization


The other major goal of ALGT is to assist and automate the gradualization of a typesystem. As seen in section \ref{simplifying-the-runtime}, there are three components needed for gradualization:

- the language syntax with a dynamic type added;
- a runtime supporting dynamic features;
- a gradual typesystem.

All these components are constructed by hand for the earlier mentioned STFL in section \ref{gradualizing-the-typesystem-manually}.

Updating the syntax to allow a dynamic type is trivially done by adding a new choice `?` to the syntactic form.

The runtime which supports the dynamic features is still a task for the programmer. Luckily, STFL is already constructed to allow these features by having runtime typechecks available and already performing these casts at runtime when needed, as noticed in \ref{the-dynamic-runtime}.

At last, the typesystem itself is gradualized. As it turns out in section \ref{gradualizing-domain-and-codomain}, this boils down to converting some helper functions over types into functions over _sets of types_. To achieve this algorithmically, abtstract interpretation was used.

A framework for abstract interpretation over syntactic forms was constructed in chapter \ref{syntax-driven-abstract-interpretation}. This powerfull technique allowed to reason about an entire set passing through a function at once efficiently, based on an efficient notation for sets in combination with transformations over this notation.

This abstract interpretation gives guidance on how the gradualized functions should behave, allowing a gradual typesystem to be built. As bonus does the abstract interpretation give some extra checks for the metalanguage. 

To test this abstract interpretation, gradual counterparts of `domain` and `codomain` were constructed in the process to gradualize STFL.

Gradualizing STFL manually is possible, altough some care should taken regarding implementation of the runtime and of the gradualized functions.

\medskip

The next step is automating this gradualization. This is done by stating all the changes and refactorings needed for gradualization in a `.language-changes`-file; allowing easy recalculation of the gradual language if changes to STFL are performed. While gradualization is not fully automated, it allows both dialects to stay in sync.

## Conclusion

All considered, ALGT is a powerfull tool which allows for easy and light, yet formally correct language design. The tool helps the designer, by offering a clutter-free experiences and helpfull error messages when small and bigger errors are made. 

The tool offers support to gradualize the typesystem automatically.

