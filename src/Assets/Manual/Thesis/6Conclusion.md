
 Conclusion and future work
============================

ALGT
----

The ALGT-tool well suited for its goal of creating programming languages, offering an intuitive and clutter-free experience to create programming languages. Creating a new programming language, from downloading the tool to running a target program, can be done in a few hours with only basic knowledge of programming language development.

The parser can be created based on the syntax, where multiple checks prevent small but easily made errors. The interpreter for the language can be constructed using well-known reduction rules, which are thoroughly checked as well by a rigourous typecheck. When helper functions are needed, they can be defined straightforwardly and with excellent support of the typechecker, with liveness and totality checks, preventing forgotten cases. At last can some automatic tests be added, by adding the properties the language should have.

In other words, ALGT is a tool which can be used for all aspects of language design.

### Future work

A tool such as ALGT is never finished, and countless features could be added. Some usefull features for the short term coule be:

- A latex typesetting of a language, making publication of the language easy.
- The tutorial/manual could use some more polishing, as not all features are described in it yet.
- A way to import libraries and modules, making crosscompilations easier
- A description of the ALGT-language in ALGT, which could be used to add some syntactic sugars to the language


Gradualization
--------------

When gradualizing programming languages, the tool provides usefull services as well; renaming and changing syntax, functions and operations can be easily done with the refactoring support, making it easy to state only the differences between two languages; making it easy to keep the static and gradual dialects in sync. The construction of the gradualized functions is eased, as a gradual counterpart of each function is a available via abstract interpretation.

### Future workings

Gradualization is not fully automated yet, as the gradual counterparts of the functions should still be denoted manually. It might be usefull to make the abstract interpretation available within an ALGT-language. How this should be done practically is an open question, as this would mix concrete values and set representions, introducing syntax compromises within the ALGT-syntax.

It is also unclear which typesystems can be fully gradualized and which are not. Especially languages featuring a compile time runtime for types, such as System F or dependently typed languages, are notoriously hard to gradualize.   
\newline

In conclusion, gradualization will remain a research topic for the coming years, in which I made a humble attempt to automate it further.

