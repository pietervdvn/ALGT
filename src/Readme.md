Program structure
=================

This document details all program parts.

Data flow
---------

The main module, `main.hs`, loads a 'typeSystem' file. This file is parsed (see the various modules within `Parser/`)
Once a `TypeSystem` has been constructed, this piece of data can be used to parse an examplefile (using `Parser/TargetLanguageParser`),
resulting in one **parsetree** (or more, if `--line-by-line` is used).

This parsetree can then be given to a function for rewriting or rule to test wether a relation holds for it.

Module overview
---------------

`TypeSystem.hs` is the most important module, as it contains all data structures. Go read it first.
`Parser/*` contains the parsers, built on parsec. Some notable parsers:
`Parser/ExpressionParser` contains some extra typing code, which is notable too. Every parser using expressions, uses the same two-stepped technique, borrowing the 'typeAs' from here
`Parser/TargetLanguageParser` will hapilly parse any file you give it, according to a given BNF.

`ParseTreeInterpreter` handles interpretation of functions and rules, with 'simple' parsetrees

Cabal dependencies
------------------

parsec
optparse-applicative
