% title: Build your own language
% author: Pieter Vander Vennet


Build your own language
========================

Building a extremely simple language
------------------------------------

Supporting

 - Numbers
 - Addition
 - Functions
 - Typechecking


Examples
--------

$$demo.demo!file


Examples
--------

For a first version, we'll omit all types:

$$demodyn.demo!file


Tools
-----

ALGT

[github.com/pietervdvn/ALGT](http://github.com/pietervdvn/ALGT)

All the files you need are in the **demo**-directory, download it entirely


Declaring the syntax
=====================


What is the syntax of the language?
-----------------------------------

Syntax =  _what it looks like_


Backus-Naur-Formulation
------------------------

What is BNF?

	name ::= "literal1" | "literal2" | "literal3"

Possible files:

	literal1

OR

	literal2

OR

	literal3

Syntactic form names are written with a __lowercase letter__

Backus-Naur-Formulation
------------------------

What is BNF?

	name ::= "literal1" "literal2"

Possible files:

	literal1 literal2

Whitespace is ignored by default! See the manual for other options

Backus-Naur-Formulation
------------------------

	name		::= "literal1"
	otherName	::= name

Possible files:

	literal1


Backus-Naur-Formulation
-----------------------

	name		::= "literal0"
	otherName	::= name name | "literal1" name | "literal2"


Backus-Naur-Formulation
-----------------------

Defining numbers:

	int		::= "0" | "1" | "2" ...

Backus-Naur-Formulation
-----------------------

Defining variables:


	var		::= "a" | "b" | "c" | ... | "someVariableName" | ...


Backus-Naur-Formulation
-----------------------

 Too much work...

 Special rules __Number__ and __Identifier__ have been provided as builtin

 Builtins are written with an _uppercase_



How do we define a language?
----------------------------

In the file _Demo.language_

$$Demo.language![1..7]!file


Parsing!
--------

./ALGT Demo.language demo.demo int -l

$$($$DemoDynA.language $$demo.demo!1 int -l)


Addition
--------

$$DemoDynA.language![5..10]!file

Addition
--------

	41 + 1

$$($$DemoDynA.language![1..10] $$demo.demo!2 expr -l)


Addition
--------

Use extra flag --ptsvg Name to create an image of your parsetree:

![PT]$$svg($$DemoDynA.language![1..10] $$demo.demo!2 expr -l --ptsvg DemoDynPT)


Addition
--------

	42

$$($$DemoDynA.language![1..10] $$demo.demo!1 expr -l)

Addition
--------

$$DemoDynA.language![6..11]!file

$$($$DemoDynA.language![1..11] $$demo.demo!1 expr -l)

Addition
--------

What with 1 + 2 + 3?

$$DemoDynA.language![10..11]!file

Addition
--------

Let's change expression to be recursive!

$$DemoDynB.language![6..11]!file


Left recursion
--------------

$$DemoDynB.language![9..11]!file
$$($$DemoDynB.language![1..11])


Left recursion
--------------

Simply use int for the first term:

$$DemoDynC.language![9..12]!file
$$($$DemoDynC.language![1..11] $$demo.demo!3 expr -l )


Left recursion
---------------

![pt]$$svg($$DemoDynC.language![1..11] $$demo.demo!3 expr -l --ptsvg DemoPT1)


Adding Functions
----------------

	( \ x : Int . x + 1)


Adding Variables
----------------

$$DemoDyn.language!10!file

Adding Terms
------------

We'll want to introduce a syntactic form __term__ , for variables, ints and functions:

$$DemoDyn.language![11..14]!file

Adding Terms
------------

$$DemoDyn.language!15!file

Parsing stuff
--------------

Still done with "./ALGT Demo.language demo.demo expr -l"

$$($$DemoDyn.language $$demodyn.demo!4 expr -l)

Parsing stuff
-------------

![PT]$$svg($$DemoDyn.language $$demodyn.demo!4 expr -l --ptsvg DemoPT2)








 Building the evaluator
========================

Function or natural deduction?
-------------------------------

ALGT supports two ways to perform computations:

Functions and natural deduction

For the evaluator, we'll use natural deduction 


Declaring the relation
-----------------------

In a new section in the _.language_

$$DemoDyn.language![25..28]!file

(→: U+2192)


Declaring EvalPlus
------------------

	-------------------------- [EvalPlus]
	 ... → ...

The conclusion goes beneath the line

The rulename goes on the right


Declaring EvalPlus
------------------

	-------------------------- [EvalPlus]
	 i0 "+" i1 → ...





Declaring EvalPlus
------------------

	-------------------------- [EvalPlus]
	 i0 "+" i1 → !plus(i0, i1)


Builtin functions do have an exclamation mark



Declaring EvalPlus
------------------

$$DemoDyn.language![36..38]!file




Running →
-----------

$$DemoDyn.language![36..38]!file

./ALGT DemoDyn.language demodyn.demo expr -l -r →

$$($$DemoDyn.language![1..38] $$demodyn.demo!2 expr -l -r →)



Function application
--------------------


	---------------------------------------------------------- [EvalApp]
         function arg → ...


Function application
--------------------


	---------------------------------------------------------- [EvalApp]
         ("(" "\\" x "." expr ")") arg → ...

Extra parentheses around function, to group the subterm!

Function application
--------------------


$$DemoDyn.language![41..43]!file


Builtin function __!subs__: replace this, with that, everywhere in


For __!subs__ is an explicit type needed


Function application
--------------------

$$($$DemoDyn.language![1..43] $$demodyn.demo!4 expr -l -r →)




Contexts
--------

What with 1 + 2 + 3?

$$DemoDyn.language![45..48]!file



Contexts
--------

$$($$DemoDyn.language![1..48] $$demodyn.demo!3 expr -l -r →)


Bigstep
-------

$$DemoDyn.language!29!file

$$DemoDyn.language![54..61]!file


Bigstep
-------

$$($$DemoDyn.language $$demodyn.demo!3 expr -l -r →* --short-proofs 8)




Your turn!
==========


Your turn
---------

Now it's your turn to give these a try.

- github.com/pietervdvn/ALGT
- Download the __demo__-directory
- Overview of commands and usefull stuff in __readme.md__




If there is still time, we'll also build a typechecker for the demo langauge...


