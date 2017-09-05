% title: ALGT - A framework to Describe and Gradualize languages 
% author: Pieter Vander Vennet



ALGT - A framework to Describe and Gradualize languages 
=======================================================

Contents
------

- Gradual typing
- Representing Arbitrary Programming Languages
- Abstract interpretation
- Automating Gradualization
- Conclusion


Gradual Typing
==============

Static typing
-------------

A typechecker is run before the program is executed

        String x = "abc"
        y = x - 5	# Type mismatch: expected int as operand, but got String


Dynamic typing
--------------

Type-errors cause the program to crash at runtime

        x = "abc"
        y = x - 5	# Error: could not subtract "abc" - 5


Gradual typing
--------------

Mix both approaches as needed

Some parts are statically typed

Some other parts are dynamically typed

Gradual typing
--------------

Introduces a **dynamic** type `?`

	String x = "abc"
	y        = x - 5	# Type mismatch
	? z      = x
	f(x)			# Type mismatch
	f(z)	        	# Runtime error: "abc" is not an int 

	f(int x):
		...


Gradual typing
--------------

Runtime type errors always originate from the dynamic parts

All type annotations given: total static typing

No type annotations given: total dynamic typing


Why gradual typing?
-------------------

Some code benefits from the dynamic approach

- Code with everchanging requirements
- Code that bridges components

Some code benefits from the static approach

- Difficult algorithms
- Interface definitions

Over time, a codebase can be migrated in small pieces to a static/dynamic approach

- A _quick 'n dirty_ dynamic prototype maturing into a static program


Why are there so little gradual programming languages?
-------------------------------------------------------

Designing a gradual type system is hard

Little tools exist to help designing a gradual typesystem


Representing Arbitrary Programming Languages
============================================

ALGT
----

A framework to create programming languages based on:

- BNF
- Functions
- Deduction rules

STFL
----

A Simply Typed Functional Language is implemented as example

	(\ x : Int . x + 1) 41
	(\ b : Bool . If y Then 0 Else 1) True

STFL Syntax
-----------

	 Syntax
	========
 
	typeTerm::= "Int" | "Bool" | | "(" type ")"
	type	::= typeTerm "->" type | typeTerm
	var	::= Identifier
	number	::= Number
	bool	::= "True" | "False"
	value	::= bool | number

	e	::= eL "+" e | eL "::" type 
		| eL e | eL

	eL	::= value | var | "(" e ")"
		| "(" "\\" var ":" type "." e ")"  
		| "If" e "Then" e "Else" e 

STFL Parsing
------------

Using this definition, programs can be parsed

	20 + 22

![](Parsetree2022VD.png)


STFL Parsing
------------

Using this definition, programs can be parsed

	(Int) -> Bool

![](TypeTreeVD.png)


STFL helper functions
---------------------

	dom 			  : type -> type
	dom("(" T1 ")")		  = dom(T1)
	dom(("(" T1 ")") "->" T2) = T1
	dom(T1 "->" T2) 	  = T1


	cod 		          : type -> type
	cod("(" T2 ")")	          = cod(T2)
	cod(T1 "->" ("(" T2 ")")) = T2
	cod(T1 "->" T2)           = T2


 STFL Natural deduction
------------------------

	 n1:Number	n2:Number
	------------------------------------	[EvalPlus]
	 n1 "+" n2 → !plus(n1, n2)



 Abstract interpretation
=========================


 Lifting functions
-------------------


The dynamic type `?` behaves as a set of types

`?` = `{"Int", "Bool", "Int -> Bool", "Bool -> Int", ...}`


Lifting functions
------------------

`dom("?")` is equivalent to

	{dom("Int"), dom("Bool"), dom("Int -> Bool"), dom("(Bool -> Bool) -> Int"), ...}

 . . .

This equals

	{ ɛ,  ɛ, "Int", "Bool -> Bool", ... }

This is the same as `?`, thus `dom("?") = "?"`



Lifting functions
-----------------

How to know that `dom("?") = "?"` algorithmically?

By interpreting the functions on an abstract domain

What is abstract interpretation?
--------------------------------

Functions are executed on an _abstract domain_ instead of the actual value

An abstract domain could be, for numbers, the sign: `+`, `-`, `0` 

A function `succ`, which adds one, would become:

	succ(+)	= {+}
	succ(0)	= {+}
	succ(-) = {-,0}




Syntax Driven Abstract Interpretation
-------------------------------------

Based on the syntax, an efficient representation for sets is constructed


Syntax Driven Abstract Interpretation
-------------------------------------

For example, all types (`"Int", "Bool", "Int -> Bool"`, ...) are represented with

	{type}



Syntax Driven Abstract Interpretation
-------------------------------------


All function types giving a bool (`"Int -> Bool", "Bool -> Bool", "(Int -> Int) -> Bool"`, ...) 

are represented with

	{typeTerm "->" "Bool"}

Syntax Driven Abstract Interpretation
-------------------------------------

This efficient set representation allows addition and subtraction

This allows to calculate the result for an entire set at once, the lifted function:

	dom 			  : type -> type
	# {type}

	dom("(" T1 ")")		  = dom(T1)	# yiels {type}
	# {"Int", "Bool", typeTerm "->" type, typeTerm "->" type}

	dom(("(" T1 ")") "->" T2) = T1		# yields {type}
	# {"Int", "Bool", "Int" "->" type, "Bool" "->" type}

	dom(T1 "->" T2) 	  = T1		# {"Int", "Bool"}
	# Fallthrough: {"Int", "Bool"}

Syntax Driven Abstract Interpretation
-------------------------------------


The end result of `dom({type})` is `{"Int", "Bool"} + {type} + {type} = {type}`

As `?` represents all types, `dom("?") = "?"`


Automating Gradualization {-}
==========================

Automating Gradualization
-------------------------

With these lifted functions, the gradual counterpart of helper functions can be constructed

. . .

\medskip
Some functions and relations should be renamed, to represent their new semantics 

- `==` becomes `~`
- `type` becomes `gtype`
- `equals` becomes `isConsistant`
- ...

. . .

Refactoring support is builtin


Refactoring format
-------------------

	 Syntax Changes
	================

	typeTerm 	::= ... | "?"
	Rename type to gtype

	 Function Changes
	==================

	dom		: gtype -> gtypeTerm
	...
	dom("?")	= "?"



Conclusion
==========

ALGT as tool for gradual language development
------------------------------------

ALGT helps designing programming languages:

- Lightweight yet formal specification
- Parse and interpret target programs
- Typechecked and testing

ALGT helps creating a gradual counterpart of a language

- Builtin lifting of functions, with abstract interpretation
- Builtin refactoring support


	
