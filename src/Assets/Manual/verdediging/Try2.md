\center
\textbf{\Huge{ALGT}}

\bigskip

\huge{A Framework to Design and}\par
\huge{Gradualize Programming languages}

\bigskip

\large{Pieter Vander Vennet}

\large{Promoted by Prof. Christophe Scholliers}










------------------

\center
\huge{Programming Language Design}



Programming Language design
---------------------------

In the last decades, thousands of new programming languages have been designed

 
. . .

\medskip

Tools and programming languages have been developed to help programming language design


Language Design Tools
---------------------

- ANTLR
- Maude Rewrite System
- PLT-Redex
- OTT
- The Gradualizer


\center
\includegraphics[width=1in]{ANTLR.jpg}
\hspace{0.5in}
\includegraphics[width=1in]{PLTRedex.jpg}
\hspace{0.5in}
\includegraphics[width=1.4in]{XText.png}


 PLT-Redex
-----------


\begin{columns} 
 \column{2in}
\includegraphics[width=2in]{PLTRedex.jpg}
 \column{.6\textwidth}
Language design in Scheme

\begin{lstlisting}
#lang racket
(require redex)
(define-language L
   ( e (e e)
       (λ (x t) e) x (amb e ...) number
	(+ e ...) (if0 e e e) (fix e))
    (t (→ t t) num) 
    (x variable-not-otherwise-mentioned))

(define red
  (reduction-relation
   Ev
   #:domain p
   (--> (in-hole P (if0 0 e_1 e_2))
        (in-hole P e_1) "if0t")
\end{lstlisting}

\end{columns}



----------------------

\center
\huge{Gradual Typing}


------------

\center
\includegraphics[width=2.5in]{TypeScript.png}


Gradual Typing
---------------

Allows dynamic features

Still offers all guarantees of static typing in static types

Type errors and crashes can only occur in dynamic parts


Gradual typing
--------------

Why exist so little gradual programming languages?




-----------------

\center
\huge{ALGT}


Tool for language design
------------------

- lightweight yet formal
- stays close to the conventional definitions
- specifies the syntax, evaluator and typesystem of any programming language
- interprets target programs
- is checked by a typechecker and automatic tests

-------------------

Tool for gradualization

- Helps to create a gradual counterpart of the language
- Automates gradualization



-----------------

\center
\huge{Specifying Arbitrary Programming Languages}


 STFL
------

As example, we build a Simply Typed Functional Language

This example is gradualized later

 STFL: examples
---------------

Numbers

	42


 STFL: examples
---------------

Addition

	41 + 1


 STFL: examples
---------------

Booleans

	True
	False

 STFL: examples
-----------------

If-test

	If True Then 41 else 0

 STFL: examples
---------------

Variables

	x


 STFL: examples
---------------

Type casts

	42 :: Int
	x  :: Bool


 STFL: examples
-----------------

Lambda functions (aka anonymous functions) ... 

	(\ x : Int . x + 1)

. . .

... and application

	(\x : Int . x + 1) 41

Specifying STFL
---------------

How to specify a language?

. . .

- Syntax
- Evaluation
- Typesystem

. . .

Conventionally, done in a LaTeX-notation


-------------------

![](STFLGreek.png)


Syntax
======

Syntax
-------

![](STFLSyntaxGreek.png)


-------


	Syntax
	======	

	typeTerm::= "Int" | "Bool" | "(" type ")"
	type	::= typeTerm "->" type | typeTerm

. . .

	bool	::= "True" | "False"
	number	::= Number
	var	::= Identifier
	value	::= bool | number | var | "(" "\\" var ":" type "." expr ")" 
	term	::= value 
		| "If" expr "Then" expr "Else" expr
		| "(" expr ")"
	expr	::= term "+" expr
		| term "::" type 
		| term expr
		| term
 

Evaluator
=========

Evaluator
----------

![](STFLEvaluatorSimpleGreek.png)



Evaluator
---------

![](STFLEvaluatorPlusGreek.png)


Evaluator
----------

![](STFLEvaluatorGreek.png)


Evaluator
---------


	 Relations
	===========

	(→)	: expr (in), expr (out)



Evaluator
---------

	 Rules
	=======


	---------------- [EvalParens]
	 "(" e ")" → e


	---------------------------------------	 [EvalIfTrue]
	 "If" "True" "Then" e1 "Else" e2 → e1


	---------------------------------------- [EvalIfFalse]
	 "If" "False" "Then" e1 "Else" e2 → e2






Evaluator
---------



	 n1:Number	n2:Number
	------------------------------------	[EvalPlus]
	 n1 "+" n2 → !plus(n1, n2)



Typesystem
===========

Typesystem
----------

![](STFLTypeSystemGreek.png)


Typesystem
-----------

![](STFLTypesystemApp.png)


Typesystem
----------

	 Relations
	===========

	(→)	: expr (in), expr (out)
	(⊢)	: typingEnvironment (in), expr (in), type (out)

(Typingenvironment should be declared as syntactic form as well)

Typesystem
-----------

	 Γ ⊢ e1, Tfunc	Γ ⊢ e2, Targ	Targ == dom(Tfunc)
	---------------------------------------------------	[Tapp]
	 Γ ⊢ e1 e2, cod(Tfunc)



Executing STFL
---------------

As syntax, evaluator and typesystem are specified, ALGT can parse and execute STFL-programs


Executing STFL
--------------

Demo


Testing
--------

Properties can be stated, which ALGT can test

 . . .


	 e0 :: T0	 e0 → e1	e1 :: T1
	------------------------------------------ [Preservation]
		 	T0 == T1


		 e0 :: T
	-------------------------- [Progress]
	 e0:value    |     e0 → e1 



Testing
--------

Demo


----------------------

\center
\huge{Gradualization}

How?
----


Each aspect of the language is updated to allow gradual types.

. . .

Based on the paper _Abstracting Gradual Typing_ by _Ronald Garcia_, _Alison M. Clark_ and _Eric Tanter_.


Syntax
------

A new type `?` is introduced
It represents the **dynamic type** (or unknown type)


	typeTerm	::= "Bool" | "Int" | "(" type ")" | "?"

In usage

	(\ a : Int . a + 1) True
	(\ a : Int . a + 1) (True :: ?)
	(\ a : ? . a + 1)    True


Evaluator
---------

No changes needed, as it was carefully constructed to allow dynamic behaviour


	 n1 : Number	n2 : Number
	---------------------------- [EvalPlus]
	 n1 + n2 → !plus(n1, n2)


 Typesystem
------------



	 Γ ⊢ e1, Tfunc	Γ ⊢ e2, Targ	Targ == dom(Tfunc)
	---------------------------------------------------	[Tapp]
	 Γ ⊢ e1 e2, cod(Tfunc)

Reasoning about types is explicit in helper functions



Typesystem: helper functions
-----------------------------

	Functions
	=========

	dom 			  : type -> type
	dom( "(" T ")" )	  = dom(T)
	dom(("(" T1 ")") "->" T2) = T1
	dom(T1 "->" T2) 	  = T1

	# Not defined for "Int", "Bool"


(Codomain is constructed analogously)

Typesystem: helper functions
----------------------------



	dom("?") =  ?????

. . .

"?" acts a _set_ containing all types


Dynamic type as set
--------------------


"?" acts as the set containing all types, being
 `{"Int", "Bool", "Int" "->" "Bool", "(" "Bool" "->" "Int" ")" -> "Bool", ... }`

. . .

\bigskip
Thus, dom("?") acts as `{dom("Int"), dom("Bool"), dom("Int" "->" "Bool"), dom("Bool" "->" "Int"),` 
\hspace{1cm}    ` dom("(" "Int" "->" "Bool" ")" -> "Bool"), ... }`

. . .

\bigskip
`dom("?") = { ɛ,  ɛ, "Int", "Bool", "Int -> Bool", ... }`

which is again the set of all types.


Dynamic type as set
--------------------

Thus: `dom("?") = "?"`

. . .

How to automate this?

The set of types is infinite


-------------------

\center
\Huge{Abstract Interpretation}

What is abstract interpretation
-------------------------------

Functions are executed on an _abstract domain_ instead of the actual value

An abstract domain could be, for numbers, the sign: `+`, `-`, `0` 

. . . 

A function `succ`, which adds one, would become:

	succ(+)	= {+}
	succ(0)	= {+}
	succ(-) = {-,0}


Syntax Driven Abstract Interpretation
-------------------------------------

Based on the syntax, an efficient representation for sets of syntactic forms is constructed

. . .

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
	# {"Int", "Bool", typeTerm "->" type}	

	dom(("(" T1 ")") "->" T2) = T1		# yields {type}
	# {"Int", "Bool", "Int" "->" type, "Bool" "->" type}

	dom(T1 "->" T2) 	  = T1		# {"Int", "Bool"}
	# Fallthrough: {"Int", "Bool"}



Syntax Driven Abstract Interpretation
-------------------------------------

The end result of `dom({type})` is `{type}`

. . .

Thus `dom("?") = "?"`


--------------

\center
\huge{Automating Gradualization}

-------------------

	 Syntax Changes
	================
	typeTerm 	::= ... | "?"
	Rename type to gtype
	Rename typeTerm to gtypeTerm

	 Function Changes
	==================
	dom		: gtype -> gtypeTerm
	...
	dom("?")	= "?"

	 Relation Changes
	==================
	Rename (==) to (~)


Automatic refactoring
---------------------

STFL.language + Gradualize.language-changes = GTFL.language


--------------

\center
\huge{Overview}


---------------

ALGT is a tool for language design

- lightweight yet formal specifications, close to conventions
- specifies the syntax, evaluator and typesystem of any programming language
- interprets target programs to execute them
- is checked by a typechecker and automatic tests

ALGT is a tool for gradualization

- Helps to create a gradual counterpart of the language
- Automates gradualization with builtin refactoring support 
