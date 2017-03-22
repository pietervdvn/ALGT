
The typechecker
===============

Introducing types
==================

Introducing types
------------------

We'll need syntactic forms for types

. . .

$$Demo.language!15,16!file


Introducing types
-----------------

Explicit type tags on the input arguments:

$$Demo.language![9..12]!file

Introducing types
-----------------

Which also means we'll have to update __EvalApp__

. . . 

$$Demo.language![72..73]!file


Functions
=========


Domain and codomain
-------------------

Function type		dom		cod
-------------		-----		-----
`Int -> Int`		`Int`		`Int`
`Int -> (Int -> Int)`	`Int`		`Int -> Int`
`(Int -> Int) -> Int`	`Int -> Int`	`Int`


Domain
------

$$Demo.language![32..36],40!file


Domain
-------

	dom(T1 "->" T2)	= T1

![](TypeTrees0annot.png)



Domain
-------

![](TypeTrees2annot.png)

	dom(T1 "->" T2)	= T1



Domain
-------

	dom( ( "(" T1 ")" ) "->" T2)	= T1

![](TypeTrees2annot1.png)


Domain
------

$$Demo.language!37!file

Domain
------

$$Demo.language![36..40]!file

Domain
------

Built-in totality check

$$($$Demo.language![1..40])![1..5]

 . . .

$$Demo.language!41!file




Codomain
--------

$$Demo.language![43..50]!file



The typing environment
======================

Variables
---------

	(\x : Int . x + 1) 41

How to keep track of what type a variable (such as **x**) has?


Typing environment
------------------

$$Demo.language!19,20!file

The typing environment will be denoted with Γ (U+393)


Typing
======


The typing relation
---------------------

$$Demo.language!59!file

(⊢ is pronounced _entails_; U+22a2)

Typing constants
----------------

$$Demo.language![102..104]!file

. . .

$$($$Demo.language $$demo.demo!1 expr -r :: --nc)![1..7]


Typing plus
-----------

	
        -------------------------------------- [TPlus]
        Γ ⊢ i0 "+" i1 , "Int"


Typing plus
-----------

$$Demo.language![106..108]!file

. . .

$$($$Demo.language $$demo.demo!2 expr -r :: --nc)![1..9]


Typing variables
----------------

We lookup the variable in the __typingEnvironment__:

$$Demo.language![96..98]!file

. . .


\begin{lstlisting}[style=terminal]

------------------------ [ Tx ]
x : Int {} ⊢ x , Int

\end{lstlisting}

Typing functions
----------------

	 
	------------------------------------------------------------------- [TLambda]
	 Γ ⊢ "(" "\\" x ":" TArg "." expr ")" , ???

What type do we return?

Typing functions
----------------


	 Γ ⊢ expr, TExpr
	------------------------------------------------------------------- [TLambda]
	 Γ ⊢ "(" "\\" x ":" TArg "." expr ")" , ???


Hmm, something is missing here...

Typing functions
----------------



	 (x ":" TArg) Γ ⊢ expr, TExpr
	------------------------------------------------------------------- [TLambda]
	 Γ ⊢ "(" "\\" x ":" TArg "." expr ")" , ???

\begin{block}{Typing environment syntax}
$$Demo.language!19,20!file
\end{block}
Nearly done...


Typing functions
----------------


	 (x ":" TArg) Γ ⊢ expr, TExpr
	------------------------------------------------------------------- [TLambda]
	 Γ ⊢ "(" "\\" x ":" TArg "." expr ")" , TArg "->" TExpr

There is a catch... 

. . .

TArg 	= __Int -> Int__

TExpr 	= __Int__

. . .

TArg "->" TExpr = __Int -> Int -> Int__


Typing functions
----------------

$$Demo.language![111..114]!file

. . .

$$($$Demo.language $$demoFunc.demo expr -l -r :: --nc)![1..11]


Typing application
------------------

	
	-------------------------------------------------------------	[Tapp]
	 Γ ⊢ e1 e2, ???


Typing application
------------------

	 Γ ⊢ e1, Tfunc
	-------------------------------------------------------------	[Tapp]
	 Γ ⊢ e1 e2, ???

Typing application
------------------

	 Γ ⊢ e1, Tfunc	Γ ⊢ e2, Targ
	-------------------------------------------------------------	[Tapp]
	 Γ ⊢ e1 e2, ???

Typing application
------------------

	 Γ ⊢ e1, Tfunc	Γ ⊢ e2, Targ
	-------------------------------------------------------------	[Tapp]
	 Γ ⊢ e1 e2, cod(TFunc)



Typing application
------------------

$$Demo.language![116..118]!file


. . . 

$$($$Demo.language $$demo.demo!4 expr -l -r :: --nc --short-proofs 1)![5..13]


Typing: practically
-------------------

Define relation __::__ to type in an empty environment

$$Demo.language!60!file

$$Demo.language![121..123]!file


__./ALGT Demo.language demo.demo expr -l -r ::__


-------------

One more thing

Syntax coloring
===============

Syntax coloring
---------------


Extra section, just under the syntax

$$Demo.language![23..29]!file


Syntax coloring
---------------

__--style Terminal__

$$!($$Demo.language $$demo.demo!4 expr -l --latex --style Terminal)!2



__--style White__

$$!($$Demo.language $$demo.demo!4 expr -l --latex --style White)!2

