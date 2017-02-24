
 Overview
==========

This document gives an overview of the _ALGT_-tool. 
First, a general overview of what the tool does is given and why it was developped. 
Second, a hands-on tutorial develops a Simply Typed Functional Language (STFL).
Thirdly, the reference manual gives an in-depth overview of the possibilities and command line flags. 
Fourth, some concepts and algorithms are explained more thoroughly, together with properties they use.
And at last, the 'dynamize' and 'gradualize' options are explained in depth, as these are what the master dissertation is about.

This documentation is about version **0.1.15.1** (`Total Dynamism (html-manual, pattern functions)`), generated on 2017-2-23 .

 What is ALGT?
===============

_ALGT_ (_Automated Language Generation Tool_ [^AGT-Name]) is a tool to formally specify any programming language. This is done by first declaring a syntax, using BNF, and then declaring semantics by introducing logical deduction rules, such as evaluation or typing rules.  Eventually, properties can be introduced which can be tested. Of course, these can be run.

The tool is kept as general as possible, so any language can be modelled.


[^AGT-Name]: Note the similarity of _ALGT_ and _AGT_ . The tool started its life as _AGT_, based on the paper [_Abstracting Gradual Typing_](https://pleiad.cl/papers/2016/garciaAl-popl2016.pdf). When it became more general, another name and acronym was chosen.

 Tutorial: developing a simple programming language
====================================================

We will develop a programming language which can work with booleans and integers. Apart from doing basic arithmetic, we can also use anonymous functions.

Example programs are:

\begin{lstlisting}
True
False
If True Then False Else True
If If True Then False Else True Then True Else False
42
20 + 22
1 + 2 + 3
(\x : Int . x + 1) 41
\end{lstlisting}




The expression `(\x : Int . x + 1)` is a _lambda expression_. This is an anonymous function, taking one argument - named x- of type `Int`. When applied (e.g. `(\x : Int . x + 1) 41`, the expression right of the `.` is returned, with the variable `x` substituted by the argument, becoming `41 + 1`.


 Setting up a language
-----------------------

A language is declared inside a `.language` file [^extension]. Create `STFL.language`, and put a title in it:

[^extension]: Actually, the extension doesn't matter at all.


\begin{lstlisting}
 
 STFL 
******

# A Simply Typed Functional Language

\end{lstlisting}



You can put comments wherever you want after a `#`, e.g. to give some explanation about the language 

 Declaring the syntax
-----------------------

For a full reference on syntax, see the [reference manual on syntax](#syntax).

### Simple booleans


A program is nothing more then a string of a specific form. To describe strings of an arbitrary structure, *BNF* [^BNF] can be used.

[^BNF]: Backus-Naur-form, as introduced by John Backus in the [ALGOL60-report](http://www.softwarepreservation.org/projects/ALGOL/report/Algol60_report_CACM_1960_June.pdf). 

The syntax of our programming language is defined in the **Syntax**section of _STFL.language_:

\begin{lstlisting}
 
 STFL 
******

# A Simply Typed Functional Language

 Syntax
========
\end{lstlisting}



What do we write here? Let's start with declaring the boolean values `True` and `False`. We express how these can be parsed by writing `bool	::= "True" | "False"`. This tells the tool that a syntactic form named `bool` exists and it is either `True` of `False`.
Note the double quotes, these are important to indicate that we want this string literally. The `|` epxresses that it can choose between those forms.

_STFL.language_ now looks like:

\begin{lstlisting}
 
 STFL 
******

# A Simply Typed Functional Language

 Syntax
========

bool	::= "True" | "False"
\end{lstlisting}


Lets try running this! Create `examples.stfl`, which contains:

\begin{lstlisting}
True
False
\end{lstlisting}


We can parse these by running (in your terminal) `./ALGT STFL.language examples.stfl bool -l`. The first argument is the language file, the second the examples, the `bool` tells ALGT what syntactic rule to parse. The `-l` flag expresses that each line should be treated individually.

If all went well, you should get the following output:

\begin{lstlisting}[style=terminal]
# "True" was parsed as:
"True": bool.0
# "False" was parsed as:
"False": bool.1

\end{lstlisting}

The most interesting part here is that `True` has been parsed with `bool.0`, thus the first choice of the `bool`-form, while `False` has been parsed with the second form.



### If-statements



Now, let's add expressions of the form `If True Then False Else True`. We define a new syntactic form: `expr	 ::= "If" bool "Then" bool "Else" bool`.[^spaces] This tell _ALGT_ that an expression starts with a literal `If`, is followed by a `bool` (so either `True` or `False`), is followed by a literal `Then`, ... The tool uses the double quotes `"` to distinguish between a literal string and another syntactic form.


[^spaces]: Don't worry about spaces and tabs, we deal with them. If you want need to parse stuff like "duizendeneen" or whitespace sensitive languages, please refer to the [reference manual](#syntax)


_STFL.language_ now looks like:

\begin{lstlisting}
 
 STFL 
******

# A Simply Typed Functional Language

 Syntax
========

bool	::= "True" | "False"
expr	::= "If" bool "Then" bool "Else" bool
\end{lstlisting}



This captures already some example expressions. Let's add `If True Then False Else True` to _examples.stfl_:

\begin{lstlisting}
True
False
If True Then False Else True
\end{lstlisting}


Let's run our tool, this time with `./ALGT STFL.language examples.stfl` **expr** `-l`

\begin{lstlisting}[style=terminal]
"examples.stfl (line 0)" (line 1, column 1):
unexpected "T"
expecting "If"
Could not parse expression of the form expr

\end{lstlisting}


Oops! Seems like our parser now always wants to see a `If` in the beginning, and can't handle `True` anymore. Perhaps we should tell that a `bool` is a valid expression to:

\begin{lstlisting}

 Syntax
========

bool	::= "True" | "False"
expr	::= "If" bool "Then" bool "Else" bool
	| bool
\end{lstlisting}



Lets see what this gives:

\begin{lstlisting}[style=terminal]
+  expr.0
|  "If": expr.0
|  "True": bool.0
|  "Then": expr.0
|  "False": bool.1
|  "Else": expr.0
|  "True": bool.0
\end{lstlisting}

Looks a lot better! The third example shows clearly how the expression falls apart in smaller pieces.



What with a nested `If`?

`If If True Then False Else True Then True Else False` clearly can't be parsed, as the condition should be a `bool`, according to our current syntax.

Well, we can just write `expr` instead of `bool` in our syntax:

\begin{lstlisting}
expr	::= "If" expr "Then" expr "Else" expr
	| bool
\end{lstlisting}


Running this gives

\begin{lstlisting}[style=terminal]
# "If If True Then False Else True Then True Else False" was parsed as:
+  expr.0
|  "If": expr.0
|  +  expr.0
|  |  "If": expr.0
|  |  "True": bool.0
|  |  "Then": expr.0
|  |  "False": bool.1
|  |  "Else": expr.0
|  |  "True": bool.0
|  "Then": expr.0
|  "True": bool.0
|  "Else": expr.0
|  "False": bool.1

\end{lstlisting}

This clearly shows how the parse trees are nested. 
This can be rendered too:[^ptsvg] 

![ParseTree of a nested condition[^ptsvg]](ParseTreeNested_0.png){width=100%}


[^ptsvg]: These images can be created with `ALGT STLF.language examples.stfl -l --ptsvg Outputname`


### Adding numbers, subtyping and forbidden left recursion

Time to spice things up with numbers. To make things easier, integers are built in as `Number`. It's good practice to introduce a new syntactic rule for them:

\begin{lstlisting}
int	::= Number\end{lstlisting}


As an `int` is a valid expression, we add it to the `expr` form:

\begin{lstlisting}
expr	::= "If" expr "Then" expr "Else" expr
	| bool
	| int
\end{lstlisting}
 

Note that every `int` now also is an `expr`, just as every `bool` is an `expr`. This typing relationship can be visualized with `ALGT STFL.language -lsvg Subtyping.svg`[^slowsvgs] :

![Subtyping relation of STFL.language](Subtyping.png){width=25%}

[^slowsvgs]: Creating this svg might take a long time for complicated syntaxes, as ALGT calculates the ordering of labels resulting in the least intersecting lines.




Now that numbers have been added, let's run this with a number as example:

\begin{lstlisting}
42\end{lstlisting}


should give

\begin{lstlisting}[style=terminal]
# "42" was parsed as:
42: int.0

\end{lstlisting}


So far, so good! Time to add addition:

\begin{lstlisting}
expr	::= "If" expr "Then" expr "Else" expr
	| bool
	| int
\end{lstlisting}
 
		| expr "+" expr

We add some example:

\begin{lstlisting}
20 + 22
1 + 2 + 3
\end{lstlisting}



And run it:

\begin{lstlisting}[style=terminal]
Error:
  While checking the syntax:
    Potential infinite left recursion detected in the syntax.
    Left cycles are:
        expr -> expr
    

\end{lstlisting}

Oops! Looks like we did something wrong. What is this left recursion?
Whenever the parser wants to parse an expression, it tries every choice from left to right. This means that whenever it tries to parse `expr`, it should first try to parse `expr`. That's not really helpfull, so the parser might get in an infinite loop then. 

Not allowing left recursion also means that no loops in the subtypings occur. In other words, the subtyping relationship is a [lattice](https://en.wikipedia.org/wiki/Lattice_(order)).

The solution to this problem is splitting `expr` in two parts: a `term` with simple elements in front, and `expr` with advanced forms:

\begin{lstlisting}
expr	::= term
	| term "+" expr
term	::= "If" expr "Then" expr "Else" expr
	| bool
	| int
\end{lstlisting}


Let's retry this:

\begin{lstlisting}[style=terminal]
Error:
  While checking the syntax:
    While checking for dead choices in expr:
      The choice 'term "+" expr' will never be parsed.
      The previous choice 'term' will already consume a part of it.
      Swap them and you'll be fine.
      
    

\end{lstlisting}

What went wrong this time? The parser tries choice after choice. When parsing `20 + 22` against `expr ::= term | term "+" term`, it'll first try `term` (and not `term "+" term`). It succesfully parses `20` against the lonely `term`, thus the input string `+ 22` is left. The parser doesn't know what to do with this leftover part, so we get an error.

To fix this, we change the order:

\begin{lstlisting}
expr	::= term "+" expr
	| term
\end{lstlisting}


When we try again, we get:

\begin{lstlisting}[style=terminal]
# "20 + 22" was parsed as:
+  expr.0
|  20: int.0
|  "+": expr.0
|  22: int.0
# "1 + 2 + 3" was parsed as:
+  expr.0
|  1: int.0
|  "+": expr.0
|  +  expr.0
|  |  2: int.0
|  |  "+": expr.0
|  |  3: int.0

\end{lstlisting}


![Parsetree of `20+22`](Parsetree6_0.png){width=50%}


![Parsetree of `1 + 2 + 3`](Parsetree7_0.png){width=50%}



### Lambda expressions

The lambda expression is the last syntactic form we'd like to add. Recall that these look like `(\x : Int . x + 1)`. 

#### Variables

The first thing we should deal with, are variables. A builtin is provided for those, namely `Identifiers`, matching all words starting with a lowercase (matching `[a-z][a-zA-Z0-9]*`). Let's introduce them in our syntax:

\begin{lstlisting}
var	::= Identifier\end{lstlisting}


A `var` is valid in expressions too, e.g. in the expression `x + 1`, so we want to add it to our `term`:

\begin{lstlisting}
term	::= "If" expr "Then" expr "Else" expr
	| bool
	| int
	| var
\end{lstlisting}


#### Types

The second ingredient we still need, are types, to annotated the input types. Valid types, for starters, are `Bool` and `Int`.

But what is the type of `(\x : Int . x + 1)`? It's something that takes an `Int` and gives back an `Int`. We type this as `Int -> Int`.

And what is the type of a function, taking another function as an argument? That would be, for example, `(Int -> Int) -> Int`, meaning we need to add a form with parentheses.

Recalling the trouble we had with left recursion and ordering, we write `type` as following: 

\begin{lstlisting}
basetype::= "Bool" | "Int" | "(" type ")"
type	::= basetype "->" type | basetype
\end{lstlisting}



Some examples of types are:


![](Parsetree0_0.png){width=50%}
![](Parsetree1_0.png){width=50%}

![](Parsetree2_0.png){width=50%}
![](Parsetree3_0.png){width=50%}
![](Parsetree3_0.png){width=50%}





### Lambda expressions

Now we have what we need to define lambda expressions. As they act as a term, we add it there:

\begin{lstlisting}
term	::= "If" expr "Then" expr "Else" expr
	| "(" "\\" var ":" type "." expr ")"
	| bool
	| int
	| var
\end{lstlisting}


Backslash is the escape character, so use two of them to represent a single backslash.


We can also apply arguments to a lambda expression. We expand `expr`:

\begin{lstlisting}
expr	::= term "+" expr
	| term expr
	| term
\end{lstlisting}





### What about nonsensical input?

With the current syntax, expresions as `If 5 Then True else False`, `True + 5`, `True 5` or `(\x : Int : x + 1) True` can be written.
We allow these forms to be parsed, as the next stage of the compiler (the typechecker) will catch these errors. How to construct this, will be explained in [a following section](#building-a-typechecker).


### Recap

Our _STFL.language_ contains

\begin{lstlisting}
 
 STFL 
******

# A Simply Typed Functional Language

 Syntax
========

basetype::= "Bool" | "Int" | "(" type ")"
type	::= basetype "->" type | basetype

bool	::= "True" | "False"
int	::= Number
var	::= Identifier

expr	::= term "+" expr
	| term expr
	| term


term	::= "If" expr "Then" expr "Else" expr
	| "(" "\\" var ":" type "." expr ")"
	| bool
	| int
	| var
typing			::= var ":" type
typingEnvironment	::= typing "," typingEnvironment | "{}"

 Functions
===========


domain 				: type -> type
domain("(" T ")")		= domain(T)
domain(("(" T1 ")") "->" T2)	= T1
domain(T1 "->" T2)		= T1

codomain 			: type -> type
codomain("(" T ")")		= codomain(T)
codomain(T1"->" ("(" T2 ")")) 	= T2
codomain(T1 "->" T2)		= T2


 Relations
===========

# (:) is reserved, as predicate that a term is of a production rule
# one can refer to rewrite rules/functions earlier on

(→)	: expr (in), expr (out)		Pronounced as "evaluation"
(→*)	: expr (in), expr (out)		Pronounced as "big step"
(✓)	: expr (in)			Pronounced as "is canonical"

(⊢)	: typingEnvironment (in), expr (in), type (out)	Pronounced as "context entails typing"

(::)	: expr (in), type (out)	Pronounced as "type in empty context"

(==)	: type (in), type (in)	Pronounced as "equals"


 Rules
=======




 expr0 → expr1
----------------------------		[EvalCtx]
 expr[expr0] → expr[expr1]



 n1:int		n2:int
------------------------------------	[EvalPlus]
 n1 "+" n2 → !plus:int(n1, n2)




---------------------------------------		[EvalIfTrue]
 "If" "True" "Then" e1 "Else" e2 → e1


----------------------------------------	[EvalIfFalse]
 "If" "False" "Then" e1 "Else" e2 → e2


------------------------------------------------------------	[EvalLamApp]
 ("(" "\\" var ":" type "." e ")") arg → !subs:e(var, arg, e)





 b:bool
-------			[CanonBool]
 (✓) b

 n:int
---------		[CanonNumber]
 (✓) n



 (✓) e
--------			[BigStepCanon]
 e →* e

 e0 → e1	e1 →* e2
-------------------------	[BigStepRec]
 e0 →* e2

 e0 → e1	(✓) e1
------------------------	[BigStepBase]
 e0 →* e1





 "{}" ⊢ e, T  
-----------	[TEmptyCtx]
 e :: T




 n:int
---------------		[Tnumber] 
 Γ ⊢ n, "Int"


 b:bool
----------------	[Tbool]
 Γ ⊢ b, "Bool"






--------------------	[Tx]
 Γ[ x ":" T ] ⊢ x, T


 Γ ⊢ n1, "Int"	Γ ⊢ n2, "Int"
------------------------------				[TPlus]
 Γ ⊢ n1 "+" n2, "Int"


 Γ ⊢ c, "Bool"	Γ ⊢ e1, Tl	Γ ⊢ e2, Tr	Tl == Tr
--------------------------------------------------------	[TIf]
 Γ ⊢ "If" c "Then" e1 "Else" e2, Tl



 ((x ":" T1) "," Γ) ⊢ e, T2
-------------------------------------------------------	[TLambda]
 Γ ⊢ "(" "\\" x ":" T1 "." e ")", ( "(" T1 ")") "->" T2



 Γ ⊢ e1, Tfunc	Γ ⊢ e2, Targ	Targ == domain(Tfunc)
------------------------------------------------------	[Tapp]
 Γ ⊢ e1 e2, codomain(Tfunc)






 T1 = T2 : type
--------------- [EqBase]
 T1 == T2


 T1 == T2
----------------- [EqParL]
 "(" T1 ")" == T2

 T1 == T2
----------------- [EqParR]
 T1 == "(" T2 ")"


 Ta1 == Ta2	Tb1 == Tb2
---------------------------------- [EqArrow]
 Ta1 "->" Tb1   ==   Ta2 "->" Tb2


 Properties
============

 e0 :: T	 e0 → e1
-------------------------- [Preservation]
         e1 :: T


         e0 :: T
-------------------------- [Progress]
 (✓) e0    |     e0 → e1 



# STFL can't contain loops (or recursion), so will always terminate

 
 e :: T
-------- [Termination]
 e →* v 

\end{lstlisting}


Our _examples.stfl_ contains

\begin{lstlisting}
True
False
If True Then False Else True
If If True Then False Else True Then True Else False
42
20 + 22
1 + 2 + 3
(\x : Int . x + 1) 41
\end{lstlisting}


We run this with

 - `ALGT STFL.language examples.stfl expr -l` to show the parsetrees
 - `ALGT STFL.language examples.stfl expr -l --ptsvg SVGnames` to render the parsetrees as SVG
 - `ALGT STFL.language --lsvg SVGname.svg` to visualize the subtyping relationship.

![The final subtyping relationship of STFL.language](FinalSubtyping.png)







 Functions
-----------

For a full reference on syntax, see the [reference manual on functions](#functions-1).

### Domain and codomain

It'll come in handy later on to be able to calculate the _domain_ and _codomain_ of a function type.
The _domain_ of a function is the type it can handle as input.
The _codomain_ of a function is the type it gives as output.

:Examples of domain and codomain

Function type		dom		cod
-------------		-----		-----
`Int -> Bool`		`Int`		`Bool`
`(Int -> Bool)`		`Int`		`Bool`
`Int -> Bool -> Bool`	`Int`		`Bool -> Bool`
`Int -> (Bool -> Bool)`	`Int`		`Bool -> Bool`
`(Int -> Bool) -> Bool`	`Int -> Bool`	`Bool -> Bool`
`Int`			_Undefined_	_Undefined_
`Bool`			_Undefined_	_Undefined_


Now, let's define these functions!

### The function section

We add a new header to _STFL.language_:

\begin{lstlisting}
 Functions
===========

\end{lstlisting}


In this function section, we can define the function _dom_ in the following way:

\begin{lstlisting}
domain 				: type -> type
domain(T1 "->" T2)		= T1\end{lstlisting}


So, what is going on here? Let's first take a look to the first line:

\begin{lstlisting}
domain 				: type -> type\end{lstlisting}


The `domain` is the name of the function. The `type -> type` indicates what syntactic form is taken as input (a `type`) before the `->` and what is given as output (again a `type`). [^ISMETA]

![Relevant XKCD (by Randall Munroe, #917)](hofstadter.png){width=100%}

[^ISMETA]: You probably noticed the similarity between the types declared in our own STFL and this declaration. This is intentional.


### Pattern matching

Let's have look at the body of the function:

\begin{lstlisting}
domain(T1 "->" T2)		= T1\end{lstlisting}


What happens if we throw in `Int -> Bool`? Remember that this is parsed as a tree, with three leafs. Notice that there are three elements in the pattern match to: a variable `T1`, a literal `->` and a variable `T2`. These leafs are matched respectively:

![Pattern matching in action](TypeTrees0annot.png)


The same principle applies with more advanced inputs:

![Pattern matching in action (more advanced)](TypeTrees1annot.png)

We thus always bind `T1` to the part before the top-level `->`, in other words: we always bind the input type (or domain type) to `T1`. As that is exactly what we need, we return it!

### Missing cases

This already gives us the most important part. However, what should we do if the first argument type is a function? e.g. `(Int -> Bool) -> Bool`:

![Typetree of function argument](TypeTrees2.png){width=50%}

This will match the pattern `T1 "->" T2` as following:

![Typetree of function argument, matched](TypeTrees2annot.png){width=50%}

We see that `T1` includes the `(` and `)`, which we don't want. We simply solve this by adding a extra clause:


\begin{lstlisting}
domain 				: type -> type
domain(("(" T1 ")") "->" T2)	= T1
domain(T1 "->" T2)		= T1\end{lstlisting}


We expect that the part _before_ the arrow now is surrounded by parens. Note that we put parens once without double quotes and once with. These parens capture this part of the parsetree and match it against the patterns inside the parens, as visible in the green ellipse:

![Recursive pattern matching](TypeTrees2annot1.png){width=50%}

### Recursion

There is a last missing case, namely if the entire type is wrapped in parens.

We match a type between parens, and then calculate its domain simply by calling the domain function again.

\begin{lstlisting}
domain 				: type -> type
domain("(" T ")")		= domain(T)
domain(("(" T1 ")") "->" T2)	= T1
domain(T1 "->" T2)		= T1
\end{lstlisting}



### Clause determination

As you can see, there are two clauses now. How do we now what clause is executed?

Simply put, when the function is evaluated, the arguments are pattern matched against the first clause. If this pattern match succeeds, the expression on the right hand side is returned. Do the arguments not match the patterns? Then the next clause is considered. 

In other words, clauses are tried **from top to bottom** and tested. The first clause of which the patterns match, is executed.

When no clauses match, an error message is given.

### Executing functions

Let's put this to a test. We can calculate the domain of our examples.

Create a file `typeExamples.stfl`, with contents

\begin{lstlisting}
Int -> Bool
(Int -> Bool)
Int -> Bool -> Bool
Int -> (Bool -> Bool)
(Int -> Bool) -> Bool
Int
Bool
\end{lstlisting}


Run this with `ALGT STFL.language typeExamples.stfl type -l -f domain`:

\begin{lstlisting}[style=terminal]
Warning:
  While checking the totality of function "domain":
    Following calls will fall through:
      domain("Bool")
      domain("Int")
  

# "Int -> Bool" applied to domain
Int

# "(Int -> Bool)" applied to domain
Int

# "Int -> Bool -> Bool" applied to domain
Int

# "Int -> (Bool -> Bool)" applied to domain
Int

# "(Int -> Bool) -> Bool" applied to domain
Int -> Bool

# "Int" applied to domain
Not a single clause matched:
  Could not pattern match "Int" over "(" T ")"
  Could not pattern match "Int" over ("(" T1 ")") "->" T2
  Could not pattern match "Int" over T1 "->" T2

# "Bool" applied to domain
Not a single clause matched:
  Could not pattern match "Bool" over "(" T ")"
  Could not pattern match "Bool" over ("(" T1 ")") "->" T2
  Could not pattern match "Bool" over T1 "->" T2

\end{lstlisting}

What does this output tell?

For starters, we get a warning that we forgot two cases, namely `Bool` and `Int`. For some functions this is a problem, but domain is not defined for those values.

Then, we see an overview for each function what result it gives (or an error message if the pattern matches failed).

If you want more information about the behaviour of a function, specify `--ia` or `--ifa FUNCTION-TO-ANALYZE` to get a clause-per-clause overview:

\begin{lstlisting}[style=terminal]

 Analysis of domain : type -> type 
===================================

    
     Analysis of clause 0 
    ......................
    
    Clause: 
      domain("(" T ")")        = domain(T)
    
    Possible inputs at this point: 
    #  (type)
    
    
    Possible results: 
    0   ("("::basetype type0/1/2:1 ")"::basetype)::basetype(gen: basetype/2)	--> type (Function call - ID not retrievable) : "type"
    
    
    
    
    
     Analysis of clause 1 
    ......................
    
    Clause: 
      domain(("(" T1 ")") "->" T2)
                               = T1
    
    Possible inputs at this point: 
    #  ("Bool")
    #  ("Int")
    #  ((basetype "->" type))
    
    
    Possible results: 
    1   (("("::basetype type0/0:0/2:1 ")"::basetype)::basetype(gen: basetype/2) "->"::type type0/0:2)::type(gen: type/0)	--> type0/0:0/2:1   : "type"
    
    
    
    
    
     Analysis of clause 2 
    ......................
    
    Clause: 
      domain(T1 "->" T2)       = T1
    
    Possible inputs at this point: 
    #  ("Bool")
    #  ("Int")
    #  (("Bool" "->" type))
    #  (("Int" "->" type))
    
    
    Possible results: 
    2   ("Bool"::basetype "->"::type type0/0:2)::type(gen: type/0)	--> "Bool"::basetype : "basetype"
    2   ("Int"::basetype "->"::type type0/0:2)::type(gen: type/0)	--> "Int"::basetype : "basetype"
    
    
    
    
  
   Falthrough 
  ------------
  
  ("Bool")
  ("Int")
  
  
\end{lstlisting}


### Codomain

`codomain` can be implemented totally analogously:

\begin{lstlisting}
codomain 			: type -> type
codomain("(" T ")")		= codomain(T)
codomain(T1"->" ("(" T2 ")")) 	= T2
codomain(T1 "->" T2)		= T2
\end{lstlisting}
 



 Relations and Rules
---------------------

## Building smallstep

## Building bigstep

## Building a typechecker


 Properties
------------


 Recap: used command line arguments
------------------------------------





 Reference manual
==================

 General
---------

A language is defined in a `.language`-file. It starts (optionally) with a title:

\begin{lstlisting}

 Language Name
***************

\end{lstlisting}

Comments start with a `#` and can appear quasi everywhere.

\begin{lstlisting}

# This is a comment
\end{lstlisting}

Syntax, functions, relations, ... are all defined in their own sections:


\begin{lstlisting}

 Syntax
========

\end{lstlisting}


A section header starts with an upper case, is underlined with `=` and followed by a blank line.


 Syntax
--------


All syntax is defined in the `Syntax` section. It consists out of `BNF`-rules, of the form

\begin{lstlisting}
name	::= "literal" | choice | seq1 seq2

\end{lstlisting}

Choices might be written on multiple lines, as long as at least one tab precedes them:

\begin{lstlisting}
name	::= choice1 | choice2
	| choice3

\end{lstlisting}

### Literals

A string that should be matched exactly, is enclosed in `"` (double quotes). Some characters can be escaped with a backslash, namely:

:Escape sequences

Sequence	Result
--------	------
`\n`            newline
`\t`            tab
`\"`            double quote
`\\`            backslash



### Parsing order

Rules are parsed __left to right__, in other words, choices are tried in order. No backtracking happens when a choice is made; the parser is a _recursive descent parser_.
This has two drawbacks: left recursion results in an infinite loop and the ordering of choices does matter.

\begin{lstlisting}
# Left recursion, error message.
expr	::= expr "+" int

# `int` already consumes a part of `int + int`, error message
expr	::= int | int "+" int

# Common part extraction, error message

# Correct
expr	::= int "+" int | int
\end{lstlisting}


### Builtin syntactic forms

Some syntactic forms are already provided for your convenience, namely:

:Builtin syntax


|Builtin	|Meaning				|Regex
|:--------------|:--------------------------------------|:--------------
`Identifier`|Matches an identifier|`[a-z][a-zA-Z0-9]*`
`Number`|Matches an (negative) integer. Integers parsed by this might be passed into the builtin arithmetic functions.|`-?[0-9]*`
`Lower`|Matches a lowercase letter|`[a-z]`
`Upper`|Matches an uppercase letter|`[A-Z]`
`Digit`|Matches an digit|`[0-9]`
`String`|Matches a double quote delimted string|`"([^"\]|\"|\\)*"`



### Subtyping relationship

A syntactic form equals a (possibly infinite) set of strings. By using a syntactic form `a` as choice in other syntactic form `b`, `a` will be a subset of be, giving the natural result that `a` is a subtype of `b`.

In the following examle, `bool` and `int` are both subsets of `expr`. This can be visualised with the `--lsvg Output.svg`-flag. 


	bool	::= "True" | "False"
	int	::= Number
	expr	::= ... | bool | int


### Whitespace in sequences

Whitespace (the characters `" "`,`"\t"`), is parsed by default (and ignored completely). If you want to parse a whitespace sensitive language, use other symbols to declare the rule:

:Whitespace modes

Operator	Meaning	
--------	-------
`::=`           Totally ignore whitespace
`~~=`           Parse whitespace for this rule only
`//=`           Parse whitespace for this rule and all recursively called rules


This gives rise to the following behaviour:

:Whitespace mode examples

+-----------------------+-------------------------------+
| Syntax 		| Matching String		|
+=======================+===============================+
| `a ::= "b" "c" x`	| `b c x y`			|
+-----------------------+-------------------------------+
| `x ::= "x" "y"`	| `bcxy`			|
+-----------------------+-------------------------------+
|			| `b\tc\tx\ty`			|
+-----------------------+-------------------------------+
|			| `b c\txy`			|
+-----------------------+-------------------------------+
|			| ...				|
+-----------------------+-------------------------------+
|			| 				|
+-----------------------+-------------------------------+
| `a ~~= "b" "c" x`	| `bcx y`			|
+-----------------------+-------------------------------+
| `x ::= "x" "y"`	| `bcxy`			|
+-----------------------+-------------------------------+
|			| `bcx\ty`			|
+-----------------------+-------------------------------+
|			| 				|
+-----------------------+-------------------------------+
| `a //= "b" "c" x`	| `bcxy`			|
+-----------------------+-------------------------------+
| `x ::= "x" "y"`	| 				|
+-----------------------+-------------------------------+


 Functions
-----------

### Patterns and expressions

| Expr			| Name		|  As expression						
|:----------------------|:--------------|:-------------------------------------
`x` | Variable | Recalls the parsetree associated with this variable
`42` | Number | This number
`"Token"` | Literal | This string
`func(arg0, arg1, ...)` | Function call | Evaluate this function
`!func:type(arg0, ...)` | Builtin function call | Evaluate this builtin function, let it return a `type`
`(expr or pattern:type)` | Ascription | Checks that an expression is of a type. Bit useless
`e[expr or pattern]` | Evaluation context | Replugs `expr` at the same place in `e`. Only works if `e` was created with an evaluation context
`a "b" (nested)` | Sequence | Builds the parse tree


| Expr				| As pattern			
|:------------------------------|:---------------------------------------------
`x` | Captures the argument as the name. If multiple are used in the same pattern, the captured arguments should be the same or the match fails.
`42` | Argument should be exactly this number
`"Token"` | Argument should be exactly this string
`func(arg0, arg1, ...)` | Evaluates the function, matches if the argument equals the result. Can only use variables which are declared left of this pattern
`!func:type(arg0, ...)` | Evaluates the builtin function, matches if the argument equals the result. Can only use variables which are declared left of this pattern
`(expr or pattern:type)` | Check that the argument is an element of `type`
`e[expr or pattern]` | Matches the parsetree with `e`, searches a subpart in it matching `pattern`
`a "b" (nested)` | Splits the parse tree in the appropriate parts, pattern matches the subparts





### Typechecking


#### Equality

\begin{lstlisting}

 Syntax
========

a 	::= ...
b	::= ...
c	::= a | b | d

 Functions
===========

f	: a -> b -> c
f(x, x)	= x
\end{lstlisting}

When equality checks are used in the pattern matching, the variable will be typed as the smallest common supertype of both types.
If such a supertype does not exist, an error message is given.

Note that using a supertype might be a little _too_ loose, but won't normally happen in real-world examples.

In the given example, `x` will be typed as `c`, the common super type. In this example, `x` might also be a `d`, while this is not possible for the input.
This can be solved by splitting of `a | b` as a new rule.


### Totality- and liveabilitychecks

### Higher order functions and currying?

Are not possible for now (v 0.1.15.1). Perhaps in a future version or when someone really needs it and begs for it.

### Builtin functions

 name	| Descr						| Arguments
--------+-----------------------------------------------+-------------
`plus`	 | Gives a sum of all arguments (0 if none given)	 | Ints only
`min`	 | Gives the first argument, minus all the other arguments	 | At least 1, Ints only
`mul`	 | Multiplies all the arguments. (1 if none given)	 | Ints only
`div`	 | Gives the first argument, divided by the product of the other arguments. (Integer division, rounded down))	 | At least 1, Ints only
`mod`	 | Gives the first argument, module the product of the other arguments.	 | At least 1, Ints only
`neg`	 | Gives the negation of the argument	 | Exactly 1, Ints only
`equal`	 | Checks that all the arguments are equal. Gives 1 if so, 0 if not.	 | At least 2
`error`	 | Stops the function, gives a stack trace. When used in a rule, this won't match a predicate	 | 
`freshvar`	 | Generates an identifier not present in the arguments. If the first argument is an identifier, identifiers are based on that form.	 | 
`subs`	 | (expression to replace, to replace with, in this expression) Replaces each occurence of the first expression by the second, in the third argument	 | Exactly 3



 Relations and Rules
---------------------


 Properties
------------


 Command line flags
--------------------



 Used concepts and algorithms
==============================



 Dynamization and gradualization
=================================

 Thankword
===========

Thanks to

- Christophe Scholliers
- Ilion Beyst, for always being up to date, giving ideas, spotting bugs at first sight, implementing `Nederlands` and his enthousiasm in general
- Isaura Claeys, for proofreading and finding a lot of typos
