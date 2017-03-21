

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
$$builtinEscapes


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
$$builtinSyntax


### Subtyping relationship

A syntactic form equals a (possibly infinite) set of strings. By using a syntactic form `a` as choice in other syntactic form `b`, `a` will be a subset of be, giving the natural result that `a` is a subtype of `b`.

In the following examle, `bool` and `int` are both subsets of `expr`. This can be visualised with the `--lsvg Output.svg`-flag. 


	bool	::= "True" | "False"
	int	::= Number
	expr	::= ... | bool | int


### Whitespace in sequences

Whitespace (the characters $$whitespace), is parsed by default (and ignored completely). If you want to parse a whitespace sensitive language, use other symbols to declare the rule:

:Whitespace modes

Operator	Meaning	
--------	-------
$$wsModeInfo

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



### Grouping sequences

Sometimes, you'll want to group an entire rule as a token (e.g. comments, an identifier, ...)

Add a `$` after the assignment to group it. 


	
	text			::= LineChar line | LineChar
	commentLine		::= $ "#" text "\n"

	customIdentifier	::= $ Upper Number
	


When such a token is used in a pattern or expression, the contents of this token are parsed against this rule:


	f		: customIdentifier -> statement
	f("X10")	= "X9" "# Some comment"



Syntax Style
------------



### Builtin styles

Select a style with the `--style StyleName` flag.


$$styleMatrix


### Creating your own style

Select your own style with `--style Name.style` (the extension `.style` triggers the external style).

A stylefile looks as:


	 Name
	******

	 Constants
	===========

	# Here come shorthands for often used values, e.g.:
	black		= #000000
	stringVal	= "someString"
	intValue	= 14

	 Defaults
	==========

	# Here come the defaults, thus the general properties for every default line
	
	# literal assignment uses '='
	foreground-color	= #00ff00
	# recall of a constant defined in the first section uses ':'
	background-color	: black

	 Styles
	========

	# The different styles defined. See the matrix for what you might define

	 statement
	-----------

	font-style: italic

	 keyword (statement)
	---------------------
	
	# fallbackstyle between parentheses.
	# Attributes not found in this style will be first searched in 'statement'

	font-style: bold

	 string (keyword)
	------------------

	# Styles can be empty, to denote this, type a slash:

	/


### Supported renderers and attributes

$$styleSupported


 Functions
-----------

### Patterns and expressions


Functions transform their input. A function is declared by first giving its type, followed by one or more clauses:

	f		: a -> b
	f(a, "b")	= "c"
	f(a, b)		= "d"

When an input is given, arguments are pattern matched against the patterns on between parentheses. If the match succeeds, the expression on the right is given. If not, the next clause is given.

Note that using the same variable multiple times is allowed, this will only work if these arguments are the same:

	f(a, a)		= ...



Recursion can be used just as well:

	f("a" a)	= f(a)

This is purely functional, heavily inspired on Haskell.

#### Possible expressions

| Expr			| Name		|  As expression						
|:----------------------|:--------------|:-------------------------------------
$$expressionExamples


#### Possible patterns

| Expr				| As pattern			
|:------------------------------|:---------------------------------------------
$$patternExamples




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

Can be disabled with `--no-check`, when they take to long.

### Higher order functions and currying?

Are not possible for now (v $$version). Perhaps in a future version or when someone really needs it and begs for it.

### Builtin functions

 name | Descr					| Arguments	
------+-----------------------------------------+------------------------
$$builtinFunctions


 Relations and Rules
---------------------




 Properties
------------


 Command line flags
--------------------


