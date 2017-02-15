

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


 Functions
-----------



 Relations and Rules
---------------------


 Properties
------------


 Command line flags
--------------------


