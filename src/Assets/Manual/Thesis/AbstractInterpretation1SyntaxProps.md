
\clearpage

Properties of the syntax
-------------------------

Abstract interpretation gives us a way to lift metafunctions over parsetrees to metafunctions over sets of parsetrees. To efficiently perform computations in this abstract domain, a representation for these sets should be constructed, exploiting the underlying structure of syntaxes. Here, we study the necessary properties which are exploited to construct a set representation in the next part.

Some of these properties are inherent to each syntax, others should be enforced.
For these, we present the necessary algorithms to detect these inconsistencies, both to help the programmer and to allow abstract interpretation.

### Syntactic forms as sets

When a syntactic form is declared, this is equivalent to defining a set.

The declaration of `bool ::= "True" | "False"` is equivalent to declaring $bool = \{\code{True}, \code{False} \}$. 

\begin{figure}[h!]
\center
\input{SyntaxSets.tex}
\end{figure}

This equivalence between syntactic forms is the main driver for the approach.


### Subtyping

When a syntactic form _a_ uses another bare syntactic form _b_ in its definition, we say _a_ embeds _b_ it.
In the following example, the set `expr` consists of `{"True", "False", "0", "1", "2", "3", ...}`, containing both `bool` and `int`.

	bool	::= "True" | "False"
	int	::= Number       # Number is a builtin, parsing integers
	expr	::= bool | int

This effectively establishes a _supertype_ relationship between the different syntactic forms. We can say that _every `bool` is a `expr`_, or `bool <: expr`.

This supertype relationship is a lattice - the absence of left recursion implies that no cycles can exist in this supertype relationship.
This lattice can be visualized, as in figure \ref{subtyping}.



\begin{figure}[h!]
\center
\includegraphics[width=0.4\linewidth]{Subtyping.png}
\caption{A simple subtyping relationship}
\label{subtyping}
\end{figure}



### Empty sets

Empty strings are not allowed. Consider syntax:

	a	::= "=" | ""
	b	::= "x" a "y"
	c	::= a as

Parsing `b` over string `x y` is ambiguous. Should the parsetree contain an element representing an empty `a` or not?
Parsing `c` is even more troublesome: the parser might return an infinite list, containing only empty `a`-elements. 

Empty rules are just as troublesome and are not allowed as well:

	a	::= 		# empty


This also includes all kind of degenerate recursive calls:

	a	::= a

	a	::= b
	b	::= a

Note that an empty set, by necessity, can only be defined by using _left recursion_.


### Left recursive grammers

We allow recursive definitions, this is, we allow syntactic forms to be defined in terms of itself:

	type	::= baseType "->" type | ...

Left recursion is when this recursion is used on a leftmost position of a sequence:

	a ::= ... | a "b" | ...
	

While algorithms, such as _LALR-parsers_ can handle this fine, we don't allow these.

First, this makes it easy to port a syntax created for ALGT to another parser toolchain - which possibly can't handle left recursion too.
Second, this allows for a extremely easy parser implementation.
Thirdly, this prevents having empty sets such as `a ::= a`.

We can easily detect this left recursion algorithmically, with a fixpoint algorithm. Consider the following syntax:

	a	::= "a" | "b" | "c" "d"
	b	::= a
	c	::= b | c "d"

First, we remove the tail from each sequence, e.g. sequence `"c" "d"` becomes `"c"`:

	a	::= "a" | "b" | "c"
	b	::= a
	c	::= b | d
	d	::= c

Now, we remove all tokens, thus everything that is not a call to another syntactic form:

	a	::= 		# empty
	b	::= a
	c	::= b | d
	d	::= c

At this point, we enter the main loop of the fixpoint. We remove all empty rules and their calls, until no rule can be removed:


	b	::=		# empty
	c	::= b | d
	d	::= c

Next iteration:

	c	::= d
	d	::= c

At this point, no rules can be removed anymore. Only rules containing left recursion remain, for which an error message can be generated.


### Uniqueness of sequences

When a parsetree is given, we want to be able to pinpoint exactly which syntactic form parsed it.

	a ::= ... | "a" | ...
	b ::= ... | "a" | ...

	x ::= "x"
	c ::= ... | a x | ...
	d ::= ... | a x | ...

A parsetree containg `"a"` could be parsed with both `a` and `b`, which is undesired; the sequence `"a" "x"` could be parsed with both `c` and `d`.
To detect this, we compare each sequences with each every other sequence for equality.
When such duplicate sequences exist, we demand the programmer to refactor this sequence into a new rule:

	aToken	::= "a"
	a ::= ... | aToken | ...
	b ::= ... | aToken | ...

	x ::= "x"
	ax ::= a x
	c ::= ... | ax | ...
	d ::= ... | ax | ...


This is not foolproof though. Some sequences might embed each other, as in following syntax:

	a ::= "a"
	b ::= a | "b"

	c ::= "c"
	d ::= c | "d"

	x ::= a d
	y ::= b c

Here, the string `a c` might be parsed with both syntactic forms `x` and `y`. There is no straightforward way to refactor this,
without making things overly complicated. Instead, we'll opt to work with runtime annotations on the parsetree which rule parsed it.

The uniqueness-constraint is merely added to keep things simpler and force the language designer to write a language with as little duplication as possible.








