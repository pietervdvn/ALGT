 

Representing sets of values
---------------------------

In this chapter, a general **representation** for a (possibly infinite) set of parsetrees is constructed. This representation is used as abstract domain for metafunctions, lifting any metafunction from a metafunction on parsetrees to a metafunction of sets of parsetrees.
This set representation is constructed using the properties outlined in the previous chapter, using a small syntax as example:

	baseType	::= "Bool" | "Int"
	typeTerm	::= baseType | "(" type ")"
	type		::= typeTerm "->" type | typeTerm


### Sets with concrete values

A set with only concrete values is simply represented by giving its inhabitants; the set `baseType` is thus represented as following:

	{ "Bool", "Int" }

We might also represent sequences of concrete values, in a similar way:

	{"Bool" "->" "Bool"}

We could also create a set with, for example, all function types with one argument:

	{ "Bool" "->" "Bool"
	, "Bool" "->" "Int"
	, "Int" "->" "Bool"
	, "Int" "->" "Int" }



### Symbolic sets

A set can also be represented _symbolically_. For example, we might represent `baseType` also as:

	{ baseType } = { "Bool", "Int" }

While concrete values are written with double quotes around them, symbolic representations are not. 

We can also use this symbolic representation in a sequence, with any number of concrete or symbolic values:

	  { baseType "->" baseType } 

Which would be a succint notation for:

	= { "Int" "->" baseType, "Bool" "->" baseType}
	= { "Bool" "->" "Bool"
	  , "Bool" "->" "Int"
	  , "Int" "->" "Bool"
	  , "Int" "->" "Int" }

### Infinite sets

This symbolic representation gives rise to a natural way to represent infinite sets through inductive definitions, such as `typeTerm`:

	type ::= { baseType, "(" type ")" } 
	     = { "Bool", "Int", "(" typeTerm "->" type ")" , "(" typeTerm ")"}
	     = { "Bool", "Int", "(" "Bool" ")", "(" "Int" ")", ...
	     = ...

A symbolic representation is thus a set containing sequences of either a concrete value or a symbolic value.

### Set representation of a syntax

This means that the BNF-notation of a syntax can be easily translated to this symbolic representation. Each choice in the BNF is translated into a sequence, rulecalls are translated into their symbolic value.

This is equivalent to the BNF-notation.


	baseType	::= "Bool" | "Int"
	typeTerm	::= baseType | "(" type ")"
	type		::= typeTerm "->" type | typeTerm

becomes

	baseType == {"Bool", "Int"}
	typeTerm == {baseType, "(" type ")"}
	type	 == {typeTerm "->" type, typeTerm}


Note that, per inclusion, `baseType` is a subset of `typeTerm`, and `typeTerm` is a subset of `type`. 

### Defining α and γ

Now that we have acquired this representation, we might define the _abstraction_ and _concretization_ functions for our actual abstract interpretation:

$$
\begin{array}{rcl}
\alpha(v) & = & \{v\} \\
\gamma(R) & = & R \\
compose(R, S) & = & R \cup S \\
\end{array}
$$

These definitions satisfy earlier mentioned properties trivially, _monotonicity_ and _soundness_.


\begin{lemma}
α (over sets) is monotone:\\
\[ \begin{array}{c}
 \text{As } \alpha(X) = X \\
X \subseteq Y \Rightarrow \alpha(X) \subseteq \alpha(Y)
\end{array}
 \]
\end{lemma}

\begin{lemma}
γ is monotone: \\
\[ \begin{array}{c}
 \text{As } \gamma(X) = X \\
X \subseteq Y \Rightarrow \gamma(X) \subseteq \gamma(Y)
\end{array}
 \]
\end{lemma}


\begin{lemma} α and γ are sound:
\[ \begin{array}{rl}
 &n \in \gamma(\alpha(n)) \\
= & n \in \gamma(\{n\}) \\
= & n \in \{n\}
\end{array}
\]
and
\[
\begin{array}{rl}
 & R \in \alpha(\gamma(R)) \\
= & R \in \alpha(R) \\
= & R \in \{ R \} \\
\end{array}
\]
\end{lemma}


With these, we can convert the concrete parsetree into the domain of sets. Furthermore, we know that using this interpretation makes sense. However, we're still lacking the operations to actually interpret functions with them.



