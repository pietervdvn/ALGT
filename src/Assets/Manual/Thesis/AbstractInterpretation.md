
Syntax Driven Abstract Interpretation
======================================

The question remains how to automatically interpret given functions.

ALGT has a symbolic calculator, its inner workings are explained here.

Properties of the syntax
-------------------------

### Syntactic forms as sets

When we create a syntactic form, we actually declare a set of possible values: `bool ::= "True" | "False"` is equivalent to decalring `bool ::= {"True", False"}'. This is a usefull insight, we will exploit this notation later on to perform abstract interpretation.

For practical reasons, we don't allow syntactic forms which are empty. This is in order to avoid ambiguities while parsing, allow optimazations, ...

This also implies that the _empty string_ is not a allowed.


### Syntax as lattice

When a syntactic form uses another bare syntactic form in its definition, we say it embeds it. In the following example, the set baseValue consists of `{"True", "False", "0", "1", "2", "3", ...}`, containing both `bool` and `int`.


	bool	::= "True" | "False"
	int	::=Number
	expr	::= bool | number

This effectively establishes a _supertype_ relationship between the different syntactic form. We can say that _every bool is a baseValue_, or `bool <: baseValue`.

This means that every syntax has a lattice implicitly. This lattice can be generated visualized, as in figure [subtyping] 

%% TODO image

![A simple subtyping relationship](Subtyping.png){width=25%}



### Empty sets

Empty strings are not allowed. Consider syntax:

	a	::= "=" | ""
	b	::= "x" a "y"
	c	::= a as

Parsing `b` over string `x y` is ambigous. Should the parsetree contain an element representing an empty `a` or not?
Parsing `c` is even more troublesome: the parser might return an infinite list, containing only empty `a`-elements. 

Empty rules are just as troublesome and are not allowed as well:

	a	::= 		# empty


This also includes all kind of degenerate recursive calls:

	a	::= a

	a	::= b
	b	::= a

Note that an empty set can only be defined by using _left recursion_.


### Left recursive grammers

Left recursion of a syntactic form is when a syntactic form is defined using its own definition, on a leftmost position of a sequence (e.g. `a ::= a "b"`).

While algorithms, such as _LALR-parsers_ can handle this, we don't allow these.

First, this makes it easy to port a syntax created for ALGT to another parser toolchain. Second, this allows for a extremely easy parser implementation.
Thirdly, this prevents having empty sets such as `a ::= a`.

We can easily detect this left recursion algorithmically, with a fix-point algorithm. Consider the following syntax:

	a	::= "a" | "b" | "c" "d"
	b	::= a
	c	::= b | c "d"

First, we remove the tail from each sequence, e.g. `"c" "d"` becomes `"c"`:

	a	::= "a" | "b" | "c"
	b	::= a
	c	::= b | d
	d	::= c

Now, we remove all tokens, thus all that is not a call to another syntactic form:

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





Representing sets of values
---------------------------

The first step to represent arbitrary syntactic sets. We will show how to do this, using following example syntax:


	baseType	::= "Bool" | "Int"
	typeTerm	::= baseType | "(" type ")"
	type		::= typeTerm "->" type | typeTerm


### Sets with concrete values

A set with only concrete values can be simply represented by giving its inhabitants; the set `baseType` can be represented the following way:

	{ "Bool", "Int" }

We might also represent sequences of concrete values, in a similar way:

	{"Bool" "->" "Bool"}

We could also create a set with, for example, all function types with one argument:

	{ "Bool" "->" "Bool"
	, "Bool" "->" "Int"
	, "Int" "->" "Bool"
	, "Int" "->" "Int" }

### Symbolic sets

Such a set can also be represented
 _symbolically_. For example, we might represent `baseType` also as:

	{ baseType } = { "Bool", "Int" }

While concrete values are written with double quotes around them, symbolic representations are not. 

We can also use this symbolic representation in a sequence, with any number of concrete or symbolic values:

	  { baseType "->" baseType } 
	= { "Int" "->" baseType, "Bool" "->" baseType}
	= { "Bool" "->" "Bool"
	  , "Bool" "->" "Int"
	  , "Int" "->" "Bool"
	  , "Int" "->" "Int" }

### Infinite sets

This symbolic representation gives rise to a natural way to represent infinite sets, such as `typeTerm`:

	{ baseType, "(" type ")" } 
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

Operations on representations
-----------------------------

### Unfolding

The first important operation is unfolding a single level of the symbolic representation, changing the symbolic values in the set by each of their definitions. As these definitions are important, unfolding must be done in context of them; otherwise some values are not defined.

Algorithmically unfolding a value is done as following:

Unfolding a concrete value, is just the concrete value itself:

	"Bool" == {"Bool"}

Unfolding a symbolic value is the set, as it is defined:

	baseType == { "Bool", "Int" } 
	type	 == {typeTerm "->" type, typeTerm}

WHen unfolding a symbolic value withing a sequence, it might be needed to keep these grouped. This prevents ambiguities later on.

Consider following definition:

	subtraction == {number "-" subtraction, number}

Unfolding this a few times towards subtraction, we might yield:

	subtraction == { ..., number "-" number "-" number, ... }

This is unclear. Would the syntactic expression `3 - 2 - 1` equal `(3 - 2) - 1 = 0` or `3 - (2 - 1) = 2`? Our syntax definition suggests the latter, as it is built in a right-associative way. To mirror this in the sequence, we add parentheses:

	subtraction == { ..., number "-" (number "-" number), ... }



To unfold a sequence, we unfold each of the parts and take a cartesian product:

	baseType "->" baseType
	== {"Bool", "Int"} × {"->"} × {"Bool", "Int"}
	== { "Bool" "->" "Bool"
	   , "Bool" "->" "Int"
	   , "Int" "->" "Bool"
	   , "Int" "->" "Int" }

At last, to fold a symbolic representation, we unfold each of the sequences in the set, and collect them:

	{baseType}
	== { {"Bool", "Int"} }
	== { "Bool", "Int"}

### Refolding

Refolding attempts to undo the folding process. While not strictly necessary, it allows for more compact representations throughout the algorithms - increasing speed- and more compact output - increasing readability.

For example, this would change `{"Bool", "Int", "Bool" "->" "Int"}` into `{baseType, "Bool" "->" "Int"}`, but also `{type, typeTerm, "Bool"}` into `{type}`


How does this work algorithmically? Actually, this is done in two steps:

- Group sets (e.g. `"Bool"` and `"Int"`) into their symbolic value (`baseType`)
- Filter away values that are included in another symbolic value (e.g. in `{"Bool", type}` can `"Bool"` be omitted, as it is included in `"type"`)

This is repeated until no further changes are possible on the set.


### Directed unfolds

Throughout the text, we will freely use _directed unfold_. This is an unfold of certain elements in the set, often the ones we conveniently use.

#### Grouping sets


Grouping simple sets is quite straightforward. To refold, we just look which known definitions are a subset.

E.g. given the definition `baseType == {"Bool", "Int}`, we can make the following refold:

	{"Bool", "Int", "Bool" "->" "Int"} == {baseType, "Bool" "->" "Int"}


Grouping complex sequences is not as straightforward as it seems.
Consider

	{ "Bool" "->" "Bool"
	, "Bool" "->" "Int"
	, "Int" "->" "Bool"
	, "Int" "->" "Int" }

How will we proceed to change this algorithmically? First, we sort these sequences in buckets where only a single element is different between the sequences.
	
	{"Bool" "->" "Bool", "Bool" "->" "Int"}
	{"Int" "->" "Bool", "Int" "->" "Int"}
	
We then single out the differences as a single set...

	"Bool" "->" {"Bool", "Int"}
	"Int" "->" {"Bool", "Int"}

... and unfold these recursively:

	"Bool" "->" baseType
	"Int" "->" baseType

This yields us a new set; `{"Bool" "->" baseType, "Int" "->" baseType}`. As the unfolding algorithm tries to reach a fixpoint, it will rerun. This would yield a new bucket:

	{"Bool" "->" baseType, "Int" "->" baseType}

Where the different element would this time be the first element:

	{"Bool", "Int"} "->" baseType

Refolding this yields our original expression `baseType "->" baseType`


#### Filtering out subvalues

The second step in the algorithm is the removal of already represented values.
Consider `{ baseType, "Bool"}`. In the definition of `baseType` is "Bool" included, thus it is unneeded here. We say that `baseType` _shadows_ `"Bool"`.

This can be done straightforward, by comparing each value in the set against each other value and checking wether this is contained in it.

### Addition of sets

Another extremely usefull and straightforward operation is the addition of two sets. This is joining both sets, optionally refolding it.

### Subtraction of sets

Subtraction of sets enavles a lot of usefull algorithms, but is quite complicated.

Subtraction is performed on each sequence in the set. This subtraction of a single value might result in no, one or multiple new values. 
There are a few cases to consider, depending on what is subtracted.

We will split them up as following:

- A concrete value is subtracted from a sequence
- A symbolic value is subtracted from a sequence
- A sequence is subtracted from a symbolic value
- A sequence is subtracted from a sequence


#### Subtraction of concrete values

Subtraction of a concrete value from a concrete sequence is straightforward: we check wether the sequence contains one single element and that this element is the same as the one we subtract from:

- `"Bool" - "Bool"` is empty
- `"Int" - "Bool"` is `"Int"`

If the sequence is a single symbolic value, we check if the symbolic value shadows the concrete value. If that is the case, we unfold and subtract that set recursively:

- `type - "Bool"` equals `{"Bool", "Int"} - "Bool"`, resulting in `{"Int"}`
- `type - "("` equals `type`

#### Subtraction of a symbolic value from a sequence

When a symbolic value is subtracted from a sequence, we first check wether this the sequence is shadowed by this symbolic value:

- `"Bool" - baseType` is empty, as `"Bool"` is contained and thus shadowed by `baseType
- `baseType - baseType` is empty, as baseType shadows itself
- `"Bool" - type` is empty as `"Bool"` is contained in `type` (via `typeTerm` and `baseType`) and thus shadowed
- `baseType - type` is empty too, as it is shadowed
- `("(" type ")") - type` is empty, as this is a sequence inside `typeTerm`

If the sequence is not shadowed, it might still subract a part of the sequence.


This is the case if the sequence shadows the symbolic value we wish to subtract. If that is the case, we unfold this sequence, and subtract each element in the set with the subtrahendum:

- `typeTerm - baseType` becomes `{baseType, "(" type ")"}`, resulting in `"(" type ")"`


### Subtraction of a sequence from a symbolic value

This is straightforward too. Given the symbolic value, we check wether it shadows the sequence we want to subtract. If this is the case, we unfold this symbolic value and subtract the sequence from each of the elements. If no shadowing occurs, we just return the symbolic value.

	`type - ("(" type ")") 
	= {typeTerm "->" type, typeTerm} - ("(" type ")")
	= {typeTerm "->" type, baseType, "(" type ")"} - ("(" type ")")
	= {typeTerm "->" type, baseType}

#### Subtraction of a sequence

##### Example

The last case is subtracting a sequence from another sequence. As this is rather complicated, we start with an example. As all good things in programming, this algorithm relies on recursion; we will follow the example through the call stack.

1. Consider `"(" type ")" - "(" "Bool" "->" type ")"`. It is intuitively clear that the parentheses `"("` and `")"` should remain, and that we want to subtract `type - "Bool" "->" type`.

2. Here we subtract a sequence again. We unfold `type`, to yield `{typeTerm, typeTerm "->" type} - ("Bool" "->" type)` [swap].

[^swap]: for practical reasons, I swapped the order of both elements in the text. As this is a set, that doesn't matter of course

3. Now we have two values in our set we should subtract:
- `typeTerm - ("Bool" "->" type)` yields `typeTerm`, as neither shadows the other.
- `(typeTerm "->" type) - ("Bool" "->" type)` can be aligned. It is clear we'll want to have `typeTerm - "Bool"` and `type - type` and let `"->"` unchanged.

4. `typeTerm - "Bool"` gives us `{baseType, "(" type ")"} - "Bool"`, resulting in `{"Int", "(" type ")"}`.
It is important to note that `(Bool)` is _still_ an element of this set, despite "Bool" without parentheses is not.
These are syntactical different forms having the same semantics! 

5. `type - type` is empty.


At this point, we have all the ingredients necessary, and we can put our subtraction back together.

4. The biggest puzzle is how to put `(typeTerm "->" type) - ("Bool" "->" type)` together. Intuitivelly, we'd expect it to be `{"Int" "->" type, "(" type ")" "->" type}`. More formally, the directed unfold of `typeTerm "->" type` is `{ "Bool" "->" type, "Int" "->" type, ("(" type ")") "->" type}`. Subtracting `"Bool" "->" type` yields our former result.

3. We put together the results of the recursive calls, being:
- `{typeTerm}`
- `{"Int" "->" type, "(" type ")" "->" type}`
Resulting in `{"Int" "->" type, "(" type ")" "->" type, typeTerm}`

2. We unfolded `type` to subtract the sequence, yielding `{"Int" "->" type, "(" type ")" "->" type, typeTerm}`

1. We put the parentheses back around each expression: `{"(" "Int" "->" type ")", "(" "(" type ")" "->" type ")", "(" typeTerm ")"}`, giving our desired result.

##### Algorithm

So, how do we algorithmically subtract two sequences?

Consider sequence `a b`, where `a` unfolds to `a1, a2, a3, ...` and `b` unfolds to `b1, b2, b3, ...`. This means that `a b` unfolds to `a1 b1, a1 b2, a1 b3, ... , a2 b1, a2 b2, ...`

Now, if we would subtract sequence `a1 b1` from this set, only a single value would be gone, the resulting cartesian product would remain nearly unchanged.

To do this, we first calculate the pointwise differences between the sequences. We align both sequences[^sameLength] and calculate the difference:


	a - a1= a - a1 = {a2, ...}
	b - b1 = {b2, ...}

[^sameLength]: This implies both sequences have the same length. If these don't have the same length, just return the original sequence as the subtraction does not make sense anyway.


Now, the resulting sequences are `{(a - a1) b, a (b - b1)}`. In other words, we replace each element of the sequence once with the resulting difference:

	a b c d ... - a1 b1 c1 d1 
	= {(a - a1) b c d, a (b - b1) c d, a b (c - c1) d, a b c (d - d1)} 

Applied on the example `(typeTerm "->" type) - ("Bool" "->" type)`, we get:

	
Pointwise:

	typeTerm - "Bool" = {"Int", "(" type ")"}
	"->" - "->"	= {}
	type - type	= {}

Resulting in:

	{(typeTerm - "Bool") "->" type
		, typeTerm ("->" - "->") type
		, typeTerm "->" (type - type)}
	= {{"Int", "(" type ")"} "->" type
		, typeTerm {} type
		, typeTerm "->" {}}
	= {{"Int", "(" type ")"} "->" type}
	= {"Int" "->" type, ("(" type ")") "->" type}


Algorithms using abstract interpretation
----------------------------------------

Now that we have our basic building blocks and operations, quite some usefull algorithms can be built using those.

### Calculation of the functiondomain

#### Single argument functions

Consider the following function:

	dom			: type -> type
	dom("(" t ")")	= dom(t)
	dom(T1 -> T2)	= T1


We translate the patterns in a straightforward way to sets, just like we did with the syntax. This can be done easily, as all patterns are typed.

Once this is done, we can just add these sets together, to get the domain:

	{"(" type ")", type "->" type}



We can also take the starting set `{type}` and subtract the patterns from it, to calculate which syntactic forms will _not_ match:

	type - {"(" type ")", typeTerm "->" type }
	= {typeTerm "->" type, typeTerm} - {"(" type ")", typeTerm "->" type }
	= {typeTerm} - {"(" type ")"}
	= {baseType, "(" type ")"} - {"(" type ")"}
	= {baseType}

Using the non-matching form and subtracting those from the input type, yields the same as earlier calculated:

	type - baseType
	= {typeTerm "->" type, typeTerm} - baseType
	= {typeTerm "->" type, baseType, "(" type ")"} - baseType
	= {typeTerm "->" type, "(" type ")"}

#### Multiple argument functions

Consider the function `equals`

	equals	: basetype -> basetype -> basetype
	equals("Bool", "Bool")		= "Bool"
	equals("Int", "Int")		= "Int"
	

For simplicity, we use `baseType`, to restrict input values solely to `{"Bool", "Int"}`. Also note that `("Bool", "Int")` is _not_ part of it's domain.


There are two ways to approach the domain of a function, using currying or considering all arguments at once. When using currying, the type signature would be read as `type -> (type -> type)`, thus that `equals` is a function that, given a value, produces a new function. For this analysis, this complicates things.

We rather consider the domain as a (multiple) list of representations: `[{type}, {type}]` and subtract away what _can_ be matched. What rests is the part _not_ in the domain; the domain is defined as all arguments not in the fallthrough.



We calculate the difference between two arguments analogously as the difference between two sequences. This is, we take _n_ copies of the arguments, where _n_ is the number of arguments, and subtract each argument once.

For clause 1, this gives:

	[baseType, baseType] - ["Bool", "Bool"]
	= {[baseType - "Bool", baseType], [baseType, baseType - "Bool"}
	= {["Int", baseType], [baseType, "Int"]}


We repeat this process for each clause, thus for clause 2, this gives:

	{["Int", baseType], [baseType, "Int"]} - ["Int", "Int"]
	= { ["Int", baseType] - ["Int", "Int"]
	  , [baseType, "Int"] - ["Int", "Int"] }
	= { ["Int" - "Int", baseType]
	  , ["Int", baseType - "Int"]
	  , [baseType - "Int", "Int"]
	  , [baseType, "Int" - "Int"] }
	= { [ {} , baseType]
	  , ["Int", "Bool"]
	  , ["Bool", "Int"]
	  , [baseType, {} ] }
	= { ["Int", "Bool"]
	  , ["Bool", "Int"]}


This gives us the arguments for which this function is not defined. The domain of the function are all arguments _not_ captured by these sequences.

This is algorithm is practical for small inputs, but can become slow in the presence of large types; the sequence set might contain up O(#Args*#TypeSize) elements.

### Interpreting clauses


### Calculating the codomain


