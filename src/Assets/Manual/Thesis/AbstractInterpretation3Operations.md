 



Operations on representations
-----------------------------

### Unfolding

The first important operation is unfolding a single level of the symbolic representation. This is done by substituting the symbolic values by its definition.
This operation would thus also need some context, namely these definitions.

Algorithmically unfolding a value is done as following:

Unfolding a concrete value, is just the concrete value itself:

	unfold("Bool") = {"Bool"}

Unfolding a symbolic value is the set, as it is defined:

	unfold(baseType) = { "Bool", "Int" } 
	unfold(type)	 = {(typeTerm "->" type), typeTerm}

Note the usage of parentheses around `(typeTerm "->" type")`. This groups the sequence together and is needed to prevent ambiguities later on, as illustrated in following definition:

	subtraction == {number "-" subtraction, number}

Unfolding this a few times towards subtraction, we might yield:

	subtraction == { ..., number "-" number "-" number, ... }

This is unclear. Should the syntactic expression `3 - 2 - 1` equal `(3 - 2) - 1 = 0` or `3 - (2 - 1) = 2`? Our syntax definition suggests the latter, as it is built in a right-associative way. To mirror this in the sequence, we add parentheses:

	subtraction == { ..., number "-" (number "-" number), ... }



To unfold a sequence, we unfold each of the parts and take a cartesian product:

	baseType "->" baseType
	== {"Bool", "Int"} × {"->"} × {"Bool", "Int"}
	== { "Bool" "->" "Bool"
	   , "Bool" "->" "Int"
	   , "Int" "->" "Bool"
	   , "Int" "->" "Int" }

At last, to unfold a symbolic set representation, we unfold each of the sequences in the set, and collect them:

	{baseType}
	== { {"Bool", "Int"} }
	== { "Bool", "Int" }

### Directed unfolds

Throughout the text, we will freely use _directed unfold_. This is an unfold of only certain elements in the set, often the ones we conveniently need unfolded.


### Refolding

Refolding attempts to undo the folding process. While not strictly necessary, it allows for more compact representations throughout the algorithms - increasing speed- and a more compact output - increasing readability.

For example, refolding would change `{"Bool", "Int", "Bool" "->" "Int"}` into `{baseType, "Bool" "->" "Int"}`, but also `{type, typeTerm, "Bool"}` into `{type}`


How does this work algorithmically? Actually, this is done in two steps:

- Group subsets (e.g. `"Bool"` and `"Int"`) into their symbolic value (`baseType`)
- Filter away values that are included in another symbolic value (e.g. in `{"Bool", type}`, `"Bool"` can be omitted, as it is included in `"type"`)

This is repeated until no further changes are possible on the set.



#### Grouping sets


Grouping simple sets is quite straightforward. To refold, we just look which known definitions are a subset.

E.g. given the definition `baseType == {"Bool", "Int}`, we can make the following refold as each element of `baseType` is present:

	refold({"Bool", "Int", "Bool" "->" "Int"}) 
		= {baseType, "Bool" "->" "Int"}


Grouping complex sequences is not as straightforward as it seems.
Consider

	{ "Bool" "->" "Bool"
	, "Bool" "->" "Int"
	, "Int" "->" "Bool"
	, "Int" "->" "Int" }

How will we proceed to refold this algorithmically? First, we sort these sequences in buckets where only a single element is different between the sequences.
	
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


### Resolving to a syntactic form

Given a set, it can be usefull to calculate what syntactic form does contain all of the elements of the set. E.g., given `{"Bool", "(" "Int" ")", "Int"}`, we want to know what syntactic form contains all of these values, namely `baseType`.

To do this, first we change every sequence by the syntactic form from which it was derived, its generator. For the example, this becomes `{baseType, typeTerm, baseType}`.

To get the least common supertype of those, we calculate the meet of all these types, being `typeTerm`.


### Addition of sets

Another extremely usefull and straightforward operation is the addition of two sets. This is joining both sets, optionally refolding it.

### Subtraction of sets

Subtraction of sets enabes a lot of usefull algorithms, but is quite complicated.

Subtraction is performed on each sequence in the set. This subtraction of a single element might result in no, one or multiple new elements. 
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

If the sequence is a single symbolic value, we check if the symbolic value embeds the concrete value. If that is the case, we unfold and subtract that set recursively:

- `baseType - "Bool"` equals `{"Bool", "Int"} - "Bool"`, resulting in `{"Int"}`
- `type - "("` equals `type`

#### Subtraction of a symbolic value from a sequence

When a symbolic value is subtracted from a sequence, we first check wether this the sequence is embedded in this symbolic value:

- `"Bool" - baseType` is empty, as `"Bool"` is embedded in `baseType`
- `baseType - baseType` is empty, as baseType equals itself
- `"Bool" - type` is empty as `"Bool"` is embedded in `type` (via `typeTerm` and `baseType`)
- `baseType - type` is empty too, as it is embedded as well
- `("(" type ")") - type` is empty, as this is a sequence inside `typeTerm`

If the symbolic value does not embed the sequence, it might still subract a part of the sequence.


This is the case if the sequence embeds the symbolic value we wish to subtract. If that is the case, we unfold this sequence, and subtract each element in the set with the subtrahendum:

- `typeTerm - baseType` becomes `{baseType, "(" type ")"} - baseType`, resulting in `{"(" type ")"}`


### Subtraction of a sequence from a symbolic value

This is straightforward too. Given the symbolic value, we check wether it shadows the sequence we want to subtract. If this is the case, we unfold this symbolic value and subtract the sequence from each of the elements. If no shadowing occurs, we just return the symbolic value.

	`type - ("(" type ")") 
	= {typeTerm "->" type, typeTerm} - ("(" type ")")
	= {typeTerm "->" type, baseType, "(" type ")"} - ("(" type ")")
	= {typeTerm "->" type, baseType}

#### Subtraction of a sequence

The last case is subtracting a sequence from another sequence. As this is rather complicated, we start with an example., we start with an example before giving the algorithmic approach.	

##### Example

As all good things in programming, this subtraction relies on recursion; we will follow the example through the call stack.

1. Consider `("(" type ")") - ("(" ("Bool" "->" type) ")")`. It is intuitively clear that the parentheses `"("` and `")"` should remain, and that we want to subtract `type - ("Bool" "->" type)`.

2. Here we subtract a sequence again. We unfold `type`, to yield: [^swap]

	{typeTerm, typeTerm "->" type} - ("Bool" "->" type) 



[^swap]: for practical reasons, I swapped the order of both elements in the text. As this is a set, that doesn't matter.




3. In the set, we now have two values we should subtract:
- `typeTerm - ("Bool" "->" type)` yields `typeTerm`, as neither shadows the other.
- `(typeTerm "->" type) - ("Bool" "->" type)` can be aligned. It is clear we'll want to have `typeTerm - "Bool"` and `type - type` and let `"->"` unchanged.

4. `typeTerm - "Bool"` gives us `{baseType, "(" type ")"} - "Bool"`, resulting in `{"Int", "(" type ")"}`.
It is important to note that `(Bool)` is _still_ an element of this set, despite "Bool" without parentheses is not.
Despite having the same semantics, these are two syntacticly different forms!

5. `type - type` is empty.


At this point, we have all the ingredients necessary, and we can put our subtraction back together.

5. The biggest puzzle is how to put `(typeTerm "->" type) - ("Bool" "->" type)` together. 
   Intuitivelly, we'd expect it to be `{"Int" "->" type, "(" type ")" "->" type}`.
   More formally, the directed unfold of   
	 `typeTerm "->" type` is `{ "Bool" "->" type, "Int" "->" type, ("(" type ")") "->" type}`.
   Subtracting `"Bool" "->" type` yields our former result.

6. We put together the results of the recursive calls, being:
	- `{typeTerm}`
	- `{"Int" "->" type, "(" type ")" "->" type}`   
  This results in `{"Int" "->" type, "(" type ")" "->" type, typeTerm}`

7. We unfolded `type` to subtract the sequence, yielding    
	`{"Int" "->" type, "(" type ")" "->" type, typeTerm}`

8. We put the parentheses back around each expression:   
`{"(" ("Int" "->" type) ")", "(" "(" type ")" "->" type ")", "(" typeTerm ")"}`, giving the desired result.

##### Algorithm

So, how do we algorithmically subtract two sequences?

Consider sequence `a b`, where `a` unfolds to `a1, a2, a3, ...` and `b` unfolds to `b1, b2, b3, ...`. This means that `a b` unfolds to `a1 b1, a1 b2, a1 b3, ... , a2 b1, a2 b2, ...`

Now, if we would subtract sequence `a1 b1` from this set, only a single value would be gone, the result would nearly be the original cartesian product.

To do this, we first calculate the pointwise differences between the sequences. We align both sequences[^sameLength] and calculate the difference:


	a - a1= a - a1 = {a2, ...}
	b - b1 = {b2, ...}

[^sameLength]: This implies both sequences have the same length. If these don't have the same length, just return the original sequence as the subtraction does not make sense anyway.


Now, the resulting sequences are `{(a - a1) b, a (b - b1)}`.

Generalized to sequences from aribitrary length, we replace each element of the sequence once with the pointwise difference:

	a b c d ... - a1 b1 c1 d1  ...
	= { (a - a1) b c d ...
	  , a (b - b1) c d ...
	  , a b (c - c1) d ...
	  , a b c (d - d1) ...
	  } 

We apply this on the example `(typeTerm "->" type) - ("Bool" "->" type)`, yielding following pointwise differences:

	typeTerm - "Bool" = {"Int", "(" type ")"}
	"->" - "->"	  = {}
	type - type	  = {}

Resulting in:

	  { (typeTerm - "Bool") "->" type
	  , typeTerm ("->" - "->") type
	  , typeTerm "->" (type - type)}
	= {{"Int", "(" type ")"} "->" type
	  , typeTerm {} type
	  , typeTerm "->" {}}
	= {{"Int", "(" type ")"} "->" type}
	= {"Int" "->" type, ("(" type ")") "->" type}


