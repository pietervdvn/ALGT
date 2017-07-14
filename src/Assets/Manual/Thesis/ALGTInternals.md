
 ALGT Internal algorithms
==========================


### Minimal types, liveness- and totalitychecker

The functions are checked for various easily made errors, such as that each clause can match some input (liveness), that each input has a matching clause (totality) and that the declared output syntactic form is the smallest available.

These algorithms use of abstract interpretation and are detailed in a following chapter.


Algorithms using abstract interpretation
----------------------------------------


### Calculating the codomain

We can also calculate what set a function might return. In the previous algorithm, we calculated what values a clause might receive at the beginning.

We use this information to calculate the set that a clause -and thus a function- might potentially return. For this, we use the earlier introduced abstract pattern matching and expression calculation. Afterwards, we sum all the sets.

For the first clause of the domain function, we would yield:

	dom("(" t ")")	= dom(t)	<: {type}
	Used patterns: {"(" type ")"};
	Patterns falling through: {typeTerm "->" type, baseType}

For the second clause, we yield:

	dom(T1 "->" T2)	= T1	<: {baseType}
	Used patterns: {baseType "->" type};
	Patterns falling through: {baseType}

Summing all returned values and resolving them to the smallest common supertype, gives:

	{baseType, type} = {type}


This already gives us some usefull checks for functions, namely a **pattern match totality checker** and a **clause livebility checker** (as we might detect a clause _not_ consuming patterns.



But with a slight modification to this check, we can do better.

We can calculate a dictionary of what syntactic forms a function does return. Instead of initializing this set with the returned syntactic form (thus `dom → {type}`), we initialize it with empty sets (`dom → {}`). When we would use this to resolve function calls, we yield the following:
	

	dom("(" t ")")	= dom(t)	<: {}
	dom(T1 "->" T2)	= T1		<: {baseType}
	
Summing into `{baseType}`

With this, we can update our dictionary to `dom --> {baseType}` and rerun.

	dom("(" t ")")	= dom(t)	<: {baseType}
	dom(T1 "->" T2)	= T1		<: {baseType}
	
Summing into `{baseType}`. This does not add new information; in other words, there is no need for a new iteration.

This gives rise to another check, namely that the function signature is the **smallest possible syntactic form** and partial **infinite recursion check**.

Infinite recursion can -in some cases- be detected. If we were to release previous algorithm on following function:

	f	: a -> a
	f(a)	= f(a)

we would yield:

	{f → {}}

Per rice theorem, we know it won't be possible to apply this algorithm to every program. The smallest possible syntactic form check would hang on:

	f	: type -> type
	f(t)	= "Bool" "->" f(t)






