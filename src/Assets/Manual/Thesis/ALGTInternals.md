
 ALGT Internal algorithms
==========================



### Typechecker for metafunctions

All expressions and patterns are typechecked, as type errors are easily made. Forcing parsetrees to be well-formed prevents the creation of strings which are not part of the language, what would result in hard to track bugs later on. Here, an overview of the inner workings of the typechecker are given. 

These internals are simplified, as a type expection is always available: expressions and patterns are always typed explicitly, as the type signature of the function always gives a hint of what syntactic form[^syntacticType] a pattern or expresion is. A type for a pattern indicates what type the pattern should deconstruct, or analogously for expressions, what syntactic form a parsetree would be if the expression was used to construct one.
The natural deduction rules, which will be introduced in the following part, have the same typing available and can thus be typechecked with the same algorithm.  


As expressions and patterns are **duals** in function of semantics, but the same in syntax and internal representation, the same typechecking algorithm can be used for patterns and expressions. However, some fundamental differences exist between in usage between patterns and expressions. The most striking example are variables: in a pattern context, an unknown variable occurence is a declaration; in an expression context, an unknown variable is an error
In order to keep the typechecker uniform, the typechecker merely **annotates** types to each part of the expression; checks for unknown variables are done afterwards by walking the expression again.  

To type **function calls**, a store γ containing all function signatures is provided. This dictionary γ is built before any typechecking happens by assuming the given function signatures are correct. A store for variables is not necessary, as variable typings are after the annotation.  

With these prelimaniries, we present the actual typechecking algorithm used in ALGT. The algorithm has a number of cases, depending on the kind of expression that should be typed; composite expressions are handled by recursively typing the parts before handling the whole.



#### Variables

Variables are simply annotated with the expected type. One special case is when two (or more) patterns assign the same variable, such as the clause `f(x, x) = ...`. This is perfectly valid ALGT, as this clause will match when both arguments are identical. With the type signature `f : a -> b -> c` given, type of `x` can be deduced even more accuratly: the biggest common subtype of `a` and `b`, as `x` should be both an `a` and a `b`.

Actual, type errors are checked after the initial step of type annotation, when all typing information is already available and inconsistencies can be easily detected.

To catch these inconsistencies between assignement and usage, the following strategy is used:

- First, pattern assignments are calculated; this is done by walking each pattern individually, noting which variables are assigned what types.
- When these individual pattern assignments are known, they are merged. Merging consists of building a bigger dictionary, containing all assigments. If two patterns assign the same variable, compatibility of the types is checked by taking the infimum of both types. If that infimum is another syntactic form (another regular type), then some parsetrees exists which might match the pattern and the infimum type is taken as the type of the variable. If no such infimum exists (or more accuratly, if the infimum is bottom), a type error is detected.
- With this store of all variable typings at hand, the expression can be checked for _undeclared_ variables. This is simply done by getting the assignments of the expression -the same operation as on patterns- and checking that each variable of the expression occurs in the assigment of the patterns. If not, an unknown variable error is issued.
- The last step checks for inconsistencies between declaration and usage, which checks that a variable is always fits its use, thus that no variable isused where a smaller type is expected.

This algorithm is listen in figure \ref{fig:mergingAlgo}.

#### Sequences and string literals

Sequences (and also bare string literals) are handled by comparing them to the definition of the syntactic form, which consists of one or more BNF-sequences that can be chosen. Each of these defined sequences is tried by aligning it with the sequence that should be typed. Literal string values (and literal interger values) are filtered out here directly.


#### Functions

Functions are typed using the available function signature. First, all the arguments are typed individually; then the return type of the function is compared against the expected type of the pattern/expression. The comparison used is, again, subtyping, as this always gives a sound result: 
when used in an expression, a smaller type will fit, thus subtyping is necessary.
When used as an pattern, the function is calculated and compared against the input parsetree. Here, the only requirement is that there exist _some_ parsetrees that are common to the argument type and the result type. In this case, the only check should be that the infimum of both types exists (more accuratly, that the infimum is not bottom). As $expected type <: funtion type$ gaurantees this, it is a sufficient condition. While this check is a little _to_ strict, it is sufficient for practical use.


#### Type annotations

Type annotations are used for two means. First, it allows easy capturign of syntactic forms (e.g. `isBool((_:bool)) = "True") and it allows disambiguation of definitions in more complicated grammars.

In order to typecheck type annotations, the first issue is if that type _can_ occur there. If the pattern `(_:expr)` is checked in a position where only a `bool` is a possible input argument, it's pretty useless to perform this annotation. The first check will thus be the sensibility of the annotation, namely that the annotated type `Ta` is a subtype of the expected type `T`.

If this check passes, the expression within the annotation is typed against the annotation. 


#### Evaluation contexts

Evalaution contexts implement searching behaviour: when a parsetree is matched over `e[x]`, a subtree matching `x` -which can be a compound pattern- is searched within the tree. If no such tree is found; the match fails. When this match is found, both `x` and `e` are available as variables. `someExpr` can be used e.g. to extract information from a typing store, `e[someOtherExpr]` can be used to plug the hole with another value, e.g. to implement some form of substitution (although a builtin function is available for this).

The explicit typing makes it possible to easily tag `e`, as its type `T` will already be stated by the function signature.
However, it is difficult for the typechecker to figure out what type `x` might be. This is solved by typechecking `x` against _each_ type that might occur (directly or indirectly) as subtree in `T`. If exactly one type matches, this typing is choosen. If not, an explicit typing is demanded.

This approach only works for complex expressions. Often, the programmer only wishes to capture the first occurence of a certain syntactic form, which can be written as `e[(b:bool)]`. In order to save the programmer this boilerplate, the typechecker attempts to discover a syntactic form name in the variable type. If this name is found (as prefix), it will be inherently typed. In other words `e[bool]` is equivalent to `e[(bool:bool)]`.

#### Algorithm


All the pieces of the algorithm, as defined above, are put together in pseudocode in figure \ref{fig:typecheckerAlgo}. Some trivial cases (such as pattern wildcard `_`) are omitted for clarity, despite their practical uses.

\begin{figure}
\begin{lstlisting}[style=algo]

typecheck(expr, γ, T):
    case expr of:
        variable v:	return v:T
        sequence es:	
            # includes lone string literals, sequence of one
            possible_typings = []
            for choice_sequence in T.getChoices():
                if es.length != choice_sequence.length:
                    continue
                try:
                    typed_sequence = []
                    for e, t in zip(es, choice_sequence):
                        if e is literal && t is literal:
                            if e != t then:
                                error "Inconsistent application
                            else typed_sequence += e
                        else:
                            typed_sequence += typecheck(e, γ, t)
                    possible_typings += typed_sequence
                catch:
                    # this doesn't match. Let's try the next choice...
            if possible_typings == []:
                error 
                    "Could not match $expr against"
                    "any choice of the corresponding syntactic form"
            if possible_typings.length() > 1:
                error 
                    "Multiple possible typings for $expr."
                    "Add an explicit type annotation"
            return possible_typings[0]
        function f(x1, x2, ...):
            (T1, T2, ..., RT)    <- γ[f]    # Lookup type of f
            x1'    = typecheck(x1)
            x2'    = typecheck(x2)
            if RT <: T:
                return f(x1', x2', ...) : RT
            else:
                error 
                    "Function $f does not have the desired type"
        type annotation (e:TA):
            if !(TA <: T):
                error 
                     "The typing annotation is to broad"
                     "or can never occur"
            return typecheck(e, γ, TA)
        evaluation context e[x] with x a variable:
            ts = T.occuringSubtypes().filter(x.isPrefixOf)
            # occuringSubtypes are sorted on namelength
            # the first match is the best match
            t = ts[0] 
            typecheck(e[(x:t)])
        evaluation context e[x]:
            ts = T.occuringSubtypes()
            possible_typings = []
            for t in ts:
                try{
                possibleTypings += typecheck(x, γ, t)
                }catch():
                    # This doesn't match. Let's try the next one
            if possible_typings == []:
                error 
                    "Could not match $x against"
                    "any possible embedded syntactic form"
            if possible_typings.length() > 1:
                error 
                    "Multiple possible typings for $x."
                    "Add an explicit type annotation"
            return possible_typings[0]

\end{lstlisting}



\caption{The typechecking algorithm for meta-expressions and patterns}
\label{fig:typecheckerAlgo}
\end{figure}


\begin{figure}
\begin{lstlisting}
# Checks a clause for unknown or incompatible type variables
checkClause(pattern1, pattern2, ... , expr):
    # assigned vars searches the patterns and returns a dictionary
    # containing {variableName -> type}
    assgn1 = pattern1.assignedVars()
    assgn2 = pattern2.assignedVars()
    ...

    assgnE = expr.assignedVars()

    assgns = merge(assgn1, assgn2, ...)
    
    for variable_name in assgnE.keys():
        if !assgns.contains(variable_name):
            error "Variable not defined"
        TUsage = assgnE.get(variable_name)
        TDecl = assgns.get(variable_name)    
        if !TUsage.isSubtypeOf(TDecl)):
            error "Incompatible types"

merge(assgn1, assgn2, ...):
    assgn    = {}
    for variable_name in assgn1.keys() + assgn2.keys() + ... :
        # assgn.get(T) return Top for an unknown type
        type = assgn1.get(variable_name)
               ∩ assgn2.get(variable_name)
               ∩ ...
        if type is ɛ:
            error "Incompatible types while merging assignments"
        assgn.put(variable_name, type)
    return assgn



\end{lstlisting}
\caption{Merging of variable assignemnt stores and consistent variable usage checks}
\label{fig:mergingAlgo}
\end{figure}


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






