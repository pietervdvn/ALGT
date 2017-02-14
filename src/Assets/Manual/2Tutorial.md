

 Functions
-----------

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

$$STFL.language![30..32]!file

In this function section, we can define the function _dom_ in the following way:

$$STFL.language!34!file
$$STFL.language!37!file

So, what is going on here? Let's first take a look to the first line:

$$STFL.language!34!file

The `domain` is the name of the function. The `type -> type` indicates what syntactic form is taken as input (a `type`) before the `->` and what is given as output (again a `type`). [^ISMETA]

![Relevant XKCD (by Randall Munroe, #917)](hofstadter.png){width=100%}


[^ISMETA]: You probably noticed the similarity between the types declared in our own STFL and this declaration.]


### Pattern matching

Let's have look at the body of the function:

$$STFL.language!37!file


![Pattern matching in action](TypeTrees0annot.png)


 Relations and Rules
---------------------


 Properties
------------


 Recap: used command line arguments
------------------------------------



