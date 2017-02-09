
 Tutorial: developing a simple programming language
====================================================

We will develop a programming language which can work with booleans and integers. Apart from doing basic arithmetic, we can apply anonymous functions on them.

Example programs, and the value they become, are 

Program				Evaluates to
-------				------------
`True`				`True`
`1`					`1`
`24 + 18`			`42`
`If True Then 1 Else 2`		`1`
`1 + (2 + 3)`			`6`
`(\\ x : Int . x + 1) 41`	`42`

Table: Example programs and their outcome

The expression `(\x : Int . x + 1)` is a _lambda expression_. This is an anonymous function, taking one argument - named x- of type `Int`. When applied (e.g. `(\x : Int . x + 1) 41`, the expression right of the `.` is returned, with the variable `x` substituted by the argument, becoming `41 + 1`.


 Setting up a .language
------------------------

A language is declared inside a `.language` file [^extension]. Create `STFL.language`, and put a title in it:

`

[^extension]: Actually, the extension doesn't matter at all.


 Declaring the syntax
-----------------------

For a full reference on syntax, see the [reference manual on syntax](#syntax)

A program is nothing more then a string of a specific form. To describe strings of an arbitrary structure, *BNF* [^BNF] can be used.

[^BNF]: Backus-Naur-form, as introduced by John Backus in the ALGOL60-report. 
TODO reference

Consider all possible boolean forms: `True` and `False`.


 Functions
-----------


 Relations and Rules
---------------------


 Properties
------------


 Recap: used command line arguments
------------------------------------



