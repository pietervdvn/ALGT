
 Gradualization of the STFL-typesystem
=======================================

Gradualization a type system is hard. First, we'll manually build a delta to STFL to see the differences in the type system. Once that is done, we automate as much parts of this within the tool.

 
 Manual gradualization
--------------------------

 To manually gradualize, we provide `Gradualize.language-changes`, which acts as a delta to the actual language. These changes are applied to STFL.language with the `-c` flag.

First, we simply state what changes should be applied, as based on the paper. The more complicated changes will be explained in the following chapter.

### Gradualizing the Syntax

Gradualizing the syntax is easy and straightforward. 

The only important change is the addition of the _dynamic_ type `?` to the type system, representing the entire set of all types:

$$GradualizeSTFL.language-changes!10!file

The language designer might also rename `type` to `gtype`, to indicate that the language now works with _gradual_ types. This can be done easily too:

$$GradualizeSTFL.language-changes!13,14!file
 

The syntax designer might also insert syntactic sugars, such as `"(" "\" var "." expr ")"` without explicit type annotation, with meaning `"(" "\" var ":" ? "." expr ")"`. As adding syntactic sugars is a heavily human-oriented task, it is hard to automate and won't be considered here.


### Gradualizing functions

As noted in the paper, domain and codomain should only be augmented slightly, by adding a case for handling '?':


$$GradualizeSTFL.language-changes![22..28]!file


`equate` is somewhat more complicated, as it also contains a recursive case:

$$GradualizeSTFL.language-changes![32..37]!file

### Gradualizing relations

Gradualizing the relations is straightforward as well.

As to reflect the change of semantics, we rename the relation `==` as `~`:

$$GradualizeSTFL.language-changes!51!file

Furthermore, two new cases are introduced:

$$GradualizeSTFL.language-changes![56..62]!file


### Properties

What about the properties?

So far, we defined three properties:

- Preservation
- Progress
- Termination

While progress and termination still hold for this gradual language, Preservation is broken.

Even if an expression can be typed gradually, it does not guarantee it can still be typed after a single step. Consider following example:

$$Counter.gtfl!1!file

After a single step, this expression becomes:

	x + True

Which clearly can not be typed nor evaluated.

Abstract interpretation of functions
------------------------------------
o