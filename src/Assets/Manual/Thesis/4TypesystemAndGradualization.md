
 Gradualization of the STFL-typesystem
=======================================

Gradualization a type system is hard. First, we'll manually build a delta to STFL to see the differences in the type system. Once that is done, we automate as much parts of this within the tool.


 
 Manual gradualization
--------------------------

 To manually gradualize, we provide `Gradualize.language-changes`, which acts as a delta to the actual language. These changes are applied to STFL.language with the `-c` flag.

### Gradualizing the Syntax

Gradualizing the syntax is easy and straightforward. 

The only important change is the addition of the _dynamic_ type `?` to the type system:

$$GradualizeSTFL.language-changes!10!file

The language designer might also rename `type` to `gtype`, to indicate that the language now works with _gradual_ types. This can be done easily too:

$$GradualizeSTFL.language-changes!13,14!file
 

The syntax designer might also insert syntactic sugars, such as `"(" "\" var "." expr ")"` without explicit type annotation, with meaning `"(" "\" var ":" ? "." expr ")"`. As adding syntactic sugars is a heavily human-oriented task, it is hard to automate and won't be considered here.


### Gradualizing functions

As noted in the paper, domain and codomain should only be 




