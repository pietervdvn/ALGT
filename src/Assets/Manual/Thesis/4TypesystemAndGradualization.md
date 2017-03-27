
 Gradualization of the STFL-typesystem
=======================================

Gradualization a type system is hard. We first follow the paper to gradualize the typesystem of STFL.

Afterwards, we automate as much parts of this with the tool.


 
 Manual gradualization
--------------------------

 To manually gradualize, we provide "Gradualize.language-changes", which acts as a delta to the actual language. These changes are applied to STFL.language with the `-c` flag.

### Syntax

 Gradualizing the syntax is easy and straightforward. The only important change is the addition of the _dynamic_ type to the type system:


$$GradualizeSTFL.language-changes![6..13]!file


The syntax designer might also insert syntactic sugars, such as `"(" "\" var "." expr ")"` without explicit type annotation, with meaning `"(" "\" var ":" ? "." expr ")"`. As adding syntactic sugars is a heavily human-oriented task, it is hard to automate and won't be considered here.

### 




