## OCaml-like, interpreter

Uses OCaml to build a new language called MiniML, it allows us to vary the semantics of the language. For this project, I'm creating a two models. One based on substitution semantics, and the other based on a dynamically scoped environmental model.

# Files
**absbook.ml** developed by the cs51 course staff, provides unit testing tools 
**miniml_lex.mll** & **miniml_parse.mly**  Creates a lexical analyzer for a our interpreter 
**evaluation.ml** Implements the evaluation of the interpreter based on the two operational semantics
**expr.ml** Provides the abstract syntax for language as well as defining how substitution works.
**miniml.ml** provides the read, eval, print loop for the interpreter

> To run, clone the repository, download the latest version of ocaml to compile the files, run ocamlbuild miniml.byte, then ./miniml.byte, and then you'll be in the real, eval, print loop for the interpreter.
