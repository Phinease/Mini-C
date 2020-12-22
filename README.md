# DM - Mini C - Compilation
## CHEN Shuangrui
This is a compiler using ocamllex and menhir which works for some core grammar of C.

Run:
`` ``
ocamlbuild -use-menhir main.native
./main.native test.c
`` ``

### Structure:
1. Lexical analyzer - lexer.mll
2. Parser - parser.mly
3. Type checker - verifier.ml (name of functions starting with typage_)
4. Main and error handling - main.ml
5. Interpreter - Interpreteur.ml

### Work done:
1. Minimum part
2. For loop (performed by while as a syntactic sugar)
3. Operators (/, mod, <=,>,> =, ==,! =,!, &&, ||)
4. Tables (realized by defining a typ Table of typ)
  `` ``
  int table [10]; # definition
  table [5]; #access
  table [6] = 10; #writing
  `` ``
5. Struct definition
6. Display (name of functions starting with print_)
7. Interpreter


### Part not complete:
1. Access and write structure
2. Error position column
3. In the interpreter, it does not know any parameter variables


### Test file
1. test.c - Typing for complete sentences
2. test2.c - Simple experimentation for the interpreter
3.test3.c - Testing for errors (in the comment)
