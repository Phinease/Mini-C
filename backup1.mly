%{
  open Abstrite
%}

%token<int> CST
%token<bool> Vbool
%token<string> IDENT
%token PLUS MUL
%token LT
%token INT BOOL MAIN WHILE IF ELSE
%token SEMI LBRACKET RBRACKET LPAR RPAR PRINT EQ COMMA RETURN
%token EOF

%left OR
%left AND
%left LT
%left PLUS MINUS
%left MUL

%start<unit> prog
%%

prog:
  | decls=list(decl) INT MAIN LPAR params=list(param) RPAR LBRACKET p=instrs RBRACKET EOF
  { interp_main params p }
;

decl:
  | INT s=IDENT EQ e=expr SEMI
  { set_global Int s e }
  | BOOL s=IDENT EQ e=expr SEMI
  { set_global Bool s e }
  | INT s=IDENT LPAR params=list(param) RPAR LBRACKET p=instrs RBRACKET
  { set_fun Int s params p }
  | BOOL s=IDENT LPAR params=list(param) RPAR LBRACKET p=instrs RBRACKET
  { set_fun BOOL s params p }
;

param:
  | INT s=IDENT COMMA       { (s, Int) }
  | BOOL s=IDENT COMMA      { (s, Bool) }
  | INT s=IDENT             { (s, Int) }
  | BOOL s=IDENT            { (s, Bool) }
;

instrs:
  | l=list(instr) { l }
;

instr:
  | PRINT LPAR e=expr RPAR SEMI
    { Putchar e }
  | v=IDENT EQ e=expr SEMI
    { Set (v, e) }
  | IF LPAR e=expr RPAR LBRACKET st=instrs RBRACKET ELSE LBRACKET se=instrs RBRACKET
    { If (e, st, se) }
  | WHILE LPAR e=expr RPAR LBRACKET s=instrs RBRACKET
    { While (e, s) }
  | RETURN e=expr SEMI
    { Return e }
  | e=expr SEMI
    { Expr e }
;

expr:
  | n=CST                       { Cst n }
  | e1=expr PLUS e2=expr        { Add (e1, e2) }
  | e1=expr MUL e2=expr         { Mul (e1, e2) }
  | e1=expr LT e2=expr          { Lt (e1, e2) }
  | s=IDENT LPAR e=expr RPAR    { Call (s, e) }
  | s=IDENT                     { Get s }
;
