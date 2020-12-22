%{
  open Verificateur
%}
%token<int> CST
%token<bool> Vbool
%token<string> IDENT
%token PLUS MINUS MUL DIV MOD
%token EQQ NEQ LEQ GEQ LT GT
%token NOT OR AND
%token INT BOOL VOID MAIN WHILE IF ELSE FOR TYPEDEF STRUCT
%token SEMI LBRACKET RBRACKET LPAR RPAR PRINT EQ COMMA RETURN LSQUR RSQUR
%token EOF

%left OR
%left AND
%left EQQ NEQ LEQ GEQ LT GT
%left PLUS MINUS
%left MUL DIV MOD
%nonassoc NOT

%start<prog> prog
%%

prog:
  | decl pg=prog        { pg }
  | pg=main             { pg }
;

main:
  | INT MAIN LPAR params=list(param) RPAR LBRACKET p=instrs RBRACKET EOF
  { set_fun Int "main" params p; get_prog () }
;

decl:
  | INT s=IDENT EQ e=expr SEMI
  { set_global Int s e }
  | BOOL s=IDENT EQ e=expr SEMI
  { set_global Bool s e }
  | INT s=IDENT LSQUR e= expr RSQUR SEMI
  { set_global (Table Int) s e; }
  | BOOL s=IDENT LSQUR e= expr RSQUR SEMI
  { set_global (Table Bool) s e; }
  | TYPEDEF STRUCT s1=IDENT LBRACKET lt=list(inside_struct) RBRACKET s2=IDENT SEMI
  { set_struct s1 s2 lt; }
  | INT s=IDENT LPAR params=list(param) RPAR LBRACKET p=instrs RBRACKET
  { set_fun Int s params p }
  | BOOL s=IDENT LPAR params=list(param) RPAR LBRACKET p=instrs RBRACKET
  { set_fun Bool s params p }
  | VOID s=IDENT LPAR params=list(param) RPAR LBRACKET p=instrs RBRACKET
  { set_fun Void s params p }
;

inside_struct:
  | INT s=IDENT SEMI
  { (s, Int) }
  | BOOL s=IDENT EQ e=expr SEMI
  { (s, Bool) }
  | INT s=IDENT LSQUR e=expr RSQUR SEMI
  { if typage_expr Int e then (s, Table Int) else failwith "Expected a Int for index of table" }
  | BOOL s=IDENT LSQUR e=expr RSQUR SEMI
  { if typage_expr Int e then (s, Table Bool) else failwith "Expected a Int for index of table" }
;

param:
  | INT   s=IDENT COMMA            { set_param Int s;   (s, Int)  }
  | BOOL  s=IDENT COMMA            { set_param Bool s;  (s, Bool) }
  | INT   s=IDENT                  { set_param Int s;   (s, Int)  }
  | BOOL  s=IDENT                  { set_param Bool s;  (s, Bool) }
;

instrs:
  | l=list(instr)                  { List.flatten l }
;

instr:
  | PRINT LPAR e=expr RPAR SEMI
  { typage_instr (Putchar e); [Putchar e] }
  | INT s=IDENT EQ e=expr SEMI
  { set_local Int s e; [Set (s, e)] }
  | INT s=IDENT LSQUR e= expr RSQUR SEMI
  { set_local (Table Int) s e; [Create (s, Int)] }
  | BOOL s=IDENT LSQUR e= expr RSQUR SEMI
  { set_local (Table Bool) s e; [Create (s, Bool)] }
  | BOOL s=IDENT EQ e=expr SEMI
  { set_local Bool s e; [Set (s, e)] }
  | s=IDENT EQ e=expr SEMI
  { typage_instr (Set (s, e)); [Set (s, e)] }
  | IF LPAR e=expr RPAR LBRACKET pt=instrs RBRACKET ELSE LBRACKET pf=instrs RBRACKET
  { typage_instr (If (e, pt, pf)); [If (e, pt, pf)] }
  | FOR LPAR i1=inside_for SEMI e2=expr SEMI i3=inside_for RPAR LBRACKET p=instrs RBRACKET
  { typage_instr (While (e2, p@i3)); i1@[While (e2, p@i3)] }
  | WHILE LPAR e=expr RPAR LBRACKET p=instrs RBRACKET
  { typage_instr (While (e, p)); [While (e, p)] }
  | s=IDENT LSQUR e1=expr RSQUR EQ e2=expr SEMI
  { typage_instr (Write (s, e1, e2)); [Write (s, e1, e2)] }
  | RETURN e=expr SEMI
  { [Return e] }
  | e=expr SEMI
  { typage_instr (Expr e); [Expr e] }
;

inside_for:
  | INT s=IDENT EQ e=expr
  { set_local Int s e; [Set (s, e)] }
  | BOOL s=IDENT EQ e=expr
  { set_local Bool s e; [Set (s, e)] }
  | s=IDENT EQ e=expr
  { typage_instr (Set (s, e)); [Set (s, e)] }
  | e=expr
  { [Expr e] }
;

exprs:
  | l=separated_list(COMMA, expr)               { l }
;

expr:
  | LPAR e=expr RPAR                            { e }
  | MINUS n=CST                                 { Cst (-n) }
  | n=CST                                       { Cst n }
  | b=Vbool                                     { Cst (if b then 1 else 0) }
  | e1=expr   PLUS  e2=expr                     { Add (e1, e2) }
  | e1=expr   MINUS e2=expr                     { Minus (e1, e2) }
  | e1=expr   DIV   e2=expr                     { Div (e1, e2) }
  | e1=expr   MUL   e2=expr                     { Mul (e1, e2) }
  | e1=expr   MOD   e2=expr                     { Mod (e1, e2) }
  | e1=expr   LT    e2=expr                     { Lt (e1, e2) }
  | e1=expr   GT    e2=expr                     { Gt (e1, e2) }
  | e1=expr   GEQ   e2=expr                     { Geq (e1, e2) }
  | e1=expr   LEQ   e2=expr                     { Leq (e1, e2) }
  | e1=expr   NEQ   e2=expr                     { Neq (e1, e2) }
  | e1=expr   EQQ   e2=expr                     { Eqq (e1, e2) }
  | e1=expr   OR    e2=expr                     { Or (e1, e2) }
  | e1=expr   AND   e2=expr                     { And (e1, e2) }
  | s=IDENT LPAR el=exprs RPAR                  { Call (s, el) }
  | s=IDENT LSQUR e=expr RSQUR                  { Access (s, e) }
  | s=IDENT                                     { Get s }
  | NOT e=expr                                  { Not e }
;
