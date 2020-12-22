{
  open Lexing
  open Parser

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter
      (fun (s, k) -> Hashtbl.add h s k)
      [ "int",      INT;
        "bool",     BOOL;
        "void",     VOID;
        "if",       IF;
        "for",      FOR;
        "else",     ELSE;
        "return",   RETURN;
        "while",    WHILE;
        "putchar",  PRINT;
        "main",     MAIN;
        "typedef",  TYPEDEF;
        "struct",   STRUCT
      ];
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT s
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (alpha | digit) (alpha | digit | '_')*

rule token = parse
  | ['\n']              { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+    { token lexbuf }
  | "/*"                { comment lexbuf; token lexbuf }
  | digit+ as n         { CST (int_of_string n) }
  | "true"              { CST 1 }
  | "false"             { CST 0 }
  | ident as id         { keyword_or_ident id }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { MUL }
  | '/'                 { DIV }
  | '%'                 { MOD }
  | "=="                { EQQ }
  | "!="                { NEQ }
  | "<="                { LEQ }
  | ">="                { GEQ }
  | '<'                 { LT }
  | '>'                 { GT }
  | '!'                 { NOT }
  | "||"                { OR }
  | "&&"                { AND }
  | '{'                 { LBRACKET }
  | '}'                 { RBRACKET }
  | '['                 { LSQUR }
  | ']'                 { RSQUR }
  | '('                 { LPAR }
  | ')'                 { RPAR }
  | ';'                 { SEMI }
  | ','                 { COMMA }
  | '='                 { EQ }
  | _ as c              { failwith (Printf.sprintf "invalid character: %c" c) }
  | eof                 { EOF }

and comment = parse
  | "*/"                { () }
  | "/*"                { comment lexbuf; comment lexbuf }
  | '\n'                { new_line lexbuf; comment lexbuf }
  | _                   { comment lexbuf }
  | eof                 { failwith "unterminated comment" }
