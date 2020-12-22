{
  open Lexing
  let file = Sys.argv.(1)
  let h = Hashtbl.create 17
  let print = Printf.printf

  let _ = List.iter (fun (s, k) -> Hashtbl.add h s k) [
    "int",      "INT";
    "bool",     "BOOL";
    "void",     "VOID";
    "if",       "IF";
    "else",     "ELSE";
    "return",   "RETURN";
    "while",    "WHILE";
    "putchar",  "PRINT";
    "main",     "MAIN"]

  let keyword_or_ident s = let ss = try Hashtbl.find h s with Not_found -> s in
  print "%s" ss
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (alpha | digit) (alpha | digit | '_')*

rule token = parse
  | ['\n']              { print "\n"; new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+    { print " "; token lexbuf }
  | "(*"                { comment lexbuf; }
  | digit+ as n         { print "%s" n; token lexbuf }
  | "true"              { print "TRUE"; token lexbuf }
  | "false"             { print "FALSE"; token lexbuf }
  | ident as id         { keyword_or_ident id; token lexbuf }
  | '+'                 { print "PLUS"; token lexbuf }
  | '-'                 { print "MINUS"; token lexbuf }
  | '*'                 { print "MUL"; token lexbuf }
  | '!'                 { print "NOT"; token lexbuf }
  | '<'                 { print "LT"; token lexbuf }
  | '{'                 { print "LBRACKET"; token lexbuf }
  | '}'                 { print "RBRACKET"; token lexbuf }
  | '('                 { print "LPAR"; token lexbuf }
  | ')'                 { print "RPAR"; token lexbuf }
  | ';'                 { print "SEMI"; token lexbuf }
  | ','                 { print "COMMA"; token lexbuf }
  | '='                 { print "EQ"; token lexbuf }
  | _ as c              { failwith (Printf.sprintf "invalid character: %c" c) }
  | eof                 { () }

and comment = parse
  | "*)"                { () }
  | "(*"                { comment lexbuf; comment lexbuf }
  | '\n'                { new_line lexbuf; comment lexbuf }
  | _                   { comment lexbuf }
  | eof                 { failwith "unterminated comment" }

{ let _ = token (Lexing.from_channel (open_in file)); }
