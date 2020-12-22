let _ =
  let file = Sys.argv.(1) in
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  Printf.printf "-------------------- Typage ----------------------\n";
  let prog =
    try
      Parser.prog Lexer.token lexbuf
    with
    | Failure s -> failwith (Printf.sprintf "Failure - %s at line %d\n" s lexbuf.lex_curr_p.pos_lnum)
    | _ -> failwith (Printf.sprintf "Parser Error Position: %d\n" lexbuf.lex_curr_p.pos_lnum)
  in
  Printf.printf "------------------ Interprete --------------------\n";
  Interpreteur.interp_prog prog
