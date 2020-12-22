open Format

type typ =
  | Int
  | Bool
  | Void

type instr =
  | Putchar   of expr
  | Set       of string * expr
  | If        of expr * seq * seq
  | While     of expr * seq
  | Return    of expr
  | Expr      of expr

and seq = instr list

type expr =
  | Cst  of int
  | Add  of expr * expr
  | Mul  of expr * expr
  | Lt   of expr * expr
  | Get  of string
  | Call of string * expr list

and op =
  | Plus
  | Mul
  | Lt

type prog = {
  globals:    (string * typ * int) list;
  functions:  fun_def list;
}

type fun_def = {
  name:       string;
  params:     (string * typ) list;
  return:     typ;
  locals:     (string * typ) list;
  code:       seq;
}

let tbl_global = Hashtbl.create 20
let tbl_local = Hashtbl.create 20
let globals = ref []
let locals = ref []
let functions = ref []


let rec verifi_main ps p =
  set_fun Int "main" ps p;
  let pg = { globals = !globals; functions = !functions } in
  verifi_fun pg "main";

and verifi_fun pg s =
  let func = List.find (fun x -> x.name = s) pg.functions in
  verifi_instrs pg.globals func.locals

and verifi_instrs =
  let wait_return x = (verifi_instr pg x)!=None in
  let return = List.find wait_return p in verifi_instr glb local return

and verifi_instr = function
  | Putchar e ->
    let code = verifi Int e in
    match code with
    | Cst n -> printf "%c" (Char.chr code); None
    | _ -> failwith "Erreur12"
  | Set (s, e) ->
    let t = find_type s in
    let v = verifi_expr t e in None
  | If (e, st, se) ->
    let b = verifi_expr Bool e in
    let r1 = verifi_instrs st in
    let r2
  | While (e, s) ->
    let b = verifi_expr e in
    if b then verifi_instrs (s @ [While (e, s)])
  | Return e -> Some 1
  | Expr e ->
    let _ = verifi_expr e in None

and verifi_expr t = function
  | Cst 0 -> t=Bool || t=Int
  | Cst 1 -> t=Bool || t=Int
  | Cst n -> t=Int
  | Add (e1, e2) -> verifi_expr Int e1 && verifi_expr Int e2
    if t=Int then
      begin
        let v1 = verifi_expr Int e1 in
        let v2 = verifi_expr Int e2 in
        (match v1, v2 with
        | Cst n1, Cst n2 -> Cst (n1*n2)
        | _ -> failwith "Erreur8")
      end
    else failwith "Erreur9"
  | Mul (e1, e2) ->
    if t=Int then
      begin
        let v1 = verifi_expr Int e1 in
        let v2 = verifi_expr Int e2 in
        (match v1, v2 with
        | Cst n1, Cst n2 -> Cst (n1*n2)
        | _ -> failwith "Erreur3")
      end
    else failwith "Erreur4"
  | Lt (e1, e2) ->
    if t=Bool then
      begin
        let v1 = verifi_expr Int e1 in
        let v2 = verifi_expr Int e2 in
        (match v1, v2 with
         | Cst n1, Cst n2 -> if (n1 < n2) then Cst 1 else Cst 0
        | _ -> failwith "Erreur10")
      end
    else failwith "Erreur11"
  | Get s ->
    let v = find_var s in
    verifi_expr t v
  | Call (s, el) ->
    let f = find_fun s in
    if

and set_fun t s ps p =
  functions :=
    {name = s; params = ps; return = t; locals = !locals; code = p}::(!functions);
  locals := []

and set_local t s e =
  let v = verifi_expr t e in
  match t, v with
  | Int, Cst n ->
    locals := (s, Int)::(!locals)
  | Bool, Cst n -> if n != 1 || n != 0 then failwith "Expect Bool" else
    locals := (s, Bool)::(!locals)
  | _ -> failwith "Erreur2"

and set_global t s e =
  let v = verifi_expr t e in
  match t, v with
  | Int, Cst n ->
    globals := (s, Int, n)::(!globals)
  | Bool, Cst n -> if n != 1 || n != 0 then failwith "Expect Bool" else
    globals := (s, Bool, n)::(!globals)
  | _ -> failwith "Erreur1"

and find_type s =
  let e =
    try List.find (fun a -> let (ss, t) = a in ss=s) !locals with Not_found ->
    try List.find (fun a -> let (ss, t) = a in ss=s) !globals with Not_found ->
      failwith "Erreur13" in
  let (s,t) = e in t

and find_fun s =
  let e =
    try List.find (fun a -> a.name=s) !functions with Not_found ->
      failwith "Erreur14" in
  e.return
