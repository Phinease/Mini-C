open Format

type typ =
  | Int
  | Bool
  | Void
  | Table   of typ
  | Struct  of string * (string * typ) list

type expr =
  | Cst     of int
  | Add     of expr * expr
  | Minus   of expr * expr
  | Mul     of expr * expr
  | Div     of expr * expr
  | Mod     of expr * expr
  | Gt      of expr * expr
  | Lt      of expr * expr
  | Geq     of expr * expr
  | Leq     of expr * expr
  | Neq     of expr * expr
  | Eqq     of expr * expr
  | Or      of expr * expr
  | And     of expr * expr
  | Not     of expr
  | Get     of string
  | Access  of string * expr
  | Call    of string * expr list

type instr =
  | Putchar   of expr
  | Set       of string * expr
  | Create    of string * typ
  | Write     of string * expr * expr
  | If        of expr * seq * seq
  | While     of expr * seq
  | Return    of expr
  | Expr      of expr

and seq = instr list

type fun_def = {
  name:       string;
  params:     (string * typ) list;
  return:     typ;
  locals:     (string * typ) list;
  code:       seq;
}

type prog = {
  globals:    (string * typ * expr) list;
  structs:    typ list;
  functions:  fun_def list;
}

let print = Printf.printf
let prints = Printf.sprintf
let globals = ref []
let structs = ref []
let locals = ref []
let returns = ref []
let functions = ref []

let get_prog () =
  { globals = !globals; structs = !structs; functions = !functions }

let rec typage_fun f =
  typage_return f.name f.return !returns

and typage_return s t lr =
  match lr with
  | [] -> ()
  | hd::tl -> if not(typage_expr t hd) then
      failwith (prints "Expected a %s return for fonction %s" (toString t) s)
    else typage_return s t tl

and typage_instr = function
  | Putchar e ->
    if not(typage_expr Int e) then failwith "Expected a Int in putchar"
  | Set (s, e) ->
    if not(typage_expr (find_type s) e) then failwith
    (prints "Expected a %s for variable %s" (toString (find_type s)) s)
  | If (e, pt, pf) ->
    if not(typage_expr Bool e) then failwith "Expected a Bool in if condition"
  | While (e, p) ->
    if not(typage_expr Bool e) then failwith "Expected a Bool in while condition"
  | Write (s, e1, e2) ->
    if not(typage_expr Int e1) then failwith "Expected a Int for index of table"
    else if not(typage_expr (find_type s) e1) then
      failwith (prints "Expected a %s for table %s" (toString (find_type s)) s)
  | Return e -> returns := (e::!returns)
  | Expr e -> if not(typage_expr Int e || typage_expr Void e) then failwith "Unexpected expr"
  | Create (s, t) -> ()

and typage_expr t = function
  | Cst 0 | Cst 1 -> t=Bool || t=Int
  | Cst n -> t=Int
  | Add (e1, e2) | Minus (e1, e2) | Mul (e1, e2) | Div (e1, e2) | Mod (e1, e2)
    -> t=Int && typage_expr Int e1 && typage_expr Int e2
  | Lt (e1, e2) | Gt (e1, e2) | Geq (e1, e2) | Leq (e1, e2)
    -> t=Bool && typage_expr Int e1 && typage_expr Int e2
  | Neq (e1, e2) | Eqq (e1, e2)
    -> t=Bool
  | Or (e1, e2) | And (e1, e2)
    -> t=Bool && typage_expr Bool e1 && typage_expr Bool e2
  | Not e -> t=Bool && typage_expr Bool e
  | Access (s, e) -> let tt = find_table s in t=tt && typage_expr Int e
  | Get s -> let tt = find_type s in t=tt
  | Call (s, el) ->
    let f = find_fun s in f.return=t && (typage_exprs el f.params)

and typage_exprs el pl =
  match el, pl with
  | [], [] -> true
  | [], _ | _, [] -> false
  | e::tl1, p::tl2 ->
    let (s, t) = p in (typage_expr t e)&&(typage_exprs tl1 tl2)

and set_fun t s ps p =
  let f = {name = s; params = ps; return = t; locals = !locals; code = p} in
  functions := f::(!functions);
  typage_fun f;
  print_fun f;
  locals := [];
  returns := []

and set_local t s e =
  if (existe_local s) then failwith (s ^ " already exists in local");
  match t with
  | Table _ -> if typage_expr Int e then locals := (s, t)::(!locals) else failwith
        (prints "Expected a Int for table constructor %s" s)
  | _ -> if typage_expr t e then locals := (s, t)::(!locals) else failwith
        (prints "Expected a %s for variable %s" (toString t) s)

and set_param t s = locals := (s, t)::(!locals)

and set_global t s e =
  if (existe_global s) then failwith (s ^ " already exists in global");
  print "Global %s: %s\n" s (toString t);
  match t with
  | Table _ -> if typage_expr Int e then globals := (s, t, e)::(!globals) else failwith
        (prints "Expected a Int for table constructor %s" s)
  | _ -> if typage_expr t e then globals := (s, t, e)::(!globals) else
      failwith (prints "Expected a %s for variable %s" (toString t) s)

and set_struct s1 s2 lt =
  if s1=s2 then structs := (Struct (s1, lt))::(!structs) else
    failwith "Inconsistent struct name"

and find_table s =
  let t =
    try let (s, t) = List.find (fun a -> let (ss, t) = a in ss=s) !locals in t
    with Not_found ->
    try let (s, t, e) = List.find (fun a -> let (ss, t, e) = a in ss=s) !globals in t
    with Not_found -> failwith ("unknown ident: " ^ s) in
  match t with
  | Table tt -> tt
  | _ -> failwith ("unknown table: " ^ s)

and find_type s =
  let t =
    try let (s, t) = List.find (fun a -> let (ss, t) = a in ss=s) !locals in t
    with Not_found ->
    try let (s, t, e) = List.find (fun a -> let (ss, t, e) = a in ss=s) !globals in t
    with Not_found -> failwith ("unknown ident: " ^ s) in
  match t with
  | Table tt -> tt
  | _ -> t

and find_fun s =
  try List.find (fun f -> f.name=s) !functions with Not_found ->
    failwith ("unknown function called " ^ s)

and existe_global s = List.exists (fun a -> let (ss, t, e) = a in ss=s) !globals
and existe_local s = List.exists (fun a -> let (ss, t) = a in ss=s) !locals

and print_fun f =
  print "Function %s (" f.name; print_params f.params; print "): %s\n" (toString f.return);
  print_instr f.code;
  print "Fini %s\n\n" f.name

and print_instr li =
  match li with
  | [] -> ()
  | (Putchar e)::tl ->
    print "Putchar(%s)\n" (toStringE e); print_instr tl
  | (Set (s, e))::tl ->
    print "Set %s %s: %s\n" (toString (find_type s)) s (toStringE e); print_instr tl
  | (If (e, pt, pf))::tl ->
    print "If (%s):\n" (toStringE e); print_instr pt; print "Else:\n"; print_instr pf; print "Fini If\n"; print_instr tl
  | (While (e, p))::tl ->
    print "While (%s): \n" (toStringE e); print_instr p; print "Fini While\n"; print_instr tl
  | (Write (s, e1, e2))::tl ->
    print "Write table %s[%s]: %s\n" s (toStringE e1) (toStringE e2); print_instr tl
  | (Return e)::tl -> print "Return %s\n" (toStringE e); print_instr tl
  | (Expr e)::tl -> print "%s\n" (toStringE e); print_instr tl
  | (Create (s, t))::tl ->  print "New %s table %s\n" (toString t) s; print_instr tl

and toStringE = function
  | Cst n ->          prints "Cst %d"         n
  | Add (e1, e2) ->   prints "Add(%s, %s)"      (toStringE e1) (toStringE e2)
  | Minus (e1, e2) -> prints "Minus(%s, %s)"    (toStringE e1) (toStringE e2)
  | Mul (e1, e2) ->   prints "Mul(%s, %s)"      (toStringE e1) (toStringE e2)
  | Div (e1, e2) ->   prints "Div(%s, %s)"      (toStringE e1) (toStringE e2)
  | Mod (e1, e2) ->   prints "Mod(%s, %s)"      (toStringE e1) (toStringE e2)
  | Neq (e1, e2) ->   prints "Neq(%s, %s)"      (toStringE e1) (toStringE e2)
  | Eqq (e1, e2) ->   prints "Eqq(%s, %s)"      (toStringE e1) (toStringE e2)
  | Lt (e1, e2) ->    prints "Lt(%s, %s)"       (toStringE e1) (toStringE e2)
  | Gt (e1, e2) ->    prints "Gt(%s, %s)"       (toStringE e1) (toStringE e2)
  | Geq (e1, e2) ->   prints "Geq(%s, %s)"      (toStringE e1) (toStringE e2)
  | Leq (e1, e2) ->   prints "Leq(%s, %s)"      (toStringE e1) (toStringE e2)
  | Or (e1, e2) ->    prints "Or(%s, %s)"       (toStringE e1) (toStringE e2)
  | And (e1, e2) ->   prints "And(%s, %s)"      (toStringE e1) (toStringE e2)
  | Not e ->          prints "Not(%s)"          (toStringE e)
  | Access (s, e) ->  prints "Access(%s, %s)" s (toStringE e)
  | Get s ->          prints "Get(%s)"        s
  | Call (s, el) ->   prints "Call(%s)"       s

and print_params ps =
  match ps with
  | [] -> ()
  | (s, t)::[] -> print "%s: %s"    s (toString t)
  | (s, t)::tl -> print "%s: %s, "  s (toString t); print_params tl

and toString t =
  match t with
  | Int -> "Int"
  | Bool -> "Bool"
  | Void -> "Void"
  | Table s -> "Table " ^ (toString s)
  | Struct (s, lt) -> "Struct" ^ s
