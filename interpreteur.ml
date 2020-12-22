open Verificateur

exception Retourn of int

let functions = Hashtbl.create 10
let globals = Hashtbl.create 10
let env = ref (Hashtbl.create 10)
let envs = ref []
let tables = Hashtbl.create 10

let print = Printf.printf
let prints = Printf.sprintf

let rec interp_prog pg =
  add_globals pg.globals;
  add_funs pg.functions;
  let main = Hashtbl.find functions "main" in
  interp_fun main []

and interp_fun f ps =
  envs := (!env)::(!envs);
  let t = Hashtbl.create 10 in
  env := t;
  let (params, code) = f in
  let res = ref 0 in
  add_params params ps;
  (try interp_instrs code with Retourn x -> (res := x));
  env := List.hd (!envs);
  envs := List.tl (!envs);
  !res

and interp_instrs li =
  match li with
  | [] -> ()
  | hd::tl -> interp_instr hd; interp_instrs tl

and interp_instr = function
  | Putchar e -> print "%c\n" (char_of_int (interp_expr e))
  | Set (s, e) ->
    (try Hashtbl.replace (!env) s (interp_expr e) with Not_found ->
     try Hashtbl.replace globals s (interp_expr e) with Not_found -> failwith ("No variable " ^ s))
  | If (e, pt, pf) -> if (interp_expr e)=1 then interp_instrs pt else interp_instrs pf
  | While (e, p) -> if (interp_expr e)=1 then interp_instrs p
  | Write (s, e1, e2) -> let t = find_table s in t.(interp_expr e1) <- (interp_expr e2)
  | Return e -> raise (Retourn (interp_expr e))
  | Expr e -> let _ = interp_expr e in ()
  | Create (s, t) -> let t = Array.make 100 0 in Hashtbl.add tables s t


and interp_expr = function
  | Cst n -> n
  | Add (e1, e2) -> (interp_expr e1) + (interp_expr e2)
  | Minus (e1, e2) -> (interp_expr e1) - (interp_expr e2)
  | Mul (e1, e2) -> (interp_expr e1) * (interp_expr e2)
  | Div (e1, e2) -> (interp_expr e1) / (interp_expr e2)
  | Mod (e1, e2) -> (interp_expr e1) mod (interp_expr e2)
  | Lt (e1, e2) -> if (interp_expr e1) < (interp_expr e2) then 1 else 0
  | Gt (e1, e2) -> if (interp_expr e1) > (interp_expr e2) then 1 else 0
  | Geq (e1, e2) -> if (interp_expr e1) >= (interp_expr e2) then 1 else 0
  | Leq (e1, e2) -> if (interp_expr e1) <= (interp_expr e2) then 1 else 0
  | Neq (e1, e2) -> if (interp_expr e1) != (interp_expr e2) then 1 else 0
  | Eqq (e1, e2) -> if (interp_expr e1) == (interp_expr e2) then 1 else 0
  | Or (e1, e2) -> (interp_expr e1) lor (interp_expr e2)
  | And (e1, e2) -> (interp_expr e1) land (interp_expr e2)
  | Not e -> if (interp_expr e) = 0 then 1 else 0
  | Access (s, e) -> let t = find_table s in t.(interp_expr e)
  | Get s -> find_var s
  | Call (s, el) -> let f = try Hashtbl.find functions s with Not_found ->
      failwith ("No function " ^ s) in
    interp_fun f el

and find_table s =
  try Hashtbl.find tables s with Not_found -> failwith ("No table " ^ s)

and find_var s =
  try Hashtbl.find (!env) s with Not_found ->
  try Hashtbl.find globals s with Not_found ->
    failwith ("No variable " ^ s)

and add_globals globs =
  match globs with
  | [] -> ()
  | hd::tl -> let (s, t, e) = hd in Hashtbl.add globals s (interp_expr e); add_globals tl

and add_funs funs =
  match funs with
  | [] -> ()
  | hd::tl -> Hashtbl.add functions hd.name (hd.params, hd.code); add_funs tl

and add_params params el =
  match params, el with
  | [], [] -> ()
  | (s, t)::tl1, e::tl2 -> Hashtbl.add (!env) s (interp_expr e); add_params tl1 tl2
  | _ -> failwith "Not enough params"
