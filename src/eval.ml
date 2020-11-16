open Ast

(* Values include *closures*, which are functions including their environments
   at "definition time." We also have "lazy" values, which let us delay the
   evaluation of expressions to support recursion. *)

type value = 
  | VInt of int
  | VString of string
  | VBool of bool
  | Closure of var list * stmt * store
  | VDict of (value * value) list
  | VTuple of (value * value)
  | VList of value list
and store = (var * value) list

(* Interpreter exceptions. *)
type error_info = string
exception IllformedExpression of error_info
exception UnboundVariable of var
exception IllegalBreak
exception IllegalContinue
exception IllegalReturn

(* A type for configurations. *)
type configuration = 
  store * stmt * stmt * ((stmt * stmt * stmt) list)

let make_configuration (prog: stmt) : configuration = 
  ([], prog, Pass, [])
(* A function to update the binding for x in store s.
   update (s, x, v) returns the store s[x->v]. *)
let rec update s x v : store =
  match s with
  | [] ->
    [(x, v)]
  | (y ,u)::t ->
    if x = y then (x, v)::t
    else (y, u)::(update t x v)

(* A function to look up the binding for a variable in a store.
   lookup s x returns s(x) or UnboundVariable if s is not defined on s. *)
let rec lookup s x : value =
  match s with
  | [] ->
    raise (UnboundVariable x)
  | (y,u)::t ->
    if x = y then u
    else (lookup t x)

let int_of_value (v : value) : int =
  match v with
  | VInt i -> i
  | _ ->  failwith "BIG PROBLEM"

let bool_of_value (v : value) : bool = 
  match v with
  | VBool b -> b
  | _ -> failwith "BIG PROBLEM"

let rec pow (a : int) (b : int) : int = 
  if (b = 0) then 1 else 
  if (b = 1) then a else pow (a * a) (b - 1)

let evalb (b : binop) (l : value) (r : value) : value = 
  match b with
  | Plus -> VInt((int_of_value l) + (int_of_value r))
  | Less -> VBool((int_of_value l) < (int_of_value r))
  | Greater -> VBool((int_of_value l) > (int_of_value r))
  | And -> VBool((bool_of_value l) && (bool_of_value r))
  | Or -> VBool((bool_of_value l) || (bool_of_value r))
  | Equal -> VBool((int_of_value l) = (int_of_value r))
  | Times -> VInt((int_of_value l) * (int_of_value r))
  | Minus -> VInt((int_of_value l) - (int_of_value r))
  | Divide -> VInt((int_of_value l) / (int_of_value r))
  | Mod -> VInt((int_of_value l) mod (int_of_value r))
  | Exponent -> VInt(pow (int_of_value l) (int_of_value r))
  | IntDivide -> VInt((int_of_value l) / (int_of_value r))
  | Leq -> VBool((int_of_value l) <= (int_of_value r))
  | Geq -> VBool((int_of_value l) >= (int_of_value r))
  | Is -> VBool((int_of_value l) = (int_of_value r))
  | In -> failwith "unimplemented"
  | Neq -> VBool((int_of_value l) <> (int_of_value r))

let rec evale (e : exp) (s : store) : value =
  match e with
  | Var v -> lookup s v
  | AttrAccess (e, v) -> failwith "not sure"
  | SliceAccess (e1, e2) -> failwith "idk"
  | Binary (bin, e1, e2) -> evalb bin (evale e1 s) (evale e2 s)
  | Int i -> VInt(i)
  | _ -> failwith "not implemented expression"

let call (closure: value) (arg_list: value list) = 
  failwith "Unimplemented"

let rec evals (conf:configuration) : store =
  match conf with
  | sigma, Pass, Pass, kappa -> sigma
  | sigma, Pass, c, kappa -> evals (sigma, c, Pass, kappa)
  | sigma, Assign(v, a), c, kappa ->
    let n = evale a sigma in
    evals ((v, n)::sigma, c, Pass, kappa)
  | sigma, Block(c1::t), Pass, kappa -> evals (sigma, c1, Block(t), kappa)
  | sigma, Block(c1::t), c3, kappa -> evals (sigma, c1, Block(t@[c3]), kappa)

  | sigma, Block([]), c3, kappa -> evals (sigma, Pass, c3, kappa)

  | sigma, If(b,c1,c2), c3, kappa -> let b_result = evale b sigma in 
    (match b_result with
    | VBool b' -> if b' 
      then evals (sigma, c1, c3, kappa) 
      else evals (sigma, c2, c3, kappa)
    | _-> failwith "If guard must be a boolean")

  | sigma, While(b, c1), c2, kappa -> 
    let b_result = evale b sigma in 
    (match b_result with
    | VBool b' -> if b' 
      then 
        let w = While(b, c1) in 
        let s_continue = Block([w;c2]) in 
        let s_break = c2 in
        let s_return = 
          (match kappa with 
          | [] -> Pass  
          | (_,_, s)::t -> s) in 
        evals (sigma, c1, Block([w;c2]), (s_break, s_continue, s_return)::kappa)
      else evals (sigma, Pass, c2, kappa)
    | _ -> failwith "While guard must be a boolean")

  | sigma, Print(a), c, kappa -> let n = evale a sigma in 
    (* print_int n; print_newline (); evals (sigma, Pass, c, kappa) *)
    failwith "Unimplemented: Printing"

  | sigma, Break, c, (c_b, c_c, _)::kappa_t -> 
    evals (sigma, c_b, Pass, kappa_t)
  | sigma, Continue, c, (c_b, c_c, _)::kappa_t -> 
    evals (sigma, c_c, Pass, kappa_t)
  | sigma, Return e, c, (c_b, c_c, c_r)::kappa_t -> 
    evals (("return", evale e sigma)::sigma, c_r, Pass, kappa_t) 
  | sigma, Break, c, [] -> failwith "Illegal break"

  | sigma, Continue, c, [] -> failwith "Illegal continue"
  | sigma, Return e, c, [] -> failwith "Illegal Return" (* TODO: separate cont. lists*)
  | sigma, Def (_, name, args, body), c, kappa -> 
    (name, Closure((List.map snd args), body, sigma))::sigma
  
  | (_,
(Exp _|Decl (_, _)|AttrAssgn (_, _, _)|Sliceassgn (_, _, _)|
For (_, _, _)|Class (_, _, _)), _, _) -> failwith "unimplemented"
