open Ast

(* Values include *closures*, which are functions including their environments
   at "definition time." We also have "lazy" values, which let us delay the
   evaluation of expressions to support recursion. *)
(* type value =
  | Closure of var * typ * stmt * store
  | VInt of int
  | VBool of bool
  | VTuple of value list
  | VList of typ * value list
  | VLazy of exp * store *)
type value = 
  | VInt of int
  | VString of string
  | VBool of bool
  | Closure of var * stmt * store
  | VDict of (value * value) list
  | VTuple of (value * value)
  | VList of value list
and store = (var * value) list

(* Interpreter exceptions. *)
type error_info = string
exception IllformedExpression of error_info
exception UnboundVariable of var

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

(* Evaluate a closed expression to an expression. *)
let rec evale (e : exp) (s : store) : value =
  match e with
  | Var v -> lookup s v
  | AttrAccess (e, v) -> failwith "not sure"
  | SliceAccess (e1, e2) -> failwith "idk"
  | Binary (bin, e1, e2) -> evalb bin (evale e1 s) (evale e2 s)
  | Int i -> VInt(i)
  | _ -> failwith "not implemented expression"

let print_value (v : value) =
    match v with
    | VInt i -> Format.printf "%d" i;
    | VString s -> Format.printf "%s" s;
    | VBool b -> Format.printf "%b" b;
    | Closure (v, body, store) -> Format.printf "%s" v;
    | _ -> failwith "unimplemented"
  
let rec print_store (s : store) =
  List.iter (fun (var, v) -> Format.printf "%s: " var; print_value v) s; ()

let rec evals (s : stmt) (sigma : store) : store =
  match s with
  | Block [] -> sigma
  | Block (a::b) -> evals (Block(b)) (evals a sigma)
  | Exp e -> [("shit", evale e sigma)]
  | Print e -> ignore (print_value (evale e sigma)); sigma
  | _ -> Format.printf "%s" "Hey"; []