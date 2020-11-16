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

let evalb (b : binop) (l : value) (r : value) : value = 
  match b with
  | Plus -> VInt((int_of_value l) + (int_of_value r))
  | _ -> failwith "yo"

(* Evaluate a closed expression to an expression. *)
let rec evale (e : exp) (s : store) : value =
  match e with
  | Var v -> lookup s v
  | AttrAccess (e, v) -> failwith "not sure"
  | SliceAccess (e1, e2) -> failwith "idk"
  | Binary (bin, e1, e2) -> evalb bin (evale e1 s) (evale e2 s)
  | _ -> failwith "not implemented expression"

let evals (s : stmt) (sigma : store) : store =
  match s with
  | Exp e -> []
  | _ -> failwith "poop ass"