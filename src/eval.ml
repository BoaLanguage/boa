open Ast

(* Values include *closures*, which are functions including their environments
   at "definition time." We also have "lazy" values, which let us delay the
   evaluation of expressions to support recursion. *)
type value =
  | Closure of var * typ * exp * store
  | VInt of int
  | VBool of bool
  | VTuple of value list
  | VList of typ * value list
  | VLazy of exp * store
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

(* Evaluate an expression using an environment for free variables. *)
let rec eval' (e : exp) (s : store) : value =
  let v = match e with
    | _ -> failwith "Goobyer"
  in
  (* "Force" lazy values when they are the result of a computation. *)
  match v with
  | VLazy (e, s) -> eval' e s
  | _ -> v

(* Turn values back into expressions (useful for pretty printing). *)
let rec expr_of_value v =
  match v with
  | Closure (x, t, e, s) -> Lam (x, t, e)
  | VInt i -> Int i
  | VBool b -> if b then Bool(true) else Bool(false)
  | VTuple vs -> Tuple (List.map expr_of_value vs)
  | VLazy (e, s) -> e
  | _ -> failwith "Gooby"

(* Evaluate a closed expression to an expression. *)
let eval (e : exp) : exp =
  let v = eval' e [] in
  expr_of_value v

let eval (s : stmt) : store =
  match s with
  | Exp (e) -> 