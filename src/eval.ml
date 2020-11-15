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
    | True -> VBool true
    | False -> VBool false
    | Empty t -> VList (t, [])
    | Int i -> VInt i
    | Var x ->
      lookup s x
    | App (e1, e2) ->
      let f = eval' e1 s in begin
        match f with
        | Closure (x, _, e3, s') ->
          let v = eval' e2 s in
          eval' e3 (update s' x v)
        | _ -> raise (IllformedExpression "Attempted application on non-function")
      end
    | Lam (x, t, e) -> Closure (x, t, e, s)
    | Let (x, e1, e2) ->
      let v = eval' e1 s in
      eval' e2 (update s x v)
    | Binary (op, e1, e2) ->
      let v1 = eval' e1 s in
      let v2 = eval' e2 s in
      let ex = (IllformedExpression "Bad argument to binary operator") in begin
        match op with
        | Plus -> begin
            match (v1, v2) with
            | (VInt i1, VInt i2) -> VInt (i1 + i2)
            | _ -> raise ex
          end
        | Minus -> begin
            match (v1, v2) with
            | (VInt i1, VInt i2) -> VInt (i1 - i2)
            | _ -> raise ex
          end
        | Times -> begin
            match (v1, v2) with
            | (VInt i1, VInt i2) -> VInt (i1 * i2)
            | _ -> raise ex
          end
        | Less -> begin
            match (v1, v2) with
            | (VInt i1, VInt i2) -> VBool (i1 < i2)
            | _ -> raise ex
          end
        | Greater -> begin
            match (v1, v2) with
            | (VInt i1, VInt i2) -> VBool (i1 > i2)
            | _ -> raise ex
          end
        | Equal -> begin
            match (v1, v2) with
            | (VInt i1, VInt i2) -> VBool (i1 = i2)
            | _ -> raise ex
          end
        | And -> begin
            match (v1, v2) with
            | (VBool b1, VBool b2) -> VBool (b1 && b2)
            | _ -> raise ex
          end
        | Or -> begin
            match (v1, v2) with
            | (VBool b1, VBool b2) -> VBool (b1 || b2)
            | _ -> raise ex
          end
        | Cons -> begin
            match (v1, v2) with
            | (v1, VList(t, vs)) -> VList (t, v1::vs)
            | _ -> raise ex
          end
      end
    | Unary (op, e) ->
      let v = eval' e s in begin
        match op with
        | Not -> begin
            match v with
            | VBool b -> VBool (not b)
            | _ -> raise (IllformedExpression "Bad argument to unary operator")
          end
      end
    | Tuple es ->
      let rec eval_all es = begin
        match es with
        | e::es -> (eval' e s)::(eval_all es)
        | [] -> []
      end in
      VTuple (eval_all es)
    | Proj (e, i) ->
      let v = eval' e s in begin
        match v with
        | VTuple vs -> List.nth vs i
        | _ -> raise (IllformedExpression "Bad argument to projection operator")
      end
    | Fix e -> begin
        match eval' e s with
        | Closure (x, t, e, s) ->
          let rec s' = (x, v')::s and
          v' = VLazy (e, s') in
          eval' e s'
        | _ -> raise (IllformedExpression "Bad argument to fixed-point operator")
      end
    | If (ce, te, fe) -> begin
        let cv = eval' ce s in
        match cv with
        | VBool true -> eval' te s
        | VBool false -> eval' fe s
        | _ ->
          raise (IllformedExpression "Bad argument to if conditional expression")
      end
    | Match (le, ee, hre) ->
      let lv = eval' le s in begin
        match lv with
        | VList (_, []) -> eval' ee s
        | VList (t, head::rest) ->
          let hrv = eval' hre s in begin
            match hrv with
            | Closure (x, _, e', s') ->
              let arg = VTuple ([head; VList (t, rest)]) in
              eval' e' (update s' x arg)
            | _ ->
              raise (IllformedExpression "Non-function argument to match expression")
          end
        | _ -> raise (IllformedExpression "Non-list argument to match expression")
      end
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
  | VBool b -> if b then True else False
  | VTuple vs -> Tuple (List.map expr_of_value vs)
  | VList (t, vs) -> let f ve acc = Binary (Cons, (expr_of_value ve), acc) in
    List.fold_right f vs (Empty t)
  | VLazy (e, s) -> e

(* Evaluate a closed expression to an expression. *)
let eval (e : exp) : exp =
  let v = eval' e [] in
  expr_of_value v
