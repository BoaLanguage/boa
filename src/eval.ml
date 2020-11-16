open Ast

(* Values include *closures*, which are functions including their environments
   at "definition time." We also have "lazy" values, which let us delay the
   evaluation of expressions to support recursion. *)
type value =
  | Closure of var list * stmt * store
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
exception IllegalBreak
exception IllegalContinue
exception IllegalReturn

(* A type for configurations. *)
type configuration = 
  store * stmt * stmt * ((stmt * stmt * stmt) list)


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
  (* | Closure (x, t, e, s) -> Lam (x, t, e) *)
  (* | VInt i -> Int i
  | VBool b -> if b then Bool(true) else Bool(false)
  | VTuple vs -> Tuple (List.map expr_of_value vs)
  | VLazy (e, s) -> e *)
  | _ -> failwith "Gooby"

(* Evaluate a closed expression to an expression. *)
let evale (e : exp) (sigma : store): value =
  let v = eval' e [] in
  expr_of_value v

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