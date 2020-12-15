open Ast
open Pprint

type error_info = string
exception IllTyped of error_info
type mappings = (var * typ) list
type constraints = (typ * typ) list
type substitution = (int * typ) list

let num_tvars_used = ref (-1)

let fresh_tvar () = num_tvars_used := !num_tvars_used + 1; TVar(!num_tvars_used)

let str_of_gamma = 
  List.fold_left (fun acc (v, t) -> acc ^ ", " ^ v ^ " => " ^ (str_of_typ t)) ""

let str_of_constr l = 
  List.fold_left 
  (fun acc (t1, t2) -> acc ^ ", " ^ (str_of_typ t1) ^ " == " ^ (str_of_typ t2)) ""

let t_string = TBase("String")
let t_int = TBase("Int")
let t_bool = TBase("Bool")
let t_none = TBase("None")

let no_constraints = []
let empty_substituition = []
let empty_gamma = []

let zp l = List.map2 (fun x y -> (x,y)) l
let unzip l = (List.map fst l, List.map snd l) 
let map_tuple (f : 'a list -> 'b list) l = 
  let l1, l2 = unzip l in zp (f l1) (f l2)

let lookup_typ: (var -> mappings -> typ option) = List.assoc_opt

let any (f: 'a -> bool) : ('a list -> bool) = 
List.fold_left (fun acc b -> acc || (f b)) false

let (|??) (opt: 'a option) (default: 'a): 'a = 
  match opt with
  | Some t -> t
  | None -> default


let check_expression (constraints: constraints) 
  (gamma: mappings) 
  (exp: Ast.exp): typ * constraints = 
  match exp with 
  | Int i -> (t_int, constraints)
  | String s -> (t_string, constraints)
  | _ -> failwith "Unimplemented"

let rec check_statement (constraints: constraints) 
  (gamma: mappings) 
  (statement: stmt):  mappings * constraints = 
  let check_expr = check_expression constraints gamma in 
  match statement with 
  | Exp e -> let _, constraints' = check_expr e in gamma, constraints'
  | Assign (v, e) -> 
  let var_typ = 
    (match lookup_typ v gamma with 
    | Some typ -> typ
    | None -> raise @@ IllTyped "Assignment to unbound variable") in 
  let expr_typ, constraints' = check_expr e in gamma, (expr_typ, var_typ)::constraints'
  | Block [] -> gamma, constraints
  | Block (stmt::rest) -> 
    let gamma', constraints' = check_statement constraints gamma stmt in 
    check_statement constraints' gamma' @@ Block(rest)
  | Decl (t_opt, v) -> 
    let typ = t_opt |?? fresh_tvar () in 
    (v, typ)::gamma, constraints
  | Pass -> gamma, constraints
  | _ -> failwith "Unimplemented"

let substitute (sigma: substitution): typ list -> typ list =
  let rec sub (typ: typ) : typ = 
  match typ with 
  | TVar (i)->  List.assoc_opt i sigma |?? typ
  | TBase s as typ -> typ
  | TFun(t1, t2) -> TFun(sub t1, sub t2)
  | TList t -> TList(sub t)
  | TTuple lst -> TTuple(List.map sub lst) in
  List.map @@ sub

let rec unify (constraints: constraints): substitution = 
  let rec (==>) (i: int) (typ: typ): bool = 
    match typ with 
    | TVar i' -> i = i'
    | TFun (t1, t2) -> i ==> t1 || i ==> t2
    | TBase s -> false
    | TTuple lst -> any ((==>) i) lst
    | TList tau' -> i ==> tau' in
  match constraints with 
  | [] -> empty_substituition
  | (t, t')::rest -> 
    if t = t' then unify rest
    else 
    begin
      let sub_constraints subst = map_tuple (substitute subst) rest in
      match t, t' with 
      | TVar i , _ when not (i ==> t') -> 
          let sigma' = [(i, t')] in
          (unify @@ sub_constraints sigma') @ sigma'
      | _ , TVar i when not (i ==> t) -> 
          let sigma' =  [(i, t)] in 
          (unify @@ sub_constraints sigma') @ sigma'
      | TFun (t1, t2), TFun (t1', t2') -> 
        unify @@ (t1, t1') :: (t2, t2') :: rest
      | _ -> 
      (* Format.printf "%s == %s" (str_of_typ t) (str_of_typ t');
      Format.printf "--\n%s\n--" (str_of_constr c); *)
    raise @@ IllTyped "Typing of program led to above impossible constraints"
    end

let check (statement: stmt): mappings = 
  let gamma, constraints = 
    check_statement no_constraints empty_gamma statement in
  let substitution = unify constraints in 
  let names, types = unzip gamma in
  zp names (substitute substitution types)