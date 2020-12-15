open Ast
open Pprint

type error_info = string
exception IllTyped of error_info
type mappings = (var * typ) list
type constraints = (typ * typ) list
type substitution = (int * typ) list


let t_string = TBase("String")
let t_int = TBase("Int")
let t_bool = TBase("Bool")
let t_none = TBase("None")

let no_constraints = []
let empty_substituition = []
let empty_gamma = []

let zip = List.map2 (fun x y -> (x,y))
let unzip l = (List.map fst l, List.map snd l) 
let map_tuple (f : 'a list -> 'b list) l = 
  let l1, l2 = unzip l in zip (f l1) (f l2)

let any (f: 'a -> bool) : ('a list -> bool) = 
List.fold_left (fun acc b -> acc || (f b)) false

let (|??) (opt: 'a option) (default: 'a): 'a = 
  match opt with
  | Some t -> t
  | None -> default


let check_expression (constraints: constraints) 
  (gamma: mappings) 
  (exp: Ast.exp): typ * constraints = 
  failwith "Unimplemented"

let check_statement (constraints: constraints) 
  (gamma: mappings) 
  (statement: stmt):  mappings * constraints = 
  failwith "Unimplemented"

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



let check (statement: stmt): mappings = 
  let gamma, constraints = 
    check_statement no_constraints empty_gamma statement in
  let substitution = unify constraints in 
  let names, types = unzip gamma in
  zip names @@ substitute substitution types
  
  