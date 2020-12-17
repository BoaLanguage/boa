open Ast
open Pprint

type error_info = string
exception IllTyped of error_info
type mappings = (var * scheme) list
type constraints = (typ * typ) list
type substitution = (int * typ) list

let num_tvars_used = ref (-1)

let fresh_tvar () = num_tvars_used := !num_tvars_used + 1; TVar(!num_tvars_used)
let reset_tvars () = num_tvars_used := -1

let global_substitution : substitution ref = ref []

let str_of_gamma = 
  List.fold_left (fun acc (v, t) -> acc ^ ", " ^ v ^ " => " ^ (str_of_typ t)) ""

let str_of_constr = 
  List.fold_left 
  (fun acc (t1, t2) -> acc ^ ", " ^ (str_of_typ t1) ^ " == " ^ (str_of_typ t2)) ""

let str_of_sub = 
  List.fold_left 
  (fun acc (t1, t2) -> acc ^ ", " ^ (string_of_int t1) ^ " => " ^ (str_of_typ t2)) ""

let t_string = TBase("String")
let t_int = TBase("Int")
let t_bool = TBase("Bool")
let t_none = TBase("None")
let t_unit = TBase("Unit")

let no_constraints = []
let empty_substituition = []
let empty_gamma = []

let zp l = List.map2 (fun x y -> (x,y)) l
let unzip l = (List.map fst l, List.map snd l) 
let map_tuple (f : 'a list -> 'b list) l = 
  let l1, l2 = unzip l in zp (f l1) (f l2)

let lookup_typ: (var -> mappings -> scheme option) = List.assoc_opt

let any (f: 'a -> bool) : ('a list -> bool) = 
List.fold_left (fun acc b -> acc || (f b)) false

let (|??) (opt: 'a option) (default: 'a): 'a = 
  match opt with
  | Some t -> t
  | None -> default

let substitute (sigma: substitution): typ list -> typ list =
  let rec sub (typ: typ) : typ = 
  match typ with 
  | TVar (i) -> List.assoc_opt i sigma |?? typ
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
  | [] -> []
  | (t, t')::rest -> 
    if t = t' then unify rest
    else 
    begin
      let sub_constraints subst = map_tuple (substitute subst) rest in
      match t, t' with 
      | TVar i , _ when not (i ==> t') -> 
          let sigma' = [(i, t')] in
          sigma' @ (unify @@ sub_constraints sigma')
      | _ , TVar i when not (i ==> t) -> 
          let sigma' =  [(i, t)] in 
          sigma' @ (unify @@ sub_constraints sigma')
      | TFun (t1, t2), TFun (t1', t2') -> 
        unify @@ (rest @ (t1, t1') :: (t2, t2) :: [])
      | _ -> 
    raise @@ IllTyped "Typing of program led to above impossible constraints"
    end

let rec inst_scheme scheme = 
  let rec replace_tvar_occurrences old_var typ = 
    match typ with 
    | TVar i when i = old_var -> fresh_tvar ()
    | TVar i -> TVar i
    | TFun (t1, t2) -> TFun(replace_tvar_occurrences old_var t1, replace_tvar_occurrences old_var t2)
    | TTuple tlist -> TTuple(List.map (replace_tvar_occurrences old_var) tlist)
    | TList t -> TList(replace_tvar_occurrences old_var t)
    | _ -> typ
    in
  match scheme with 
  | ([], t) -> t
  | (tv::rest, t) -> inst_scheme (rest, replace_tvar_occurrences tv t)

let check_expr (gamma : mappings) (e : exp) : typ = 
  match e with 
  | Int i -> t_int
  | Bool b -> t_bool
  | String s -> t_string
  | Var v -> 
    match lookup_typ v gamma with 
    | None -> raise @@ IllTyped "Unbound variable"
    | Some scheme -> inst_scheme scheme


let check_statement () = ()

let check (statement: stmt): mappings = 
  check_statement statement