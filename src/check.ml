open Ast
open Pprint
open Set

type error_info = string
exception IllTyped of error_info
type mappings = (var * scheme) list
type constraints = (typ * typ) list
type substitution = (int * typ) list

let num_tvars_used = ref (-1)

let fresh_tvar () = num_tvars_used := !num_tvars_used + 1; TVar(!num_tvars_used)
let reset_tvars () = num_tvars_used := -1

let global_substitution : substitution ref = ref []

let str_of_old_gamma = 
  List.fold_left (fun acc (v, t) -> acc ^ ", " ^ v ^ " => " ^ (str_of_typ t)) ""

let str_of_int_list = 
  List.fold_left (fun acc i -> acc ^ ", " ^ (string_of_int i)) ""

let str_of_gamma : mappings -> string = 
  List.fold_left (fun acc (v, (ilist, t)) -> acc ^ ", " ^ v ^ " => " ^ "forall " ^ str_of_int_list ilist ^ ", " ^ (str_of_typ t)) ""

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

module IS = Set.Make(Int)

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

let substitute_scheme (sigma: substitution): typ list -> typ list =
  let rec sub (typ: typ) : typ = 
  match typ with 
  | TVar (i) -> List.assoc_opt i sigma |?? typ
  | TBase s as typ -> typ
  | TFun(t1, t2) -> TFun(sub t1, sub t2)
  | TList t -> TList(sub t)
  | TTuple lst -> TTuple(List.map sub lst) in
  List.map @@ sub

let update_global_subst new_subst = 
  let keys, vals = unzip !global_substitution in 
  global_substitution := new_subst @ zp keys (substitute new_subst vals)
  
let rec (==>) (i: int) (typ: typ): bool = 
  match typ with 
  | TVar i' -> i = i'
  | TFun (t1, t2) -> i ==> t1 || i ==> t2
  | TBase s -> false
  | TTuple lst -> any ((==>) i) lst
  | TList tau' -> i ==> tau'

let rec ftv (t : typ) (bound : tvar list) : IS.t = 
  match t with 
  | TVar i -> if List.mem i bound then IS.empty else IS.add i IS.empty
  | TFun(t1, t2) -> IS.union (ftv t1 bound) (ftv t2 bound)
  | TBase s -> IS.empty
  | TTuple tlist -> List.fold_left (fun acc el -> IS.union (ftv el bound) acc) IS.empty tlist
  | TList typ -> ftv typ bound

let rec typ_var_diff (t1 : typ) (gamma : mappings) : int list = 
  let t1_ftv = ftv t1 [] in 
  let gamma_ftv = List.fold_left (fun acc (_, (lst, t)) -> IS.union (ftv t lst) acc) IS.empty gamma in 
  IS.elements (IS.diff t1_ftv gamma_ftv)
  
let sub_gamma (s : substitution) (gamma : mappings) : mappings = 
  let vars, schemes = unzip gamma in 
  let foralls, types = unzip schemes in 
  zp (vars) (zp foralls @@ substitute s types)

let rec unify (constraints: constraints): substitution = 
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
        unify @@ ((t1, t1') :: (t2, t2') :: rest)
      | _ -> 
    raise @@ IllTyped "Typing of program led to above impossible constraints"
    end

let rec inst_scheme scheme = 
  let rec replace_tvar_occurrences old_var typ fresh = 
    match typ with 
    | TVar i when i = old_var -> fresh
    | TVar i -> TVar i
    | TFun (t1, t2) -> TFun(replace_tvar_occurrences old_var t1 fresh, replace_tvar_occurrences old_var t2 fresh)
    | TTuple tlist -> TTuple(List.map (replace_tvar_occurrences old_var fresh) tlist)
    | TList t -> TList(replace_tvar_occurrences old_var t fresh)
    | _ -> typ
    in
  match scheme with 
  | ([], t) -> t
  | (tv::rest, t) -> inst_scheme (rest, replace_tvar_occurrences tv t (fresh_tvar ()))

let rec check_expr (gamma : mappings) (e : exp) : typ * substitution = 
  match e with 
  | Int i -> (t_int, [])
  | Bool b -> (t_bool, [])
  | String s -> (t_string, [])
  | Var v -> 
    (match lookup_typ v gamma with 
    | None -> raise @@ IllTyped "Unbound variable"
    | Some scheme -> inst_scheme scheme, [])
  | Lam (v, t_opt, exp) -> 
    let arg_typ = match t_opt with 
    | None -> fresh_tvar ()
    | Some t -> t in 
    let expr_typ, new_sub = check_expr ((v, ([], arg_typ))::gamma) exp in 
    TFun(List.hd @@ substitute new_sub [arg_typ], expr_typ), new_sub
  | Call (e0, elist) -> 
    let fn_typ, s0 = check_expr gamma e0 in 
    let new_gamma = sub_gamma s0 gamma in 
    let arg_typ, s1 = 
      (match elist with 
      | e1::[] -> check_expr new_gamma e1
      | _ -> failwith "Unimplemented fn call")
      in 
    let fresh = fresh_tvar () in 
    let s2 = unify [(List.hd @@ substitute s1 [fn_typ], TFun(arg_typ, fresh))] in 
    List.hd @@ substitute s2 [fresh], s2@s1@s0
  | _ -> failwith "Unimplemented"


let rec check_statement (gamma : mappings) (statement: stmt) : mappings = 
  match statement with 
  | Exp e -> check_expr gamma e; gamma
  | Assign (v, e) -> 
    let t0, s0 = check_expr gamma e in 
    let gamma' = sub_gamma s0 gamma in 
    (v, (typ_var_diff t0 gamma', t0))::gamma'
  | Pass
  | Block ([]) -> gamma
  | Block (st::rest) -> 
    let new_gamma = check_statement gamma st in 
    check_statement new_gamma (Block(rest))
  | _ -> failwith "Unimplemented statement"

let check (statement: stmt): mappings = 
  check_statement [] statement 