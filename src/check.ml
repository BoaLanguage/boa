open Ast
open Pprint
open Set

type error_info = string
exception IllTyped of error_info
type constraints = (typ * typ) list
type substitution = (int * typ) list

let num_tvars_used = ref (-1)

let fresh_tvar () = num_tvars_used := !num_tvars_used + 1; TVar(!num_tvars_used)
let reset_tvars () = num_tvars_used := -1

let global_substitution : substitution ref = ref []

let t_string = TBase("String")
let t_int = TBase("Int")
let t_bool = TBase("Bool")
let t_none = TBase("None")
let t_unit = TTuple []

let no_constraints = []
let empty_substituition = []
let empty_gamma = []

module TypeVarSet = Set.Make(Int)

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
  | TTuple lst -> TTuple(List.map sub lst)
  | TDict (t1, t2) -> TDict (sub t1, sub t2)
  | _ -> failwith "Unimplemented (substitute)" 
  in 
  List.map @@ sub

let substitute_scheme (sigma: substitution): typ list -> typ list =
  let rec sub (typ: typ) : typ = 
  match typ with 
  | TVar (i) -> List.assoc_opt i sigma |?? typ
  | TBase s as typ -> typ
  | TFun(t1, t2) -> TFun(sub t1, sub t2)
  | TList t -> TList(sub t)
  | TTuple lst -> TTuple(List.map sub lst)
  | TDict (t1, t2) -> TDict(sub t1, sub t2)
  | _ -> failwith "Unimplemented (substitute_scheme)"
  in 
  List.map @@ sub
  
let rec (==>) (i: tvar) (typ: typ): bool = 
  match typ with 
  | TVar i' -> i = i'
  | TFun (t1, t2) -> i ==> t1 || i ==> t2
  | TBase s -> false
  | TTuple lst -> any ((==>) i) lst
  | TList tau' -> i ==> tau'
  | TDict (t1, t2) -> i ==> t1 || i ==> t2

let rec free_type_variables (t : typ) (bound : tvar list) : TypeVarSet.t = 
  match t with 
  | TVar i -> if List.mem i bound then TypeVarSet.empty else TypeVarSet.add i TypeVarSet.empty
  | TFun(t1, t2) -> TypeVarSet.union (free_type_variables t1 bound) (free_type_variables t2 bound)
  | TBase s -> TypeVarSet.empty
  | TTuple tlist -> List.fold_left (fun acc el -> TypeVarSet.union (free_type_variables el bound) acc) TypeVarSet.empty tlist
  | TList typ -> free_type_variables typ bound
  | TDict (t1, t2) -> TypeVarSet.union (free_type_variables t1 bound) (free_type_variables t2 bound)
  | _ -> failwith "Unimplemomted"

let rec typ_var_diff (t1 : typ) (gamma : mappings) : int list = 
  let t1_ftv = free_type_variables t1 [] in 
  let gamma_ftv = List.fold_left (fun acc (_, (lst, t)) -> TypeVarSet.union (free_type_variables t lst) acc) TypeVarSet.empty gamma in 
  TypeVarSet.elements (TypeVarSet.diff t1_ftv gamma_ftv)
  
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
      | TList (t), TList(t') -> unify @@ (t,t')::rest 
      | TDict (t1, t2), TDict(t1', t2') ->
        unify @@ ((t1, t1') :: (t2, t2') :: rest)
      | _ -> 
    raise @@ IllTyped "Typing of program led to above impossible constraints"
    end

let rec init_scheme (scheme: scheme): typ = 
  let rec replace old_tvar fresh tau = 
    match tau with 
    | TVar i when i = old_tvar -> fresh
    | TVar i -> tau
    | TFun (t1, t2) -> TFun(replace old_tvar fresh t1, replace old_tvar fresh t2)
    | TTuple tlist -> TTuple(List.map (replace old_tvar fresh) tlist)
    | TList t -> TList(replace old_tvar fresh t)
    | _ -> tau
    in
  
  match scheme with 
  | ([], t) -> t
  | (tv::rest, t) -> init_scheme (rest, replace tv (fresh_tvar ()) t)

let rec check_expr (gamma : mappings) (e : exp) : typ * substitution = 
  match e with 
  | Int i -> (t_int, [])
  | Bool b -> (t_bool, [])
  | String s -> (t_string, [])
  | Var v -> 
    (match lookup_typ v gamma with 
    | None -> raise @@ IllTyped "Unbound variable"
    | Some scheme -> 
    init_scheme scheme, [])
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
  |Skip -> t_unit, empty_substituition
  (* |AttrAccess (obj_exp, attribute_name) -> 
    begin
    let obj_type, substitution = check_expr gamma obj_exp in 
    if List.
    end *)
  | SliceAccess (_, _) -> failwith "Unimplemented slice"
  | Binary (binop, e0, e1) -> 
    begin
    let t0, s0 = check_expr gamma e0 in 
    let gamma' = sub_gamma s0 gamma in 
    let t1, s1 = check_expr gamma' e1 in 
    match binop with 
    | Plus | Minus | Times | Divide | IntDivide | Mod | Exponent    -> 
      t_int, unify [(t0, t_int); (t1, t_int)] @ s1 @ s0
    | Less | Greater | Geq | Leq -> 
      t_bool, unify [(t0, t_int); (t1, t_int)] @ s1 @ s0
    | And | Or -> 
      t_bool, unify [(t0, t_bool); (t1, t_bool)] @ s1 @ s0
    | Equal | Neq | Is -> 
      t_bool, unify [(t0, t1)]@s1@s0
    | In -> failwith "unimplemented In"
    end
  | Unary (unop, e) -> 
    begin
    let e_type, substitution = check_expr gamma e in 
    match unop with 
    | Not -> (t_bool, unify [(e_type, t_bool)]) 
    | Neg -> (t_int, unify [(e_type, t_int)]) 
    end
  | Tuple elist ->
    begin 
    let acc (tlist, substituition) exp = 
      let typ, substitution' = check_expr ((sub_gamma substituition gamma)) exp in 
      (typ::tlist, substitution'@substituition)
    in
    let typ, substitution' = List.fold_left acc ([], []) elist in 
    TTuple (List.rev typ), substitution' 
    end
  | List (h::t) -> begin 
    let t0, s0 = check_expr gamma h in 
    let gamma' = sub_gamma s0 gamma in 
    let rest_typ, s2 = check_expr gamma (List(t)) in 
    let list_type = TList(t0) in 
    let s3 = unify [(list_type, rest_typ)] in 
    (list_type, s3@s2@s0)
    end
  | List [] -> TList(fresh_tvar ()), []
  | Dict ((e0, e1)::rest) -> 
    let t0, s0 = check_expr gamma e0 in 
    let gamma' = sub_gamma s0 gamma in 
    let t1, s1 = check_expr gamma' e1 in 
    let gamma'' = sub_gamma s1 gamma' in 
    let rest_typ, s2 = check_expr gamma'' (Dict(rest)) in 
    let dict_typ = (TDict(t0, t1)) in 
    let s3 = unify [(dict_typ, rest_typ)] in 
    (dict_typ, s3@s2@s1@s0)
  | Dict [] -> 
    (TDict(fresh_tvar (), fresh_tvar ()), [])
  | _ -> failwith ""


let rec check_statement (gamma : mappings) (statement: stmt) : mappings = 
  match statement with 
  | Exp e -> let _ = check_expr gamma e in gamma
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