open Ast
open Pprint

exception IllTyped of string

type constraints = (typ * typ) list

(** A counter for the type variables used in type inference *)
let num_tvars_used = ref (-1)

let fresh_tvar () =
  num_tvars_used := !num_tvars_used + 1;
  TVar !num_tvars_used

let reset_tvars () = num_tvars_used := -1

(** Variables for base types *)
let t_string = TBase "String"

let t_int = TBase "Int"

let t_bool = TBase "Bool"

let t_none = TBase "None"

let t_unit = TTuple []

(** Useful constants for type-inference constructs *)
let no_constraints = []

let empty_substitution = Substitution.empty

let empty_gamma = Mappings.empty

(** [update name scheme gamma] is the set of mappings [gamma], with 
    the most-shadowed binding for [name] replaced with [scheme]. If 
    [name] is not bound in [gamma], the binding [name] -> [scheme] 
    is added to [gamma] *)
let update (name : var) (scheme : scheme) (gamma : mappings) = 
  match Mappings.find_opt name gamma with 
  | Some (_::rest) -> Mappings.add name (scheme::rest) gamma
  | _ -> Mappings.add name [scheme] gamma

(** [shadow name scheme gamma] is the set of mappings [gamma], with
    a new shadowed mapping [name] -> [gamma] *)
let shadow (name : var) (scheme : scheme) (gamma : mappings) = 
  match Mappings.find_opt name gamma with 
  | None -> Mappings.add name [scheme] gamma
  | Some lst -> Mappings.add name (scheme::lst) gamma

(** [shadow_all lst gamma] is the set of mappings [gamma], with 
    new shadowed mappings corresponding to each var -> scheme mapping 
    present in [lst] *)
let shadow_all (lst : (var * scheme) list) (gamma : mappings) : mappings = 
  List.fold_left (fun gamma' (name, scheme) -> shadow name scheme gamma') gamma lst

(** [unshadow name gamma] is the set of mappings [gamma], with the 
    most recently-shadowed mapping for [name] removed *)
let unshadow (name : var) (gamma : mappings) : mappings = 
  match Mappings.find_opt name gamma with 
  | Some (h::rest) -> Mappings.add name rest gamma
  | _ -> gamma

let lookup_typ (name : var) (gamma : mappings) : scheme option = 
  match Mappings.find_opt name gamma with
  | Some (h::_) -> Some h
  | _ -> None

(** [assoc_opt_or_raise k lst] is the value corresponding to k in lst,
              unless k is not bound, in which case an error is raised *)
let assoc_opt_or_raise (k : var) (lst : mappings) : scheme =
  match lookup_typ k lst with
  | None -> raise @@ IllTyped ("Unbound: " ^ k)
  | Some v -> v

module TypeVarSet = Set.Make (Int)
(** An integer set module *)

(** zp and unzip are used to join and separate association lists *)
let zp l = List.map2 (fun x y -> (x, y)) l

let unzip l = (List.map fst l, List.map snd l)

let map_tuple (f : 'a list -> 'b list) l =
  let l1, l2 = unzip l in
  zp (f l1) (f l2)

let any (f : 'a -> bool) : 'a list -> bool =
  List.fold_left (fun acc b -> acc || f b) false

(** A null coalescing operator *)
let ( |?? ) (opt : 'a option) (default : 'a) : 'a =
  match opt with Some t -> t | None -> default

(** [sub_typ s t] is t, with all occurrences of type variables in the 
    domain of s replaced with the appropriate mapping *)
let rec sub_typ (sigma : substitution) (typ : typ) : typ =
  let sub = sub_typ sigma in
  match typ with
  | TVar i -> Substitution.find_opt i sigma |?? typ
  | TBase _ as typ -> typ
  | TFun (t1, t2) -> TFun (sub t1, sub t2)
  | TList t -> TList (sub t)
  | TTuple lst -> TTuple (List.map sub lst)
  | TDict (t1, t2) -> TDict (sub t1, sub t2)
  | TLimbo t -> TLimbo (sub t)
  | TMutable t -> TMutable (sub t)
  | _ -> failwith "Unimplemented (substitute)"

(** [substitute sigma lst] substitutes type variables in sigma into types in lst*)
let substitute (sigma : substitution) (lst : typ list) : typ list =
  List.map (sub_typ sigma) lst

(** [sub_scheme sigma schm] substitutes type variables in sigma into
    the types in [schm]. NOTE : [sub_scheme] assumes that there are no
    mappings in [sigma] for any quantified type variables in [schm] *)
let sub_scheme (sigma : substitution) (schm : scheme) : scheme = 
  fst schm, sub_typ sigma (snd schm)

(** [sub_gamma s gamma] is gamma, with all occurrences of type variables
    that have type assignments in s, replaced with those type assignments. *)
let sub_gamma (sigma : substitution) (gamma : mappings) : mappings =
  let sub_scheme_list = List.map (sub_scheme sigma) in 
  Mappings.map sub_scheme_list gamma

(** [i ==> typ] checks if type variable i is in typ*)
let rec ( ==> ) (i : tvar) (typ : typ) : bool =
  match typ with
  | TVar i' -> i = i'
  | TFun (t1, t2) -> i ==> t1 || i ==> t2
  | TBase _ -> false
  | TTuple lst -> any (( ==> ) i) lst
  | TList tau' -> i ==> tau'
  | TDict (t1, t2) -> i ==> t1 || i ==> t2
  | TMutable t -> i ==> t
  | TLimbo t -> i ==> t
  | _ -> failwith "Unimplemented ==>"

(** [free_type_variables t bound] is the set of type variables in t not in bound *)
let rec free_type_variables (t : typ) (bound : tvar list) : TypeVarSet.t =
  match t with
  | TVar i ->
    if List.mem i bound then TypeVarSet.empty
    else TypeVarSet.add i TypeVarSet.empty
  | TFun (t1, t2) ->
    TypeVarSet.union
      (free_type_variables t1 bound)
      (free_type_variables t2 bound)
  | TBase _ -> TypeVarSet.empty
  | TTuple tlist ->
    List.fold_left
      (fun acc el -> TypeVarSet.union (free_type_variables el bound) acc)
      TypeVarSet.empty tlist
  | TList typ -> free_type_variables typ bound
  | TDict (t1, t2) ->
    TypeVarSet.union
      (free_type_variables t1 bound)
      (free_type_variables t2 bound)
  | TLimbo t -> free_type_variables t bound
  | TMutable t -> free_type_variables t bound
  | _ -> failwith "Unimplemented"

(** [typ_var_diff t1 gamma] is a list of integers representing the set of 
    free type variables present in typ t1 that do not occur as free type variables
    in any of the type assignments in gamma. *)
let typ_var_diff (t1 : typ) (gamma : mappings) : int list =
  let t1_ftv = free_type_variables t1 [] in
  let schm_ftv schm = free_type_variables (snd schm) (fst schm) in
  let schm_lst_ftv = List.fold_left (fun acc schm -> TypeVarSet.union acc (schm_ftv schm)) TypeVarSet.empty in
  let gamma_ftv =
    Mappings.fold (fun _ lst acc -> TypeVarSet.union (schm_lst_ftv lst) acc) gamma TypeVarSet.empty
  in
  TypeVarSet.elements (TypeVarSet.diff t1_ftv gamma_ftv)

let compose (s1 : substitution) (s2 : substitution) : substitution =
  let f (_ : tvar) (second_binding : typ option) (first_binding : typ option) : typ option = 
    match second_binding, first_binding with 
    | _, Some b -> Some (sub_typ s1 b)
    | Some b, None -> Some b
    | None, None -> None in 
  Substitution.merge f s1 s2

(** [unify constraints] is a substitution generated after running unification
    on the set of constraints provided. *)
let rec unify (constraints : constraints) : substitution =
  match constraints with
  | [] -> empty_substitution
  | (t, t') :: rest -> (
      if t = t' then unify rest
      else
        let sub_constraints subst = map_tuple (substitute subst) rest in
        match (t, t') with
        | TLimbo t, other -> unify @@ ((t, other) :: rest)
        | other, TLimbo t -> unify @@ ((other, t) :: rest)
        | TMutable t, other -> unify @@ ((t, other) :: rest)
        | other, TMutable t -> unify @@ ((other, t) :: rest)
        | TVar i, _ when not (i ==> t') ->
          let sigma' = Substitution.singleton i t' in
          compose (unify @@ sub_constraints sigma') sigma'
        | _, TVar i when not (i ==> t) ->
          let sigma' = Substitution.singleton i t in
          compose (unify @@ sub_constraints sigma') sigma'
        | TFun (t1, t2), TFun (t1', t2') ->
          unify @@ ((t1, t1') :: (t2, t2') :: rest)
        | TList t, TList t' -> unify @@ ((t, t') :: rest)
        | TDict (t1, t2), TDict (t1', t2') ->
          unify @@ ((t1, t1') :: (t2, t2') :: rest)
        | _ ->
          (* constraints |> str_of_constr |> Format.printf "Constraints: %s \n"; *)
          raise
          @@ IllTyped "Typing of program led to above impossible constraints"
    )

(** [init_scheme scheme] is a typ, instantiating all of the type
    variables under the 'forall' - essentially implementing polymorphism. *)
let rec init_scheme (scheme : scheme) : typ =
  let rec replace old_tvar fresh tau =
    match tau with
    | TVar i when i = old_tvar -> fresh
    | TVar _ -> tau
    | TFun (t1, t2) ->
      TFun (replace old_tvar fresh t1, replace old_tvar fresh t2)
    | TTuple tlist -> TTuple (List.map (replace old_tvar fresh) tlist)
    | TList t -> TList (replace old_tvar fresh t)
    | _ -> tau
  in

  match scheme with
  | [], t -> t
  | tv :: rest, t -> init_scheme (rest, replace tv (fresh_tvar ()) t)

(** [check_expr gamma e] is a (typ * substitution) pair of the
    inferred type and type substitution, after checking [e]. *)
let rec check_expr (gamma : mappings) (e : exp) : typ * substitution =
  match e with
  | Int _ -> t_int, empty_substitution
  | Bool _ -> t_bool, empty_substitution
  | String _ -> t_string, empty_substitution
  | Var v -> (
      match lookup_typ v gamma with
      | None -> raise @@ IllTyped ("Unbound variable " ^ v)
      | Some (_, TLimbo _) -> raise @@ IllTyped ("Uninitialized variable " ^ v)
      | Some scheme -> init_scheme scheme, empty_substitution )
  | Lam (v, t_opt, exp) ->
    let arg_typ = match t_opt with None -> fresh_tvar () | Some t -> t in
    let expr_typ, new_sub = check_expr (shadow v ([], arg_typ) gamma) exp in
    (TFun (sub_typ new_sub arg_typ, expr_typ), new_sub)
  | Call (e0, elist) -> (
      let fn_typ, s0 = check_expr gamma e0 in
      let new_gamma = sub_gamma s0 gamma in
      match elist with
      | [] ->
        let fresh = fresh_tvar () in
        let s = unify [ (fn_typ, TFun (t_unit, fresh)) ] in
        (sub_typ s fresh, s)
      | lst ->
        let f (prev_typ, sub) arg =
          let new_typ, sub' = check_expr new_gamma arg in
          let fresh = fresh_tvar () in
          let sub'' =
            unify [ (sub_typ (compose sub sub') prev_typ, TFun (new_typ, fresh)) ]
          in
          (sub_typ (compose sub (compose sub' sub'')) fresh, compose sub (compose sub' sub''))
        in
        List.fold_left f (fn_typ, s0) lst)
  | Skip -> (t_unit, empty_substitution)
  | SliceAccess (_, _) -> failwith "Unimplemented slice"
  | Binary (binop, e0, e1) -> (
      let t0, s0 = check_expr gamma e0 in
      let gamma' = sub_gamma s0 gamma in
      let t1, s1 = check_expr gamma' e1 in
      let s1s0 = compose s1 s0 in
      match binop with
      | Plus | Minus | Times | Divide | IntDivide | Mod | Exponent ->
        (t_int, compose s1s0 (unify [ (t0, t_int); (t1, t_int) ]))
      | Less | Greater | Geq | Leq ->
        (t_bool, compose s1s0 (unify [ (t0, t_int); (t1, t_int) ]))
      | And | Or -> (t_bool, compose s1s0 (unify [ (t0, t_bool); (t1, t_bool) ]))
      | Equal | Neq | Is -> (t_bool, compose s1s0 (unify [ (t0, t1) ]))
      | In -> failwith "unimplemented In" )
  | Unary (unop, e) -> (
      let e_type, substitution = check_expr gamma e in
      match unop with
      | Not -> (t_bool, compose substitution @@ unify [ (e_type, t_bool) ] )
      | Neg -> (t_int, compose substitution @@ unify [ (e_type, t_int) ] ) )
  | Tuple elist ->
    let acc (tlist, substitution) exp =
      let typ, substitution' =
        check_expr (sub_gamma substitution gamma) exp
      in
      (typ :: tlist, compose substitution substitution')
    in
    let typ, substitution' = List.fold_left acc ([], empty_substitution) elist in
    (TTuple (List.rev typ), substitution')
  | List (h :: t) ->
    let t0, s0 = check_expr gamma h in
    let gamma' = sub_gamma s0 gamma in
    let rest_typ, s2 = check_expr gamma' (List t) in
    let list_type = TList t0 in
    let s3 = unify [ (list_type, rest_typ) ] in
    (sub_typ s3 list_type, compose s0 @@ compose s2 s3)
  | List [] -> (TList (fresh_tvar ()), empty_substitution)
  | Dict ((e0, e1) :: rest) ->
    let t0, s0 = check_expr gamma e0 in
    let gamma' = sub_gamma s0 gamma in
    let t1, s1 = check_expr gamma' e1 in
    let gamma'' = sub_gamma s1 gamma' in
    let rest_typ, s2 = check_expr gamma'' (Dict rest) in
    let dict_typ = TDict (t0, t1) in
    let s3 = unify [ (dict_typ, rest_typ) ] in
    (sub_typ s3 dict_typ, compose s0 (compose s1 (compose s2 s3)))
  | Dict [] -> (TDict (fresh_tvar (), fresh_tvar ()), empty_substitution)
  | _ -> failwith ""

(** [check_statement gamma statement] is a (mappings * substitution) pair of
    inferred types and type substitutions, after checking [statement] and 
    all sub-statements and expressions. *)
let rec check_statement (gamma : mappings) (statement : stmt) :
  mappings * substitution =
  match statement with
  | Exp e ->
    let _ = check_expr gamma e in
    (gamma, empty_substitution)
  | Decl (t_opt, v) ->
    let t = t_opt |?? fresh_tvar () in
    shadow v ([], TLimbo t) gamma, empty_substitution
  | MutableDecl (t_opt, v) ->
    let t = t_opt |?? fresh_tvar () in
    shadow v ([], TMutable t) gamma, empty_substitution
  | Assign (v, e) ->
    let sch, is_mut =
      match assoc_opt_or_raise v gamma with
      | lst, TLimbo t -> ((lst, t), false)
      | lst, TMutable t -> ((lst, t), true)
      | _ -> raise @@ IllTyped ("Illegal assignment to " ^ v)
    in
    let t0, s0 = check_expr gamma e in
    let gamma' = sub_gamma s0 gamma in
    let var_typ = init_scheme sch in
    let var_typ = sub_typ s0 var_typ in
    let s1 = unify [ (var_typ, t0) ] in
    let gamma'' = sub_gamma s1 gamma' in
    let unified_type = sub_typ s1 var_typ in
    let unified_type =
      if is_mut then TMutable unified_type else unified_type
    in
    Format.printf "%s : %s \n" (v) ((typ_var_diff unified_type gamma', unified_type) |> str_of_scheme);
    ( update v (typ_var_diff unified_type gamma', unified_type) gamma'',
      compose s0 s1 )
  | Pass -> (gamma, empty_substitution)
  | Block stmt_lst ->
    let f (old_gamma, sub) stmt =
      let new_gamma, sub' = check_statement old_gamma stmt in
      (new_gamma, compose sub sub')
    in
    List.fold_left f (gamma, empty_substitution) stmt_lst
  | If (e0, st1, st2) ->
    let t0, s0 = check_expr gamma e0 in
    let gamma' = sub_gamma s0 gamma in
    let gamma'' =
      match t0 with
      | TVar i -> sub_gamma (Substitution.singleton i t_bool) gamma'
      | TBase _ when t0 = t_bool -> gamma'
      | _ -> raise @@ IllTyped "If guard must be boolean"
    in
    let _, s1 = check_statement gamma'' st1 in
    let gamma''' = sub_gamma s1 gamma'' in
    let _, s2 = check_statement gamma''' st2 in
    (sub_gamma s2 gamma', compose s0 (compose s1 s2))
  | Def (rt_opt, fn_id, args, block) ->
    let args' = (rt_opt, "return") :: args in
    let mapper (typ_opt, var) : var * scheme =
      let t = typ_opt |?? fresh_tvar () in
      (var, ([], t))
    in
    let arg_typs = List.map mapper args' in
    let arg_typs =
      let ret, (schm, t) = List.hd arg_typs in
      (ret, (schm, TMutable t)) :: List.tl arg_typs
    in
    let fn_name_type = fresh_tvar () in
    let body_gamma = shadow_all ((fn_id, ([], fn_name_type)) :: arg_typs) gamma  in
    let _, s0 = check_statement body_gamma block in
    let gamma' =
      sub_gamma s0
      @@ shadow_all [(fn_id, ([], fn_name_type)); List.hd arg_typs] gamma
    in
    let mapper' (_, (tvar_list, typ)) =
      if tvar_list = [] then sub_typ s0 typ else raise @@ IllTyped "Quantifiers"
    in
    let arg_types = List.map mapper' arg_typs in
    let arg_typ_list = List.tl arg_types in
    let return_type = List.hd arg_types in
    let return_type =
      match return_type with
      | TMutable t -> t
      | _ -> failwith "Big problem here!"
    in
    let rec make_fn_def args =
      match args with
      | [ typ ] -> TFun (typ, return_type)
      | typ :: rest -> TFun (typ, make_fn_def rest)
      | [] -> TFun (t_unit, return_type)
    in
    let fn_typ = make_fn_def arg_typ_list in
    let slast = unify [ (fn_typ, sub_typ s0 fn_name_type) ] in
    let new_fn_typ = sub_typ (compose s0 slast) fn_typ in
    let new_gamma =
      sub_gamma slast
      @@ update fn_id
        (typ_var_diff new_fn_typ (sub_gamma s0 gamma), new_fn_typ)
        gamma'
    in
    (unshadow "return" new_gamma, compose s0 slast)
  | Print e ->
    let _, s0 = check_expr gamma e in
    (sub_gamma s0 gamma, s0)
  | Return e ->
    let g, s = check_statement gamma (Assign ("return", e)) in
    (sub_gamma s g, s)
  | _ -> failwith "Unimplemented statement"

(** [check stmt] is the mappings from identifiers to schemes
    after typechecking the program. *)
let check (statement : stmt) : mappings =
  let gamma, sub = check_statement empty_gamma statement in
  let gamma' = sub_gamma sub gamma in
  let gamma'' =
    Mappings.map
      (fun (schm_lst) ->
         num_tvars_used := -1;
         let reset_typ = match schm_lst with 
           | h::_ -> init_scheme h
           | _ -> TBase ("YIKES TYPE") in
         ([(free_type_variables reset_typ [] |> TypeVarSet.elements, reset_typ)]))
      gamma'
  in
  gamma''
