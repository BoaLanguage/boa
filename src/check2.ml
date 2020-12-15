open Ast
open Pprint

type error_info = string
exception IllTyped of error_info
type gamma = (var * typ) list
type constraints = Constraints of (typ * typ) list
type substitution = Substitution of (typ * typ) list

let t_string = TBase("String")
let t_int = TBase("Int")
let t_bool = TBase("Bool")
let t_none = TBase("None")

let any: (bool list -> bool) = 
List.fold_left (fun acc b -> acc || b) false

let all: (bool list -> bool) = 
List.fold_left (fun acc b -> acc && b) true

let lookup_typ: (var -> gamma -> typ option) = List.assoc_opt


let num_tvars_used = ref (-1)

let fresh_tvar () = num_tvars_used := !num_tvars_used + 1; TVar(!num_tvars_used)

let rec str_of_gamma = 
  List.fold_left (fun acc (v, t) -> acc ^ ", " ^ v ^ " => " ^ (str_of_typ t)) ""

let rec str_of_constr = 
  List.fold_left 
  (fun acc (t1, t2) -> acc ^ ", " ^ (str_of_typ t1) ^ " == " ^ (str_of_typ t2)) ""

let is_var_typ =
  function 
  | TVar i -> true
  | _ -> false


let (!!!) (sigma: substitution) : (typ * typ) list = 
  match sigma with
  | Substitution(x) -> x

let (!!) (constr: constraints) : (typ * typ) list = 
  match constr with
  | Constraints(x) -> x

let (^::) (h: typ * typ) (t: constraints) = 
  Constraints(h::(!! t))
let (^:::)  (h: typ * typ) (t: substitution) = 
  Substitution(h::(!!! t))
let (^@)  (c: constraints) (t: constraints) = 
  Constraints(!! c @ !!t)
let (^@@)  (c: substitution) (t: substitution) = 
  Substitution(!!!c @ !!!t)

let rec sub (tvar: typ) (sigma: substitution) : typ = 
  match tvar with 
  | TVar (i) as tv -> 
  begin
    match !!! sigma with 
    | [] -> tv
    | (tv2, typ)::rest when tv = tv2 -> typ
    | (typ, tv2)::rest when tv2 = tv -> typ
    | _::rest -> sub tvar @@ Substitution(rest)
  end
  | TBase s as typ -> typ
  | TFun(t1, t2) -> TFun(sub t1 sigma, sub t2 sigma)
  | TList t -> TList(sub t sigma)
  | TTuple lst -> TTuple(List.map (fun t -> sub t sigma) lst)

let rec apply_sub (constrs: constraints) (subst: substitution) : constraints = 
  match !! constrs with 
  | (t1, t2)::rest -> 
    let sub_rest = !! (apply_sub (Constraints(rest)) subst ) in
    let sub_head = (sub t1 subst, sub t2 subst) in 
    Constraints (sub_head::sub_rest) 
  | [] -> Constraints []

(* returns whether or not tv1 is a free variable of tv2 *)
let rec is_free_type_var_in tv1 tv2 : bool = 
  match tv1 with 
  | TVar(i) as tau -> 
  begin
    match tv2 with 
    | TVar i2 -> i = i2
    | TFun (t1, t2) -> is_free_type_var_in tau t1 || is_free_type_var_in tau t2
    | TBase s -> false
    | TTuple lst -> any @@ List.map (is_free_type_var_in tau) lst
    | TList tau' -> is_free_type_var_in tau tau'
  end
  | _ -> false

let rec str_of_typ (t : typ) : string =
  match t with 
  | TFun (t1, t2) -> (str_of_typ t1) ^ " -> " ^ (str_of_typ t2)
  | TBase s -> s
  | TTuple tlist -> "(" ^ str_of_typ_list tlist ^ ")"
  | TList t -> (str_of_typ t) ^ " list"
  | TVar i -> "TVAR "^(string_of_int i)

and str_of_typ_list (tlist : typ list) : string = 
 match tlist with 
 | [] -> ""
 | typ::next::rest -> 
  (str_of_typ typ) ^ ", " ^ str_of_typ next ^ str_of_typ_list rest
 | typ::rest -> str_of_typ typ ^ str_of_typ_list rest


and get_type (mappings : gamma) (exp : exp) : typ * gamma = 
    let rec unify (c : constraints) : substitution = 
    match !!c with 
    | [] -> Substitution []
    | (t, t')::rest -> 
      let rest = Constraints rest in
      if t = t' then 
      unify rest
      else 
      begin
        match t, t' with 
        | TVar _ , _ when not (is_free_type_var_in t t') -> 
            let new_subst = Substitution [(t, t')] in 
            (unify (apply_sub rest new_subst)) ^@@ new_subst
        | _ , TVar _ when not (is_free_type_var_in t' t) -> 
            let new_subst = Substitution [(t', t)] in 
            (unify (apply_sub rest new_subst)) ^@@ new_subst
        | TFun (t1, t2), TFun (t1', t2') -> 
          unify @@ (t1, t1') ^:: (t2, t2') ^:: rest
        | _ -> 
        Format.printf "%s == %s" (str_of_typ t) (str_of_typ t');
        Format.printf "--\n%s\n--" (str_of_constr !!c);
        raise @@ IllTyped "Typing of program led to above impossible constraints"
    end in
    let rec get_constraints (mappings : gamma) (constraints : constraints) (exp : exp) : typ * constraints = 
      match exp with
      | Bool b -> t_bool, constraints
      | String s -> t_string, constraints
      | Int int -> t_int, constraints
      (* | List elist -> get_list_typ mappings elist *)
      | Var var -> 
        begin
        match lookup_typ var mappings with
        | None -> raise @@ IllTyped "Unbound variable"
        | Some e -> (e, constraints)
        end
      | Call (e1, elist) -> 
        let (function_typ, function_constraints) = get_constraints mappings constraints e1 in 
          get_fn_app_typ mappings (function_constraints) function_typ elist 
      | Lam (v, None, e) ->
        let fresh = fresh_tvar () in 
        let lambda_expr_type, lambda_expr_constr = get_constraints ((v, fresh)::mappings) constraints e in 
          (TFun(fresh, lambda_expr_type), lambda_expr_constr)
      | Lam (v, Some t, e) ->
        let fresh = fresh_tvar () in 
        let lambda_expr_type, lambda_expr_constr = get_constraints ((v, fresh)::mappings) constraints e in 
        (TFun(fresh, lambda_expr_type), (fresh, t)^::lambda_expr_constr)
      | Let (v, e1, e2) -> 
          get_constraints mappings constraints e2 
      | Binary (binop, e1, e2) -> 
        let t1, c1 = get_constraints mappings constraints e1 in 
        let t2, c2 = get_constraints mappings constraints e2 in 
        (match binop with
        | Plus | Times | Minus -> t_int, (t1, t_int) ^:: (t2, t_int) ^:: (c1 ^@ c2)
        | Less | Equal | Greater -> t_bool, (t1, t_int) ^:: (t2, t_int) ^:: (c1 ^@ c2)
        | And  | Or -> t_bool, (t1, t_bool) ^:: (t2, t_bool) ^:: (c1 ^@ c2)
        | _ -> failwith "Check")
      | Unary (unop, exp) -> 
      begin
        let t1, c = get_constraints mappings constraints exp in 
        match unop with 
        | Not -> t1, (t1, t_bool) ^:: c
        | Neg -> t1, (t1, t_int) ^:: c
      end
      | Tuple eList -> 
      begin
        match eList with
        | [] -> (TTuple [], constraints)
        | hd::rest -> 
          let tup_typ, tup_c = get_constraints mappings constraints (Tuple(rest)) in 
          let el_typ, el_c = get_constraints mappings constraints hd in 
          match tup_typ with 
          | TTuple lst -> TTuple(el_typ::lst), el_c ^@ tup_c
          | _ -> raise @@ IllTyped "Not a tuple"
      end
      | Skip -> t_none, constraints
      | _ -> raise @@ IllTyped("Check") in
  let tau, c = get_constraints mappings (Constraints []) exp in 
  let subst = unify c in
  let new_gamma = apply_subst_to_gamma subst mappings in
  sub tau subst, new_gamma

and apply_subst_to_gamma (subst : substitution): gamma -> gamma =
    List.map (fun (n, t) -> (n, sub t subst)) 

and check_stmt (gamma : gamma) (subst : substitution) (stmt : stmt) (ret_typ : typ option) 
              : gamma = 
  begin
  match stmt with 
  | Exp e -> let _, new_gamma = (get_type gamma e) in new_gamma
  | Block [] -> gamma
  | Block (stmt::rest) -> 
  let new_gamma = (check_stmt gamma subst stmt ret_typ) in 
    check_stmt new_gamma subst (Block(rest)) ret_typ
  | Decl (t_opt, v) -> 
  begin
  match t_opt with 
  | Some t -> (v, t)::gamma
  | None -> gamma
  end
  | MutableDecl (t, v) -> 
  begin
    match t with 
    | Some t -> (v, t)::gamma
    | _ -> gamma
  end
  | Assign (v, e) -> 
    begin
      let name_typ = lookup_typ v gamma in 
      let expr_typ, new_gamma = get_type gamma e in
      match name_typ with 
      | Some s ->
        if (expr_typ = s || is_var_typ expr_typ)
        then new_gamma
        else 
          raise @@ IllTyped 
          (v ^ " is of type " 
          ^ (str_of_typ s) 
          ^ " but expr is type " 
          ^ (str_of_typ expr_typ))
      | None -> (v, expr_typ)::gamma
    end
  | If (e, st1, st2) -> 
    let expr_typ, gamma' = get_type gamma e in 
    if expr_typ = t_bool then 
    let gamma'' = (check_stmt gamma' subst st1 ret_typ) in
    let gamma''' = (check_stmt gamma'' subst st2 ret_typ) in
    gamma'''
    else raise @@ IllTyped "If guard must be boolean"

  | Def (ret, fn_id, arg_typs, body) -> 
  begin
    let return_typ = (match ret with 
      | None -> (fresh_tvar ())
      | Some t -> Format.printf "RET TYPE: %s" (str_of_typ t); t) in 
    let arg_typ_vars = 
      ("return", return_typ)::(List.map (fun (t, v) -> 
      match t with 
      | None -> (v, fresh_tvar ())
      | Some t -> (v, t)) arg_typs) in 
    let arg_typ_names = 
      (List.map fst arg_typ_vars) in 
    let new_gamma = check_stmt (arg_typ_vars@gamma) subst body None in
    Format.printf "\nNEWGAMMA: %s\n" (str_of_gamma new_gamma);
    (fn_id, construct_fn_typ (apply_subst_to_gamma subst new_gamma) arg_typ_names)::gamma
  end
  | Return (e) -> check_stmt gamma subst (Assign("return", e)) None
    (* (match ret_typ with 
    | None -> raise @@ IllTyped "Not in fn body"
    | Some (t) -> let _, gamma' = get_type gamma e in 

                  ("return", t)::gamma', s) *)
  (* 
  | While (e, st) -> 
  begin
    let expr_typ = get_constraints gamma e in 
    if expr_typ = t_bool then 
    (ignore (check_stmt gamma st ret_typ); gamma)
    else failwith "While guard must be boolean"
  end
  | For (v, e, loop) -> 
    let bound_iterator_gamma = ((v, (get_constraints gamma e))::gamma) in 
    ignore (check_stmt bound_iterator_gamma loop ret_typ); gamma
  | Print (e) -> 
    ignore (get_constraints gamma e); gamma *)
  | Pass -> gamma, subst
  | _ -> print_stmt stmt; failwith "u"
  end

  and get_fn_app_typ mappings (constr: constraints) fn_typ elist = 
    let fresh = fresh_tvar () in 
    match elist with 
    | e::[] -> 
    begin
      let arg_typ, arg_constr = get_constraints mappings constr e in 
        (fresh, (fn_typ, TFun(arg_typ, fresh)) ^:: arg_constr)
    end
    | e::rest -> 
    begin
      let arg_typ, arg_constr = get_type mappings constr e in 
      let next_fn_typ = TFun(arg_typ, fresh) in 
      get_fn_app_typ mappings ((fn_typ, next_fn_typ)::constr) fresh rest
    end
    | [] -> (fresh, [(fn_typ, TFun(TBase("Unit"), fresh))]@constr)

  and construct_fn_typ gamma arg_names = 
  match arg_names with
  | [] -> 
    (match lookup_typ "return" gamma with 
    | Some t -> t
    | _ -> failwith "Oh boy")
  | n::rest -> 
  let arg_typ = lookup_typ n gamma in 
  (match arg_typ with 
  | Some t ->  TFun(t, construct_fn_typ gamma rest)
  | _ -> failwith "Oh boy 2")

  (* and get_list_typ mappings explist = 
  match explist with 
  | [] -> TBase "None"
  | el::[] -> TList (get_constraints mappings el)
  | el::rest ->
   let t = (List.fold_right 
   (fun exp typ -> let t2 = (get_constraints mappings exp) in 
   if typ = t2 then t2 else raise @@ IllTyped "List elements of differing types")
   rest (get_constraints mappings el)) in TList(t) *)