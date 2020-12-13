open Ast
open Pprint

type error_info = string
exception IllTyped of error_info
type gamma = (var * typ) list
type constr = (typ * typ) list
type substitution = (typ * typ) list

let rec str_of_gamma = 
  List.fold_left (fun acc (v, t) -> acc ^ ", " ^ v ^ " => " ^ (str_of_typ t)) ""

let rec str_of_constr = 
  List.fold_left 
  (fun acc (t1, t2) -> acc ^ ", " ^ (str_of_typ t1) ^ " == " ^ (str_of_typ t2)) ""

let rec sub tvar sigma : typ = 
  match tvar with 
  | TVar (i) as tv -> 
  begin
    match sigma with 
    | [] -> tv
    | (tv2, typ)::rest when tv = tv2 -> typ
    | (typ, tv2)::rest when tv2 = tv -> typ
    | _::rest -> sub tvar rest
  end
  | TBase s as typ -> typ
  | TFun(t1, t2) -> TFun(sub t1 sigma, sub t2 sigma)
  | TList t -> TList(sub t sigma)
  | TTuple lst -> TTuple(List.map (fun t -> sub t sigma) lst)

let rec apply_sub constrs subst : constr = 
  match constrs with 
  | (t1, t2)::rest -> (sub t1 subst, sub t2 subst)::(apply_sub rest subst)
  | [] -> []

(* returns whether or not tv1 is a free variable of tv2 *)
let rec is_typevar_fv tv1 tv2 : bool = 
  match tv1 with 
  | TVar(i) as tau -> 
  begin
    match tv2 with 
    | TVar i2 -> i = i2
    | TFun (t1, t2) -> is_typevar_fv tau t1 || is_typevar_fv tau t2
    | TBase s -> false
    | TTuple lst -> 
    List.fold_left (fun acc tau' -> acc || is_typevar_fv tau tau') false lst
    | TList tau' -> is_typevar_fv tau tau'
  end
  | _ -> false

let is_typ_tvar t : bool = 
  match t with 
  | TVar _ -> true
  | _ -> false

let is_typ_tfun t : bool = 
  match t with 
  | TFun _ -> true
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

let rec lookup_typ (name : var) (gamma : gamma) : typ option = 
  match gamma with 
  | [] -> None
  | (v, typ)::rest -> 
    if v = name then Some(typ) else lookup_typ name rest

let type_var (t : typ) c n : typ * constr = (TVar(n + 1), (t, TVar(n + 1))::c)

let rec get_constrs (mappings : gamma) (constraints : constr) (tv : int) (exp : exp) : typ * constr * int = 
  let fresh = TVar(tv) in 
  match exp with
  | Bool b -> (TBase("Bool"), constraints, tv)
  | String s -> (TBase("String"), constraints, tv)
  | Int int -> (TBase("Int"), constraints, tv)
  (* | List elist -> get_list_typ mappings elist *)
  | Var var -> (match List.assoc_opt var mappings with
      | None -> raise (IllTyped "Unbound variable")
      | Some e -> (e, constraints, tv))
  | Call (e1, elist) -> 
    let (fn_typ, fn_constr, new_tv) = get_constrs mappings constraints (tv + 1) e1 in 
      Format.printf "%s\n" (str_of_constr fn_constr);
      get_fn_app_typ mappings (fn_constr) (new_tv + 1) fn_typ elist 
  | Lam (v, None, e) ->
    let lambda_expr_type, lambda_expr_constr, new_tv = get_constrs ((v, fresh)::mappings) constraints (tv + 1) e in 
    let (r, c) = (TFun(fresh, lambda_expr_type), lambda_expr_constr) in 
    (r, c, new_tv)
  | Lam (v, Some t, e) ->
    let lambda_expr_type, lambda_expr_constr, new_tv = get_constrs ((v, fresh)::mappings) constraints (tv + 1) e in 
    let (r,c) = (TFun(fresh, lambda_expr_type), (fresh, t)::lambda_expr_constr) in 
    (r, c, new_tv)
  | Let (v, e1, e2) -> 
    let exp_typ, exp_constr, new_tv = get_constrs mappings constraints (tv + 1) e2 in 
      (exp_typ, exp_constr, new_tv)
  | Binary (binop, e1, e2) -> 
    let t1, c1, tv1 = get_constrs mappings constraints (tv + 1) e1 in 
    let t2, c2, tv2 = get_constrs mappings constraints (tv1 + 1) e2 in 
    (match binop with
     | Plus
     | Times
     | Minus -> (TBase("Int"), [(t1, TBase("Int")); (t1, TBase("Int"))]@c1@c2, tv2)
     | Less
     | Equal
     | Greater -> (TBase("Bool"), [(t1, TBase("Int")); (t1, TBase("Int"))]@c1@c2, tv2)
     | And
     | Or -> (TBase("Bool"), [(t1, TBase("Bool")); (t1, TBase("Bool"))]@c1@c2, tv2)
     | _ -> failwith "Check")
  | Unary (unop, exp) -> 
  begin
    let t1, c, new_tv = get_constrs mappings constraints (tv + 1) exp in 
    (match unop with 
    | Not -> (t1, [(t1, TBase("Bool"))]@c, new_tv)
    | Neg -> (t1, [(t1, TBase("Int"))]@c, new_tv))
  end
  | Tuple eList -> (match eList with
      | [] -> (TTuple [], constraints, tv)
      | hd::rest -> 
        let tup_typ, tup_c, new_tv = get_constrs mappings constraints (tv + 1) (Tuple(rest)) in 
        let el_typ, el_c, tv2 = get_constrs mappings constraints (new_tv + 1) hd in 
        (match tup_typ with 
        | TTuple lst -> (TTuple(el_typ::lst), el_c@tup_c, tv2)
        | _ -> raise @@ IllTyped "Not a tuple"))
  | Skip -> (TBase("None"), constraints, tv)
  | _ -> raise @@ IllTyped("Check")

and unify (c : constr) : substitution = 
  match c with 
  | [] -> []
  | (t, t')::rest -> 
    if t = t' then 
    unify rest
    else 
    begin
      match t, t' with 
      | TVar i as tau, tau' when not (is_typevar_fv tau tau') -> 
        let new_subst = [(t, t')] in 
        (unify (apply_sub rest new_subst))@new_subst
      | (tau), (TVar i as tau') when not (is_typevar_fv tau' tau) -> 
        let new_subst = [(t', t)] in 
        (unify (apply_sub rest new_subst))@new_subst
      | TFun (t1, t2), TFun (t1', t2') -> 
        unify ([(t1, t1')]@[(t2, t2')]@rest)
      | _ -> 
      Format.printf "--\n%s\n--" (str_of_constr c);
      raise @@ IllTyped "Type inference fail"
    end

and get_type mappings tv exp = 
    let tau, c, new_tv = get_constrs mappings [] tv exp in 
    (* Format.printf "--GET_TYPE FOR EXPR--";
    print_expr exp;
    Format.printf "\nMAPPINGS\n%s\n" (str_of_gamma mappings);
    Format.printf "\nPRE_UNIFY\n%s\n" (str_of_constr (c));
    Format.printf "\nPOST_UNIFY\n%s\n" (str_of_constr (unify c));
    Format.printf "\nTYPE_ASSGN\n%s\n" (str_of_typ tau); *)
    (sub tau (unify c), new_tv)

and check_stmt (gamma : gamma) (stmt : stmt) (ret_typ : typ option) (tv : int) : gamma * int = 
  begin
  match stmt with 
  | Exp e -> let _, t = (get_type gamma (tv + 1) e) in (gamma, t)
  | Block [] -> (gamma, tv)
  | Block (stmt::rest) -> 
  let new_gamma, new_tv = (check_stmt gamma stmt ret_typ (tv + 1)) in 
    check_stmt new_gamma (Block(rest)) ret_typ (new_tv + 1)
  | Decl (t_opt, v) -> 
    (match t_opt with 
    | Some t -> ((v, t)::gamma, tv)
    | None -> (gamma, tv))
  | Assign (v, e) -> 
  begin
    let name_typ = lookup_typ v gamma in 
    let expr_typ, new_tv = get_type gamma (tv + 1) e in
    match name_typ with 
    | Some s ->
      if expr_typ = s
      then (gamma, new_tv) 
      else 
        raise (IllTyped 
        (v 
        ^ " is of type " 
        ^ (str_of_typ s) 
        ^ " but expr is type " 
        ^ (str_of_typ expr_typ)))
    | None -> ((v, expr_typ)::gamma, new_tv)
  end
  (* | Assign (v, e) -> 
    let name_typ = lookup_typ v gamma in 
    let expr_typ = get_constrs gamma e in
    if expr_typ = name_typ
    then gamma 
    else 
      raise (IllTyped 
      (v 
      ^ " is of type " 
      ^ (str_of_typ name_typ) 
      ^ " but expr is type " 
      ^ (str_of_typ expr_typ)))
  | Decl (t, v) -> (v, t)::gamma
  | Block (st::rest) -> check_stmt (check_stmt gamma st ret_typ) (Block(rest)) ret_typ
  | MutableDecl (t, v) -> (v, t)::gamma
  | Block ([]) -> gamma
  | If (e, st1, st2) -> 
        (let expr_typ = get_constrs gamma e in 
        if expr_typ = TBase("Bool") then 
        (ignore (check_stmt gamma st1 ret_typ); 
        ignore (check_stmt gamma st2 ret_typ); 
        gamma)
        else failwith "If guard must be boolean")
  | While (e, st) -> 
  begin
    let expr_typ = get_constrs gamma e in 
    if expr_typ = TBase("Bool") then 
    (ignore (check_stmt gamma st ret_typ); gamma)
    else failwith "While guard must be boolean"
  end
  | For (v, e, loop) -> 
    let bound_iterator_gamma = ((v, (get_constrs gamma e))::gamma) in 
    ignore (check_stmt bound_iterator_gamma loop ret_typ); gamma
  | Def (ret, fn_id, arg_typs, body) -> 
  begin
    let _ = 
      check_stmt ((List.map (fun (t, v) -> (v, t)) arg_typs)@gamma) body (Some ret) in
    (fn_id, construct_fn_typ ret arg_typs)::gamma
  end
  | Return (e) -> 
    (match ret_typ with 
    | None -> failwith "Not in fn body"
    | Some (t) -> if get_type gamma e = t
                  then gamma 
                  else failwith "Incorrect return type")
  | Print (e) -> 
    ignore (get_constrs gamma e); gamma *)
  | Pass -> (gamma, tv)
  | _ -> print_stmt stmt; failwith "u"
  end

  and get_fn_app_typ mappings constr tv fn_typ elist = 
    let fresh = TVar(tv) in 
    match elist with 
    | e::[] -> 
    begin
      let arg_typ, arg_constr, new_tv = get_constrs mappings constr (tv + 1) e in 
        (fresh, [(fn_typ, TFun(arg_typ, fresh))]@arg_constr@constr, new_tv)
    end
    | e::rest -> 
    begin
      let hof_typ, hof_constr, new_tv = get_fn_app_typ mappings constr (tv + 1) fn_typ rest in 
        (fresh, constr@hof_constr, new_tv)
    end
    | [] -> (fresh, [(fn_typ, TFun(TBase("Unit"), fresh))]@constr, tv)

  and construct_fn_typ ret_typ arg_typs = 
  match arg_typs with
  | [] -> ret_typ
  | (arg, _)::rest -> TFun(arg, construct_fn_typ ret_typ rest)

  (* and get_list_typ mappings explist = 
  match explist with 
  | [] -> TBase "None"
  | el::[] -> TList (get_constrs mappings el)
  | el::rest ->
   let t = (List.fold_right 
   (fun exp typ -> let t2 = (get_constrs mappings exp) in 
   if typ = t2 then t2 else raise @@ IllTyped "List elements of differing types")
   rest (get_constrs mappings el)) in TList(t) *)