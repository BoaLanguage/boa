open Ast
open Pprint

type error_info = string
exception IllTyped of error_info
type gamma = (var * typ) list
type constr = (typ * typ) list
type substitution = (typ * typ) list

let latest_tvar = ref (-1)

let fresh_tvar () = latest_tvar := !latest_tvar + 1; TVar(!latest_tvar)

let rec str_of_gamma = 
  List.fold_left (fun acc (v, t) -> acc ^ ", " ^ v ^ " => " ^ (str_of_typ t)) ""

let rec str_of_constr = 
  List.fold_left 
  (fun acc (t1, t2) -> acc ^ ", " ^ (str_of_typ t1) ^ " == " ^ (str_of_typ t2)) ""

let is_var_typ =
  function 
  | TVar i -> true
  | _ -> false

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

let rec get_constrs (mappings : gamma) (constraints : constr) (exp : exp) : typ * constr = 
  match exp with
  | Bool b -> (TBase("Bool"), constraints)
  | String s -> (TBase("String"), constraints)
  | Int int -> (TBase("Int"), constraints)
  (* | List elist -> get_list_typ mappings elist *)
  | Var var -> (match List.assoc_opt var mappings with
      | None -> raise (IllTyped "Unbound variable")
      | Some e -> (e, constraints))
  | Call (e1, elist) -> 
    let (fn_typ, fn_constr) = get_constrs mappings constraints e1 in 
      (* Format.printf "%s\n" (str_of_constr fn_constr); *)
      get_fn_app_typ mappings (fn_constr) fn_typ elist 
  | Lam (v, None, e) ->
    let fresh = fresh_tvar () in 
    let lambda_expr_type, lambda_expr_constr = get_constrs ((v, fresh)::mappings) constraints e in 
      (TFun(fresh, lambda_expr_type), lambda_expr_constr)
  | Lam (v, Some t, e) ->
    let fresh = fresh_tvar () in 
    let lambda_expr_type, lambda_expr_constr = get_constrs ((v, fresh)::mappings) constraints e in 
    (TFun(fresh, lambda_expr_type), (fresh, t)::lambda_expr_constr)
  | Let (v, e1, e2) -> 
      get_constrs mappings constraints e2 
  | Binary (binop, e1, e2) -> 
    let t1, c1 = get_constrs mappings constraints e1 in 
    let t2, c2 = get_constrs mappings constraints e2 in 
    (match binop with
     | Plus
     | Times
     | Minus -> (TBase("Int"), [(t1, TBase("Int")); (t1, TBase("Int"))]@c1@c2)
     | Less
     | Equal
     | Greater -> (TBase("Bool"), [(t1, TBase("Int")); (t1, TBase("Int"))]@c1@c2)
     | And
     | Or -> (TBase("Bool"), [(t1, TBase("Bool")); (t1, TBase("Bool"))]@c1@c2)
     | _ -> failwith "Check")
  | Unary (unop, exp) -> 
  begin
    let t1, c = get_constrs mappings constraints exp in 
    (match unop with 
    | Not -> (t1, [(t1, TBase("Bool"))]@c)
    | Neg -> (t1, [(t1, TBase("Int"))]@c))
  end
  | Tuple eList -> (match eList with
      | [] -> (TTuple [], constraints)
      | hd::rest -> 
        let tup_typ, tup_c = get_constrs mappings constraints (Tuple(rest)) in 
        let el_typ, el_c = get_constrs mappings constraints hd in 
        (match tup_typ with 
        | TTuple lst -> (TTuple(el_typ::lst), el_c@tup_c)
        | _ -> raise @@ IllTyped "Not a tuple"))
  | Skip -> (TBase("None"), constraints)
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
      Format.printf "%s == %s" (str_of_typ t) (str_of_typ t');
      Format.printf "--\n%s\n--" (str_of_constr c);
      raise @@ IllTyped "Typing of program led to above impossible constraints"
    end

and get_type (mappings : gamma) (s : substitution) (exp : exp) : typ * substitution = 
    let tau, c = get_constrs mappings s exp in 
    (* Format.printf "--GET_TYPE FOR EXPR--";
    print_expr exp;
    Format.printf "\nMAPPINGS\n%s\n" (str_of_gamma mappings);
    Format.printf "\nPRE_UNIFY\n%s\n" (str_of_constr (c));
    Format.printf "\nPOST_UNIFY\n%s\n" (str_of_constr (unify c));
    Format.printf "\nTYPE_ASSGN\n%s\n" (str_of_typ tau); *)
    let subst = unify c in 
    sub tau subst, subst

and apply_subst_to_gamma (subst : substitution) (gamma : gamma) : gamma =
    match gamma with 
    | (n, t)::rest -> (n, sub t subst)::(apply_subst_to_gamma subst rest)
    | [] -> []

and check_stmt (gamma : gamma) (subst : substitution) (stmt : stmt) (ret_typ : typ option) 
              : gamma * substitution = 
  begin
  match stmt with 
  | Exp e -> let _, s = (get_type gamma subst e) in gamma, s
  | Block [] -> gamma, subst
  | Block (stmt::rest) -> 
  let new_gamma, s = (check_stmt gamma subst stmt ret_typ) in 
    check_stmt new_gamma s (Block(rest)) ret_typ
  | Decl (t_opt, v) -> 
    (match t_opt with 
    | Some t -> (v, t)::gamma, subst
    | None -> gamma, subst)
  | MutableDecl (t, v) -> 
    (match t with 
    | Some t -> (v, t)::gamma, subst
    | _ -> gamma, subst)
  | Assign (v, e) -> 
    begin
      let name_typ = lookup_typ v gamma in 
      let expr_typ, new_subst = get_type gamma subst e in
      match name_typ with 
      | Some s ->
        if (expr_typ = s || is_var_typ expr_typ)
        then gamma, new_subst
        else 
          raise (IllTyped 
          (v 
          ^ " is of type " 
          ^ (str_of_typ s) 
          ^ " but expr is type " 
          ^ (str_of_typ expr_typ)))
      | None -> Format.printf "\nASSGN TYPE: %s\n" (str_of_typ expr_typ); (v, expr_typ)::gamma, new_subst
    end
  | If (e, st1, st2) -> 
    (let expr_typ, s = get_type gamma subst e in 
    if expr_typ = TBase("Bool") then 
    let _, s1 = (check_stmt gamma s st1 ret_typ) in
    let _, s2 = (check_stmt gamma s1 st2 ret_typ) in
    gamma, s2
    else failwith "If guard must be boolean")
  | Def (ret, fn_id, arg_typs, body) -> 
  begin
    let arg_typ_vars = 
      (List.map (fun (t, v) -> 
      match t with 
      | None -> (v, fresh_tvar ())
      | Some t -> (v, t)) arg_typs) in 
    let return_typ = (match ret with 
      | None -> Some (fresh_tvar ())
      | Some t -> Format.printf "RET TYPE: %s" (str_of_typ t); ret) in 
    let arg_typ_names = 
      (List.map (fun (_, v) -> v) arg_typs) in 
    let new_gamma, new_subst = check_stmt (arg_typ_vars@gamma) subst body return_typ in
    Format.printf "\nNEWGAMMA: %s\n" (str_of_gamma new_gamma);
    (fn_id, construct_fn_typ (apply_subst_to_gamma new_subst new_gamma) arg_typ_names)::gamma, subst
  end
  | Return (e) -> 
    (match ret_typ with 
    | None -> failwith "Not in fn body"
    | Some (t) -> let t, s = get_type gamma subst e in 
                  ("return", t)::gamma, s)
  (* 
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
  | Print (e) -> 
    ignore (get_constrs gamma e); gamma *)
  | Pass -> gamma, subst
  | _ -> print_stmt stmt; failwith "u"
  end

  and get_fn_app_typ mappings constr fn_typ elist = 
    let fresh = fresh_tvar () in 
    match elist with 
    | e::[] -> 
    begin
      let arg_typ, arg_constr = get_constrs mappings constr e in 
        (fresh, [(fn_typ, TFun(arg_typ, fresh))]@arg_constr)
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
  | el::[] -> TList (get_constrs mappings el)
  | el::rest ->
   let t = (List.fold_right 
   (fun exp typ -> let t2 = (get_constrs mappings exp) in 
   if typ = t2 then t2 else raise @@ IllTyped "List elements of differing types")
   rest (get_constrs mappings el)) in TList(t) *)