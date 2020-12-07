open Ast
open Pprint

type error_info = string
exception IllTyped of error_info
type gamma = (var * typ) list

let rec str_of_typ (t : typ) : string =
  match t with 
  | TFun (t1, t2) -> (str_of_typ t1) ^ " -> " ^ (str_of_typ t2)
  | TBase s -> s
  | TTuple tlist -> "(" ^ str_of_typ_list tlist ^ ")"
  | TList t -> (str_of_typ t) ^ " list"

and str_of_typ_list (tlist : typ list) : string = 
 match tlist with 
 | [] -> ""
 | typ::next::rest -> 
  (str_of_typ typ) ^ ", " ^ str_of_typ next ^ str_of_typ_list rest
 | typ::rest -> str_of_typ typ ^ str_of_typ_list rest

let rec lookup_typ (name : var) (gamma : gamma) : typ = 
  match gamma with 
  | [] -> failwith (name ^ " has no type!")
  | (v, typ)::rest -> 
    if v = name then typ else lookup_typ name rest

let rec get_type (mappings : gamma) (exp : exp) : typ = 
  match exp with
  | Bool b -> TBase "Bool"
  | String s -> TBase "String"
  | Int int -> TBase "Int"
  | List elist -> get_list_typ mappings elist
  | Var var -> (match List.assoc_opt var mappings with
      | None -> raise (IllTyped "Unbound variable")
      | Some e -> e)
  | Call (e1, elist) -> 
    let fn_typ = get_type mappings e1 in 
      get_fn_type mappings fn_typ elist
  | Lam (v, t, exp) -> TFun (t, (get_type ((v, t)::mappings) exp))
  | Let (v, e1, e2) -> get_type ((v, (get_type mappings e1))::mappings) e2
  | Binary (binop, e1, e2) -> 
    let t1 = get_type mappings e1 in 
    let t2 = get_type mappings e2 in 
    (match binop with
     | Plus
     | Times
     | Minus -> if t1 = t2 && t2 = TBase "Int" then TBase "Int" else raise (IllTyped "Invalid binop")
     | Less
     | Equal
     | Greater -> if t1 = t2 && t2 = TBase "Int" then TBase "Bool" else raise (IllTyped "Invalid binop")
     | And
     | Or -> if t1 = t2 && t2 = TBase "Bool" then TBase "Bool" else raise (IllTyped "Invalid binop")
     | _ -> failwith "Check")
  | Unary (unop, exp) -> (match get_type mappings exp with
      | TBase "Int" -> TBase "Int"
      | _ -> raise (IllTyped  "Invalid Operator"))
  | Tuple eList -> (match eList with
      | [] -> TTuple []
      | hd::rest -> (match get_type mappings (Tuple rest) with
          | TTuple list -> TTuple ((get_type mappings hd)::list)
          | _ -> raise (IllTyped "Invalid tuple")))
  | _ -> failwith "Check"

and check_stmt (gamma : gamma) (stmt : stmt) (ret_typ : typ option) : gamma = 
  begin
  match stmt with 
  | Exp e -> ignore (get_type gamma e); gamma
  | Assign (v, e) -> 
    let name_typ = lookup_typ v gamma in 
    let expr_typ = get_type gamma e in
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
        (let expr_typ = get_type gamma e in 
        if expr_typ = TBase("Bool") then 
        (ignore (check_stmt gamma st1 ret_typ); 
        ignore (check_stmt gamma st2 ret_typ); 
        gamma)
        else failwith "If guard must be boolean")
  | While (e, st) -> 
  begin
    let expr_typ = get_type gamma e in 
    if expr_typ = TBase("Bool") then 
    (ignore (check_stmt gamma st ret_typ); gamma)
    else failwith "While guard must be boolean"
  end
  | For (v, e, loop) -> 
    let bound_iterator_gamma = ((v, (get_type gamma e))::gamma) in 
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
    ignore (get_type gamma e); gamma
  | _ -> print_stmt stmt; failwith "u"
  

  end

  and get_fn_type mappings fn_typ elist = 
    (match fn_typ with 
    | TFun(t1, t2) -> 
      begin
        match elist with 
        | e::[] -> 
          if (get_type mappings e) = t1 then t2 
          else failwith "Argument is incorrect type"
        | e::rest -> 
          if (get_type mappings e) = t1 then 
            begin
              match t2 with 
              | TFun (t2, t3) as t -> get_fn_type mappings t rest
              | _ -> failwith "Too many arguments supplied"
            end
          else failwith "Argument is incorrect type"
        | [] -> failwith "Not enough function arguments/partial application"
      end
    | _ -> failwith "Expr is not a function, it cannot be called")

  and construct_fn_typ ret_typ arg_typs = 
  match arg_typs with
  | [] -> ret_typ
  | (arg, _)::rest -> TFun(arg, construct_fn_typ ret_typ rest)

  and get_list_typ mappings explist = 
  match explist with 
  | [] -> TBase "None"
  | el::[] -> TList (get_type mappings el)
  | el::rest ->
   let t = (List.fold_right 
   (fun exp typ -> let t2 = (get_type mappings exp) in 
   if typ = t2 then t2 else raise @@ IllTyped "List elements of differing types")
   rest (get_type mappings el)) in TList(t)