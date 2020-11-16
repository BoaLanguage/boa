open Ast

type error_info = string
exception IllTyped of error_info

let rec get_type mappings exp = 
  match exp with
  | Bool b -> TBase "Bool"
  | Int int -> TBase "Int"
  | Var var -> (match List.assoc_opt var mappings with
      | None -> raise (IllTyped "Unbound variable")
      | Some e -> e)
  | Call (e1, elist) -> failwith "Unimplemented"
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
     | _ -> failwith "Unimplemented")
  | Unary (unop, exp) -> (match get_type mappings exp with
      | TBase "Int" -> TBase "Int"
      | _ -> raise (IllTyped  "Invalid Operator"))
  | Tuple eList -> (match eList with
      | [] -> TTuple []
      | hd::rest -> (match get_type mappings (Tuple rest) with
          | TTuple list -> TTuple ((get_type mappings hd)::list)
          | _ -> raise (IllTyped "Invalid tuple")))
  | _ -> failwith "Unimplemented"

let rec check_exp (e:exp) : typ =
  get_type [] e