open Ast

type error_info = string
exception IllTyped of error_info

let rec get_type mappings exp = 
  match exp with
  | True -> TBase "Bool"
  | False -> TBase "Bool"
  | Empty typ -> TList typ
  | Int int -> TBase "Int"
  | Var var -> (match List.assoc_opt var mappings with
      | None -> raise (IllTyped "Unbound variable")
      | Some e -> e)
  | App (e1, e2) -> 
    (match get_type mappings e1 with
     | TFun (t1, t2) -> if t1 = get_type mappings e2 then t2 else raise (IllTyped "Argument is incorrect type")
     | _ -> raise (IllTyped "Stuck"))
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
     | Cons -> (match t2 with 
         | TList t -> if t1 = t then TList t else raise (IllTyped "Element must be same type as list")
         | _ -> raise (IllTyped "Second operand of cons must be some list type")))
  | Unary (unop, exp) -> (match get_type mappings exp with
      | TBase "Int" -> TBase "Int"
      | _ -> raise (IllTyped  "Invalid Operator"))
  | Tuple eList -> (match eList with
      | [] -> TTuple []
      | hd::rest -> (match get_type mappings (Tuple rest) with
          | TTuple list -> TTuple ((get_type mappings hd)::list)
          | _ -> raise (IllTyped "Invalid tuple")))
  | Proj (exp, int) -> (match get_type mappings exp with
      | TTuple list -> List.nth list int
      | _ -> raise (IllTyped "Invalid Projection"))
  | Fix exp -> 
    (match get_type mappings exp with 
     | TFun (TFun(t1, t2), TFun(t1',t2')) when TFun(t1, t2) = TFun(t1',t2') -> TFun(t1, t2)
     | _ -> raise (IllTyped "Invalid fix"))
  | If (e1, e2, e3) -> 
    if get_type mappings e1 = TBase "Bool" then 
      (let t1 = get_type mappings e2 in 
       let t2 = get_type mappings e3 in 
       if t1 = t2 then t1 else raise (IllTyped "Branches must be of same type")) 
    else raise (IllTyped "If guard must be of type Bool")
  | Match (e1, e2, e3) -> (match get_type mappings e1 with
      | TList t -> 
        (let t1 = get_type mappings e2 in 
         let t2 = get_type mappings e3 in 
         (match t2 with
          | TFun (TTuple (a::b::[]), c) -> 
            (match b with 
             | TList t when t = a -> if t1 = c then t1 else raise (IllTyped "Branches do not match")
             | _ -> raise (IllTyped "Second tuple element must be list"))
          | _ -> raise (IllTyped "Second branch must be an abstraction")))
      | _ -> raise (IllTyped "Guard of match must be list type"))

let rec check_exp (e:exp) : typ =
  get_type [] e