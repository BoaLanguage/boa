open Ast
open Pprint

(* Values include *closures*, which are functions including their environments
   at "definition time." We also have "lazy" values, which let us delay the
   evaluation of expressions to support recursion. *)

(* Interpreter exceptions. *)
type error_info = string

exception IllformedExpression of error_info

exception UnboundVariable of var

exception IllegalBreak

exception IllegalContinue

exception IllegalReturn

exception TypecheckerFail of error_info

exception Unimplemented of error_info

(* A type for configurations. *)
type configuration = env * stmt * stmt * (stmt * stmt * stmt) list

let make_configuration (prog : stmt) : configuration = ([], prog, Pass, [])

(* A function to update the binding for x in env s.
   update (s, x, v) returns the env s[x->v]. *)
let rec update s x v =
  match s with
  | [] -> [ (x, v) ]
  | (y, u) :: t -> if x = y then (x, v) :: t else (y, u) :: update t x v

let rec take n lst =
  match (lst, n) with
  | el :: rest, n when n > 0 -> el :: take (n - 1) rest
  | _ -> []

(* A function to look up the binding for a variable in a env.
   lookup s x returns s(x) or UnboundVariable if s is not defined on s. *)
let rec lookup s x : value =
  match s with
  | [] -> raise (UnboundVariable x)
  | (y, u) :: t -> if x = y then u else lookup t x

let rec lookup_unwrap_optional s x : value =
  match s with
  | [] -> raise (UnboundVariable x)
  | (y, u) :: t ->
      if x = y then
        match u with
        | Some v -> v
        | _ -> raise @@ TypecheckerFail ("Uninitialized variable: " ^ x)
      else lookup_unwrap_optional t x

let int_of_value (v : value) : int =
  match v with VInt i -> i | _ -> raise @@ TypecheckerFail "Expected integer"

let bool_of_value (v : value) : bool =
  match v with VBool b -> b | _ -> raise @@ TypecheckerFail "Expected boolean"

let rec pow (a : int) (b : int) : int =
  if b = 0 then 1 else if b = 1 then a else pow (a * a) (b - 1)

let evalb (b : binop) (l : value) (r : value) : value =
  match b with
  | Plus -> VInt (int_of_value l + int_of_value r)
  | Less -> VBool (int_of_value l < int_of_value r)
  | Greater -> VBool (int_of_value l > int_of_value r)
  | And -> VBool (bool_of_value l && bool_of_value r)
  | Or -> VBool (bool_of_value l || bool_of_value r)
  | Equal -> VBool (int_of_value l = int_of_value r)
  | Times -> VInt (int_of_value l * int_of_value r)
  | Minus -> VInt (int_of_value l - int_of_value r)
  | Divide -> VInt (int_of_value l / int_of_value r)
  | Mod -> VInt (int_of_value l mod int_of_value r)
  | Exponent -> VInt (pow (int_of_value l) (int_of_value r))
  | IntDivide -> VInt (int_of_value l / int_of_value r)
  | Leq -> VBool (int_of_value l <= int_of_value r)
  | Geq -> VBool (int_of_value l >= int_of_value r)
  | Is -> VBool (int_of_value l = int_of_value r)
  | In -> raise @@ Unimplemented "binop in"
  | Neq -> VBool (int_of_value l <> int_of_value r)

let eval_fun (f : stmt) (e : env) : value =
  match f with
  | Def (_, v, arg_type_lst, statement) ->
      let args = List.map snd arg_type_lst in
      let e' = ref e in
      let closure = VClosure (args, statement, e') in
      e' := (v, closure) :: e;
      closure
  | _ -> raise @@ TypecheckerFail "Not a function definition"
(**[evale e s] evaluates expression e with environment s*)
let rec evale (e : exp) (s : env) : value =
  let evaluate_var v =
    let lookup_result = lookup s v in
    match lookup_result with
    | VRef vr -> (
        match !vr with
        | Some v -> v
        | None -> raise @@ TypecheckerFail "Uninitialized mutable" )
    | _ -> lookup_result
  in
  let evaluate_application callable_expression args =
    call
      (evale callable_expression s)
      (List.map (fun f -> f s) (List.map evale args))
  in
  let evaluate_attribute_access object_expression attribute_identifier =
    let obj_val = evale object_expression s in
    let obj = extract_class obj_val in
    let cls_ref = extract_class (lookup obj "__class__") in
    let res = lookup (obj @ cls_ref) attribute_identifier in
    match res with
    | VClosure (_, _, _) ->
        VMethodCall (obj_val, res) (** Bind object to self *)
    | v -> deref_value v
  in
  let evaluate_slice_access sliceable_expression index_expression =
    let arg = evale index_expression s in
    call (evaluate_attribute_access sliceable_expression "__slice__") [ arg ]
  in
  let evaluate_unary_operator operator operand =
    let operand_value = evale operand s in
    match (operator, operand_value) with
    | Not, VBool b -> VBool (not b)
    | Neg, VInt i -> VInt (-i)
    | _, _ -> raise @@ TypecheckerFail "Unary Operator Application"
  in
  let evaluate_tuple expression_list =
    let value_lst =
      List.map (fun expression -> evale expression s) expression_list
    in
    VTuple value_lst
  in
  let evaluate_list expression_list = 
    let value_lst =
      List.map (fun expression -> evale expression s) expression_list
    in
    VList value_lst in
  let evaluate_dict kv_expression_list =
    let kv_value_list =
      List.map (fun (k, v) -> (evale k s, evale v s)) kv_expression_list
    in
    VDict kv_value_list
  in
  match e with
  | Var identifier -> evaluate_var identifier
  | Call (fn, args) -> evaluate_application fn args
  | AttrAccess (e, v) -> evaluate_attribute_access e v
  | SliceAccess (e1, e2) -> evaluate_slice_access e1 e2
  | Binary (bin, e1, e2) -> evalb bin (evale e1 s) (evale e2 s)
  | Int i -> VInt i
  | Unary (operator, operand) -> evaluate_unary_operator operator operand
  | Bool b -> VBool b
  | Tuple explist -> evaluate_tuple explist
  | List explist -> evaluate_list explist
  | Dict expexplist -> evaluate_dict expexplist
  | Skip -> VNone
  | Lam (v, t, e) -> VClosure ([ v ], Return e, ref s)
  | String s -> VString(s)
  | _ -> raise @@ Unimplemented "Expression evaluation"

and evals (conf : configuration) : env =
  let sigma, current_statement, next_statement, kappa = conf in
  let evaluate_assignment identifier expression =
    let new_value = evale expression sigma in
    let new_sigma =
      match List.assoc_opt identifier sigma with
      | Some (VRef r) ->
          r := Some new_value;
          sigma
      | _ -> (identifier, new_value) :: sigma
    in
    evals (new_sigma, next_statement, Pass, kappa)
  in
  let evaluate_attribute_assignment object_expression attribute_identifier
      new_value_expression =
    let new_value = evale new_value_expression sigma in
    let obj = evale object_expression sigma in
    match obj with
    | VPreObj old_obj -> (
        match List.assoc_opt attribute_identifier old_obj with
        | Some result ->
            result := new_value;
            evals (sigma, next_statement, Pass, kappa)
        | _ -> raise @@ TypecheckerFail "Unbound Attribute" )
    | VObj obj -> (
        match List.assoc_opt attribute_identifier obj with
        | Some (VRef value) ->
            value := Some new_value;
            evals (sigma, next_statement, Pass, kappa)
        | Some _ -> raise @@ TypecheckerFail "Immutable Attribute"
        | _ -> raise @@ TypecheckerFail "Unbound Attribute" )
    | _ -> raise @@ TypecheckerFail "Not an object"
  in
  let evaluate_member_declaration identifier =
    let attrs = lookup sigma "__attrs__" in
    match attrs with
    | VList lst ->
        let new_sigma =
          update sigma "__attrs__" (VList (VString identifier :: lst))
        in
        evals (new_sigma, Pass, next_statement, kappa)
    | _ -> raise @@ TypecheckerFail "__attrs__ is not a list"
  in
  let evaluate_mutable_member_declaration identifier =
    let mattrs = lookup sigma "__mattrs__" in
    match mattrs with
    | VList lst ->
        let new_sigma =
          update sigma "__mattrs__" (VList (VString identifier :: lst))
        in
        evals (new_sigma, Pass, next_statement, kappa)
    | _ -> raise @@ TypecheckerFail "__mattrs__ is not a list"
  in
  let evaluate_if guard statement_true statement_false =
    let b_result = evale guard sigma in
    match b_result with
    | VBool b' ->
        if b' then (ignore (evals (sigma, statement_true, next_statement, kappa)); sigma)
        else (ignore (evals (sigma, statement_false, next_statement, kappa)); sigma)
    | _ -> raise @@ TypecheckerFail "If guard must be boolean"
  in
  let evaluate_while guard_expression body_statement =
    let guard = evale guard_expression sigma in
    match guard with
    | VBool b' ->
        if b' then
          let while_statement = While (guard_expression, body_statement) in
          let s_continue = Block [ while_statement; next_statement ] in
          let s_break = next_statement in
          let s_return = match kappa with [] -> Pass | (_, _, s) :: t -> s in
          evals
            ( sigma,
              body_statement,
              Block [ while_statement; next_statement ],
              (s_break, s_continue, s_return) :: kappa )
        else evals (sigma, Pass, next_statement, kappa)
    | _ -> raise @@ TypecheckerFail "While guard must be a boolean"
  in
  let evaluate_break _ =
    match kappa with
    | (c_b, c_c, _) :: kappa_t -> evals (sigma, c_b, Pass, kappa_t)
    | _ -> raise IllegalBreak
  in
  let evaluate_continue _ =
    match kappa with
    | (c_b, c_c, _) :: kappa_t -> evals (sigma, c_c, Pass, kappa_t)
    | _ -> raise IllegalContinue
  in
  let evaluate_class class_identifier superclass statement =
    let new_sigma =
      ("__mattrs__", VList []) :: ("__attrs__", VList []) :: sigma
    in
    let obj_dict = evals (new_sigma, statement, Pass, []) in
    let new_dict = (take (List.length obj_dict - List.length sigma)) obj_dict in
    let clobj = VObj new_dict in
    evals
      ( (class_identifier, VRef (ref (Some clobj))) :: sigma,
        next_statement,
        Pass,
        kappa )
  in
  match (current_statement, next_statement) with
  | Pass, Pass -> sigma
  | Pass, c -> evals (sigma, c, Pass, kappa)
  | Assign (v, a), _ -> evaluate_assignment v a
  | AttrAssgn (objexp, attr, e), c ->
      evaluate_attribute_assignment objexp attr e
  | Decl (_, _), c -> evals (sigma, Pass, c, kappa)
  | MutableDecl (_, v), c ->
      evals ((v, VRef (ref None)) :: sigma, c, Pass, kappa)
  | MemDecl (_, v), c -> evaluate_member_declaration v
  | MutableMemDecl (_, v), c -> evaluate_mutable_member_declaration v
  | Block (c1 :: t), Pass -> evals (sigma, c1, Block t, kappa)
  | Block (c1 :: t), c3 -> evals (sigma, c1, Block (t @ [ c3 ]), kappa)
  | Block [], c3 -> evals (sigma, Pass, c3, kappa)
  | If (b, c1, c2), c3 -> evaluate_if b c1 c2
  | While (b, c1), c2 -> evaluate_while b c1
  | Print a, c ->
      Pprint.print_value @@ evale a sigma;
      Format.printf "%s" "\n";
      evals (sigma, Pass, c, kappa)
  | Break, c -> evaluate_break ()
  | Continue, c -> evaluate_continue ()
  | Return e, _ -> evaluate_assignment "return" e
  | Def (rt, name, args, body), c ->
      evals
        ( (name, eval_fun (Def (rt, name, args, body)) sigma) :: sigma,
          Pass,
          c,
          kappa )
  | Class (name, super, stmt), c -> evaluate_class name super stmt
  | Exp (e), c -> let _  = evale e sigma in evals (sigma, Pass, c, kappa)
  | _ -> raise @@ Unimplemented "Statement"
(**[call callable args] evaluates the application of a closure-like object *)
and call (callable : value) (args : value list) : value =
  let zip = List.map2 (fun x y -> (x, y)) in
  let call_method obj closure =
    match call closure (obj :: args) with
    | VRef r -> (
        match !r with
        | Some v -> v
        | _ ->
            raise @@ TypecheckerFail "Method returned uninitialized reference" )
    | v -> v
  in
  let call_closure params body env_ref =
    if (List.length args < List.length params) then 
    let rec take_tail n lst = if n = 0 then lst 
    else match lst with 
    |[] -> failwith "error" 
    |h::t -> take_tail (n - 1) t in
    
    let unbound_params = take_tail (List.length args) params in 
    let bound_params = take (List.length args) params in  
    let new_env = ref ((zip bound_params args) @ !env_ref) in
    VClosure(unbound_params, body, new_env)
    else 
    let callenv =
      match args with [] -> !env_ref | _ -> ("return", VRef(ref None)) :: zip params args @ !env_ref
    in
    deref_value @@ lookup (evals (callenv, body, Pass, [])) "return"
  in
  let call_reference r =
    match !r with
    | Some (VObj cls_obj as v) -> call v args
    | _ -> raise @@ TypecheckerFail "Callable is uninitialized"
  in
  let call_init class_object =
    match lookup class_object "__init__" with
    | VClosure (params, body, env) as init -> (
        let attrs = lookup class_object "__attrs__" in
        let mattrs = lookup class_object "__mattrs__" in
        let extract_string_list vlist =
          match vlist with
          | VList val_list ->
              let extract_string vstring =
                match vstring with
                | VString s -> s
                | _ ->
                    raise @@ TypecheckerFail "Atrribute names must be strings"
              in
              List.map extract_string val_list
          | _ -> raise @@ TypecheckerFail "Attributes must be a list"
        in
        let attrs', mattrs' =
          (extract_string_list attrs, extract_string_list mattrs)
        in
        let preinit_attrs =
          List.map (fun x -> (x, ref VNone)) (attrs' @ mattrs')
        in
        let cls_ref =
          [ ("__class__", ref (VRef (ref (Some (VObj class_object))))) ]
        in
        let initial_obj_list = preinit_attrs @ cls_ref in
        let instance = VPreObj initial_obj_list in
        let new_args = VRef (ref (Some instance)) :: args in
        match call init new_args with
        | VPreObj attrsmattrs ->
            let attrs =
              List.filter (fun (name, vref) -> List.mem name attrs') attrsmattrs
            in
            let attrs = List.map (fun (name, vref) -> (name, !vref)) attrs in
            let mattrs =
              List.filter
                (fun (name, vref) -> List.mem name mattrs')
                attrsmattrs
            in
            let mattrs =
              List.map
                (fun (name, vref) -> (name, VRef (ref (Some !vref))))
                mattrs
            in
            VObj
              ( [ ("__class__", VRef (ref (Some (VObj class_object)))) ]
              @ attrs @ mattrs )
        | _ -> raise @@ TypecheckerFail "__init__ did not return pre-object" )
    | _ -> raise @@ TypecheckerFail "__init__ must evaluate to a closure"
  in
  match callable with
  | VMethodCall (obj, closure) -> call_method obj closure
  | VClosure (params, body, env_ref) -> call_closure params body env_ref
  | VRef r -> call_reference r
  | VObj cls_obj -> call_init cls_obj
  | _ -> raise @@ TypecheckerFail "Expression not callable"

(*
      let new_env: env = ("class", VObj(cls_obj))::(!env) in
      call (VClosure(List.tl params, body, ref new_env)) args ) *)
and extract_class value =
  match value with
  | VRef r -> (
      match !r with
      | Some (VObj cls_ref) -> cls_ref
      | _ -> raise @@ TypecheckerFail "Not a class" )
  | VObj cls_ref -> cls_ref
  | _ -> raise @@ TypecheckerFail "Not a class"

and deref_value value =
  match value with
  | VRef r -> (
      match !r with
      | Some v -> v
      | None -> raise @@ TypecheckerFail "Uninitialized reference" )
  | _ -> value
