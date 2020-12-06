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

(* A type for configurations. *)
type configuration = 
  env * stmt * stmt * ((stmt * stmt * stmt) list)

let make_configuration (prog: stmt) : configuration = 
  ([], prog, Pass, [])
(* A function to update the binding for x in env s.
   update (s, x, v) returns the env s[x->v]. *)
let rec update s x v =
  match s with
  | [] ->
    [(x, v)]
  | (y ,u)::t ->
    if x = y then (x, v)::t
    else (y, u)::(update t x v)

let rec take n lst =
  match lst, n with
  | el::rest, n when n > 0 -> el::(take (n - 1) rest)
  | _ -> []

(* A function to look up the binding for a variable in a env.
   lookup s x returns s(x) or UnboundVariable if s is not defined on s. *)
let rec lookup s x : value =
  match s with
  | [] ->
    raise (UnboundVariable x)
  | (y,u)::t ->
    if x = y then u
    else (lookup t x)

let rec lookup_opt s x : value =
  match s with
  | [] ->
    raise (UnboundVariable x)
  | (y,u)::t ->
    if x = y then 
      (match u with 
      | Some v -> v
      | _ -> failwith "TYPECHECK FAIL")
    else (lookup_opt t x)

let rec lookup_value vdict vkey : value = 
    match vdict with
    | [] -> failwith "No member of that name/val"
    | (k, v)::rest -> 
      if k = vkey then v
      else lookup_value rest vkey

let int_of_value (v : value) : int =
  match v with
  | VInt i -> i
  | _ ->  failwith "BIG PROBLEM"

let bool_of_value (v : value) : bool = 
  match v with
  | VBool b -> b
  | _ -> failwith "BIG PROBLEM"

let rec pow (a : int) (b : int) : int = 
  if (b = 0) then 1 else 
  if (b = 1) then a else pow (a * a) (b - 1)

let evalb (b : binop) (l : value) (r : value) : value = 
  match b with
  | Plus -> VInt((int_of_value l) + (int_of_value r))
  | Less -> VBool((int_of_value l) < (int_of_value r))
  | Greater -> VBool((int_of_value l) > (int_of_value r))
  | And -> VBool((bool_of_value l) && (bool_of_value r))
  | Or -> VBool((bool_of_value l) || (bool_of_value r))
  | Equal -> VBool((int_of_value l) = (int_of_value r))
  | Times -> VInt((int_of_value l) * (int_of_value r))
  | Minus -> VInt((int_of_value l) - (int_of_value r))
  | Divide -> VInt((int_of_value l) / (int_of_value r))
  | Mod -> VInt((int_of_value l) mod (int_of_value r))
  | Exponent -> VInt(pow (int_of_value l) (int_of_value r))
  | IntDivide -> VInt((int_of_value l) / (int_of_value r))
  | Leq -> VBool((int_of_value l) <= (int_of_value r))
  | Geq -> VBool((int_of_value l) >= (int_of_value r))
  | Is -> VBool((int_of_value l) = (int_of_value r))
  | In -> failwith "unimplemented"
  | Neq -> VBool((int_of_value l) <> (int_of_value r))

let eval_fun (f: stmt) (e: env) : value = 
  match f with
  | Def (_, v, arg_type_lst, statement) -> 
    (
      let args = List.map snd arg_type_lst in
      let e' = ref e in 
      let closure = VClosure(args, statement, e') in 
      e' := (v, closure)::e; closure      
    )
  | _-> failwith "TYPECHECKERFAIL"

let rec evale (e : exp) (s : env) : value =
  match e with
  | Var v -> (match lookup s v with 
              | VRef vr-> 
                (match !vr with 
                | Some v -> v
                | None -> failwith "Typechecker: Uninitialized")
              | any -> any)
  | Call (fn, args) -> 
  call (evale fn s) (List.map (fun f -> f s) (List.map (evale) args))
  | AttrAccess (e, v) -> 
    let dict = evale e s in
    (match dict with
    | VRef r -> 
        (match !r with 
        | (Some (VObj(o))) -> lookup o v
        | _ -> failwith "god damnit")
    | VObj l -> (lookup l v)
    | _ -> failwith "TYPECHECK FAIL")
  | SliceAccess (e1, e2) -> 
      let obj = evale e1 s in 
      let arg = evale e2 s in 
      (match obj with 
      | VDict o -> call (lookup_value o (VString("__slice__"))) ([arg])
      | _ -> failwith "TYPECHECK FAIL")
  | Binary (bin, e1, e2) -> evalb bin (evale e1 s) (evale e2 s)
  | Int i -> VInt(i)
  | Unary (u, exp) -> 
    let v = evale exp s in 
    (match u with
    | Not -> (match v with 
              | VBool b -> VBool(not b)
              | _ -> failwith "TYPECHECK FAIL")
    | Neg -> (match v with
              | VInt i -> VInt(-i)
              | _ -> failwith "TYPECHECK FAIL"))
  | Bool b -> VBool(b)
  | Tuple explist -> (match explist with 
                      | [] -> VList([])
                      | exp::rest -> (match (evale (Tuple(rest)) s) with
                                     | VList l -> VList((evale exp s)::l)
                                     | _ -> failwith "TYPECHECK FAIL"))
  | List explist -> (match explist with 
                    | [] -> VList([])
                    | exp::rest -> (match (evale (List(rest)) s) with
                                  | VList l -> VList((evale exp s)::l)
                                  | _ -> failwith "TYPECHECK FAIL"))
  | Dict expexplist -> (match expexplist with 
                        | [] -> VDict([])
                        | (e1, e2)::rest -> (match evale (Dict(rest)) s with
                                            | VDict d -> VDict((evale e1 s, evale e2 s)::d)
                                            | _ -> failwith "TYPECHECK FAIL"))
  | Skip -> VNone
  | Lam (v, t, e) -> VClosure ([v], Return(e), ref s)
  | _ -> failwith "not implemented expression"

and evals (conf:configuration) : env =
  match conf with
  | sigma, Pass, Pass, kappa -> sigma
  | sigma, Pass, c, kappa -> evals (sigma, c, Pass, kappa)
  | sigma, Assign(v, a), c, kappa ->
    begin
    let n = evale a sigma in 
    let new_sigma = 
    (match List.assoc_opt v sigma with 
    | Some (VRef r) -> r := Some n; sigma 
    | _ -> (v, n)::sigma)      
    in evals (new_sigma, c, Pass, kappa)
    end
  | sigma, AttrAssgn(objexp, attr, e), c, kappa ->
    begin
      let v = evale e sigma in 
      let obj = evale objexp sigma in
      (match obj with 
      | VPreObj(old_obj) -> 
          (match List.assoc_opt attr old_obj with 
          | Some (result) -> 
          begin
            result := v;
            evals (sigma, c, Pass, kappa)
          end
          | _ -> failwith "Unbound attribute")
      | VObj obj -> 
        begin
        let new_obj = 
          (match List.assoc_opt attr obj with
          | Some (result) -> 
            (match result with 
            | VRef value -> value := (Some v)
            | _ -> failwith "Immutable Attribute")
          | _ -> failwith "Unbound Attribute")      
          in evals (sigma, c, Pass, kappa)
        end
      
      | _ -> print_value obj; ("TYPECHECK FAIL: " ^ attr) |> failwith)
      
    end
  | sigma, Decl (_, _), c, kappa -> evals (sigma, Pass, c, kappa)
  | sigma, MutableDecl (_, v), c, kappa -> 
    evals ((v, VRef (ref None))::sigma, c, Pass, kappa)
  | sigma, MemDecl(_, v), c, kappa -> 
      let attrs = lookup sigma "__attrs__" in 
      (match attrs with 
      | VList lst -> let new_sigma = update sigma "__attrs__" (VList((VString(v))::lst)) in 
        evals (new_sigma, Pass, c, kappa) 
      | _ -> failwith "TYPECHECK FAILED: ATTRS NOT A LIST")
  | sigma, MutableMemDecl(_, v), c, kappa -> 
      let mattrs = lookup sigma "__mattrs__" in 
      (match mattrs with 
      | VList lst -> let new_sigma = update sigma "__mattrs__" (VList(((VString(v))::lst))) in 
        evals (new_sigma, Pass, c, kappa) 
      | _ -> failwith "TYPECHECK FAILED: MATTRS NOT A LIST")
  | sigma, Block(c1::t), Pass, kappa -> evals (sigma, c1, Block(t), kappa)
  | sigma, Block(c1::t), c3, kappa -> evals (sigma, c1, Block(t@[c3]), kappa)
  
  | sigma, Block([]), c3, kappa -> evals (sigma, Pass, c3, kappa)

  | sigma, If(b,c1,c2), c3, kappa -> let b_result = evale b sigma in 
    (match b_result with
    | VBool b' -> if b' 
      then evals (sigma, c1, c3, kappa) 
      else evals (sigma, c2, c3, kappa)
    | _-> failwith "If guard must be a boolean")

  | sigma, While(b, c1), c2, kappa -> 
    let b_result = evale b sigma in 
    (match b_result with
    | VBool b' -> if b' 
      then 
        let w = While(b, c1) in 
        let s_continue = Block([w;c2]) in 
        let s_break = c2 in
        let s_return = 
          (match kappa with 
          | [] -> Pass  
          | (_,_, s)::t -> s) in 
        evals (sigma, c1, Block([w;c2]), (s_break, s_continue, s_return)::kappa)
      else evals (sigma, Pass, c2, kappa)
    | _ -> failwith "While guard must be a boolean")

  | sigma, Print(a), c, kappa -> let n = evale a sigma in 
    Pprint.print_value n; Format.printf "%s" "\n"; evals (sigma, Pass, c, kappa)

  | sigma, Break, c, (c_b, c_c, _)::kappa_t -> 
    evals (sigma, c_b, Pass, kappa_t)
  | sigma, Continue, c, (c_b, c_c, _)::kappa_t -> 
    evals (sigma, c_c, Pass, kappa_t)
  | sigma, Return e, _, _ ->  ("return", evale e sigma)::sigma
  | sigma, Break, c, [] -> failwith "Illegal break"
  | sigma, Continue, c, [] -> failwith "Illegal continue"
  | sigma, Def (rt, name, args, body), c, kappa -> 
    evals ((name, eval_fun (Def (rt, name, args, body)) sigma)::sigma, Pass, c, kappa)
  | sigma, Class (name, super, stmt), c, kappa ->
    let new_sigma = ("__mattrs__", VList([]))::("__attrs__", VList([]))::sigma in
    let obj_dict = evals (new_sigma, stmt, Pass, []) in
    let clobj = VObj(take (List.length obj_dict - List.length sigma) obj_dict) in
    evals ((name, VRef (ref (Some(clobj))))::sigma, c, Pass, kappa)
  | _ -> failwith "unimplemented"

and call (vclosure : value) (args : value list) : value = 
  let zip p a = (p, a) in
  (* failwith "poopy" *)
  match vclosure with 
  | VClosure (params, body, env_ref) -> 
  let callenv = 
    (match args with 
    | [] -> !env_ref
    | _ -> 
    (List.map2 zip params args)@(!env_ref)) in 
    lookup (evals (callenv, body, Pass, [])) "return"
  | VRef r -> 
    (match !r with 
    | Some (VObj(cls_obj) as v) -> call v args
    | _ -> failwith "Not good")
  | VObj cls_obj -> 
  begin
    match (lookup cls_obj "__init__") with
    | VClosure (params, body, env) as init -> 
    let attrs = lookup cls_obj "__attrs__" in 
    let mattrs = lookup cls_obj "__mattrs__" in
    let extract_string_list vlist = 
      (match vlist with 
      | VList (val_list) -> 
        let extract_string vstring = 
          (match vstring  with
          | VString s -> s
          | _ -> failwith "INFRASTRUCTURE FAIL") in
        List.map extract_string val_list
      | _ -> failwith "INFRASTRUCTURE FAILLLL") in 
    let attrs', mattrs' = extract_string_list attrs, extract_string_list mattrs in
    let preinit_attrs = List.map (fun x -> (x, ref VNone)) (attrs'@mattrs') in
    let cls_ref = [("__class__", ref (VRef (ref (Some (VObj cls_obj)))))] in
    let initial_obj_list = preinit_attrs@cls_ref in
    let instance = VPreObj(initial_obj_list) in 
    let new_args = (VRef (ref (Some instance)))::args in
    (match call init new_args with 
      | VPreObj(attrsmattrs) -> 
        let attrs = 
          List.filter (fun (name, vref) -> List.mem name attrs') attrsmattrs in
        let attrs = 
          List.map (fun (name, vref) -> (name, !vref)) attrs in
        let mattrs = List.filter (fun (name, vref) -> List.mem name mattrs') attrsmattrs in
        let mattrs =  List.map (fun (name, vref) -> (name, VRef (ref (Some !vref)))) mattrs in
          VObj([("__class__", VRef(ref (Some (VObj(cls_obj)))))]@attrs@mattrs)
    | _ -> failwith "Infra fail")
    | _ -> failwith "Typecheck fail"
  end 
  
  | _ -> failwith "TYPECHECK FAIL"

  (*
      let new_env: env = ("class", VObj(cls_obj))::(!env) in
      call (VClosure(List.tl params, body, ref new_env)) args ) *)