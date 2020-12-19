open Ast
(* Pretty printing helper functions *)


let print_ident x =
  Format.printf "%s" x

let print_empty p pt t e =
  Format.printf "@[<2>(empty :@ " ;
  pt t;
  Format.printf ")@]"

let print_unop p s x =
  Format.printf "@[<2>(%s@ " s;
  p x;
  Format.printf ")@]"

let print_binop p s x y =
  Format.printf "@[<2>(";
  p x;
  Format.printf "@ %s@ " s;
  p y;
  Format.printf ")@]"

let print_lambda p pt x t e =
  Format.printf "@[<2>(lambda %s@ :@ " x;
  pt t;
  Format.printf ".@ ";
  p e;
  Format.printf ")@]"

let print_let p x e1 e2 =
  Format.printf "@[<2>(let %s@ =@ " x;
  p e1;
  Format.printf "@ in@ ";
  p e2;
  Format.printf ")@]"

let print_if p e1 e2 e3 =
  Format.printf "@[<2>(if ";
  p e1;
  Format.printf "@ then@ ";
  p e2;
  Format.printf "@ else@ ";
  p e3;
  Format.printf ")@]"

let print_match p e1 e2 e3 =
  Format.printf "@[<2>(match ";
  p e1;
  Format.printf "@ with_empty@ ";
  p e2;
  Format.printf "@ with_head_rest@ ";
  p e3;
  Format.printf ")@]"

let rec print_seq p sep exprs =
  match exprs with
  | e1::es -> (
      p e1;
      (match es with
       | _::_ -> Format.printf sep
       | _ -> ());
      print_seq p sep es)
  | _ -> ()

let string_of_binop o =
  match o with
  | Plus -> "+"
  | Less -> "<"
  | Greater -> ">"
  | Equal -> "="
  | And -> "and"
  | Or -> "or"
  | Minus -> "-"
  | Times -> "*"
  | _ -> failwith "Print"

let string_of_unop o =
  match o with
  | Not -> "not"
  | _ -> failwith "Print"

let insert_delimiter_between_list (delim : string) (mapper : 'a -> string) (lst: 'a list): string  = 
  let mapped = List.map mapper lst in
    match mapped with
    | [] -> ""
    | h::[] -> h 
    | h::t -> List.fold_left (fun acc n -> acc ^ delim ^ n) h t 

let rec str_of_typ t =
  match t with
  | TBase s -> s
  | TFun (t1, t2) ->
    "(" ^ (str_of_typ t1) ^ " -> " ^ (str_of_typ t2) ^ ")"
  | TTuple lst ->
      "TUPLE: " ^
      insert_delimiter_between_list " * " (fun t -> (str_of_typ t)) lst
  | TList tl ->
    (str_of_typ tl) ^ " list"
  | TVar v -> "'" ^ (String.make 1 (Char.chr (v + 97)))
  | TDict (t1, t2) -> "Dict : " ^ (str_of_typ t1 ^ " : " ^ str_of_typ t2)
  | TLimbo t -> "LIMBO: " ^ str_of_typ t
  | TMutable t -> "MUTABLE: " ^ str_of_typ t
  | _ -> failwith "Unimplemented (str_of_typ)"

let str_of_int_list lst : string = 
  insert_delimiter_between_list ", " (fun i -> String.make 1 @@ Char.chr @@ (i) + 97) lst

let str_of_scheme sch = 
  let ilist, t = sch in 
  "\\./ " ^ str_of_int_list ilist ^ ". " ^ (str_of_typ t)

let str_of_gamma : Ast.mappings -> string = 
  insert_delimiter_between_list "\n" (fun (v, sch) -> v ^ " => " ^ str_of_scheme sch)

let str_of_constr = 
  List.fold_left 
  (fun acc (t1, t2) -> acc ^ ", " ^ (str_of_typ t1) ^ " == " ^ (str_of_typ t2)) ""

let str_of_sub = 
  List.fold_left 
  (fun acc (t1, t2) -> acc ^ ", " ^ ((String.make 1 (Char.chr (t1 + 97)))) ^ " => " ^ (str_of_typ t2)) ""

(* Pretty print type t *)
let print_typ t =
  let rec loop t =
    match t with
    | TBase s -> print_ident s
    | TFun (t1, t2) ->
      Format.printf "@[<2>(";
      print_binop loop "->" t1 t2;
      Format.printf ")@]"
    | TTuple ts ->
      Format.printf "@[<2>(";
      print_seq loop " * " ts;
      Format.printf ")@]"
    | TList tl ->
      Format.printf "@[<2>(";
      print_unop loop "List" tl;
      Format.printf ")@]"
    | TVar v -> Format.printf "TVar %s" (string_of_int v);
  in
  loop t

let rec print_expr e = 
  match e with 
  | Var v -> print_ident v
  | Int i -> Format.printf "%s" (string_of_int i)
  | Bool b -> Format.printf "%s" (string_of_bool b)
  | Call (fn, args) -> 
    (print_expr fn); 
    print_expr (Tuple(args))
  | Tuple lst -> 
    Format.printf "(";
    print_list lst;
    Format.printf ")";
  | Skip -> Format.printf "None"
  | Lam (v, t_opt, e) -> 
    Format.printf "lambda %s -> " v; print_expr e;
  | _ -> Format.printf "yikes"

and print_list lst = 
  match lst with
  | [] -> Format.printf ""
  | e::[] -> print_expr e
  | e::r -> 
  print_expr e; Format.printf ", "; print_list r

let rec print_stmt s =
  match s with
  | Exp e -> print_expr e
  | Block (s::rest) -> 
  Format.printf "{";
  print_stmt s; 
  Format.printf ";\n";
  print_lst rest;
  Format.printf "}\n"
  | Decl (t, v) -> 
  Format.printf "%s : " v;
  (match t with 
  | Some t -> print_typ t
  | None -> ())
  | AttrAssgn (ex, n, va) -> 
  print_expr (AttrAccess(ex, n));
  Format.printf " = "; print_expr va
  | Assign (v, e) ->
  Format.printf "%s := " v;
  print_expr e
  | SliceAssgn (e1, e2, e3) ->
  print_expr e1;
  Format.printf "[";
  print_expr e2;
  Format.printf "] := ";
  print_expr e3
  | Return (e1) -> Format.printf "return: ";
  print_expr e1
  | Print (e1) -> Format.printf "print: ";
  print_expr e1
  | Pass -> Format.printf ": pass :";
  | If (e1, s1, s2) -> 
  Format.printf "If ("; print_expr e1; Format.printf ")\n";
  print_stmt s1;
  Format.printf "Else: \n";
  print_stmt s2;
  | Def (t1, fn, args, b) -> 
  Format.printf "Function %s -> " fn;
  (match t1 with 
  | Some t -> print_typ t
  | None -> Format.printf "None");
  Format.printf "\n";
  print_stmt b;
  | Break -> Format.printf ": break :"
  | Continue -> Format.printf ": continue :"
  | Block [] -> Format.printf "[]"
  | While (e, b) -> 
  Format.printf "While "; print_expr e; Format.printf "\n";
  print_stmt b
  | For (v, e, b) ->
  Format.printf "For %s in " v;
  print_expr e;
  Format.printf "\n";
  print_stmt b
  | Class (v, e, b) -> 
  Format.printf "Class %s (subclass of " v;
  print_expr e;
  Format.printf ")\n";
  print_stmt b
  | MutableDecl (t, v) -> 
  Format.printf "Mutable decl: %s" v;
  | _ -> Format.printf "DECLS"

and print_lst lst = 
match lst with 
| [] -> Format.printf ""
| s::[] -> print_stmt s
| s::rest -> print_stmt s; Format.printf ";\n"; print_lst rest

(* Pretty print expression e *)

let rec print_value (v : value) =
  match v with
  | VInt i -> Format.printf "%d" i;
  | VRef vr -> Format.printf "mutable ("; 
    (match !vr with 
    | Some v -> print_value (v); Format.printf ")";
    | None -> Format.printf "None)";)
  | VString s -> Format.printf "%s" s;
  | VBool b -> Format.printf "%b" b;
  | VClosure (v, body, env) -> Format.printf "%s" "Some closure";
  | VDict d -> 
    (match d with 
    | [] -> Format.printf "{}"
    | (v1, v2)::rest -> 
    Format.printf "(";
    print_value v1; 
    Format.printf ": ";
    print_value v2;
    Format.printf "), ";
    print_value (VDict(rest)))
  | VList l -> 
    (match l with
    | [] -> Format.printf ""
    | v::rest -> 
    print_value v;
    Format.printf ", ";
    print_value (VList(rest)))
  | VObj l -> 
      Format.printf "{\n";
      let rec loop l = 
      match l with 
      | [] -> ()
      | (v, va)::rest -> 
      Format.printf "\t%s <- " v;
      print_value va;
      Format.printf "\n";
      loop rest in 
      loop l; Format.printf "\n}\n";
  | VNone -> Format.printf "None";
  | VPreObj (a) -> Format.printf "Preobj";
  | VMethodCall (obj, m) -> 
      print_value obj;
      Format.printf ".";
      print_value m;
  | _ -> failwith "Not Printable"

let rec print_env (s : env) =
  List.iter (fun (var, v) -> Format.printf "\n%s: " var; print_value v) s; ()