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
  print_typ t
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
  | _ -> Format.printf ""

and print_lst lst = 
match lst with 
| [] -> Format.printf ""
| s::[] -> print_stmt s
| s::rest -> print_stmt s; Format.printf ";\n"; print_lst rest

(* Pretty print expression e *)