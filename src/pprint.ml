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

let print_tuple p exprs =
  Format.printf "@[<2>(";
  print_seq p ", " exprs;
  Format.printf ")@]"

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
  | Cons -> "::"

let string_of_unop o =
  match o with
  | Not -> "not"

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

(* Pretty print expression e *)
let print_exp e =
  let rec loop e =
    match e with
    | True -> Format.printf "%s" "true"
    | False -> Format.printf "%s" "false"
    | Empty t -> print_empty loop print_typ t e
    | Var x -> print_ident x
    | App (l, r) -> print_binop loop "" l r
    | Lam (x, t, e) -> print_lambda loop print_typ x t e
    | Let (x, e1, e2) -> print_let loop x e1 e2
    | Int i -> Format.printf "%i" i
    | Binary (o, l, r) -> print_binop loop (string_of_binop o) l r
    | Unary (o, e) -> print_unop loop (string_of_unop o) e
    | Tuple es -> print_tuple loop es
    | Proj (e, i) -> (loop e); Format.printf ".%i" i
    | Fix e -> Format.printf "fix "; loop e
    | If (e1, e2, e3) -> print_if loop e1 e2 e3
    | Match (e1, e2, e3) -> print_match loop e1 e2 e3
  in
  Format.printf "@[";
  loop e;
  Format.printf "@]"

