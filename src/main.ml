open Ast
open Parser

(* Command-line arguments. *)
let filename = ref ""
let nocheck = ref false

let options = [
  "-nocheck", Arg.Unit (fun _ -> nocheck := true), "Disable type checker"
]

let str_of_token token = 
  match token with
    | VAR (s) -> "SOME VARIABLE " ^ s
    | TNAME (s) -> s
    | INDENTLEVEL (i) -> string_of_int i
    | INT (i) -> string_of_int i
    | BOOL (b) -> string_of_bool b
    | LPAREN -> "("
    | RPAREN -> ")"
    | LBRACK -> "["
    | RBRACK -> "]"
    | DOT -> "."
    | COLON -> ":"
    | COMMA -> ","
    | NLS -> "NLS"
    | ARROW -> "->"
    | LAMBDA -> "LAMBDA"
    | EQUALS -> "="
    | EQUALSEQUALS -> "=="
    | DEF
    | IS
    | IN
    | PLUS
    | MINUS
    | EOF
    | LESS
    | GREATER
    | LEQ
    | GEQ
    | NEQ
    | MOD
    | INTDIV
    | DIV
    | RETURN
    | INDENT
    | STAR
    | STARSTAR
    | IF
    | ELSE
    | ELIF
    | AND
    | OR
    | NOT
    | SEMICOLON
    | NEWLINE -> "Yo"
    | PRINT ->  "fuck that shit"

let rec print_lexbuf l =
  match l with
  | l when l.Lexing.lex_buffer_len = l.Lexing.lex_curr_pos -> ignore (Lexer.token l); Format.printf "%d"
  | l -> (Lexer.token l |> str_of_token |> Format.printf "%s"); print_lexbuf l

let () =
  (* (1) Parse the command-line arguments. *)
  let usage_msg = Format.sprintf "Usage: %s [opts] <file>\n" Sys.argv.(0) in
  let _ = begin
    Arg.parse options (fun f ->
        if !filename = "" then filename := f else Arg.usage options usage_msg
      ) usage_msg;
    if !filename = "" then (Arg.usage options usage_msg; exit 1);
  end in

  (* (2) Parse the file to an expression. *)
  let file = open_in (!filename) in
  let lexbuf = Lexing.from_channel file in
  let e =
    (* Format.printf "%s\n" ((Lexer.token lexbuf) |> str_of_token);
    Format.printf "%s\n" ((Lexer.token lexbuf) |> str_of_token); *)
    (Format.printf "%s\n" (Lexing.lexeme lexbuf));
    try Parser.prog Lexer.token lexbuf
    with Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at %d:%d\n"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1 in

  (* (3) Pretty-print the expression. *)
  let _ =
    Format.printf "@[";
    Format.printf "Expression:@\n  @[";
    (Pprint.print_stmt e);
    Format.printf "@]@\n@\n" in

  (* (4) Typecheck the expression. *)
  let _ = if !nocheck then
      Format.printf "Type checking skipped.@\n@\n"
    else
      ignore (Check.check_stmt [] e (None));
    (* (match e with
    | s -> Check.check_stmt s
    | Exp (exp) -> 
      let t = Check.check_exp exp in
      Format.printf "@[";
      Format.printf "Type:@\n  @[";
      Pprint.print_typ t;
      Format.printf "@]@\n@\n"
    | _ -> failwith "not a stmt") *)
  in

  (* (5) Evaluate the expression. *)
  let _ =
    Format.printf "Evaluating the expression...@\n@\n";
    Format.print_flush () in

  let v = Eval.evals (Eval.make_configuration e) in

  (* (6) Pretty-print the final value. *)
  let _ =
    Format.printf "Result:@\n  @[";
    Pprint.print_env v;
    Format.printf "@]@\n";
    Format.printf "@]" in
  ()