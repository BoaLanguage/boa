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
    | VAR (s) -> "VAR("^s^")"
    | TNAME (s) -> "TNAME("^s^")"
    | INT (i) -> "INT("^string_of_int i^")"
    | BOOL (b) -> "BOOL("^string_of_bool b^")"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACK -> "LBRACK"
    | RBRACK -> "RBRACK"
    | DOT -> "DOT"
    | COLON -> "COLON"
    | COMMA -> "COMME"
    | ARROW -> "ARROW"
    | LAMBDA -> "LAMBDA"
    | EQUALS -> "EQUALS"
    | EQUALSEQUALS -> "EQUALSEQUALS"
    | DEF -> "DEF"
    | IS -> "IS"
    | IN -> "IN"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | EOF -> "END OF FILE"
    | LESS -> "LESS"
    | GREATER -> "GREATER"
    | LEQ -> "LEQ"
    | GEQ -> "GEQ"
    | NEQ -> "NEQ"
    | MOD -> "MOD"
    | INTDIV -> "INTDIV"
    | DIV -> "DIV"
    | RETURN -> "RETURN"
    | INDENT -> "INDENT"
    | STAR -> "STAR"
    | STARSTAR -> "STARSTAR"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | ELIF -> "ELIF"
    | AND -> "AND"
    | OR -> "OR"
    | NOT -> "NOT"
    | SEMICOLON -> "SEMICOLON"
    | NEWLINE (i) -> "NEWLINE("^string_of_int i^")"
    | PRINT ->  "PRINT"
    | FOR -> "FOR"
    | CLASS -> "CLASS"
    | LIST -> "LIST"
    | WHILE -> "WHILE"
    | MEMBER -> "MEMBER"
    | DEDENT  -> "DEDDENT"
    | LET -> "LET"
    | VARKEYWORD -> "VARKEYWORD"
    | STRING s -> "STRING("^ s ^ ")"
    | NOP -> "NOP"

let rec print_lexbuf l =
  match l with
  | l when l.Lexing.lex_eof_reached -> ignore (Lexer.token l);
  | l -> (Lexer.token l |> str_of_token |> Format.printf "%s "); print_lexbuf l

let indent_level = ref 0
let dedent_stack : token list ref = ref []

let token_wrapper lexbuf = 
  let token = Lexer.token lexbuf in 
  let dedents = !dedent_stack in 
  match token with 
  | NEWLINE (i) -> 
    begin
      let level = !indent_level in
      if i > level then 
        (indent_level := i;
        dedent_stack := !dedent_stack + 1;
        INDENT)
      else (if i = level then 
        NOP
      else DEDENT)
    end
  | _ -> token

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
    (print_lexbuf lexbuf);
    Format.printf "\n---END_LEX--";
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