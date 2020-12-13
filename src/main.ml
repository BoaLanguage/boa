open Ast
open Parser

(* Command-line arguments. *)
let filename = ref ""
let nocheck = ref false

exception IllegalIndentationError

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
    | DEDENT  -> "DEDENT"
    | LET -> "LET"
    | VARKEYWORD -> "VARKEYWORD"
    | STRING s -> "STRING("^ s ^ ")"
    | NOP -> "NOP"
    | EOL -> "EOL"
    | _ -> "Pimpy"

let rec print_lexbuf l =
  match l with
  | l when l.Lexing.lex_eof_reached -> ignore (Lexer.token l);
  | l -> (Lexer.token l |> str_of_token |> Format.printf "%s "); print_lexbuf l

let indent_level = ref 0
let dedent_stack : int list ref = ref [0]
let temp_tokens : token list ref = ref []
let rec token_wrapper lexbuf = 
  let rec find_dedents (dedent_stack_copy: int list) 
    (level: int) (acc: token list): int list * token list = 
    match dedent_stack_copy with 
    | [] -> raise IllegalIndentationError
    | h :: t -> if (h = level) then (dedent_stack_copy, acc)
      else find_dedents t level (DEDENT::acc) 
  in 
  if !temp_tokens <> [] then 
    let token = List.hd !temp_tokens in 
    temp_tokens := List.tl !temp_tokens; print_endline @@ str_of_token token; token 
  else if !indent_level < 0 then (print_endline "EOF"; EOF)  else
  let token = Lexer.token lexbuf in 
  match token with 
  | NEWLINE (i) -> 
    begin
      let level = !indent_level in
      print_endline "EOL";
      if i > level then 
        (indent_level := i;
        dedent_stack := i::!dedent_stack;
        temp_tokens := [INDENT];
        EOL)
      else if i = level then 
        EOL
      else 
        let new_dedent_stack, dedent_list = find_dedents !dedent_stack i [EOL] in 
        indent_level := i;
        dedent_stack := new_dedent_stack;
        temp_tokens := dedent_list;
        EOL
    end
  | EOF -> let _, dedent_list = find_dedents !dedent_stack 0 [EOL] in 
        print_int (List.length dedent_list);
        indent_level := -1;
        dedent_stack := [];
        temp_tokens := dedent_list;
        EOL
  | _ -> print_endline @@ str_of_token token; token

let rec print_tokens token_fun lexbuf = 
  let token = token_fun lexbuf in 
  print_endline @@ str_of_token token;
  if token = EOF then () else print_tokens token_fun lexbuf

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
    (* print_tokens token_wrapper lexbuf;
    Format.printf "\n---END_LEX--"; *)
    try Parser.prog token_wrapper lexbuf
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
      Format.printf "Inferred Types: [%s]\n" (Check.check_stmt [] e (None) 0 |> fst |> Check.str_of_gamma);
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
    Format.printf "\n\nState After Execution:@\n  @[";
    Pprint.print_env v;
    Format.printf "@]@\n";
    Format.printf "@]" in
  ()