open Boa
open Boa.Parser

(* Command-line arguments. *)
let filename = ref ""
let nocheck = ref false

exception IllegalIndentationError

let options = [
  "-nocheck", Arg.Unit (fun _ -> nocheck := true), "Disable type checker"
]

let indent_level = ref 0
let dedent_stack : int list ref = ref [0]
let temp_tokens : token list ref = ref []
let token_wrapper lexbuf = 
  let rec find_dedents (dedent_stack_copy: int list) 
      (level: int) (acc: token list): int list * token list = 
    match dedent_stack_copy with 
    | [] -> raise IllegalIndentationError
    | h :: t -> if (h = level) then (dedent_stack_copy, acc)
      else find_dedents t level (DEDENT::acc) 
  in 
  if !temp_tokens <> [] then 
    let token = List.hd !temp_tokens in 
    temp_tokens := List.tl !temp_tokens; token 
  else if !indent_level < 0 then (EOF)  else
    let token = Lexer.token lexbuf in 
    match token with 
    | NEWLINE (i) -> 
      begin
        let level = !indent_level in
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
      indent_level := -1;
      dedent_stack := [];
      temp_tokens := dedent_list;
      EOL
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
    (* print_tokens token_wrapper lexbuf;
       Format.printf "\n---END_LEX--"; *)
    try Parser.prog token_wrapper lexbuf
    with _ ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Format.printf "Syntax error at %d:%d\n"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1 in

  (* (4) Typecheck the expression. *)
  let _ = if !nocheck then
      Format.printf "Type checking skipped.@\n@\n"
    else
      let res = Check.check e in 
      Format.printf "Inferred Types: [\n%s\n]\n" (res |> Pprint.str_of_gamma)
  in

  (* (5) Evaluate the expression. *)
  let _ =
    Format.printf "Evaluating the expression...@\n";
    Format.print_flush () in

  let v = Eval.evals (Eval.make_configuration e) in
  (* (6) Pretty-print the final value. *)
  let _ =
    Format.printf "\n\nState After Execution:@\n  @[";
    Pprint.print_env v;
    Format.printf "@]@\n";
    Format.printf "@]" in
  ()