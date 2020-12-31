open Boa
open Boa.Parser
open Boa.Indent

(* Command-line arguments. *)
let filename = ref ""
let nocheck = ref false


let options = [
  "-nocheck", Arg.Unit (fun _ -> nocheck := true), "Disable type checker"
]

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

  let v =  (Boa.Codegen.codegen_statement e); in
  Llvm.dump_value v;  print_string "ready> "; flush stdout;

  (* (6) Pretty-print the final value. *)
  (* let _ =
    Format.printf "\n\nState After Execution:@\n  @[";
    Pprint.print_env v;
    Format.printf "@]@\n";
    Format.printf "@]" in
  () *)