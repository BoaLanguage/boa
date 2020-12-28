open Parser

type indent_state = {
    mutable indent_levels: int list;
    mutable temp_tokens: token list;
}

exception IllegalIndentationError


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
