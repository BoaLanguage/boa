{
open Parser
open Printf
exception Eof
exception Err

let get_indent_level nlseq = 
  
   let lst = String.split_on_char '\n' nlseq in 
   let inds = List.nth lst (List.length lst - 1) in 
  String.length inds
 
}
 
let digit = ['0'-'9']
let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let ws = [' ']
let str = ['"'] ([^'"'] | ['\\'] ['"'])* ['"']
let small_str = ['\''] ([^'"'] | ['\\'] ['\''])* ['\'']
let nl = ['\n'] ( ( ['\t'] | [' '] )* ['\n'] )* ( ['\t'] | [' '] )*

let bs = [' ']
let comment = ['#'] [^'\n']* (['\n'] | eof)

rule token = parse
| ws               { token lexbuf }
| comment          { token lexbuf }
| "let"            { LET }
| "var"            { VARKEYWORD }
| nl as s          { NEWLINE(get_indent_level s) }
| nl "elif" as s   { EOLIF(get_indent_level s - 4) }
| nl "else" as s   { EOLSE(get_indent_level s - 4) }
| "("              { LPAREN }
| ")"              { RPAREN }
| "."              { DOT }
| ":"              { COLON }
| "->"             { ARROW }
| "lambda"         { LAMBDA }
| "member"         { MEMBER }
| "=="             { EQUALSEQUALS }
| "="              { EQUALS }
| "list"           { LIST }
| "def"            { DEF }
| "is"             { IS }
| "for"            { FOR }
| "class"          { CLASS }
| "while"          { WHILE }
| "+"              { PLUS }
| "-"              { MINUS }
| "<"              { LESS }
| ">"              { GREATER }
| ">="             { GEQ }
| "<="             { LEQ }
| ","              { COMMA }
| "%"              { MOD }
| "**"             { STARSTAR }
| "//"             { INTDIV }
| "!="             { NEQ }
| "/"              { DIV }
| "in"             { IN }
| "*"              { STAR }
| "True"           { BOOL(true) }
| "False"          { BOOL(false) }
| "if"             { IF }
| "and"            { AND }
| "or"             { OR }
| "not"            { NOT }
| "return"         { RETURN }
| "print"          { PRINT }
| "["              { LBRACK }
| "]"              { RBRACK }
| "{"              { LBRACE }
| "}"              { RBRACE }
| "\""
| id as v          { VAR(v) }
| digit+ as n      { INT(int_of_string n) }
| str as s         { STRING(String.sub s 1 (String.length s - 2)) }
| small_str as s   { STRING(String.sub s 1 (String.length s - 2)) }
| eof              { EOF }

| _ as c  {
            let pos = lexbuf.Lexing.lex_curr_p in
            printf "Error at line %d\n" pos.Lexing.pos_lnum;
            printf "Unrecognized character: [%c]\n" c;
            exit 1
          }