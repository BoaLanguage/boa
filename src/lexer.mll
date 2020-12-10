{
open Parser
open Printf
open String
exception Eof
exception Err
}
 
let digit = ['0'-'9']
let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let ws = [' ']
let str = ['"'] ([^'"'] | ['\\'] ['"'])* ['"']
let small_str = ['\''] ([^'"'] | ['\\'] ['\''])* ['\'']
let nl = ['\n']+ ([' ']* | ['\t']*)
let bs = [' ']

rule token = parse
| ws               { token lexbuf }
| "let"            { LET }
| "var"            { VARKEYWORD }
| nl as s          { NEWLINE(String.length s - 1) }
| "("              { LPAREN }
| ")"              { RPAREN }
| "."              { DOT }
| ";"              { SEMICOLON }
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
| "else"           { ELSE }
| "elif"           { ELIF }
| "and"            { AND }
| "or"             { OR }
| "not"            { NOT }
| "return"         { RETURN }
| "print"          { PRINT }
| "["              { LBRACK }
| "]"              { RBRACK }
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