{
open Parser
open Printf
exception Eof
exception Err
}

let digit = ['0'-'9']
let id = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9']*
let bigid = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let ws = [' ' '\t']

rule token = parse
| ws               { token lexbuf }
| '\n'             { NEWLINE }
| "("              { LPAREN }
| ")"              { RPAREN }
| "."              { DOT }
| ";"              { SEMICOLON }
| ":"              { COLON }
| "->"             { ARROW }
| "lambda"         { LAMBDA }
| "=="             { EQUALSEQUALS }
| "="              { EQUALS }
| "def"            { DEF }
| "is"             { IS }
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
| id as v          { VAR(v) }
| bigid as i       { TNAME(i) }
| digit+ as n      { INT(int_of_string n) }
| "["              { LBRACK }
| "]"              { RBRACK }
| eof              { EOF }

| _ as c  {
            let pos = lexbuf.Lexing.lex_curr_p in
            printf "Error at line %d\n" pos.Lexing.pos_lnum;
            printf "Unrecognized character: [%c]\n" c;
            exit 1
          }
