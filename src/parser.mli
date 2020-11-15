type token =
  | VAR of (string)
  | TNAME of (string)
  | LPAREN
  | RPAREN
  | DOT
  | COLON
  | EOF
  | LAMBDA
  | ARROW
  | LET
  | EQUALS
  | IN
  | INT of (int)
  | PLUS
  | MINUS
  | LESS
  | GREATER
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | EMPTY
  | CONS
  | MATCH
  | WEMPTY
  | WHDREST
  | LIST
  | COMMA
  | STAR
  | FIX

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
