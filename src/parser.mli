type token =
  | VAR of (string)
  | TNAME of (string)
  | INT of (int)
  | BOOL of (bool)
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | DOT
  | COLON
  | COMMA
  | ARROW
  | LAMBDA
  | EQUALS
  | EQUALSEQUALS
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
  | STAR
  | STARSTAR
  | IF
  | ELSE
  | ELIF
  | AND
  | OR
  | NOT

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.stmt
