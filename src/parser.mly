%{
  open Ast
  open Printf
  open Lexing

  let tuple_type ts =
  match ts with
  | [t] -> t
  | _ -> TTuple ts

  let tuple_expr es =
  match es with
  | [e] -> e
  | _ -> Tuple es
%}

%token <string> VAR TNAME
%token <int> INT
%token <bool> BOOL
%token LPAREN RPAREN LBRACK RBRACK DOT COLON COMMA
%token ARROW LAMBDA EQUALS EQUALSEQUALS DEF IS IN PLUS MINUS
%token EOF LESS GREATER LEQ GEQ NEQ
%token COMMA MOD INTDIV DIV
%token STAR STARSTAR IF ELSE ELIF AND OR NOT MOD

%type <Ast.stmt> prog

%start prog

%%
prog: stmt EOF                          { $1 }

stmt: LPAREN INT RPAREN                 { Exp (Int($2)) }