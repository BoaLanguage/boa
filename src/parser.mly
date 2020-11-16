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
%token STAR STARSTAR IF ELSE ELIF AND OR NOT MOD SEMICOLON

%type <Ast.stmt> prog

%start prog

%%
prog: block EOF                          { $1 }

block:
    | blockl                            { Block($1) }

blockl: 
    | stmt                              { [$1] }
    | stmt blockl                       { $1::$2 }

fndef:
    | DEF TNAME 
      VAR declwrapper COLON block       { Def(TBase($2), $3, $4, $6) }

declwrapper:
    | LPAREN declist RPAREN             { $2 }
    | LPAREN RPAREN                     { [] }

declist:
    | TNAME VAR                         { [(TBase($1), $2)] }
    | TNAME VAR COMMA declist           { (TBase($1), $2)::$4 }

stmt:
    | istmt SEMICOLON                   { $1 }

istmt: 
    | LPAREN exp RPAREN                 { Exp ($2) }
    | VAR EQUALS exp                    { Assign($1, $3) }
    | decl                              { $1 }
    | fndef                             { $1 }
decl:
    | TNAME VAR                         { Decl (TBase($1), $2) }

exp:
    | INT                               { Int($1) }