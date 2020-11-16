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

stmt: 

exp:
    | LET VAR EQUALS exp IN exp         { Let($2, $4, $6) }
    | texp                              { $1 }

texp:
    | tlist                             { tuple_expr $1 }

tlist:
    | fexp COMMA tlist                  { $1::$3 }
    | fexp                              { [$1] }

fexp:
    | FIX lexp                          { Fix($2) }
    | lexp                              { $1 }

lexp:
    | LAMBDA VAR COLON typ DOT fexp     { Lam($2, $4, $6) }
    | mexp                              { $1 }

mexp: 
    | MATCH exp WEMPTY exp WHDREST fexp { Match($2, $4, $6) }
    | ifexp                             { $1 }

ifexp:
    | IF unexp THEN exp ELSE fexp       { If($2, $4, $6) }
    | unexp                             { $1 }

unexp:
    | NOT unexp                         { Unary(Not, $2) }
    | boolbinexp                        { $1 }

boolbinexp:
    | boolbinexp AND cmpexp             { Binary(And, $1, $3) }
    | boolbinexp OR cmpexp              { Binary(Or, $1, $3) }
    | cmpexp                            { $1 }

cmpexp:
    | cmpexp LESS consexp               { Binary(Less, $1, $3) }
    | cmpexp GREATER consexp            { Binary(Greater, $1, $3) }
    | cmpexp EQUALS consexp             { Binary(Equal, $1, $3) }
    | consexp                           { $1 }

consexp:
    | plusexp CONS consexp               { Binary(Cons, $1, $3) }
    | plusexp                            { $1 }

plusexp:
    | plusexp PLUS multexp              { Binary(Plus, $1, $3) }
    | plusexp MINUS multexp             { Binary(Minus, $1, $3) }
    | multexp                           { $1 }

multexp:
    | multexp STAR appexp              { Binary(Times, $1, $3) }
    | appexp                           { $1 }

appexp:
    | appexp aexp                       { App($1, $2) }
    | aexp                              { $1 }

aexp:
    | VAR                               { Var($1) }
    | INT                               { Int($1) }
    | TRUE                              { True }
    | FALSE                             { False }
    | EMPTY COLON atyp                  { Empty($3) }
    | LPAREN exp RPAREN                 { $2 }

typ:
    | ftyp                              { $1 }

ftyp:
    | atyp ARROW ftyp                   { TFun($1, $3) }
    | ttlist                            { tuple_type $1 }

ttlist:
    | atyp STAR ttlist                  { $1::$3 }
    | atyp                              { [$1] }

atyp:
    | TNAME                             { TBase($1) }
    | LIST atyp                         { TList($2) } 
    | LPAREN typ RPAREN                 { $2 }                   
