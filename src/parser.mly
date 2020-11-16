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
%token COMMA MOD INTDIV DIV RETURN
%token STAR STARSTAR IF ELSE ELIF AND OR NOT MOD SEMICOLON NEWLINE

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
    | DEF VAR declwrapper 
      ARROW TNAME COLON
      NEWLINE block                     { Def(TBase($5), $2, $3, $8) }

declwrapper:
    | LPAREN declist RPAREN             { $2 }
    | LPAREN RPAREN                     { [] }

declist:
    | TNAME VAR                         { [(TBase($1), $2)] }
    | TNAME VAR COMMA declist           { (TBase($1), $2)::$4 }

stmt:
    | istmt NEWLINE                   { $1 }
    | istmt EOF                       { $1 }

istmt: 
    | LPAREN exp RPAREN                 { Exp ($2) }
    | VAR EQUALS exp                    { Assign($1, $3) }
    | decl                              { $1 }
    | fndef                             { $1 }
    | RETURN exp                        { Return($2) }
decl:
    | TNAME VAR                         { Decl (TBase($1), $2) }

exp:
    | INT                               { Int($1) }
    | VAR                               { Var($1) }
    | bexp                              { $1 }
    | LPAREN exp RPAREN                 { $2 }

bexp:
    | exp PLUS exp                      { Binary(Plus, $1, $3) }
    | exp LESS exp                      { Binary(Less, $1, $3) }
    | exp GREATER exp                   { Binary(Greater, $1, $3) }
    | exp AND exp                       { Binary(And, $1, $3) }
    | exp OR exp                        { Binary(Or, $1, $3) }
    | exp EQUALS exp                    { Binary(Equal, $1, $3) }
    | exp STAR exp                      { Binary(Times, $1, $3) }
    | exp MINUS exp                     { Binary(Minus, $1, $3) }
    | exp DIV exp                       { Binary(Divide, $1, $3) }
    | exp MOD exp                       { Binary(Mod, $1, $3) }
    | exp STARSTAR exp                  { Binary(Exponent, $1, $3) }
    | exp INTDIV exp                    { Binary(IntDivide, $1, $3) }
    | exp LEQ exp                       { Binary(Leq, $1, $3) }
    | exp GEQ exp                       { Binary(Geq, $1, $3) }
    | exp IS exp                        { Binary(Is, $1, $3) }
    | exp IN exp                        { Binary(In, $1, $3) }
    | exp NEQ exp                       { Binary(Neq, $1, $3) }