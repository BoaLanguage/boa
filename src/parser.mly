%{
  open Ast
  open Printf
  open Lexing
  open Stack
 
  type bb = 
  | End
  | Pair of stmt * bb

  let tuple_type ts =
  match ts with
  | [t] -> t
  | _ -> TTuple ts

  let tuple_expr es =
  match es with
  | [e] -> e
  | _ -> Tuple es
%}

%token <string> VAR TNAME STRING
%token <int> NEWLINE
%token <int> INT
%token <bool> BOOL
%token LPAREN RPAREN LBRACK RBRACK DOT COLON COMMA FOR
%token ARROW LAMBDA EQUALS EQUALSEQUALS DEF IS IN PLUS MINUS
%token EOF LESS GREATER LEQ GEQ NEQ CLASS WHILE MEMBER LIST
%token COMMA MOD INTDIV DIV RETURN INDENT DEDENT LET VARKEYWORD
%token STAR STARSTAR IF ELSE ELIF AND OR NOT MOD SEMICOLON PRINT NOP EOL

%type <Ast.stmt> prog

%start prog

%%
prog: 
    | stmtlist COMMA                     { Block($1) }
    /* | stmt 
      ind_tuple EOF                     { block_structure (($1, 0)::$2) 0 } */

block:
    | INDENT stmtlist DEDENT            { Block($2) }
    | INDENT nll stmtlist nll DEDENT    { Block($3) }
    | INDENT nll stmtlist DEDENT        { Block($3) }
    | INDENT stmtlist nll DEDENT        { Block($2) }

nll:
    | EOL                           { [] }   
    | nll EOL                       { [] }                         

/* indented_block:
    | indented_stmtlist                 { Block($1) }

indented_stmtlist:
    | INDENT stmt                       { [$2] }
    | indented_stmtlist 
      EOL INDENT stmt               { $1@[$4] } */

stmtlist: 
    | stmt                              { [$1] }
    /* | stmtlist EOL                  { $1 } */
    | stmtlist EOL stmt             { $1@[$3] }
/* 
ind_block:
    | ind_tuple                         { block_structure $1 0 } */
/* 
ind_tuple:
    | INDENTLEVEL stmt                  { [($2, $1)] }
    | ind_tuple INDENTLEVEL stmt        { $1@[($3, $2)] } */

stmt:
   |  expr                              { Exp($1) }
   /* |  thint                             { Decl(fst $1, snd $1) } */
   |  expr DOT VAR EQUALS expr          { AttrAssgn($1, $3, $5) }
   |  LET thint EQUALS expr             { Block([
                                        Decl(fst $2, snd $2);
                                        Assign(snd $2, $4)
                                        ]) }
    | VARKEYWORD thint EQUALS expr      { Block([
                                        MutableDecl(fst $2, snd $2);
                                        Assign(snd $2, $4)
                                        ]) }                                      
    | VAR EQUALS expr                   { Assign($1, $3) }
    | expr LBRACK expr RBRACK 
      EQUALS expr                       { SliceAssgn($1, $3, $6) }
    | RETURN expr                       { Return ($2) }
    | PRINT expr                        { Print ($2) }
    | iff                               { $1 }
    | DEF VAR paramlist ARROW 
      typ EOL block                 { Def($5, $2, $3, $7) }
    | WHILE expr EOL block          { While($2, $4) }
    | FOR VAR IN expr EOL block     { For($2, $4, $6) }
    | CLASS VAR EOL block           { Class($2, Skip, $4) }
    | CLASS VAR 
      LPAREN expr RPAREN 
      EOL block                     { Class($2, $4, $7) }
    | MEMBER VARKEYWORD VAR COLON typ   { MutableMemDecl($5, $3) }
    | MEMBER LET VAR COLON typ          { MemDecl($5, $3) }

iff:
    | IF expr EOL 
      block                    { If ($2, $4, Exp(Skip)) }
    | IF expr EOL            
      block EOL
      ELSE EOL
      block                    { If ($2, $4, $8) }
/* 
elifchain:
    | ELIF expr EOL block           { If($2, $4, Exp(Skip)) }
    | elifchain ELIF expr EOL block { If() } */

expr:
    | LPAREN expr RPAREN                { $2 }
    | VAR                               { Var($1) }
    | expr arglist                      { Call($1, $2) }
    | tuple                             { $1 }
    | INT                               { Int($1) }
    | expr LBRACK expr RBRACK           { SliceAccess($1, $3) }
    | expr DOT VAR                      { AttrAccess($1, $3) }
    | NOT expr                          { Unary(Not, $2) }
    | MINUS expr                        { Unary(Neg, $2) }
    | BOOL                              { Bool($1) }
    | LAMBDA thint ARROW expr           { Lam(snd $2, fst $2, $4) }
    | tuple                             { $1 }
    | lst                               { $1 }
    | dict                              { $1 }
    | bexp                              { $1 }
    | STRING                            { String($1) }

bexp:
    | expr PLUS expr                      { Binary(Plus, $1, $3) }
    | expr LESS expr                      { Binary(Less, $1, $3) }
    | expr GREATER expr                   { Binary(Greater, $1, $3) }
    | expr AND expr                       { Binary(And, $1, $3) }
    | expr OR expr                        { Binary(Or, $1, $3) }
    | expr EQUALS expr                    { Binary(Equal, $1, $3) }
    | expr STAR expr                      { Binary(Times, $1, $3) }
    | expr MINUS expr                     { Binary(Minus, $1, $3) }
    | expr DIV expr                       { Binary(Divide, $1, $3) }
    | expr MOD expr                       { Binary(Mod, $1, $3) }
    | expr STARSTAR expr                  { Binary(Exponent, $1, $3) }
    | expr INTDIV expr                    { Binary(IntDivide, $1, $3) }
    | expr LEQ expr                       { Binary(Leq, $1, $3) }
    | expr GEQ expr                       { Binary(Geq, $1, $3) }
    | expr IS expr                        { Binary(Is, $1, $3) }
    | expr IN expr                        { Binary(In, $1, $3) }
    | expr NEQ expr                       { Binary(Neq, $1, $3) }

lst:
    | LBRACK RBRACK                     { List([]) }
    | LBRACK exprlist RBRACK            { List($2) }

thint:
    | VAR COLON typ                     { ($3, $1) }
    | LPAREN thint RPAREN               { $2 }

typ:
    | VAR                               { TBase($1) }
    | typ ARROW typ                     { TFun($1, $3) }
    | typlist                           { TTuple($1) }
    | typ LIST                          { TList($1) }
    | LPAREN typ RPAREN                 { $2 }

typlist:
    | typ                               { [$1] }
    | typlist STAR typ                  { $1@[$3] }

arglist:
    | LPAREN RPAREN                     { [] }
    | LPAREN exprlist RPAREN            { $2 }

paramlist:
    | LPAREN RPAREN                     { [] }
    | LPAREN thintlist RPAREN           { $2 }

thintlist:
    | thint                             { [$1] }
    | thintlist COMMA thint             { $1@[$3] }

dict:
    | INDENT DEDENT                     { Dict([]) }
    | INDENT kvplist DEDENT             { Dict($2) }

kvplist:
    | kvp                               { [$1] }
    | kvplist COMMA kvp                 { $1@[$3] }

kvp:
    | expr COLON expr                   { ($1, $3) }

exprlist:
    | expr                              { [$1] }
    | exprlist COMMA expr               { $1@[$3] }

tuple:
    | LPAREN exprlist RPAREN            { Tuple($2) }