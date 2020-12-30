%{
  open Ast

  let add_elif_to_if if_stmt elif = 
  match if_stmt with 
  | If (c, b, _) -> Format.printf "nice!"; If (c, b, elif)
  | _ -> failwith "Attempted to add elif to non-if statement"
%}

%token <string> VAR STRING
%token <int> INT NEWLINE
%token <bool> BOOL
%token LPAREN RPAREN LBRACK RBRACK DOT COLON COMMA FOR LBRACE RBRACE
%token ARROW LAMBDA EQUALS EQUALSEQUALS DEF IS IN PLUS MINUS
%token EOF LESS GREATER LEQ GEQ NEQ CLASS WHILE MEMBER LIST
%token MOD INTDIV DIV RETURN INDENT DEDENT LET VARKEYWORD
%token STAR STARSTAR IF ELSE ELIF AND OR NOT PRINT EOL

%nonassoc STARSTAR NEQ LPAREN LESS LEQ LBRACK IS INTDIV IN GREATER GEQ EQUALSEQUALS
%left PLUS MINUS OR
%left STAR AND
%left DIV MOD
%nonassoc NOT
%nonassoc DOT

%type <Ast.stmt> prog

%start prog

%%
prog: 
    | stmtlist EOF                     { Block($1) }

iblock:
    | INDENT block DEDENT               { $2 }

block:
    | stmtlist                          { Block($1) }

stmtlist: 
    | stmt_newline                      { [$1] }
    | stmtlist stmt_newline             { $1@[$2] }

stmt_newline:
    | simple_stmt EOL                  { $1 }
    | compound_stmt                    { $1 }
    | EOL                              { Pass }

simple_stmt:
   |  expr                              { Exp($1) }
   |  expr DOT VAR EQUALS expr          { AttrAssgn($1, $3, $5) }
   |  LET thint EQUALS expr             { Block([
                                        Decl(Some (fst $2), snd $2);
                                        Assign(snd $2, $4)
                                        ]) }
    | VARKEYWORD thint EQUALS expr      { Block([
                                        MutableDecl(Some (fst $2), snd $2);
                                        Assign(snd $2, $4)
                                        ]) }                                      
    | VAR EQUALS expr                   { Assign($1, $3) }
    | expr LBRACK expr RBRACK 
      EQUALS expr                       { SliceAssgn($1, $3, $6) }
    | RETURN expr                       { Return ($2) }
    | PRINT expr                        { Print ($2) }
    | MEMBER VARKEYWORD VAR COLON typ   { MutableMemDecl($5, $3) }
    | MEMBER LET VAR COLON typ          { MemDecl($5, $3) }
    | LET VAR EQUALS expr               { Block([
                                        Decl(None, $2);
                                        Assign($2, $4)
                                        ]) }
    | VARKEYWORD VAR EQUALS expr        { Block([
                                        MutableDecl(None, $2);
                                        Assign($2, $4)
                                        ]) }
    | LET thint                         { Decl(Some(fst $2), snd $2) }
    | LET VAR                           { Decl(None, $2) }
    | VARKEYWORD thint                  { MutableDecl(Some(fst $2), snd $2) }
    | VARKEYWORD VAR                    { MutableDecl(None, $2) }
    
compound_stmt:
    | ifstmt                             { $1 }
    | DEF VAR paramlist ARROW 
      typ COLON EOL iblock               { Def(Some($5), $2, $3, $8) }
    | DEF VAR paramlist COLON EOL iblock     { Def(None, $2, $3, $6) }
    | WHILE expr COLON EOL iblock            { While($2, $5) }
    | FOR VAR IN expr COLON EOL iblock       { For($2, $4, $7) }
    | CLASS VAR COLON EOL iblock             { Class($2, Skip, $5) }
    | CLASS VAR 
      LPAREN expr RPAREN 
      COLON EOL iblock                      { Class($2, $4, $8) }

ifstmt:
    | iff                               { $1 }
    /* | ifelif                            { $1 } */

iff:
    | IF expr COLON
      iblock                            { If ($2, $4, Exp(Skip)) }

/* ifelif:
    | iff ELIF expr COLON 
      iblock                            { add_elif_to_if $1 (If($3, $5, Exp(Skip))) } */

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
    | LAMBDA thint ARROW expr           { Lam(snd $2, Some(fst $2), $4) }
    | LAMBDA VAR ARROW expr             { Lam($2, None, $4) }
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
    | expr EQUALSEQUALS expr              { Binary(Equal, $1, $3) }
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

thintopt:
    | VAR COLON typ                     { (Some($3), $1) }
    | LPAREN thintopt RPAREN            { $2 }

typ:
    | VAR                               { TBase($1) }
    | typ ARROW typ                     { TFun($1, $3) }
    | typ STAR typ                      { 
                                          match $1, $3 with 
                                          | TTuple l1, TTuple l2 -> TTuple(l1 @ l2)
                                          | TTuple l1, t2 -> TTuple(l1@[t2])
                                          | t1, TTuple(l2) -> TTuple(t1::l2)
                                          | t1, t2 -> TTuple([t1; t2])
                                        }
    | typ LIST                          { TList($1) }
    | LPAREN typ RPAREN                 { $2 }

arglist:
    | LPAREN RPAREN                     { [] }
    | LPAREN exprlist RPAREN            { $2 }

paramlist:
    | LPAREN RPAREN                     { [] }
    | LPAREN thintoptlist RPAREN        { $2 }

thintoptlist:
    | thintopt                          { [$1] }
    | VAR                               { [(None, $1)] }
    | thintoptlist COMMA thintopt       { $1@[$3] }
    | thintoptlist COMMA VAR            { $1@[(None, $3)] }

dict:
    | LBRACE RBRACE                     { Dict([]) }
    | LBRACE kvplist RBRACE             { Dict($2) }

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
    | LPAREN exprlist COMMA RPAREN      { Tuple($2) }