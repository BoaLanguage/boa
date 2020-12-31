%{
  open Ast

  let rec add_block_to_if if_stmt elif = 
  match if_stmt with 
  | If (c, b, _else) -> If (c, b, add_block_to_if _else elif)
  | _ -> elif
%}

%token <string> VAR STRING
%token <int> INT NEWLINE EOLIF EOLSE
%token <bool> BOOL
%token LPAREN RPAREN LBRACK RBRACK DOT COLON COMMA FOR LBRACE RBRACE
%token ARROW LAMBDA EQUALS EQUALSEQUALS DEF IS IN PLUS MINUS
%token EOF LESS GREATER LEQ GEQ NEQ CLASS WHILE MEMBER LIST
%token MOD INTDIV DIV RETURN INDENT DEDENT LET VARKEYWORD
%token STAR STARSTAR IF ELSE ELIF AND OR NOT PRINT EOL

%right ARROW
%nonassoc STARSTAR IS IN EQUALSEQUALS LIST
%nonassoc LPAREN LBRACK
%left PLUS MINUS OR
%left STAR AND
%left DIV MOD INTDIV
%nonassoc NOT NEQ GEQ LEQ GREATER LESS
%right DOT

%type <Ast.stmt> prog

%start prog

%%
prog: 
    | block = stmtlist EOF                      { Block(block) }

iblock:
    | INDENT block = block DEDENT               { block }

block:
    | block = stmtlist                          { Block(block) }

stmtlist: 
    | statement = stmt_newline                  { [statement] }
    | block = stmtlist 
      statement = stmt_newline                  { block@[statement] }

stmt_newline:
    | statement = simple_stmt EOL               { statement }
    | statement = compound_stmt                 { statement }
    | EOL                                       { Pass }

simple_stmt:
   |  expression = expr                         { Exp(expression) }
   |  expression = expr 
      DOT name = VAR 
      EQUALS value = expr                       { AttrAssgn(expression, name, value) }
   |  LET hint = thint EQUALS value = expr      { Block([
                                                  Decl(Some (snd hint), fst hint);
                                                  Assign(fst hint, value)
                                                ]) }
    | VARKEYWORD hint = thint 
      EQUALS value = expr                       { Block([
                                                  MutableDecl(Some (snd hint), fst hint);
                                                  Assign(fst hint, value)
                                                ]) }                                      
    | name = VAR EQUALS value = expr            { Assign(name, value) }
    | expression = expr LBRACK 
      accessor = expr RBRACK 
      EQUALS value = expr                       { SliceAssgn(expression, accessor, value) }
    | RETURN value = expr                       { Return (value) }
    | PRINT value = expr                        { Print (value) }
    | MEMBER VARKEYWORD 
      name = VAR COLON 
      typ = typ                                 { MutableMemDecl(typ, name) }
    | MEMBER LET 
      name = VAR COLON 
      typ = typ                                 { MemDecl(typ, name) }
    | LET 
      name = VAR EQUALS 
      value = expr                              { Block([
                                                  Decl(None, name);
                                                  Assign(name, value)
                                                ]) }
    | VARKEYWORD 
      name = VAR EQUALS 
      value = expr                              { Block([
                                                  MutableDecl(None, name);
                                                  Assign(name, value)
                                                ]) }
    | LET hint = thint                          { Decl(Some(snd hint), fst hint) }
    | LET name = VAR                            { Decl(None, name) }
    | VARKEYWORD hint = thint                   { MutableDecl(Some(snd hint), fst hint) }
    | VARKEYWORD name = VAR                     { MutableDecl(None, name) }
    
compound_stmt:
    | ifstmt = ifstmt                           { ifstmt }
    | DEF 
      fn_name = VAR 
      params = paramlist ARROW 
      return_type = typ COLON EOL 
      body = iblock                             { Def(Some(return_type), fn_name, params, body) }
    | DEF 
      fn_name = VAR 
      params = paramlist COLON EOL 
      body = iblock                             { Def(None, fn_name, params, body) }
    | WHILE 
      guard = expr COLON EOL 
      body = iblock                             { While(guard, body) }
    | FOR 
      identifier = VAR IN 
      iterable = expr COLON EOL 
      body = iblock                             { For(identifier, iterable, body) }
    | CLASS 
      name = VAR COLON EOL 
      body = iblock                             { Class(name, Skip, body) }
    | CLASS 
      name = VAR 
      LPAREN super = expr RPAREN 
      COLON EOL 
      body = iblock                             { Class(name, super, body) }

ifstmt:
    | if_stmt = iff                             { if_stmt }
    | if_elif = ifelif                          { if_elif }
    | if_else = ifelse                          { if_else }

iff:
    | IF 
      guard = expr 
      COLON EOL
      body = iblock                             { If (guard, body, Exp(Skip)) }

ifelif:
    | if_stmt = iff ELIF 
      elif_guard = expr COLON EOL
      elif_body = iblock                        { add_block_to_if if_stmt (If(elif_guard, elif_body, Exp(Skip))) }
    | elif_chain = ifelif ELIF 
      elif_guard = expr COLON EOL
      elif_body = iblock                        { add_block_to_if elif_chain (If(elif_guard, elif_body, Exp(Skip))) }

ifelse:
    | if_stmt = iff ELSE COLON EOL 
      else_body = iblock                        { add_block_to_if if_stmt else_body }
    | elif_chain = ifelif ELSE COLON EOL 
      else_body = iblock                        { add_block_to_if elif_chain else_body }

expr:
    | LPAREN exp = expr RPAREN                  { exp }
    | name = VAR                                { Var(name) }
    | expression = expr 
      arguments = arglist                       { Call(expression, arguments) }
    | tuple = tuple                             { tuple }
    | literal = INT                             { Int(literal) }
    | expression = expr 
      LBRACK accessor = expr RBRACK             { SliceAccess(expression, accessor) }
    | expression = expr DOT 
      name = VAR                                { AttrAccess(expression, name) }
    | NOT expression = expr                     { Unary(Not, expression) }
    | MINUS expression = expr                   { Unary(Neg, expression) }
    | t_f = BOOL                                { Bool(t_f) }
    | LAMBDA 
      LPAREN arg_hint = thint RPAREN 
      ARROW body = expr                         { Lam(fst arg_hint, Some(snd arg_hint), body) }
    | LAMBDA 
      arg_name = VAR ARROW 
      body = expr                               { Lam(arg_name, None, body) }
    | lst = lst                                 { lst }
    | dict = dict                               { dict }
    | bexp = bexp                               { bexp }
    | str = STRING                              { String(str) }

bexp:
    | left = expr PLUS right = expr             { Binary(Plus, left, right) }
    | left = expr LESS right = expr             { Binary(Less, left, right) }
    | left = expr GREATER right = expr          { Binary(Greater, left, right) }
    | left = expr AND right = expr              { Binary(And, left, right) }
    | left = expr OR right = expr               { Binary(Or, left, right) }
    | left = expr EQUALSEQUALS right = expr     { Binary(Equal, left, right) }
    | left = expr STAR right = expr             { Binary(Times, left, right) }
    | left = expr MINUS right = expr            { Binary(Minus, left, right) }
    | left = expr DIV right = expr              { Binary(Divide, left, right) }
    | left = expr MOD right = expr              { Binary(Mod, left, right) }
    | left = expr STARSTAR right = expr         { Binary(Exponent, left, right) }
    | left = expr INTDIV right = expr           { Binary(IntDivide, left, right) }
    | left = expr LEQ right = expr              { Binary(Leq, left, right) }
    | left = expr GEQ right = expr              { Binary(Geq, left, right) }
    | left = expr IS right = expr               { Binary(Is, left, right) }
    | left = expr IN right = expr               { Binary(In, left, right) }
    | left = expr NEQ right = expr              { Binary(Neq, left, right) }

lst:
    | LBRACK RBRACK                             { List([]) }
    | LBRACK exprs = exprlist RBRACK            { List(exprs) }
    | LBRACK 
      expr = single_expr_list 
      RBRACK                                    { List(expr) }

thint:
    | name = VAR COLON typ = typ                { (name, typ) }
    | LPAREN hint = thint RPAREN                { hint }

thintopt:
    | name = VAR COLON typ = typ                { (Some(typ), name) }
    | LPAREN hint = thintopt RPAREN             { hint }

typ:
    | name = VAR                                { TBase(name) }
    | arg = typ ARROW return = typ              { TFun(arg, return) }
    | left = typ STAR right = typ               { 
                                                  match left, right with 
                                                  | TTuple l1, TTuple l2 -> TTuple(l1 @ l2)
                                                  | TTuple l1, t2 -> TTuple(l1@[t2])
                                                  | t1, TTuple(l2) -> TTuple(t1::l2)
                                                  | t1, t2 -> TTuple([t1; t2])
                                                }
    | typ = typ LIST                            { TList(typ) }
    | LPAREN typ = typ RPAREN                   { typ }

arglist:
    | LPAREN RPAREN                             { [] }
    | LPAREN lst = exprlist RPAREN              { lst }
    | LPAREN 
      lst = single_expr_list 
      RPAREN                                    { lst }

paramlist:
    | LPAREN RPAREN                             { [] }
    | LPAREN lst = thintoptlist RPAREN          { lst }

thintoptlist:
    | hint_opt = thintopt                       { [hint_opt] }
    | name = VAR                                { [(None, name)] }
    | lst = thintoptlist COMMA 
      hint_opt = thintopt                       { lst@[hint_opt] }
    | lst = thintoptlist COMMA 
      name = VAR                                { lst@[(None, name)] }

dict:
    | LBRACE RBRACE                             { Dict([]) }
    | LBRACE kvps = kvplist RBRACE              { Dict(kvps) }

kvplist:
    | kvp = kvp                                 { [kvp] }
    | lst = kvplist COMMA 
      kvp = kvp                                 { lst@[kvp] }

kvp:
    | key = expr COLON 
      value = expr                              { (key, value) }

exprlist:
    | lst = exprlist COMMA 
      expression = expr                         { lst@[expression] }
    | lst = single_expr_list COMMA 
      expression = expr                         { lst@[expression] }

single_expr_list:
    | expression = expr                         { [expression] }

tuple:
    | LPAREN lst = exprlist RPAREN              { Tuple(lst) }
    | LPAREN 
      lst = single_expr_list 
      COMMA RPAREN                              { Tuple(lst) }