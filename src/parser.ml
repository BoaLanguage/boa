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
  | RETURN
  | STAR
  | STARSTAR
  | IF
  | ELSE
  | ELIF
  | AND
  | OR
  | NOT
  | SEMICOLON
  | NEWLINE

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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
# 60 "parser.ml"
let yytransl_const = [|
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* LBRACK *);
  264 (* RBRACK *);
  265 (* DOT *);
  266 (* COLON *);
  267 (* COMMA *);
  268 (* ARROW *);
  269 (* LAMBDA *);
  270 (* EQUALS *);
  271 (* EQUALSEQUALS *);
  272 (* DEF *);
  273 (* IS *);
  274 (* IN *);
  275 (* PLUS *);
  276 (* MINUS *);
    0 (* EOF *);
  277 (* LESS *);
  278 (* GREATER *);
  279 (* LEQ *);
  280 (* GEQ *);
  281 (* NEQ *);
  282 (* MOD *);
  283 (* INTDIV *);
  284 (* DIV *);
  285 (* RETURN *);
  286 (* STAR *);
  287 (* STARSTAR *);
  288 (* IF *);
  289 (* ELSE *);
  290 (* ELIF *);
  291 (* AND *);
  292 (* OR *);
  293 (* NOT *);
  294 (* SEMICOLON *);
  295 (* NEWLINE *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* TNAME *);
  259 (* INT *);
  260 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\005\000\006\000\006\000\007\000\
\007\000\004\000\004\000\008\000\008\000\008\000\008\000\008\000\
\010\000\009\000\009\000\009\000\009\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\002\000\008\000\003\000\002\000\002\000\
\004\000\002\000\002\000\003\000\003\000\001\000\001\000\002\000\
\002\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
\000\000\002\000\000\000\015\000\000\000\014\000\000\000\017\000\
\019\000\018\000\000\000\000\000\020\000\000\000\000\000\001\000\
\004\000\011\000\010\000\000\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\
\006\000\000\000\000\000\000\000\009\000\000\000\005\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000\012\000\049\000\070\000\013\000\
\020\000\014\000\021\000"

let yysindex = "\255\255\
\022\255\000\000\008\255\024\255\025\255\031\255\025\255\000\000\
\033\000\000\000\022\255\000\000\018\000\000\000\025\255\000\000\
\000\000\000\000\025\255\064\255\000\000\034\255\107\255\000\000\
\000\000\000\000\000\000\107\255\087\255\000\000\025\255\025\255\
\025\255\025\255\025\255\025\255\025\255\025\255\025\255\025\255\
\025\255\025\255\025\255\025\255\025\255\025\255\025\255\029\255\
\028\255\000\000\107\255\107\255\107\255\107\255\107\255\107\255\
\107\255\107\255\107\255\107\255\107\255\107\255\107\255\107\255\
\107\255\107\255\107\255\033\255\000\000\030\255\040\255\032\255\
\000\000\035\255\042\255\007\255\000\000\022\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\000\000\
\000\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\002\000\003\000\004\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\000\000\000\000\000\000\000\000\041\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\226\255\038\000\000\000\000\000\000\000\231\255\000\000\
\022\000\000\000\000\000"

let yytablesize = 316
let yytable = "\001\000\
\027\000\036\000\037\000\022\000\029\000\023\000\024\000\034\000\
\035\000\038\000\031\000\033\000\030\000\028\000\032\000\025\000\
\026\000\026\000\003\000\016\000\013\000\015\000\003\000\004\000\
\016\000\017\000\005\000\018\000\023\000\019\000\068\000\022\000\
\024\000\072\000\069\000\073\000\028\000\006\000\048\000\071\000\
\029\000\074\000\075\000\068\000\076\000\078\000\008\000\079\000\
\025\000\077\000\007\000\000\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\030\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\041\000\042\000\043\000\050\000\044\000\045\000\000\000\
\000\000\000\000\046\000\047\000\031\000\000\000\000\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\000\000\044\000\045\000\000\000\000\000\
\031\000\046\000\047\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\000\000\
\044\000\045\000\000\000\000\000\000\000\046\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\036\000\
\037\000\022\000\029\000\023\000\024\000\034\000\035\000\038\000\
\031\000\033\000\030\000\028\000\032\000\025\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\036\000\037\000\022\000\029\000\023\000\024\000\034\000\035\000\
\038\000\031\000\033\000\030\000\028\000\032\000\025\000\026\000\
\027\000\003\000\016\000\013\000"

let yycheck = "\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\001\001\001\002\001\
\001\001\001\001\005\001\003\001\007\000\005\001\002\001\001\001\
\000\000\001\001\006\001\006\001\015\000\016\001\005\001\012\001\
\019\000\002\001\011\001\002\001\010\001\039\001\006\001\078\000\
\011\000\075\000\029\001\255\255\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\014\001\255\255\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\006\001\030\001\031\001\255\255\
\255\255\255\255\035\001\036\001\014\001\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\255\255\030\001\031\001\255\255\255\255\
\014\001\035\001\036\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\255\255\
\030\001\031\001\255\255\255\255\255\255\035\001\036\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\006\001\006\001\
\006\001\006\001\006\001\006\001\006\001\006\001\006\001\006\001\
\006\001\006\001\006\001\006\001\006\001\006\001\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\039\001\
\039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
\039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
\039\001\039\001\039\001\039\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACK\000\
  RBRACK\000\
  DOT\000\
  COLON\000\
  COMMA\000\
  ARROW\000\
  LAMBDA\000\
  EQUALS\000\
  EQUALSEQUALS\000\
  DEF\000\
  IS\000\
  IN\000\
  PLUS\000\
  MINUS\000\
  EOF\000\
  LESS\000\
  GREATER\000\
  LEQ\000\
  GEQ\000\
  NEQ\000\
  MOD\000\
  INTDIV\000\
  DIV\000\
  RETURN\000\
  STAR\000\
  STARSTAR\000\
  IF\000\
  ELSE\000\
  ELIF\000\
  AND\000\
  OR\000\
  NOT\000\
  SEMICOLON\000\
  NEWLINE\000\
  "

let yynames_block = "\
  VAR\000\
  TNAME\000\
  INT\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    Obj.repr(
# 31 "parser.mly"
                                         ( _1 )
# 303 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'blockl) in
    Obj.repr(
# 34 "parser.mly"
                                        ( Block(_1) )
# 310 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 37 "parser.mly"
                                        ( [_1] )
# 317 "parser.ml"
               : 'blockl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'blockl) in
    Obj.repr(
# 38 "parser.mly"
                                        ( _1::_2 )
# 325 "parser.ml"
               : 'blockl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'declwrapper) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 43 "parser.mly"
                                        ( Def(TBase(_5), _2, _3, _8) )
# 335 "parser.ml"
               : 'fndef))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'declist) in
    Obj.repr(
# 46 "parser.mly"
                                        ( _2 )
# 342 "parser.ml"
               : 'declwrapper))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                                        ( [] )
# 348 "parser.ml"
               : 'declwrapper))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                                        ( [(TBase(_1), _2)] )
# 356 "parser.ml"
               : 'declist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'declist) in
    Obj.repr(
# 51 "parser.mly"
                                        ( (TBase(_1), _2)::_4 )
# 365 "parser.ml"
               : 'declist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'istmt) in
    Obj.repr(
# 54 "parser.mly"
                                      ( _1 )
# 372 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'istmt) in
    Obj.repr(
# 55 "parser.mly"
                                      ( _1 )
# 379 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 58 "parser.mly"
                                        ( Exp (_2) )
# 386 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 59 "parser.mly"
                                        ( Assign(_1, _3) )
# 394 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 60 "parser.mly"
                                        ( _1 )
# 401 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fndef) in
    Obj.repr(
# 61 "parser.mly"
                                        ( _1 )
# 408 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 62 "parser.mly"
                                        ( Return(_2) )
# 415 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                                        ( Decl (TBase(_1), _2) )
# 423 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
                                        ( Int(_1) )
# 430 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                                        ( Var(_1) )
# 437 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 69 "parser.mly"
                                        ( _1 )
# 444 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 70 "parser.mly"
                                        ( _2 )
# 451 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 73 "parser.mly"
                                        ( Binary(Plus, _1, _3) )
# 459 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 74 "parser.mly"
                                        ( Binary(Less, _1, _3) )
# 467 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 75 "parser.mly"
                                        ( Binary(Greater, _1, _3) )
# 475 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 76 "parser.mly"
                                        ( Binary(And, _1, _3) )
# 483 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 77 "parser.mly"
                                        ( Binary(Or, _1, _3) )
# 491 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 78 "parser.mly"
                                         ( Binary(Equal, _1, _3) )
# 499 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 79 "parser.mly"
                                       ( Binary(Times, _1, _3) )
# 507 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 80 "parser.mly"
                                        ( Binary(Minus, _1, _3) )
# 515 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 81 "parser.mly"
                                     ( Binary(Divide, _1, _3) )
# 523 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 82 "parser.mly"
                                        ( Binary(Mod, _1, _3) )
# 531 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 83 "parser.mly"
                                        ( Binary(Exponent, _1, _3) )
# 539 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 84 "parser.mly"
                                     ( Binary(IntDivide, _1, _3) )
# 547 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 85 "parser.mly"
                                        ( Binary(Leq, _1, _3) )
# 555 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 86 "parser.mly"
                                        ( Binary(Geq, _1, _3) )
# 563 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 87 "parser.mly"
                                        ( Binary(Is, _1, _3) )
# 571 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 88 "parser.mly"
                                        ( Binary(In, _1, _3) )
# 579 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 89 "parser.mly"
                                        ( Binary(Neq, _1, _3) )
# 587 "parser.ml"
               : 'bexp))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stmt)
