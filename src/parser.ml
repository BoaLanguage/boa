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
\001\000\002\000\003\000\003\000\005\000\006\000\006\000\008\000\
\008\000\007\000\007\000\004\000\004\000\009\000\009\000\009\000\
\009\000\009\000\011\000\012\000\012\000\010\000\010\000\010\000\
\010\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\002\000\008\000\003\000\002\000\003\000\
\003\000\001\000\003\000\002\000\002\000\003\000\001\000\001\000\
\001\000\002\000\003\000\001\000\003\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\043\000\000\000\
\002\000\000\000\017\000\000\000\000\000\015\000\016\000\000\000\
\000\000\000\000\022\000\000\000\000\000\000\000\024\000\000\000\
\023\000\000\000\000\000\001\000\004\000\000\000\013\000\012\000\
\008\000\000\000\000\000\009\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\000\000\006\000\000\000\000\000\011\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\011\000\056\000\079\000\012\000\
\013\000\035\000\014\000\015\000\023\000"

let yysindex = "\255\255\
\024\255\000\000\028\255\031\255\023\255\032\255\000\000\027\000\
\000\000\024\255\000\000\027\255\018\000\000\000\000\000\043\255\
\032\255\037\255\000\000\031\255\044\255\091\255\000\000\046\255\
\000\000\032\255\159\255\000\000\000\000\032\255\000\000\000\000\
\000\000\159\255\114\255\000\000\000\000\032\255\032\255\032\255\
\032\255\032\255\032\255\032\255\032\255\032\255\032\255\032\255\
\032\255\032\255\032\255\032\255\032\255\032\255\025\255\040\255\
\159\255\000\000\159\255\159\255\159\255\159\255\159\255\159\255\
\159\255\159\255\159\255\159\255\159\255\159\255\159\255\159\255\
\159\255\159\255\159\255\037\255\038\255\000\000\048\255\045\255\
\047\255\000\000\038\255\050\255\000\000\016\255\024\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\000\000\020\000\000\000\000\000\000\000\000\000\
\000\000\137\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\000\000\001\000\002\000\003\000\004\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\000\000\000\000\000\000\000\000\051\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\227\255\051\000\000\000\000\000\000\000\235\255\024\000\
\000\000\042\000\000\000\000\000\000\000"

let yytablesize = 318
let yytable = "\001\000\
\031\000\040\000\041\000\026\000\033\000\027\000\028\000\038\000\
\039\000\042\000\035\000\037\000\034\000\032\000\036\000\029\000\
\030\000\031\000\003\000\020\000\018\000\019\000\021\000\024\000\
\003\000\076\000\028\000\021\000\004\000\077\000\078\000\018\000\
\025\000\019\000\019\000\020\000\026\000\016\000\076\000\005\000\
\030\000\017\000\077\000\021\000\033\000\022\000\016\000\027\000\
\084\000\036\000\055\000\081\000\006\000\082\000\087\000\083\000\
\010\000\088\000\034\000\086\000\029\000\085\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\057\000\
\000\000\000\000\000\000\000\000\000\000\000\000\080\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\037\000\000\000\000\000\000\000\021\000\000\000\000\000\000\000\
\000\000\038\000\080\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\058\000\
\051\000\052\000\000\000\000\000\000\000\053\000\054\000\000\000\
\038\000\000\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\047\000\048\000\049\000\050\000\023\000\051\000\
\052\000\000\000\000\000\000\000\053\000\054\000\000\000\023\000\
\000\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\000\000\023\000\023\000\
\000\000\000\000\000\000\023\000\023\000\038\000\000\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\046\000\047\000\
\048\000\049\000\050\000\000\000\051\000\052\000\000\000\000\000\
\000\000\053\000\054\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\040\000\
\041\000\026\000\033\000\027\000\028\000\038\000\039\000\042\000\
\035\000\037\000\034\000\032\000\036\000\029\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\040\000\041\000\026\000\033\000\027\000\028\000\038\000\039\000\
\042\000\035\000\037\000\034\000\032\000\036\000\029\000\030\000\
\032\000\003\000\020\000\018\000\019\000\021\000"

let yycheck = "\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\001\
\001\001\001\001\000\000\004\000\005\001\005\001\006\001\001\001\
\001\001\003\001\003\001\005\001\005\001\010\001\001\001\016\001\
\014\001\014\001\005\001\020\000\002\001\004\000\010\001\006\000\
\002\001\006\001\005\001\012\001\029\001\006\001\039\001\011\001\
\006\001\087\000\017\000\010\001\010\000\083\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\030\000\
\255\255\255\255\255\255\255\255\255\255\255\255\055\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\006\001\255\255\255\255\255\255\077\000\255\255\255\255\255\255\
\255\255\015\001\083\000\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\006\001\
\030\001\031\001\255\255\255\255\255\255\035\001\036\001\255\255\
\015\001\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\006\001\030\001\
\031\001\255\255\255\255\255\255\035\001\036\001\255\255\015\001\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\030\001\031\001\
\255\255\255\255\255\255\035\001\036\001\015\001\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\255\255\030\001\031\001\255\255\255\255\
\255\255\035\001\036\001\255\255\255\255\255\255\255\255\255\255\
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
\039\001\039\001\039\001\039\001\039\001\039\001"

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
# 308 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'blockl) in
    Obj.repr(
# 34 "parser.mly"
                                        ( Block(_1) )
# 315 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 37 "parser.mly"
                                        ( [_1] )
# 322 "parser.ml"
               : 'blockl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'blockl) in
    Obj.repr(
# 38 "parser.mly"
                                        ( _1::_2 )
# 330 "parser.ml"
               : 'blockl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'declwrapper) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 43 "parser.mly"
                                        ( Def(TBase(_5), _2, _3, _8) )
# 340 "parser.ml"
               : 'fndef))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'declist) in
    Obj.repr(
# 46 "parser.mly"
                                        ( _2 )
# 347 "parser.ml"
               : 'declwrapper))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                                        ( [] )
# 353 "parser.ml"
               : 'declwrapper))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                                        ( (TBase(_3), _1) )
# 361 "parser.ml"
               : 'thint))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'thint) in
    Obj.repr(
# 51 "parser.mly"
                                        ( _2 )
# 368 "parser.ml"
               : 'thint))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'thint) in
    Obj.repr(
# 54 "parser.mly"
                                       ( [_1] )
# 375 "parser.ml"
               : 'declist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'thint) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'declist) in
    Obj.repr(
# 55 "parser.mly"
                                       ( _1::_3 )
# 383 "parser.ml"
               : 'declist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'istmt) in
    Obj.repr(
# 58 "parser.mly"
                                      ( _1 )
# 390 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'istmt) in
    Obj.repr(
# 59 "parser.mly"
                                      ( _1 )
# 397 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 62 "parser.mly"
                                        ( Exp (_2) )
# 404 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assn) in
    Obj.repr(
# 63 "parser.mly"
                                        ( _1 )
# 411 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 64 "parser.mly"
                                        ( _1 )
# 418 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fndef) in
    Obj.repr(
# 65 "parser.mly"
                                        ( _1 )
# 425 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 66 "parser.mly"
                                        ( Return(_2) )
# 432 "parser.ml"
               : 'istmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 69 "parser.mly"
                                        ( Assign(_1, _3) )
# 440 "parser.ml"
               : 'assn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'thint) in
    Obj.repr(
# 72 "parser.mly"
                                        ( Decl(fst _1, snd _1) )
# 447 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'thint) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 73 "parser.mly"
                                        ( Block([Decl(fst _1, snd _1); Assign(snd _1, _3)]) )
# 455 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 76 "parser.mly"
                                        ( Int(_1) )
# 462 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                                        ( Var(_1) )
# 469 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bexp) in
    Obj.repr(
# 78 "parser.mly"
                                        ( _1 )
# 476 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 79 "parser.mly"
                                        ( _2 )
# 483 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 82 "parser.mly"
                                        ( Binary(Plus, _1, _3) )
# 491 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 83 "parser.mly"
                                        ( Binary(Less, _1, _3) )
# 499 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 84 "parser.mly"
                                        ( Binary(Greater, _1, _3) )
# 507 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 85 "parser.mly"
                                        ( Binary(And, _1, _3) )
# 515 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 86 "parser.mly"
                                        ( Binary(Or, _1, _3) )
# 523 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 87 "parser.mly"
                                        ( Binary(Equal, _1, _3) )
# 531 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 88 "parser.mly"
                                        ( Binary(Times, _1, _3) )
# 539 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 89 "parser.mly"
                                        ( Binary(Minus, _1, _3) )
# 547 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 90 "parser.mly"
                                        ( Binary(Divide, _1, _3) )
# 555 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 91 "parser.mly"
                                        ( Binary(Mod, _1, _3) )
# 563 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 92 "parser.mly"
                                        ( Binary(Exponent, _1, _3) )
# 571 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 93 "parser.mly"
                                        ( Binary(IntDivide, _1, _3) )
# 579 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 94 "parser.mly"
                                        ( Binary(Leq, _1, _3) )
# 587 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 95 "parser.mly"
                                        ( Binary(Geq, _1, _3) )
# 595 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 96 "parser.mly"
                                        ( Binary(Is, _1, _3) )
# 603 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 97 "parser.mly"
                                        ( Binary(In, _1, _3) )
# 611 "parser.ml"
               : 'bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 98 "parser.mly"
                                        ( Binary(Neq, _1, _3) )
# 619 "parser.ml"
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
