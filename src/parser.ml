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
# 57 "parser.ml"
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
  285 (* STAR *);
  286 (* STARSTAR *);
  287 (* IF *);
  288 (* ELSE *);
  289 (* ELIF *);
  290 (* AND *);
  291 (* OR *);
  292 (* NOT *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* TNAME *);
  259 (* INT *);
  260 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\000\000"

let yylen = "\002\000\
\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\003\000\000\000\000\000\001\000\002\000"

let yydgoto = "\002\000\
\004\000\005\000"

let yysindex = "\255\255\
\252\254\000\000\255\254\000\000\003\000\254\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000"

let yytablesize = 4
let yytable = "\001\000\
\003\000\006\000\007\000\008\000"

let yycheck = "\001\000\
\005\001\003\001\000\000\006\001"

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
  STAR\000\
  STARSTAR\000\
  IF\000\
  ELSE\000\
  ELIF\000\
  AND\000\
  OR\000\
  NOT\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 31 "parser.mly"
                                        ( _1 )
# 179 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 33 "parser.mly"
                                        ( Exp (Int(_2)) )
# 186 "parser.ml"
               : 'stmt))
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
