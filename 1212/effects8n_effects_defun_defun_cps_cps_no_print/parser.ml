type token =
  | LPAREN
  | RPAREN
  | FUN
  | RIGHT
  | HANDLER
  | LBRACE
  | RETURN
  | COMMA
  | SEMI
  | RBRACE
  | WITH
  | HANDLE
  | NUMBER of (int)
  | VAR of (string)
  | OP of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Syntax
(* 補助的な変数、関数、型などの定義 *)
# 25 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* FUN *);
  260 (* RIGHT *);
  261 (* HANDLER *);
  262 (* LBRACE *);
  263 (* RETURN *);
  264 (* COMMA *);
  265 (* SEMI *);
  266 (* RBRACE *);
  267 (* WITH *);
  268 (* HANDLE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  269 (* NUMBER *);
  270 (* VAR *);
  271 (* OP *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\003\000\
\004\000\004\000\002\000\002\000\005\000\005\000\005\000\005\000\
\006\000\007\000\007\000\008\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\002\000\002\000\004\000\001\000\004\000\
\001\000\001\000\003\000\001\000\003\000\001\000\001\000\000\000\
\004\000\001\000\002\000\008\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\009\000\
\000\000\021\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\005\000\012\000\004\000\011\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\000\000\003\000\000\000\000\000\
\008\000\000\000\019\000\006\000\000\000\000\000\013\000\017\000\
\000\000\000\000\000\000\000\000\020\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\025\000\026\000\027\000\028\000"

let yysindex = "\007\000\
\001\255\000\000\001\255\253\254\014\255\001\255\000\000\000\000\
\004\255\000\000\004\255\000\000\000\000\022\255\023\255\006\255\
\017\255\000\000\000\000\000\000\000\000\001\255\016\255\030\255\
\025\255\024\255\000\000\018\255\001\255\000\000\032\255\026\255\
\000\000\018\255\000\000\000\000\001\255\028\255\000\000\000\000\
\029\255\036\255\035\255\001\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\000\000\000\001\000\000\000\000\000\034\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\255\000\000\038\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\253\255\254\255\000\000\014\000\000\000\000\000\250\255\000\000"

let yytablesize = 281
let yytable = "\014\000\
\007\000\003\000\017\000\004\000\003\000\005\000\018\000\001\000\
\020\000\001\000\015\000\006\000\023\000\007\000\008\000\009\000\
\007\000\008\000\030\000\016\000\024\000\035\000\019\000\021\000\
\019\000\036\000\022\000\039\000\029\000\031\000\032\000\034\000\
\024\000\040\000\033\000\037\000\041\000\043\000\044\000\038\000\
\045\000\000\000\042\000\016\000\000\000\000\000\014\000\018\000\
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
\000\000\012\000\007\000\000\000\000\000\000\000\000\000\000\000\
\007\000\000\000\007\000\001\000\007\000\012\000\012\000\007\000\
\000\000\001\000\000\000\001\000\000\000\001\000\000\000\000\000\
\001\000"

let yycheck = "\003\000\
\000\000\001\001\006\000\003\001\001\001\005\001\009\000\001\000\
\011\000\000\000\014\001\011\001\007\001\013\001\014\001\015\001\
\013\001\014\001\022\000\006\001\015\001\028\000\009\000\002\001\
\011\000\029\000\004\001\034\000\012\001\014\001\001\001\008\001\
\015\001\037\000\010\001\004\001\009\001\002\001\004\001\014\001\
\044\000\255\255\014\001\010\001\255\255\255\255\010\001\010\001\
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
\255\255\001\001\002\001\255\255\255\255\255\255\255\255\255\255\
\008\001\255\255\010\001\002\001\012\001\013\001\014\001\015\001\
\255\255\008\001\255\255\010\001\255\255\012\001\255\255\255\255\
\015\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  FUN\000\
  RIGHT\000\
  HANDLER\000\
  LBRACE\000\
  RETURN\000\
  COMMA\000\
  SEMI\000\
  RBRACE\000\
  WITH\000\
  HANDLE\000\
  EOF\000\
  "

let yynames_block = "\
  NUMBER\000\
  VAR\000\
  OP\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 36 "parser.mly"
        ( _1 )
# 194 "parser.ml"
               : Syntax.e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 38 "parser.mly"
        ( Val (_1) )
# 201 "parser.ml"
               : Syntax.e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.e) in
    Obj.repr(
# 40 "parser.mly"
        ( Val (Fun (_2, _4)) )
# 209 "parser.ml"
               : Syntax.e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 42 "parser.mly"
        ( App (_1, _2) )
# 217 "parser.ml"
               : Syntax.e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 44 "parser.mly"
        ( Op (_1, _2) )
# 225 "parser.ml"
               : Syntax.e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Syntax.e) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.e) in
    Obj.repr(
# 46 "parser.mly"
        ( With (_2, _4) )
# 233 "parser.ml"
               : Syntax.e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_value) in
    Obj.repr(
# 50 "parser.mly"
        ( _1 )
# 240 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'handler) in
    Obj.repr(
# 52 "parser.mly"
        ( Handler (_3) )
# 247 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
        ( Var (_1) )
# 254 "parser.ml"
               : 'simple_value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 58 "parser.mly"
        ( Num (_1) )
# 261 "parser.ml"
               : 'simple_value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Syntax.e) in
    Obj.repr(
# 62 "parser.mly"
        ( _2 )
# 268 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_value) in
    Obj.repr(
# 64 "parser.mly"
        ( Val (_1) )
# 275 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ret) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ops) in
    Obj.repr(
# 68 "parser.mly"
        ( {return = Some _1; ops = _3} )
# 283 "parser.ml"
               : 'handler))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ret) in
    Obj.repr(
# 70 "parser.mly"
        ( {return = Some _1; ops = []} )
# 290 "parser.ml"
               : 'handler))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ops) in
    Obj.repr(
# 72 "parser.mly"
        ( {return = None; ops = _1} )
# 297 "parser.ml"
               : 'handler))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
        ( {return = None; ops = []} )
# 303 "parser.ml"
               : 'handler))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.e) in
    Obj.repr(
# 78 "parser.mly"
        ( (_2, _4) )
# 311 "parser.ml"
               : 'ret))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'op) in
    Obj.repr(
# 82 "parser.mly"
        ( [_1] )
# 318 "parser.ml"
               : 'ops))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'op) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ops) in
    Obj.repr(
# 84 "parser.mly"
        ( _1 :: _2 )
# 326 "parser.ml"
               : 'ops))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Syntax.e) in
    Obj.repr(
# 88 "parser.mly"
        ( (_1, _3, _5, _8) )
# 336 "parser.ml"
               : 'op))
(* Entry expr *)
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
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.e)
