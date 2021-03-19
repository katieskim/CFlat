type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | VOID
  | NOTE
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | NOTELIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "microcparse.mly"
open Ast
# 44 "microcparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* NOT *);
  269 (* EQ *);
  270 (* NEQ *);
  271 (* LT *);
  272 (* LEQ *);
  273 (* GT *);
  274 (* GEQ *);
  275 (* AND *);
  276 (* OR *);
  277 (* RETURN *);
  278 (* IF *);
  279 (* ELSE *);
  280 (* FOR *);
  281 (* WHILE *);
  282 (* INT *);
  283 (* BOOL *);
  284 (* FLOAT *);
  285 (* VOID *);
  286 (* NOTE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  287 (* LITERAL *);
  288 (* BLIT *);
  289 (* ID *);
  290 (* FLIT *);
  291 (* NOTELIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\007\000\007\000\
\003\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\012\000\012\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\000\000\002\000\
\003\000\000\000\002\000\002\000\003\000\003\000\005\000\007\000\
\009\000\005\000\000\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\004\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\055\000\000\000\010\000\011\000\012\000\013\000\
\014\000\001\000\003\000\004\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\015\000\000\000\
\000\000\009\000\016\000\000\000\000\000\000\000\000\000\018\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\031\000\000\000\030\000\032\000\019\000\000\000\000\000\000\000\
\046\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\000\022\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\049\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\024\000\
\000\000\000\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\018\000\025\000\029\000\
\019\000\045\000\046\000\052\000\078\000\079\000"

let yysindex = "\052\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\255\017\255\000\000\119\255\
\042\255\077\255\088\255\000\000\093\255\119\255\000\000\069\255\
\119\255\000\000\000\000\072\255\052\255\105\255\009\255\000\000\
\000\000\009\255\009\255\009\255\107\255\108\255\109\255\000\000\
\000\000\004\255\000\000\000\000\000\000\185\255\117\000\057\255\
\000\000\000\000\167\000\106\255\009\255\009\255\009\255\009\255\
\009\255\000\000\009\255\009\255\009\255\009\255\009\255\009\255\
\009\255\009\255\009\255\009\255\009\255\009\255\000\000\000\000\
\000\000\135\000\113\255\153\000\167\000\116\255\127\255\167\000\
\039\255\039\255\000\000\000\000\206\000\206\000\180\255\180\255\
\180\255\180\255\194\000\181\000\130\255\009\255\130\255\000\000\
\009\255\112\255\205\255\000\000\167\000\130\255\009\255\000\000\
\133\255\130\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\134\255\
\000\000\000\000\137\255\000\000\000\000\000\000\000\000\000\000\
\091\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\142\255\000\000\000\000\000\000\000\000\
\000\000\165\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\255\000\000\000\000\142\255\000\000\147\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\064\255\000\000\150\255\044\255\
\226\255\246\255\000\000\000\000\099\000\103\000\019\000\039\000\
\059\000\079\000\138\255\007\255\000\000\000\000\000\000\000\000\
\000\000\096\255\000\000\000\000\065\255\000\000\153\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\134\000\000\000\030\000\000\000\000\000\128\000\
\000\000\170\255\225\255\204\255\000\000\000\000"

let yytablesize = 480
let yytable = "\047\000\
\010\000\075\000\049\000\050\000\051\000\056\000\098\000\045\000\
\100\000\045\000\031\000\028\000\045\000\028\000\057\000\104\000\
\034\000\015\000\016\000\107\000\035\000\074\000\051\000\076\000\
\077\000\080\000\045\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\091\000\092\000\040\000\
\041\000\042\000\043\000\044\000\048\000\017\000\048\000\061\000\
\062\000\048\000\105\000\024\000\001\000\031\000\028\000\032\000\
\033\000\014\000\031\000\034\000\032\000\072\000\099\000\035\000\
\034\000\101\000\053\000\054\000\035\000\053\000\054\000\051\000\
\036\000\037\000\020\000\038\000\039\000\036\000\037\000\021\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\040\000\
\041\000\042\000\043\000\044\000\018\000\022\000\018\000\018\000\
\023\000\023\000\018\000\023\000\023\000\026\000\018\000\023\000\
\030\000\015\000\073\000\023\000\053\000\054\000\055\000\018\000\
\018\000\094\000\018\000\018\000\023\000\023\000\096\000\023\000\
\023\000\018\000\018\000\018\000\018\000\018\000\023\000\023\000\
\023\000\023\000\023\000\031\000\097\000\032\000\102\000\106\000\
\006\000\034\000\044\000\007\000\044\000\035\000\027\000\044\000\
\005\000\006\000\007\000\008\000\009\000\051\000\036\000\037\000\
\052\000\038\000\039\000\027\000\044\000\044\000\027\000\048\000\
\040\000\041\000\042\000\043\000\044\000\033\000\000\000\033\000\
\000\000\000\000\033\000\033\000\033\000\033\000\033\000\000\000\
\000\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\058\000\059\000\060\000\061\000\062\000\000\000\059\000\
\060\000\061\000\062\000\000\000\000\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\103\000\000\000\000\000\
\000\000\000\000\000\000\059\000\060\000\061\000\062\000\000\000\
\000\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\000\000\034\000\000\000\034\000\000\000\000\000\034\000\
\034\000\034\000\000\000\000\000\000\000\000\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\035\000\000\000\
\035\000\000\000\000\000\035\000\035\000\035\000\000\000\000\000\
\000\000\000\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\000\000\040\000\000\000\000\000\
\040\000\000\000\005\000\006\000\007\000\008\000\009\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\041\000\
\000\000\041\000\000\000\000\000\041\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\042\000\000\000\042\000\000\000\000\000\
\042\000\000\000\000\000\000\000\000\000\000\000\000\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\043\000\
\000\000\043\000\000\000\000\000\043\000\000\000\000\000\000\000\
\000\000\000\000\000\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\038\000\000\000\038\000\000\000\039\000\
\038\000\039\000\000\000\000\000\039\000\000\000\000\000\038\000\
\038\000\000\000\000\000\039\000\039\000\038\000\038\000\071\000\
\000\000\039\000\039\000\059\000\060\000\061\000\062\000\000\000\
\000\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\093\000\000\000\000\000\000\000\059\000\060\000\061\000\
\062\000\000\000\000\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\095\000\000\000\000\000\000\000\059\000\
\060\000\061\000\062\000\000\000\000\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\059\000\060\000\061\000\
\062\000\000\000\000\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\059\000\060\000\061\000\062\000\000\000\
\000\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\059\000\060\000\061\000\062\000\000\000\000\000\063\000\064\000\
\065\000\066\000\067\000\068\000\059\000\060\000\061\000\062\000\
\000\000\000\000\000\000\000\000\065\000\066\000\067\000\068\000"

let yycheck = "\031\000\
\000\000\054\000\034\000\035\000\036\000\002\001\093\000\001\001\
\095\000\003\001\002\001\001\001\006\001\003\001\011\001\102\000\
\008\001\001\001\002\001\106\000\012\001\053\000\054\000\055\000\
\056\000\057\000\020\001\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\031\001\
\032\001\033\001\034\001\035\001\001\001\016\000\003\001\009\001\
\010\001\006\001\103\000\022\000\001\000\002\001\025\000\004\001\
\005\001\033\001\002\001\008\001\004\001\005\001\094\000\012\001\
\008\001\097\000\003\001\003\001\012\001\006\001\006\001\103\000\
\021\001\022\001\033\001\024\001\025\001\021\001\022\001\003\001\
\024\001\025\001\031\001\032\001\033\001\034\001\035\001\031\001\
\032\001\033\001\034\001\035\001\002\001\006\001\004\001\005\001\
\004\001\002\001\008\001\004\001\005\001\033\001\012\001\008\001\
\033\001\001\001\001\001\012\001\002\001\002\001\002\001\021\001\
\022\001\001\001\024\001\025\001\021\001\022\001\003\001\024\001\
\025\001\031\001\032\001\033\001\034\001\035\001\031\001\032\001\
\033\001\034\001\035\001\002\001\006\001\004\001\023\001\003\001\
\003\001\008\001\001\001\003\001\003\001\012\001\001\001\006\001\
\026\001\027\001\028\001\029\001\030\001\003\001\021\001\022\001\
\003\001\024\001\025\001\003\001\019\001\020\001\025\000\032\000\
\031\001\032\001\033\001\034\001\035\001\001\001\255\255\003\001\
\255\255\255\255\006\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\001\001\007\001\008\001\009\001\010\001\255\255\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\255\255\255\255\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\255\255\
\003\001\255\255\255\255\006\001\007\001\008\001\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\026\001\027\001\028\001\029\001\030\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\001\001\
\255\255\003\001\255\255\255\255\006\001\255\255\255\255\255\255\
\255\255\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\001\001\
\255\255\003\001\255\255\255\255\006\001\255\255\255\255\255\255\
\255\255\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\001\001\255\255\003\001\255\255\001\001\
\006\001\003\001\255\255\255\255\006\001\255\255\255\255\013\001\
\014\001\255\255\255\255\013\001\014\001\019\001\020\001\003\001\
\255\255\019\001\020\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\003\001\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  VOID\000\
  NOTE\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  NOTELIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "microcparse.mly"
            ( _1 )
# 335 "microcparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "microcparse.mly"
                 ( ([], [])               )
# 341 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "microcparse.mly"
               ( ((_2 :: fst _1), snd _1m) )
# 349 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "microcparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 357 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "microcparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 372 "microcparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "microcparse.mly"
                  ( [] )
# 378 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 49 "microcparse.mly"
                  ( _1 )
# 385 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "microcparse.mly"
                             ( [(_1,_2)]     )
# 393 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "microcparse.mly"
                             ( (_3,_4) :: _1 )
# 402 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "microcparse.mly"
          ( Int   )
# 408 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "microcparse.mly"
          ( Bool  )
# 414 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "microcparse.mly"
          ( Float )
# 420 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "microcparse.mly"
          ( Void  )
# 426 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "microcparse.mly"
          ( Note  )
# 432 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "microcparse.mly"
                     ( [] )
# 438 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 64 "microcparse.mly"
                     ( _2 :: _1 )
# 446 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "microcparse.mly"
               ( (_1, _2) )
# 454 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "microcparse.mly"
                   ( [] )
# 460 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 71 "microcparse.mly"
                   ( _2 :: _1 )
# 468 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "microcparse.mly"
                                            ( Expr _1               )
# 475 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 75 "microcparse.mly"
                                            ( Return _2             )
# 482 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 76 "microcparse.mly"
                                            ( Block(List.rev _2)    )
# 489 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "microcparse.mly"
                                            ( If(_3, _5, Block([])) )
# 497 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "microcparse.mly"
                                            ( If(_3, _5, _7)        )
# 506 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "microcparse.mly"
                                            ( For(_3, _5, _7, _9)   )
# 516 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "microcparse.mly"
                                            ( While(_3, _5)         )
# 524 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "microcparse.mly"
                  ( Noexpr )
# 530 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "microcparse.mly"
                  ( _1 )
# 537 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "microcparse.mly"
                     ( Literal(_1)            )
# 544 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "microcparse.mly"
                    ( Fliteral(_1)           )
# 551 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 90 "microcparse.mly"
                     ( BoolLit(_1)            )
# 558 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "microcparse.mly"
                     ( NoteLit(_1)           )
# 565 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "microcparse.mly"
                     ( Id(_1)                 )
# 572 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "microcparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 580 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "microcparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 588 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "microcparse.mly"
                     ( Binop(_1, Mult,  _3)   )
# 596 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "microcparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 604 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "microcparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 612 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "microcparse.mly"
                     ( Binop(_1, Neq,   _3)   )
# 620 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "microcparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 628 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "microcparse.mly"
                     ( Binop(_1, Leq,   _3)   )
# 636 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "microcparse.mly"
                     ( Binop(_1, Greater, _3) )
# 644 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "microcparse.mly"
                     ( Binop(_1, Geq,   _3)   )
# 652 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "microcparse.mly"
                     ( Binop(_1, And,   _3)   )
# 660 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "microcparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 668 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "microcparse.mly"
                         ( Unop(Neg, _2)      )
# 675 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "microcparse.mly"
                     ( Unop(Not, _2)          )
# 682 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "microcparse.mly"
                     ( Assign(_1, _3)         )
# 690 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 108 "microcparse.mly"
                              ( Call(_1, _3)  )
# 698 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 109 "microcparse.mly"
                       ( _2                   )
# 705 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "microcparse.mly"
                  ( [] )
# 711 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 113 "microcparse.mly"
               ( List.rev _1 )
# 718 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "microcparse.mly"
                            ( [_1] )
# 725 "microcparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "microcparse.mly"
                         ( _3 :: _1 )
# 733 "microcparse.ml"
               : 'args_list))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
