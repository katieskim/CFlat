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
  | STRING
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | NOTELIT of (string)
  | STRLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 46 "parser.ml"
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
  287 (* STRING *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  288 (* LITERAL *);
  289 (* BLIT *);
  290 (* ID *);
  291 (* FLIT *);
  292 (* NOTELIT *);
  293 (* STRLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\007\000\
\007\000\003\000\008\000\008\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\012\000\012\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\013\000\013\000\014\000\014\000\
\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\001\000\000\000\
\002\000\003\000\000\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\000\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\004\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\000\000\001\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\002\000\000\000\057\000\000\000\010\000\011\000\012\000\013\000\
\014\000\015\000\001\000\003\000\004\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\008\000\000\000\000\000\016\000\
\000\000\000\000\009\000\017\000\000\000\000\000\000\000\000\000\
\019\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\032\000\000\000\031\000\033\000\034\000\020\000\000\000\
\000\000\000\000\051\000\052\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\000\023\000\022\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\041\000\042\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\000\000\000\000\000\000\027\000\000\000\000\000\
\000\000\025\000\000\000\000\000\026\000"

let yydgoto = "\002\000\
\003\000\004\000\012\000\013\000\014\000\019\000\026\000\030\000\
\020\000\047\000\048\000\054\000\080\000\081\000"

let yysindex = "\009\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\240\254\069\255\000\000\
\139\255\244\254\042\255\052\255\000\000\059\255\139\255\000\000\
\038\255\139\255\000\000\000\000\049\255\044\255\085\255\098\255\
\000\000\000\000\098\255\098\255\098\255\091\255\093\255\101\255\
\000\000\000\000\010\255\000\000\000\000\000\000\000\000\245\255\
\180\000\080\255\000\000\000\000\003\000\086\255\098\255\098\255\
\098\255\098\255\098\255\000\000\098\255\098\255\098\255\098\255\
\098\255\098\255\098\255\098\255\098\255\098\255\098\255\098\255\
\000\000\000\000\000\000\198\000\108\255\216\000\003\000\087\255\
\105\255\003\000\065\255\065\255\000\000\000\000\255\000\255\000\
\089\255\089\255\089\255\089\255\243\000\230\000\188\255\098\255\
\188\255\000\000\098\255\096\255\032\000\000\000\003\000\188\255\
\098\255\000\000\119\255\188\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\120\255\000\000\000\000\122\255\000\000\000\000\000\000\000\000\
\000\000\116\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\125\255\000\000\000\000\000\000\
\000\000\000\000\225\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\040\255\000\000\000\000\125\255\
\000\000\124\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\255\000\000\
\126\255\056\255\052\000\072\000\000\000\000\000\041\255\172\000\
\092\000\112\000\132\000\152\000\088\255\008\255\000\000\000\000\
\000\000\000\000\000\000\152\255\000\000\000\000\047\255\000\000\
\133\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\113\000\000\000\246\255\000\000\000\000\109\000\
\000\000\167\255\224\255\202\255\000\000\000\000"

let yytablesize = 529
let yytable = "\049\000\
\011\000\077\000\051\000\052\000\053\000\100\000\018\000\102\000\
\050\000\001\000\050\000\058\000\025\000\050\000\106\000\029\000\
\055\000\015\000\109\000\055\000\059\000\021\000\076\000\053\000\
\078\000\079\000\082\000\050\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\091\000\092\000\093\000\094\000\
\029\000\043\000\029\000\043\000\022\000\032\000\043\000\033\000\
\034\000\056\000\107\000\035\000\056\000\043\000\043\000\036\000\
\036\000\023\000\036\000\043\000\043\000\036\000\024\000\101\000\
\037\000\038\000\103\000\039\000\040\000\016\000\017\000\027\000\
\053\000\063\000\064\000\041\000\042\000\043\000\044\000\045\000\
\046\000\032\000\031\000\033\000\074\000\016\000\075\000\035\000\
\049\000\098\000\049\000\036\000\055\000\049\000\056\000\061\000\
\062\000\063\000\064\000\032\000\037\000\038\000\057\000\039\000\
\040\000\035\000\049\000\049\000\096\000\036\000\099\000\041\000\
\042\000\043\000\044\000\045\000\046\000\019\000\104\000\019\000\
\019\000\108\000\006\000\019\000\007\000\028\000\053\000\019\000\
\054\000\041\000\042\000\043\000\044\000\045\000\046\000\028\000\
\019\000\019\000\028\000\019\000\019\000\050\000\000\000\000\000\
\000\000\000\000\000\000\019\000\019\000\019\000\019\000\019\000\
\019\000\024\000\000\000\024\000\024\000\000\000\000\000\024\000\
\000\000\000\000\000\000\024\000\005\000\006\000\007\000\008\000\
\009\000\010\000\000\000\000\000\024\000\024\000\000\000\024\000\
\024\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\024\000\024\000\024\000\024\000\024\000\032\000\000\000\033\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\036\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\037\000\038\000\000\000\039\000\040\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\042\000\043\000\044\000\045\000\
\046\000\035\000\000\000\035\000\000\000\000\000\035\000\035\000\
\035\000\035\000\035\000\000\000\000\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\060\000\000\000\000\000\
\000\000\000\000\000\000\061\000\062\000\063\000\064\000\000\000\
\000\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\000\000\
\000\000\000\000\005\000\006\000\007\000\008\000\009\000\010\000\
\105\000\000\000\000\000\000\000\000\000\000\000\061\000\062\000\
\063\000\064\000\000\000\000\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\072\000\039\000\000\000\039\000\000\000\
\000\000\039\000\039\000\039\000\000\000\000\000\000\000\000\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\040\000\000\000\040\000\000\000\000\000\040\000\040\000\040\000\
\000\000\000\000\000\000\000\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\045\000\000\000\045\000\000\000\
\000\000\045\000\000\000\000\000\000\000\000\000\000\000\000\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\046\000\000\000\046\000\000\000\000\000\046\000\000\000\000\000\
\000\000\000\000\000\000\000\000\046\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\047\000\000\000\047\000\000\000\
\000\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
\048\000\000\000\048\000\000\000\000\000\048\000\000\000\000\000\
\000\000\000\000\000\000\000\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\044\000\000\000\044\000\000\000\
\000\000\044\000\000\000\000\000\000\000\000\000\073\000\000\000\
\044\000\044\000\061\000\062\000\063\000\064\000\044\000\044\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\095\000\000\000\000\000\000\000\061\000\062\000\063\000\064\000\
\000\000\000\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\097\000\000\000\000\000\000\000\061\000\062\000\
\063\000\064\000\000\000\000\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\072\000\061\000\062\000\063\000\064\000\
\000\000\000\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\061\000\062\000\063\000\
\064\000\000\000\000\000\000\000\000\000\067\000\068\000\069\000\
\070\000"

let yycheck = "\032\000\
\000\000\056\000\035\000\036\000\037\000\095\000\017\000\097\000\
\001\001\001\000\003\001\002\001\023\000\006\001\104\000\026\000\
\003\001\034\001\108\000\006\001\011\001\034\001\055\000\056\000\
\057\000\058\000\059\000\020\001\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\001\001\001\001\003\001\003\001\003\001\002\001\006\001\004\001\
\005\001\003\001\105\000\008\001\006\001\013\001\014\001\012\001\
\001\001\006\001\003\001\019\001\020\001\006\001\004\001\096\000\
\021\001\022\001\099\000\024\001\025\001\001\001\002\001\034\001\
\105\000\009\001\010\001\032\001\033\001\034\001\035\001\036\001\
\037\001\002\001\034\001\004\001\005\001\001\001\001\001\008\001\
\001\001\003\001\003\001\012\001\002\001\006\001\002\001\007\001\
\008\001\009\001\010\001\002\001\021\001\022\001\002\001\024\001\
\025\001\008\001\019\001\020\001\001\001\012\001\006\001\032\001\
\033\001\034\001\035\001\036\001\037\001\002\001\023\001\004\001\
\005\001\003\001\003\001\008\001\003\001\001\001\003\001\012\001\
\003\001\032\001\033\001\034\001\035\001\036\001\037\001\003\001\
\021\001\022\001\026\000\024\001\025\001\033\000\255\255\255\255\
\255\255\255\255\255\255\032\001\033\001\034\001\035\001\036\001\
\037\001\002\001\255\255\004\001\005\001\255\255\255\255\008\001\
\255\255\255\255\255\255\012\001\026\001\027\001\028\001\029\001\
\030\001\031\001\255\255\255\255\021\001\022\001\255\255\024\001\
\025\001\255\255\255\255\255\255\255\255\255\255\255\255\032\001\
\033\001\034\001\035\001\036\001\037\001\002\001\255\255\004\001\
\255\255\255\255\255\255\008\001\255\255\255\255\255\255\012\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\021\001\022\001\255\255\024\001\025\001\255\255\255\255\255\255\
\255\255\255\255\255\255\032\001\033\001\034\001\035\001\036\001\
\037\001\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\001\001\255\255\255\255\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\255\255\026\001\027\001\028\001\029\001\030\001\031\001\
\001\001\255\255\255\255\255\255\255\255\255\255\007\001\008\001\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\001\001\255\255\003\001\255\255\255\255\006\001\007\001\008\001\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\003\001\255\255\
\013\001\014\001\007\001\008\001\009\001\010\001\019\001\020\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\003\001\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\003\001\255\255\255\255\255\255\007\001\008\001\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\007\001\008\001\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\007\001\008\001\009\001\
\010\001\255\255\255\255\255\255\255\255\015\001\016\001\017\001\
\018\001"

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
  STRING\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  NOTELIT\000\
  STRLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 33 "parser.mly"
            ( _1 )
# 357 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                 ( ([], [])               )
# 363 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 37 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 371 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 38 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 379 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 42 "parser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 394 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                  ( [] )
# 400 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 50 "parser.mly"
                  ( _1 )
# 407 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                             ( [(_1,_2)]     )
# 415 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                             ( (_3,_4) :: _1 )
# 424 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
            ( Int   )
# 430 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
            ( Bool  )
# 436 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
            ( Float )
# 442 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
            ( Void  )
# 448 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
            ( Note )
# 454 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
            ( String )
# 460 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                     ( [] )
# 466 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 66 "parser.mly"
                     ( _2 :: _1 )
# 474 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 69 "parser.mly"
               ( (_1, _2) )
# 482 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                   ( [] )
# 488 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 73 "parser.mly"
                   ( _2 :: _1 )
# 496 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                                            ( Expr _1               )
# 503 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 77 "parser.mly"
                                            ( Return _2             )
# 510 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 78 "parser.mly"
                                            ( Block(List.rev _2)    )
# 517 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 79 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 525 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 534 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 544 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
                                            ( While(_3, _5)         )
# 552 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                  ( Noexpr )
# 558 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                  ( _1 )
# 565 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
                     ( Literal(_1)            )
# 572 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
                    ( Fliteral(_1)           )
# 579 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 92 "parser.mly"
                     ( BoolLit(_1)            )
# 586 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
                     ( NoteLit(_1)            )
# 593 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
                     ( StrLit(_1) )
# 600 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parser.mly"
                     ( Id(_1)                 )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                     ( Assign(_1, _3)         )
# 615 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 97 "parser.mly"
                              ( Call(_1, _3)  )
# 623 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                       ( _2                   )
# 630 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 638 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 646 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 654 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 686 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 694 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 702 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 710 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 726 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                         ( Unop(Neg, _2)      )
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( Unop(Not, _2)          )
# 740 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                  ( [] )
# 746 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 116 "parser.mly"
               ( List.rev _1 )
# 753 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                            ( [_1] )
# 760 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                         ( _3 :: _1 )
# 768 "parser.ml"
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
