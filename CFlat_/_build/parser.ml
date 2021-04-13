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
  | TONE
  | OCTAVE
  | RHYTHM
  | LITERAL of (int)
  | OLIT of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | STRLIT of (string)
  | TLIT of (string)
  | RLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 51 "parser.ml"
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
  288 (* TONE *);
  289 (* OCTAVE *);
  290 (* RHYTHM *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  291 (* LITERAL *);
  292 (* OLIT *);
  293 (* BLIT *);
  294 (* ID *);
  295 (* FLIT *);
  296 (* STRLIT *);
  297 (* TLIT *);
  298 (* RLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\012\000\013\000\014\000\011\000\007\000\007\000\
\003\000\008\000\008\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\017\000\017\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\018\000\
\018\000\019\000\019\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\005\000\000\000\002\000\
\003\000\000\000\002\000\002\000\003\000\003\000\005\000\007\000\
\009\000\005\000\000\000\001\000\001\000\001\000\003\000\004\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\068\000\000\000\010\000\011\000\012\000\013\000\
\014\000\018\000\015\000\016\000\017\000\001\000\003\000\004\000\
\000\000\000\000\033\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\031\000\000\000\000\000\009\000\032\000\000\000\
\000\000\000\000\000\000\034\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\028\000\021\000\000\000\020\000\
\022\000\027\000\029\000\045\000\023\000\024\000\025\000\026\000\
\035\000\000\000\000\000\000\000\000\000\062\000\063\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\000\038\000\037\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\053\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\048\000\000\000\
\030\000\000\000\000\000\042\000\000\000\000\000\000\000\040\000\
\000\000\000\000\041\000"

let yydgoto = "\002\000\
\003\000\004\000\015\000\016\000\017\000\022\000\029\000\033\000\
\023\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\065\000\092\000\093\000"

let yysindex = "\008\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\234\254\097\255\000\000\239\000\254\254\047\255\062\255\000\000\
\053\255\239\000\000\000\033\255\239\000\000\000\000\000\043\255\
\051\255\094\255\223\255\000\000\000\000\223\255\223\255\223\255\
\099\255\103\255\107\255\000\000\000\000\000\000\015\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\071\255\153\000\092\255\000\000\000\000\003\000\
\109\255\223\255\223\255\223\255\223\255\223\255\000\000\223\255\
\223\255\223\255\223\255\223\255\223\255\223\255\223\255\223\255\
\223\255\223\255\223\255\070\255\000\000\000\000\000\000\171\000\
\114\255\189\000\003\000\123\255\130\255\003\000\093\255\093\255\
\000\000\000\000\246\000\246\000\020\255\020\255\020\255\020\255\
\234\000\221\000\136\255\215\255\223\255\215\255\000\000\223\255\
\000\000\117\255\055\000\000\000\003\000\215\255\223\255\000\000\
\139\255\215\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\140\255\000\000\000\000\141\255\000\000\
\000\000\000\000\000\000\000\000\133\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\145\255\
\000\000\000\000\000\000\000\000\000\000\000\000\005\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\207\000\000\000\000\000\000\000\000\000\048\255\
\000\000\000\000\145\255\000\000\147\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\255\000\000\148\255\079\255\075\000\095\000\
\000\000\000\000\135\000\139\000\105\255\146\255\187\255\115\000\
\059\255\063\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\174\255\000\000\000\000\064\255\000\000\150\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\119\000\000\000\032\000\000\000\000\000\120\000\
\000\000\000\000\000\000\132\000\118\000\096\000\110\000\221\255\
\191\255\000\000\000\000"

let yytablesize = 529
let yytable = "\060\000\
\014\000\089\000\062\000\063\000\064\000\046\000\066\000\046\000\
\001\000\066\000\046\000\046\000\046\000\046\000\046\000\018\000\
\069\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\070\000\072\000\073\000\074\000\075\000\088\000\064\000\
\090\000\091\000\094\000\024\000\095\000\096\000\097\000\098\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\044\000\025\000\044\000\021\000\035\000\121\000\036\000\037\000\
\027\000\028\000\038\000\060\000\032\000\060\000\039\000\061\000\
\060\000\061\000\067\000\026\000\061\000\067\000\030\000\040\000\
\041\000\115\000\042\000\043\000\117\000\060\000\060\000\047\000\
\034\000\047\000\061\000\064\000\047\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\035\000\019\000\036\000\
\086\000\019\000\020\000\038\000\066\000\074\000\075\000\039\000\
\067\000\056\000\045\000\056\000\068\000\087\000\056\000\051\000\
\040\000\041\000\109\000\042\000\043\000\056\000\056\000\056\000\
\056\000\056\000\056\000\056\000\056\000\111\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\034\000\112\000\
\034\000\034\000\113\000\118\000\034\000\122\000\006\000\007\000\
\034\000\043\000\057\000\031\000\057\000\064\000\065\000\057\000\
\043\000\034\000\034\000\061\000\034\000\034\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\057\000\059\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\039\000\
\084\000\039\000\039\000\107\000\000\000\039\000\000\000\000\000\
\000\000\039\000\000\000\058\000\000\000\058\000\000\000\000\000\
\058\000\000\000\039\000\039\000\000\000\039\000\039\000\058\000\
\058\000\058\000\058\000\058\000\058\000\058\000\058\000\000\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\035\000\114\000\036\000\116\000\000\000\000\000\038\000\000\000\
\035\000\000\000\039\000\120\000\000\000\000\000\038\000\123\000\
\000\000\000\000\039\000\040\000\041\000\000\000\042\000\043\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\072\000\073\000\074\000\075\000\000\000\000\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\000\000\
\000\000\000\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\012\000\013\000\071\000\000\000\000\000\000\000\000\000\
\000\000\072\000\073\000\074\000\075\000\000\000\000\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\119\000\
\000\000\000\000\000\000\000\000\000\000\072\000\073\000\074\000\
\075\000\000\000\000\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\050\000\000\000\050\000\000\000\000\000\
\050\000\050\000\050\000\000\000\000\000\000\000\000\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\051\000\
\000\000\051\000\000\000\000\000\051\000\051\000\051\000\000\000\
\000\000\000\000\000\000\051\000\051\000\051\000\051\000\051\000\
\051\000\051\000\051\000\059\000\000\000\059\000\000\000\000\000\
\059\000\000\000\000\000\000\000\000\000\000\000\000\000\059\000\
\059\000\059\000\059\000\059\000\059\000\059\000\059\000\054\000\
\000\000\054\000\000\000\055\000\054\000\055\000\000\000\000\000\
\055\000\000\000\000\000\054\000\054\000\000\000\000\000\055\000\
\055\000\054\000\054\000\085\000\000\000\055\000\055\000\072\000\
\073\000\074\000\075\000\000\000\000\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\108\000\000\000\000\000\
\000\000\072\000\073\000\074\000\075\000\000\000\000\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\110\000\
\000\000\000\000\000\000\072\000\073\000\074\000\075\000\000\000\
\000\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\083\000\024\000\000\000\000\000\000\000\024\000\024\000\024\000\
\024\000\000\000\000\000\024\000\024\000\024\000\024\000\024\000\
\024\000\024\000\024\000\072\000\073\000\074\000\075\000\000\000\
\000\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\072\000\073\000\074\000\075\000\000\000\000\000\076\000\077\000\
\078\000\079\000\080\000\081\000\072\000\073\000\074\000\075\000\
\000\000\000\000\000\000\000\000\078\000\079\000\080\000\081\000\
\005\000\006\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000"

let yycheck = "\035\000\
\000\000\067\000\038\000\039\000\040\000\001\001\003\001\003\001\
\001\000\006\001\006\001\007\001\008\001\009\001\010\001\038\001\
\002\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\011\001\007\001\008\001\009\001\010\001\066\000\067\000\
\068\000\069\000\070\000\038\001\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\001\001\003\001\003\001\020\000\002\001\119\000\004\001\005\001\
\004\001\026\000\008\001\001\001\029\000\003\001\012\001\001\001\
\006\001\003\001\003\001\006\001\006\001\006\001\038\001\021\001\
\022\001\109\000\024\001\025\001\112\000\019\001\020\001\001\001\
\038\001\003\001\020\001\119\000\006\001\035\001\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\002\001\001\001\004\001\
\005\001\001\001\002\001\008\001\002\001\009\001\010\001\012\001\
\002\001\001\001\036\001\003\001\002\001\001\001\006\001\042\001\
\021\001\022\001\001\001\024\001\025\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\003\001\035\001\036\001\
\037\001\038\001\039\001\040\001\041\001\042\001\002\001\006\001\
\004\001\005\001\003\001\023\001\008\001\003\001\003\001\003\001\
\012\001\001\001\001\001\029\000\003\001\003\001\003\001\006\001\
\003\001\021\001\022\001\036\000\024\001\025\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\035\000\035\001\
\036\001\037\001\038\001\039\001\040\001\041\001\042\001\002\001\
\059\000\004\001\005\001\084\000\255\255\008\001\255\255\255\255\
\255\255\012\001\255\255\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\021\001\022\001\255\255\024\001\025\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\042\001\
\002\001\108\000\004\001\110\000\255\255\255\255\008\001\255\255\
\002\001\255\255\012\001\118\000\255\255\255\255\008\001\122\000\
\255\255\255\255\012\001\021\001\022\001\255\255\024\001\025\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\035\001\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\035\001\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\255\255\
\255\255\255\255\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\001\001\255\255\255\255\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\001\001\
\255\255\255\255\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\001\001\255\255\003\001\255\255\255\255\
\006\001\007\001\008\001\255\255\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\001\001\
\255\255\003\001\255\255\255\255\006\001\007\001\008\001\255\255\
\255\255\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\001\001\
\255\255\003\001\255\255\001\001\006\001\003\001\255\255\255\255\
\006\001\255\255\255\255\013\001\014\001\255\255\255\255\013\001\
\014\001\019\001\020\001\003\001\255\255\019\001\020\001\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\003\001\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\003\001\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001"

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
  TONE\000\
  OCTAVE\000\
  RHYTHM\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  OLIT\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  STRLIT\000\
  TLIT\000\
  RLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 33 "parser.mly"
            ( _1 )
# 382 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                 ( ([], [])               )
# 388 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 37 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 396 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 38 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 404 "parser.ml"
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
# 419 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                  ( [] )
# 425 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 50 "parser.mly"
                  ( _1 )
# 432 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                             ( [(_1,_2)]     )
# 440 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                             ( (_3,_4) :: _1 )
# 449 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
            ( Int   )
# 455 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
            ( Bool  )
# 461 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
            ( Float )
# 467 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
            ( Void  )
# 473 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
            ( Note  )
# 479 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
            ( Tone  )
# 485 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
            ( Octave )
# 491 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
            ( Rhythm )
# 497 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
            ( String )
# 503 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 68 "parser.mly"
                     ( Literal(_1)            )
# 510 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                    ( Fliteral(_1)           )
# 517 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 70 "parser.mly"
                     ( BoolLit(_1)            )
# 524 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                     ( StrLit(_1)             )
# 531 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'notelit) in
    Obj.repr(
# 72 "parser.mly"
                     ( _1                     )
# 538 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tlit) in
    Obj.repr(
# 73 "parser.mly"
                     ( _1                     )
# 545 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'olit) in
    Obj.repr(
# 74 "parser.mly"
                     ( _1                     )
# 552 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rlit) in
    Obj.repr(
# 75 "parser.mly"
                     ( _1                     )
# 559 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "parser.mly"
                     ( ToneLit(_1)            )
# 566 "parser.ml"
               : 'tlit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 81 "parser.mly"
                     ( OctaveLit(_1)          )
# 573 "parser.ml"
               : 'olit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                     ( RhythmLit(_1)          )
# 580 "parser.ml"
               : 'rlit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'tlit) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'olit) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'rlit) in
    Obj.repr(
# 87 "parser.mly"
                                    ( NoteLit(_2, _3, _4))
# 589 "parser.ml"
               : 'notelit))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                     ( [] )
# 595 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 92 "parser.mly"
                     ( _2 :: _1 )
# 603 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 95 "parser.mly"
               ( (_1, _2) )
# 611 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                   ( [] )
# 617 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "parser.mly"
                   ( _2 :: _1 )
# 625 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                                            ( Expr _1               )
# 632 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 103 "parser.mly"
                                            ( Return _2             )
# 639 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 104 "parser.mly"
                                            ( Block(List.rev _2)    )
# 646 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 654 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 663 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 108 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 673 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 109 "parser.mly"
                                            ( While(_3, _5)         )
# 681 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                  ( Noexpr )
# 687 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                  ( _1 )
# 694 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 116 "parser.mly"
                     ( _1 )
# 701 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                     ( Id(_1)                 )
# 708 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     ( Assign(_1, _3)         )
# 716 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 119 "parser.mly"
                              ( Call(_1, _3)  )
# 724 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                       ( _2                   )
# 731 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 739 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 747 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 755 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 763 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 771 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 779 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 787 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 795 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 803 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 811 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 819 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 827 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                         ( Unop(Neg, _2)      )
# 834 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                     ( Unop(Not, _2)          )
# 841 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
                  ( [] )
# 847 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 138 "parser.mly"
               ( List.rev _1 )
# 854 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                            ( [_1] )
# 861 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                         ( _3 :: _1 )
# 869 "parser.ml"
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
