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
  | TONEACCESS
  | OCTAVEACCESS
  | RHYTHMACCESS
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
# 54 "parser.ml"
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
  268 (* TONEACCESS *);
  269 (* OCTAVEACCESS *);
  270 (* RHYTHMACCESS *);
  271 (* NOT *);
  272 (* EQ *);
  273 (* NEQ *);
  274 (* LT *);
  275 (* LEQ *);
  276 (* GT *);
  277 (* GEQ *);
  278 (* AND *);
  279 (* OR *);
  280 (* RETURN *);
  281 (* IF *);
  282 (* ELSE *);
  283 (* FOR *);
  284 (* WHILE *);
  285 (* INT *);
  286 (* BOOL *);
  287 (* FLOAT *);
  288 (* VOID *);
  289 (* NOTE *);
  290 (* STRING *);
  291 (* TONE *);
  292 (* OCTAVE *);
  293 (* RHYTHM *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  294 (* LITERAL *);
  295 (* OLIT *);
  296 (* BLIT *);
  297 (* ID *);
  298 (* FLIT *);
  299 (* STRLIT *);
  300 (* TLIT *);
  301 (* RLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\012\000\013\000\014\000\011\000\007\000\007\000\
\003\000\008\000\008\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\017\000\017\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\018\000\018\000\019\000\019\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\005\000\000\000\002\000\
\003\000\000\000\002\000\002\000\003\000\003\000\005\000\007\000\
\009\000\005\000\000\000\001\000\001\000\001\000\003\000\004\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\071\000\000\000\010\000\011\000\012\000\013\000\
\014\000\018\000\015\000\016\000\017\000\001\000\003\000\004\000\
\000\000\000\000\033\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\031\000\000\000\000\000\009\000\032\000\000\000\
\000\000\000\000\000\000\034\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\028\000\021\000\000\000\020\000\
\022\000\027\000\029\000\045\000\023\000\024\000\025\000\026\000\
\035\000\000\000\000\000\000\000\000\000\065\000\066\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\050\000\
\051\000\036\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\052\000\
\038\000\037\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\055\000\056\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\048\000\000\000\030\000\000\000\000\000\042\000\000\000\
\000\000\000\000\040\000\000\000\000\000\041\000"

let yydgoto = "\002\000\
\003\000\004\000\015\000\016\000\017\000\022\000\029\000\033\000\
\023\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\065\000\095\000\096\000"

let yysindex = "\017\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\255\028\255\000\000\253\000\013\255\060\255\067\255\000\000\
\070\255\253\000\000\000\043\255\253\000\000\000\000\000\045\255\
\051\255\097\255\199\255\000\000\000\000\199\255\199\255\199\255\
\099\255\104\255\107\255\000\000\000\000\000\000\056\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\201\255\074\255\160\000\095\255\000\000\000\000\005\000\
\113\255\199\255\199\255\199\255\199\255\199\255\000\000\000\000\
\000\000\000\000\199\255\199\255\199\255\199\255\199\255\199\255\
\199\255\199\255\199\255\199\255\199\255\199\255\072\255\000\000\
\000\000\000\000\181\000\114\255\202\000\005\000\118\255\119\255\
\005\000\062\255\062\255\000\000\000\000\004\001\004\001\029\255\
\029\255\029\255\029\255\000\001\240\000\121\255\155\255\199\255\
\155\255\000\000\199\255\000\000\100\255\244\255\000\000\005\000\
\155\255\199\255\000\000\126\255\155\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\129\255\000\000\000\000\150\255\000\000\
\000\000\000\000\000\000\000\000\103\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\149\255\
\000\000\000\000\000\000\000\000\000\000\000\000\005\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\223\000\000\000\000\000\000\000\000\000\006\255\
\000\000\000\000\149\255\000\000\151\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\255\000\000\153\255\
\082\255\038\000\061\000\000\000\000\000\131\000\149\000\069\000\
\092\000\100\000\123\000\059\255\224\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\147\255\000\000\000\000\058\255\
\000\000\157\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\129\000\000\000\246\255\000\000\000\000\125\000\
\000\000\000\000\000\000\130\000\105\000\079\000\247\255\221\255\
\191\255\000\000\000\000"

let yytablesize = 546
let yytable = "\060\000\
\014\000\092\000\062\000\063\000\064\000\046\000\044\000\046\000\
\044\000\021\000\046\000\046\000\046\000\046\000\046\000\028\000\
\069\000\001\000\032\000\069\000\046\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\019\000\020\000\091\000\064\000\
\093\000\094\000\097\000\075\000\076\000\077\000\078\000\098\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\106\000\
\107\000\108\000\109\000\018\000\035\000\024\000\036\000\037\000\
\124\000\069\000\038\000\063\000\070\000\063\000\025\000\070\000\
\063\000\039\000\070\000\071\000\072\000\073\000\077\000\078\000\
\026\000\027\000\040\000\041\000\118\000\042\000\043\000\120\000\
\063\000\063\000\047\000\030\000\047\000\034\000\064\000\047\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\035\000\019\000\036\000\089\000\066\000\117\000\038\000\119\000\
\034\000\067\000\034\000\034\000\068\000\039\000\034\000\123\000\
\045\000\090\000\112\000\126\000\051\000\034\000\040\000\041\000\
\114\000\042\000\043\000\116\000\115\000\121\000\034\000\034\000\
\125\000\034\000\034\000\006\000\044\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\039\000\043\000\039\000\039\000\
\007\000\067\000\039\000\068\000\035\000\031\000\036\000\043\000\
\061\000\039\000\038\000\087\000\059\000\110\000\000\000\000\000\
\000\000\039\000\039\000\039\000\000\000\039\000\039\000\000\000\
\000\000\000\000\040\000\041\000\000\000\042\000\043\000\000\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\035\000\074\000\000\000\000\000\000\000\000\000\038\000\075\000\
\076\000\077\000\078\000\000\000\000\000\039\000\000\000\000\000\
\079\000\080\000\081\000\082\000\083\000\084\000\085\000\086\000\
\064\000\000\000\064\000\000\000\000\000\064\000\000\000\000\000\
\000\000\000\000\000\000\000\000\044\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\122\000\000\000\064\000\000\000\
\000\000\000\000\075\000\076\000\077\000\078\000\000\000\000\000\
\000\000\000\000\000\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\086\000\075\000\076\000\077\000\078\000\000\000\
\000\000\000\000\000\000\000\000\079\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\000\000\005\000\006\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\053\000\000\000\
\053\000\000\000\000\000\053\000\053\000\053\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\000\053\000\053\000\
\053\000\053\000\053\000\053\000\053\000\054\000\000\000\054\000\
\000\000\000\000\054\000\054\000\054\000\059\000\000\000\059\000\
\000\000\000\000\059\000\000\000\054\000\054\000\054\000\054\000\
\054\000\054\000\054\000\054\000\059\000\059\000\059\000\059\000\
\059\000\059\000\059\000\059\000\060\000\000\000\060\000\000\000\
\000\000\060\000\000\000\000\000\061\000\000\000\061\000\000\000\
\000\000\061\000\000\000\060\000\060\000\060\000\060\000\060\000\
\060\000\060\000\060\000\061\000\061\000\061\000\061\000\061\000\
\061\000\061\000\061\000\062\000\000\000\062\000\000\000\000\000\
\062\000\000\000\000\000\057\000\000\000\057\000\000\000\000\000\
\057\000\000\000\062\000\062\000\062\000\062\000\062\000\062\000\
\062\000\062\000\057\000\057\000\000\000\058\000\000\000\058\000\
\057\000\057\000\058\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\088\000\000\000\058\000\058\000\075\000\076\000\
\077\000\078\000\058\000\058\000\000\000\000\000\000\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\111\000\
\000\000\000\000\000\000\075\000\076\000\077\000\078\000\000\000\
\000\000\000\000\000\000\000\000\079\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\113\000\000\000\000\000\000\000\
\075\000\076\000\077\000\078\000\000\000\000\000\000\000\000\000\
\000\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\024\000\000\000\000\000\000\000\024\000\024\000\024\000\
\024\000\000\000\000\000\000\000\000\000\000\000\024\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\075\000\076\000\
\077\000\078\000\000\000\000\000\000\000\000\000\000\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\075\000\076\000\
\077\000\078\000\075\000\076\000\077\000\078\000\000\000\079\000\
\080\000\081\000\082\000\083\000\084\000\081\000\082\000\083\000\
\084\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\012\000\013\000"

let yycheck = "\035\000\
\000\000\067\000\038\000\039\000\040\000\001\001\001\001\003\001\
\003\001\020\000\006\001\007\001\008\001\009\001\010\001\026\000\
\003\001\001\000\029\000\006\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\001\001\002\001\066\000\067\000\
\068\000\069\000\070\000\007\001\008\001\009\001\010\001\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\083\000\
\084\000\085\000\086\000\041\001\002\001\041\001\004\001\005\001\
\122\000\002\001\008\001\001\001\003\001\003\001\003\001\006\001\
\006\001\015\001\011\001\012\001\013\001\014\001\009\001\010\001\
\006\001\004\001\024\001\025\001\112\000\027\001\028\001\115\000\
\022\001\023\001\001\001\041\001\003\001\041\001\122\000\006\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\002\001\001\001\004\001\005\001\002\001\111\000\008\001\113\000\
\002\001\002\001\004\001\005\001\002\001\015\001\008\001\121\000\
\039\001\001\001\001\001\125\000\045\001\015\001\024\001\025\001\
\003\001\027\001\028\001\003\001\006\001\026\001\024\001\025\001\
\003\001\027\001\028\001\003\001\038\001\039\001\040\001\041\001\
\042\001\043\001\044\001\045\001\038\001\039\001\040\001\041\001\
\042\001\043\001\044\001\045\001\002\001\001\001\004\001\005\001\
\003\001\003\001\008\001\003\001\002\001\029\000\004\001\003\001\
\036\000\015\001\008\001\059\000\035\000\087\000\255\255\255\255\
\255\255\015\001\024\001\025\001\255\255\027\001\028\001\255\255\
\255\255\255\255\024\001\025\001\255\255\027\001\028\001\255\255\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\002\001\001\001\255\255\255\255\255\255\255\255\008\001\007\001\
\008\001\009\001\010\001\255\255\255\255\015\001\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\038\001\039\001\040\001\041\001\
\042\001\043\001\044\001\045\001\001\001\255\255\023\001\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\255\255\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\255\255\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\001\001\255\255\
\003\001\255\255\255\255\006\001\007\001\008\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\001\001\255\255\003\001\
\255\255\255\255\006\001\007\001\008\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\016\001\017\001\255\255\001\001\255\255\003\001\
\022\001\023\001\006\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\003\001\255\255\016\001\017\001\007\001\008\001\
\009\001\010\001\022\001\023\001\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\003\001\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\255\255\255\255\255\255\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\007\001\008\001\
\009\001\010\001\255\255\255\255\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\007\001\008\001\
\009\001\010\001\007\001\008\001\009\001\010\001\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\018\001\019\001\020\001\
\021\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001"

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
  TONEACCESS\000\
  OCTAVEACCESS\000\
  RHYTHMACCESS\000\
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
# 35 "parser.mly"
            ( _1 )
# 395 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                 ( ([], [])               )
# 401 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 39 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 409 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 40 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 417 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 44 "parser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 432 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                  ( [] )
# 438 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 52 "parser.mly"
                  ( _1 )
# 445 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                             ( [(_1,_2)]     )
# 453 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                             ( (_3,_4) :: _1 )
# 462 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
            ( Int   )
# 468 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
            ( Bool  )
# 474 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
            ( Float )
# 480 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
            ( Void  )
# 486 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
            ( Note  )
# 492 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
            ( Tone  )
# 498 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
            ( Octave )
# 504 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
            ( Rhythm )
# 510 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
            ( String )
# 516 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser.mly"
                     ( Literal(_1)            )
# 523 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                    ( Fliteral(_1)           )
# 530 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 72 "parser.mly"
                     ( BoolLit(_1)            )
# 537 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
                     ( StrLit(_1)             )
# 544 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'notelit) in
    Obj.repr(
# 74 "parser.mly"
                     ( _1                     )
# 551 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tlit) in
    Obj.repr(
# 75 "parser.mly"
                     ( _1                     )
# 558 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'olit) in
    Obj.repr(
# 76 "parser.mly"
                     ( _1                     )
# 565 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rlit) in
    Obj.repr(
# 77 "parser.mly"
                     ( _1                     )
# 572 "parser.ml"
               : 'literal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
                     ( ToneLit(_1)            )
# 579 "parser.ml"
               : 'tlit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
                     ( OctaveLit(_1)          )
# 586 "parser.ml"
               : 'olit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                     ( RhythmLit(_1)          )
# 593 "parser.ml"
               : 'rlit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'tlit) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'olit) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'rlit) in
    Obj.repr(
# 89 "parser.mly"
                                    ( NoteLit(_2, _3, _4))
# 602 "parser.ml"
               : 'notelit))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                     ( [] )
# 608 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 94 "parser.mly"
                     ( _2 :: _1 )
# 616 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 97 "parser.mly"
               ( (_1, _2) )
# 624 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                   ( [] )
# 630 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "parser.mly"
                   ( _2 :: _1 )
# 638 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                                            ( Expr _1               )
# 645 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 105 "parser.mly"
                                            ( Return _2             )
# 652 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 106 "parser.mly"
                                            ( Block(List.rev _2)    )
# 659 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 667 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 108 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 676 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 110 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 686 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 111 "parser.mly"
                                            ( While(_3, _5)         )
# 694 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                  ( Noexpr )
# 700 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                  ( _1 )
# 707 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literal) in
    Obj.repr(
# 118 "parser.mly"
                     ( _1 )
# 714 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "parser.mly"
                     ( Id(_1) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     ( Assign(_1, _3)         )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 121 "parser.mly"
                              ( Call(_1, _3)  )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 122 "parser.mly"
                     ( ToneAccess(_1)         )
# 744 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 123 "parser.mly"
                     ( OctaveAccess(_1)       )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 124 "parser.mly"
                     ( RhythmAccess(_1)       )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                       ( _2                   )
# 765 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 773 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 781 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 789 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 797 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 805 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 813 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 821 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 829 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 837 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 845 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 853 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 861 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                         ( Unop(Neg, _2)      )
# 868 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                     ( Unop(Not, _2)          )
# 875 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
                  ( [] )
# 881 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 143 "parser.mly"
               ( List.rev _1 )
# 888 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                            ( [_1] )
# 895 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                         ( _3 :: _1 )
# 903 "parser.ml"
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
