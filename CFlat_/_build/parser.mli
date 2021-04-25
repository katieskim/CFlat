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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
