(* Ocamllex scanner for C-FLAT *)
 
{ open parser.mly }
 
let digit = ['0' - '9']
let digits = digit+
 
rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }        (* Whitespace *)
| "(:"       { comment lexbuf }               (* Comments *)
| '('        { LPAREN }
| ')'        { RPAREN }
| '{'        { LBRACE }
| '}'        { RBRACE } 
| '['        { LBRACKET } 
| ']'        { RBRACKET }
| ';'        { SEMI } 
| ':'        { COLON } 
| '.'        { DOT }
| ','        { COMMA } 
| '"'        { DQUOTE } 
| '''        { SQUOTE }
| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIVIDE } 
| '%'        { MOD }
| '='        { ASSIGN }
| "=="       { EQ }
| "!="       { NEQ }
| '<'        { LT }
| "<="       { LEQ }
| ">"        { GT }
| ">="       { GEQ }
| "&&"       { AND }
| "||"       { OR }
| "!"        { NOT } 
| "?"        { QUES }
| "if"       { IF }
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE } 
| "in"       { IN } 
| "do"       { DO }     
| "break"    { BREAK } 
| "continue" { CONTINUE } 
| "def"      { DEF }    
| "return"   { RETURN } 
| "print"    { PRINT } 
| "play"     { PLAY } 
| "bplay"    { BPLAY }
| "int"      { INT }
| "bool"     { BOOL }
| "float"    { FLOAT } 
| "char"     { CHAR } 
| "string"   { STRING }
| "note"     { NOTE }
| "measure"  { MEASURE }
| "none"     { NONE }
| "void"     { VOID }
| "true"     { BLIT(true)  }
| "false"    { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.' digit ( ['e' 'E'] ['+' '-']? digits )?	as lxm { FLIT(lxm) }
| ['A'-'Z''a'-'z''0'-'9']    as lxm { STRLIT(lxm) }
| ['A'-'Z''a'-'z''0'-'9']    as lxm { CHARLIT(lxm) }
| ['A'-'G' 'R'] ['+' '-' '.']? as lxm { TLIT(lxm) }
| ['A'-'G' 'R'] ['a'-'z' 'A'-'Z' '0'-'9' '_']+	as lxm { ID(lxm) }
| ['a'-'z' 'H'-'Q' 'S'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
							as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
  
and comment = parse
  ":)" { token lexbuf }
| _    { comment lexbuf }

