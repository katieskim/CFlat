(* Ocamllex scanner for CFlat *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "(:"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| "<"      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "note"   { NOTE }
| "tone"   { TONE }
| "octave" { OCTAVE }
| "rhythm" { RHYTHM }
| "string" { STRING }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| '/' ((['A'-'G']['+''-''.']?) as lxm) '/'  { TLIT(lxm) }
| '/' ((['0'-'9'] | "-1") as lxm) '/'       { OLIT(int_of_string lxm) }
| '/' ((['s''e''q''h''w']['.']?) as lxm) '/'            { RLIT(lxm) }
| '"' (['a'-'z' 'A'-'Z' '0'-'9' '_' ' ']* as lxm) '"'   { STRLIT(lxm) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )?   as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*       as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  ":)" { token lexbuf }
| _    { comment lexbuf }


