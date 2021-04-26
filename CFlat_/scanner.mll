(* Ocamllex scanner for CFlat *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let esc    = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii  = ([' '-'!' '#'-'[' ']'-'~'])

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "(:"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACK }
| ']'      { RBRACK }                   (* arrays *)
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
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
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "make"  { MAKE }
| "return" { RETURN }
| "note"   { NOTE }
| "tone"   { TONE }
| "octave" { OCTAVE }
| "rhythm" { RHYTHM }
| ".tone()"   { TONEACCESS }
| ".octave()" { OCTAVEACCESS }
| ".rhythm()" { RHYTHMACCESS }
| ".tone"     { TONESET }
| ".octave"   { OCTAVESET }
| ".rhythm"   { RHYTHMSET }
| ".raiseTone"    { TONERAISE }
| "string" { STRING }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "string" { STRING }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| '/' ((['A'-'G']['+''-''.']?) as lxm) '/'  { TLIT(lxm) }
| '/' (digit | "-1" as lxm) '/'       { OLIT(int_of_string lxm) }
| '/' ((['s''e''q''h''w']['.']?) as lxm) '/'            { RLIT(lxm) }
| '"' ((ascii | esc)* as s)'"'                          { STRLIT(s) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )?   as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*       as lxm { ID(lxm) }
(* make sure it can't be R or A B C D E F G *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  ":)" { token lexbuf }
| _    { comment lexbuf }


