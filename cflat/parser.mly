  /* Ocamlyacc parser for C-FLAT */
  
  %{
  open Ast
  %}
  
  %token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN QUES
  %token NOT EQ NEQ LT LEQ GT GEQ AND OR  %token LBRACKET RBRACKET DOT DQUOTE SQUOTE MOD IN DO BREAK  %token NOTE MEASURE NONE CONTINUE DEF CHAR STRING 
  %token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID
  %token <int> LITERAL
  %token <bool> BLIT
  %token <string> ID FLIT %token <> TLIT                     (* note to self, figure out what <> for TLIT *)
  %token EOF
  
  %start program
  %type <Ast.program> program
  
  %nonassoc NOELSE
  %nonassoc ELSE
  %right ASSIGN
  %left OR
  %left AND
  %left EQ NEQ
  %left LT GT LEQ GEQ
  %left PLUS MINUS
  %left TIMES DIVIDE MOD
  %right NOT
  
  %%
  
  program:
      decls EOF { $1 }
  
  decls:     
      /* nothing */ { ([], [])               }
   | init_decls
    | decls vdecl { (($2 :: fst $1), snd $1) }
    | decls fdecl { (fst $1, ($2 :: snd $1)) }
   | decks adecl jfkdlandfjbjsknmalkfjnmlfakjnlidk???
 init_decl
   : simpl_typ ID ASSIGN expr SEMI 
   | array_typ ID ASSIGN expr SEMI   #check expr?!?!
  
  vdecl:
     simpl_typ ID SEMI { ($1, $2) }
  
  fdecl:
     DEF typ ID LPAREN formals_opt RPAREN LBRACE body_list RBRACE
       { { typ = $1;
           fname = $2;
           formals = List.rev $4;
           locals = List.rev $7;
           body = List.rev $8 } }
  
  formals_opt:
      /* nothing */ { [] }
    | formal_list   { $1 }
  
  formal_list:
      typ ID                   { [($1,$2)]     }
    | formal_list COMMA typ ID { ($3,$4) :: $1 }
  
    body_list:
        /* nothing */  { [] }
      | pass the head to the body label
        and pass the rest of it to the body_list again

    body:
        /* nothing */  { [] }
      pattern match that head, one line, as either a statement or decl or init

    adecl:
       simpl_typ LBRACKET RBRACKET ID SEMI { ($1, $2) }













    typ
      : simpl_typ {()}
      | array_typ {()}

  simpl_typ
    : INT      { Int     }
    | BOOL     { Bool    }
    | FLOAT    { Float   }
    | VOID     { Void    }
   | NONE     { None    }
   | NOTE     { Note    }
   | MEASURE  { Measure 
 array_typ:
   typ LBRACKET RBRACKET { Array }

 
 stmt_list
   : (* nothing *)  { [] }
   | stmt_list stmt { $2 :: $1 }
 
 stmt
   : expr SEMI                               { Expr $1               }
   | RETURN expr_opt SEMI                    { Return $2             }
   | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
   | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
   | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
   | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                             { For($3, $5, $7, $9)   }
   | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
 
 expr_opt
   : (* nothing *) { Noexpr }
   | expr          { $1 }
 
 primary_expr
   : LITERAL            { Literal($1)            }     #single integer
   | FLIT               { FLiteral($1)           }
   | BLIT               { BoolLit($1)            }   
   | TLIT 		       { TLiteral($1)	     }     ?   
   | STRLIT	       { String($1)	            }
   | note_expr          {                        }   
   | measure_expr       {                        }     
   | array_expr         {                        }   
   | func_expr          {                        }    
   | array_expr         {                        }   
   | func_expr          {                        } 
  | built_in_func_expr {                        }
    | ID                 {                        }

   
    note_expr
      : ID 
      | LPAREN TLIT LITERAL LITERAL RPAREN        {( )}
      | LPAREN TLIT LITERAL FLIT RPAREN	          {( )}

    measure_expr
      : ID
      | note_expr        				  { ( ) }
      | measure_expr COMMA note_expr 		  { ( ) }


     un_op
      : NOT        {()}
      | PLUS PLUS  {()}
      | MINUS MINUS {()}
      | 


    un_expr
      : primary_expr {()}
      | MINUS un_expr   {()}
      | NOT un_expr      {()}
      | un_expr PLUS PLUS {()}
      | un_expr MINUS MINUS   {()}



     mult_op
      : TIMES       {()}
      | DIVIDE       {()}
      | MOD        {()}
	
     mult_expr
      : un_expr   {()}
      | mult_expr mult_op un_expr   {()}



     add_op
      : PLUS  {()}
      | MINUS  {()}
	
     add_expr
      : mult_expr  {()}
      | add_expr add_op mult_expr  {()}


     rel_op
      : LT  {()}
      | GT  {()}
      | LEQ {()}
      | GEQ {()}
	
     rel_expr
      : add_expr {()}
      | rel_expr relation_op add_expr {()}

     eq_expr
      : rel_expr {()}
      | eq_expr eq_op rel_expr {()}


     eq_op
      : EQ {()}
      | NEQ {()}
	
     eq_expr
      : rel_expr {()}
      | eq_expr eq_op rel_expr {()}


     bool_op
      : AND {()}
      | OR {()}
	
     bool_expr
      : eq_expr {()}
      | bool_expr bool_op eq_expr 	{()}

    assign_expr
	: bool_expr		{()}
| ID ASSIGN assign_expr {()}   


    func_expr
      : ID LPAREN args_opt RPAREN { Call($1, $3)  }

     args_opt
      : /* nothing */ { [] }
      | args_list  { List.rev $1 }

    args_list
      : expr                    { [$1] }
      | args_list COMMA expr { $3 :: $1 }


