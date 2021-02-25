/* Ocamlyacc parser for C-FLAT */
  
%{
open Ast
%}
  
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN QUES
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token LBRACKET RBRACKET DOT DQUOTE SQUOTE MOD IN DO BREAK
%token NOTE MEASURE NONE CONTINUE DEF CHAR STRING 
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID 
%token <float> FLIT
%token <string> TLIT         
%token <string> STRLIT            
%token EOF

%start program
%type <Ast.program> program
  
%left SEMI
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


program																				
	: master_decl EOF		    { $1 }
	| program master_decl		{ () }


master_decl
	: init_decls      {()}
	| vdecl		{ () }              /*(($2 :: fst $1), snd $1) */
	| adecl		{ () }
	| fdecl		{ () }                /*(fst $1, ($2 :: snd $1))*/

init_decls
	: simpl_typ	ID ASSIGN primary_expr SEMI	{ () }
	| array_typ	ID ASSIGN array_expr SEMI	{ () }
	| NOTE	ID ASSIGN note_expr SEMI	{ () }
	| MEASURE	ID ASSIGN measure_expr SEMI	{ () }

vdecl
	: simpl_typ	ID SEMI	{ ($1, $2) }
	| NOTE	ID SEMI { () }
	| MEASURE	ID SEMI	{ () }

adecl
	: array_typ ID SEMI	{ () }


fdecl
	: DEF master_typ ID LPAREN formal_opt RPAREN LBRACE stmt_list RBRACE	{ () }
  
formal_opt
	: /* nothing*/ 	{ [] }
	| formal_list   { $1 }
  
formal_list
	: typ ID			{ [($1, $2)]    }
	| formal_list COMMA typ ID	{ ($3,$4) :: $1 }
  
master_typ
	: typ		     { () }
	| array_typ	{ () }

typ
	: simpl_typ	{ () }
	| NOTE	{ () }
	| MEASURE	{ () }
	| NONE		{ () }
	| VOID		{ () }

simpl_typ
	: INT		{ () }
	| FLOAT		{ () }
	| CHAR		{ () }
	| STRING	{ () }
	| BOOL		{ () }

array_typ
	: simpl_typ	LBRACKET RBRACKET	{ () }
	| NOTE	LBRACKET RBRACKET	{ () }
	| MEASURE	LBRACKET RBRACKET	{ () }

master_array
	: array_primary	{ () }
	/*| array_note	{ () }*/ 
	| array_measure { () }

array_primary
	: primary_expr				{ [] }
	| array_primary COMMA primary_expr	{ [] }

/*array_note
	: note_expr				{ [] }
	| array_note COMMA note_expr		{ [] }*/

array_measure
	: measure_expr				{ [] }
	| array_measure COMMA measure_expr	{ [] }

array_expr
	: LBRACKET master_array RBRACKET	{ () }

stmt_list
	: /* nothing*/   { [] }
	| stmt_list stmt { $2 :: $1 }
 
stmt
	: primary_expr SEMI							{ Expr $1               }
	| RETURN expr_opt SEMI						{ Return $2             }
	| LBRACE stmt_list RBRACE					{ Block(List.rev $2)    }
	| IF LPAREN primary_expr RPAREN stmt %prec NOELSE			{ If($3, $5, Block([])) }
	| IF LPAREN primary_expr RPAREN stmt ELSE stmt				{ If($3, $5, $7)        }
	| FOR LPAREN expr_opt SEMI primary_expr SEMI expr_opt RPAREN stmt	{ For($3, $5, $7, $9)   }
	| WHILE LPAREN primary_expr RPAREN stmt					{ While($3, $5)         }
 

expr_opt
	: /* nothing */ { Noexpr }
	| primary_expr         { $1 }
 
primary_expr
	: LITERAL            { Literal($1)            }
	| FLIT               { FLiteral($1)           }
	| BLIT               { BoolLit($1)            }   
	| TLIT 		     { TLiteral($1)	      }   
	| STRLIT	     { String($1)	      }
/*
	| note_expr          { ()                     }   
	| measure_expr       { ()                     }     */   

	| ID                 { ()                     }


   
note_expr
	:LPAREN TLIT LITERAL LITERAL RPAREN	  { () }
	| LPAREN TLIT LITERAL FLIT RPAREN	      { () }

measure_expr

	: note_expr				{ () }
	| array_expr COMMA note_expr		{ () }

un_op
	: NOT		{ () }
	| PLUS PLUS	{ () }
	| MINUS MINUS	{ () }

un_expr

	: primary_expr		{ () }
	| MINUS un_expr		{ [] }
	| NOT un_expr		{ [] }
	| un_expr PLUS PLUS	{ [] }
	| un_expr MINUS MINUS	{ [] }

mult_op
	: TIMES		{ () }
	| DIVIDE	{ () }
	| MOD		{ () }
	
mult_expr
	: un_expr			{ () }
	| mult_expr mult_op un_expr	{ [] }

add_op
	: PLUS	{ () }
	| MINUS	{ () }
	
add_expr
	: mult_expr			{ [] }
	| add_expr add_op mult_expr	{ [] }

rel_op
	: LT  { () }
	| GT  { () }
	| LEQ { () }
	| GEQ { () }
	
rel_expr
	: add_expr			{ [] }
	| rel_expr rel_op add_expr { [] }

eq_expr
	: rel_expr			{ [] }
	| eq_expr eq_op rel_expr	{ [] }

eq_op
	: EQ	{ () }
	| NEQ	{ () }
	
eq_expr
	: rel_expr			{ [] }
	| eq_expr eq_op rel_expr	{ [] }

bool_op
	: AND	{ () }
	| OR	{ () }
	
bool_expr
	: eq_expr			{ [] }
	| bool_expr bool_op eq_expr	{ [] }

assign_expr
	: bool_expr		{ [] }
	| ID ASSIGN assign_expr	{ [] }

func_expr
	: ID LPAREN args_opt RPAREN	{ Call($1, $3) }

built_in_func_expr
	: DOT ID LPAREN args_opt RPAREN   { Call($2, $4) }

args_opt
	: /* nothing */	{ [] }
	| args_list	{ List.rev $1 }

args_list
	: expr_opt			{ [$1] }
	| args_list COMMA expr_opt	{ $3 :: $1 }

