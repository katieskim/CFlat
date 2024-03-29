(* Abstract Syntax Tree and functions for printing it *)

type primitive_typ = Int | Bool | Float | Void | Note | String | Tone | Octave | Rhythm 

type typ = PrimitiveType of primitive_typ | ArrayType of primitive_typ

type bind = typ * string

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | NoteLit of expr * expr * expr
  | ToneLit of string
  | OctaveLit of int
  | RhythmLit of string
  | ToneAccess of string
  | OctaveAccess of string
  | RhythmAccess of string
  | ToneSet of string * expr
  | OctaveSet of string * expr
  | RhythmSet of string * expr
  | StrLit of string
  | Id of string
  | Assign of string * expr
  | Call of string * expr list
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | NoteLit(t, o, r) -> string_of_expr t ^ ", " ^ string_of_expr o ^ ", " ^ string_of_expr r
  | ToneLit(l) -> l
  | OctaveLit(l) -> string_of_int l
  | RhythmLit(l) -> l
  | ToneAccess(n) -> n
  | OctaveAccess(n) -> n
  | RhythmAccess(n) -> n
  | ToneSet(n, e) -> n ^ ".settone(" ^ string_of_expr e ^ ")"
  | OctaveSet(n, e) -> n ^ ".setoctave(" ^ string_of_expr e ^ ")"
  | RhythmSet(n, e) -> n ^ ".setrhythm(" ^ string_of_expr e ^ ")"
  | StrLit(l) -> l
  | Id(s) -> s
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_primitive_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | Note -> "note"
  | Tone -> "tone"
  | Octave -> "octave"
  | Rhythm -> "rhythm"
  | String -> "string"

let rec string_of_typ = function
    PrimitiveType(t) -> string_of_primitive_typ t
  | ArrayType(t) -> (string_of_primitive_typ t) ^ "[]"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
