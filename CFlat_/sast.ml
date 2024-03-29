(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx

and sx =
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SNoteLit of sexpr * sexpr * sexpr
  | SToneLit of string
  | SOctaveLit of int
  | SRhythmLit of string
  | SToneAccess of string
  | SOctaveAccess of string
  | SRhythmAccess of string
  | SToneSet of string * sexpr
  | SOctaveSet of string * sexpr
  | SRhythmSet of string * sexpr
  | SStrLit of string
  | SId of string
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SFliteral(l) -> l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SNoteLit(t, o, r) -> string_of_sexpr t ^ ", " ^ string_of_sexpr o ^ ", " ^ string_of_sexpr r
  | SToneLit(l) -> l
  | SOctaveLit(l) -> string_of_int l
  | SRhythmLit(l) -> l
  | SToneAccess(n) -> n
  | SOctaveAccess(n) -> n
  | SRhythmAccess(n) -> n
  | SToneSet(n, e) -> n ^ ".settone(" ^ string_of_sexpr e ^ ")"
  | SOctaveSet(n, e) -> n ^ ".setoctave(" ^ string_of_sexpr e ^ ")"
  | SRhythmSet(n, e) -> n ^ ".setrhythm(" ^ string_of_sexpr e ^ ")"
  | SStrLit(l) -> l
  | SId(s) -> s
  | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2	
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SNoexpr -> ""
				  ) ^ ")"		

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
