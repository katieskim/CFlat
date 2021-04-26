(* Semantic checking for the Cflat compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(PrimitiveType(Void), b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 

    let add_bind map (name, tys) = StringMap.add name {
      typ = PrimitiveType(Void);
      fname = name; 
      formals = tys;
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty
                             [ ("print", [(PrimitiveType(Int), "x")]);
			                         ("printb", [(PrimitiveType(Bool), "x")]);
			                         ("printf", [(PrimitiveType(Float), "x")]);
			                         ("printbig", [(PrimitiveType(Int), "x")]);
                               ("prints", [(PrimitiveType(String), "x")]);
						                   ("printn", [(PrimitiveType(Note), "x")]);
                               ("printt", [(PrimitiveType(Tone), "x")]);
                               ("printr", [(PrimitiveType(Rhythm), "x")]);
                               ("printo", [(PrimitiveType(Octave), "x")]);
                               ("playnote", [(PrimitiveType(Note), "x")]);
                               ("bplaynote", [(PrimitiveType(Note), "x"); (PrimitiveType(Int), "y")]);
                               ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)
  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal  l -> (PrimitiveType(Int), SLiteral l)
      | Fliteral l -> (PrimitiveType(Float), SFliteral l)
      | BoolLit l -> (PrimitiveType(Bool), SBoolLit l)
      | NoteLit (t, o, r) -> (PrimitiveType(Note), SNoteLit (expr t, expr o, expr r))
      | ToneLit l -> (PrimitiveType(Tone), SToneLit l)
      | OctaveLit l -> (PrimitiveType(Octave), SOctaveLit l)
      | RhythmLit l -> (PrimitiveType(Rhythm), SRhythmLit l)
      | ToneAccess n -> (PrimitiveType(Tone), SToneAccess n)
      | OctaveAccess n -> (PrimitiveType(Octave), SOctaveAccess n)
      | RhythmAccess n -> (PrimitiveType(Rhythm), SRhythmAccess n)
      | ToneSet(n, e) -> (PrimitiveType(Tone), SToneSet (n, expr e))
      | OctaveSet(n, e) -> (PrimitiveType(Octave), SOctaveSet (n, expr e))
      | RhythmSet(n, e) -> (PrimitiveType(Rhythm), SRhythmSet (n, expr e))
      | StrLit l -> (PrimitiveType(String), SStrLit l)
      | Noexpr -> (PrimitiveType(Void), SNoexpr)
      | Id s -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      (* | ArrayAssign (var, e1, e2) as ex ->
          let lt = type_of_identifier var
            and (rt, e2') = expr e2 in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
              string_of_typ rt ^ " in " ^ string_of_expr ex
            in (check_assign lt rt err, SArrayAssign(var, expr e1, (rt, e2'))) *)

          (* let t = (type_of_identifier a_name)
            in check_array_or_throw t a_name; 
          let arr_t = get_array_type t 
            and expr_t = (expr e) in
          check_assign arr_t expr_t ex

            let check_array_or_throw typ a_name =
              if match_array typ then () else raise (Failure (a_name ^ " is not an array"))

            let get_array_type = function
                ArrayType(typ) -> typ
              | _ -> raise (Failure "invalid array type") *)

      | MakeArray(t, e) as ex -> 
          let (t', e') = expr e in
          if t' = PrimitiveType(Int) 
            then (ArrayType(t), SMakeArray(t, (t',e')))
              (* (ArrayType(t), SMakeArray(t, expr ex)) *)
          else raise (Failure ("illegal make, must provide integer size for " ^  string_of_expr e))
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = PrimitiveType(Int) || t = PrimitiveType(Float) -> t
          | Not when t = PrimitiveType(Bool) -> PrimitiveType(Bool)
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = PrimitiveType(Int)   -> PrimitiveType(Int)
          | Add | Sub | Mult | Div when same && t1 = PrimitiveType(Float) -> PrimitiveType(Float)
          | Equal | Neq            when same               -> PrimitiveType(Bool)
          | Less | Leq | Greater | Geq
                     when same && (t1 = PrimitiveType(Int) || t1 = PrimitiveType(Float)) -> PrimitiveType(Bool)
          | And | Or when same && t1 = PrimitiveType(Bool) -> PrimitiveType(Bool)
          | _ -> raise (Failure ("illegal binary operator " ^
                          string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                          string_of_typ t2 ^ " in " ^ string_of_expr e))
            in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      in let err = "expected Boolean expression in " ^ string_of_expr e ^ " insted type " ^ (string_of_typ t')
      in if t' != PrimitiveType(Bool) then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
        SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (
          Failure ("return gives " ^ string_of_typ t ^ " expected " ^
            string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = match check_stmt (Block func.body) with
	                SBlock(sl) -> sl
                | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
