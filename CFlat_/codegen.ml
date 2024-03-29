(*

Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "CFlat" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context in
  let str_t      = L.pointer_type i8_t in
  let arr_t      = L.array_type i32_t 9 in
  let named_struct_note_t = L.named_struct_type context "named_struct_note_t" in 
  ignore (L.struct_set_body named_struct_note_t [| L.pointer_type i8_t; L.i32_type context; L.pointer_type i8_t |] false);

  (* Return the LLVM type for a CFlat type *)
  let ltype_of_primitive_typ = function
      A.PrimitiveType(A.Int)   -> i32_t
    | A.PrimitiveType(A.Bool)  -> i1_t
    | A.PrimitiveType(A.Float) -> float_t
    | A.PrimitiveType(A.Void)  -> void_t
    | A.PrimitiveType(A.Note)  -> named_struct_note_t
    | A.PrimitiveType(A.Tone)  -> str_t
    | A.PrimitiveType(A.Octave)  -> i32_t
    | A.PrimitiveType(A.Rhythm)  -> str_t
    | A.PrimitiveType(A.String) -> str_t
  in

  let ltype_of_typ = function
      A.PrimitiveType(t) -> ltype_of_primitive_typ(A.PrimitiveType(t))
    | A.ArrayType(t) -> L.array_type (ltype_of_primitive_typ (A.PrimitiveType(t))) 9
  in


  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.PrimitiveType(A.Float) -> L.const_float (ltype_of_primitive_typ t) 0.0
        | _ -> L.const_int (ltype_of_primitive_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

  let play_note_t : L.lltype =
      L.function_type i32_t [| L.pointer_type named_struct_note_t |] in
  let play_note_func : L.llvalue =
      L.declare_function "play_note" play_note_t the_module in

  let bplay_note_t : L.lltype =
      L.function_type i32_t [| L.pointer_type named_struct_note_t ; i32_t |] in
  let bplay_note_func : L.llvalue =
      L.declare_function "bplay_note" bplay_note_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
 
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and note_format_str = L.build_global_stringptr "/%s/ /%d/ /%s/\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder 
    and tone_format_str = L.build_global_stringptr "%s\n" "fmt" builder 
    and octave_format_str = L.build_global_stringptr "%d\n" "fmt" builder 
    and rhythm_format_str = L.build_global_stringptr "%s\n" "fmt" builder 
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
	    let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	    StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	      let local_var = L.build_alloca (ltype_of_typ t) n builder
	      in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	      SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoteLit (t, o, r) -> let t' = expr builder t and 
                                o' = expr builder o and 
                                r' = expr builder r in
                                L.const_named_struct named_struct_note_t [| t'; o'; r' |]
                                (* let note_struct = (let t' = expr builder t and 
                                  o' = expr builder o and 
                                  r' = expr builder r in
                                L.const_named_struct named_struct_note_t [| t'; o'; r' |]) in
                                L.build_gep note_struct [| note_struct |] "note_ptr" builder
                                *)
      | SToneLit t ->  L.build_global_stringptr (t ^ "\x00") "tone_ptr" builder 
      | SOctaveLit o ->  L.const_int i32_t o
      | SRhythmLit r ->  L.build_global_stringptr (r ^ "\x00") "rhythm_ptr" builder 
      | SStrLit l   -> L.build_global_stringptr (l ^ "\x00") "str_ptr" builder 
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                            ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.PrimitiveType(Float), _) as e1, op, e2) ->
                          let e1' = expr builder e1 and e2' = expr builder e2 in
                            ( match op with 
                              A.Add     -> L.build_fadd
                            | A.Sub     -> L.build_fsub
                            | A.Mult    -> L.build_fmul
                            | A.Div     -> L.build_fdiv 
                            | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	                          | A.Neq     -> L.build_fcmp L.Fcmp.One
	                          | A.Less    -> L.build_fcmp L.Fcmp.Olt
	                          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	                          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	                          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	                          | A.And | A.Or ->
	                            raise (Failure "internal error: semant should have rejected and/or on float")
                            ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
                          let e1' = expr builder e1 and e2' = expr builder e2 in
                            ( match op with
                              A.Add     -> L.build_add
                            | A.Sub     -> L.build_sub
                            | A.Mult    -> L.build_mul
                            | A.Div     -> L.build_sdiv
                            | A.And     -> L.build_and
                            | A.Or      -> L.build_or
                            | A.Equal   -> L.build_icmp L.Icmp.Eq
                            | A.Neq     -> L.build_icmp L.Icmp.Ne
                            | A.Less    -> L.build_icmp L.Icmp.Slt
                            | A.Leq     -> L.build_icmp L.Icmp.Sle
                            | A.Greater -> L.build_icmp L.Icmp.Sgt
                            | A.Geq     -> L.build_icmp L.Icmp.Sge
                            ) e1' e2' "tmp" builder
      | SUnop (op, ((t, _) as e)) ->
                          let e' = expr builder e in
                            ( match op with
                              A.Neg when t = A.PrimitiveType(A.Float) -> L.build_fneg
                            | A.Neg                  -> L.build_neg
                            | A.Not                  -> L.build_not
                            ) e' "tmp" builder
      | SToneAccess n -> let tb = L.build_struct_gep (lookup n) 0 "@tone" builder in
                            L.build_load tb ".tone" builder
      | SOctaveAccess n -> let ob = L.build_struct_gep (lookup n) 1 "@octave" builder in
                            L.build_load ob ".octave" builder
      | SRhythmAccess n -> let rb = L.build_struct_gep (lookup n) 2 "@rhythm" builder in
                            L.build_load rb ".rhythm" builder
      | SToneSet (n, e) -> let tb = L.build_struct_gep (lookup n) 0 "@tone" builder in
                            let e' = expr builder e in
                            ignore(L.build_store e' tb builder); e'
      | SOctaveSet (n, e) -> let tb = L.build_struct_gep (lookup n) 1 "@octave" builder in
                            let e' = expr builder e in
                            ignore(L.build_store e' tb builder); e'
      | SRhythmSet (n, e) -> let tb = L.build_struct_gep (lookup n) 2 "@rhythm" builder in
                            let e' = expr builder e in
                            ignore(L.build_store e' tb builder); e'

                          (* L.build_global_stringptr ("hiiii" ^ "\x00") "tone_ptr" builder  *)
                          (* L.build_extractvalue (lookup n) 0 ".tone" builder *)
                            (* in
                            L.value_name tv *)
                            (* L.build_global_stringptr tv "tone_ptr" builder *)
                          (* let nv = lookup n in
                            let tv = L.build_extractvalue nv 0 ".tone" builder in
                            let tvv = L.const_extractvalue tv [| 0 |] in
                            L.build_load tvv "note.tone" builder *)
                          (* L.const_extractvalue (lookup n) [| 0 |] *)
                          (* let nv = L.build_load (lookup n) n builder in
                            L.const_extractvalue nv [| 0 |] *)
                            (* L.build_extractvalue nv 0 ".tone" builder *)
                            (* let tb = L.build_struct_gep nv 0 "@tone" builder in
                            L.build_load tb ".tone" builder *)

      | SCall ("print", [e]) | SCall ("printb", [e]) ->
	      L.build_call printf_func [| int_format_str ; (expr builder e) |]
	      "printf" builder
      | SCall ("printbig", [e]) ->
	      L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | SCall ("printf", [e]) -> 
	      L.build_call printf_func [| float_format_str ; (expr builder e) |]
	      "printf" builder
      | SCall ("prints", [e]) -> 
	      L.build_call printf_func [| str_format_str ; (expr builder e) |]
	      "printf" builder
      | SCall ("printn", [e]) -> let (_, SId n) = e in
                            let t' = expr builder (A.PrimitiveType(A.Tone), SToneAccess n) 
                            and o' = expr builder (A.PrimitiveType(A.Octave), SOctaveAccess n)
                            and r' = expr builder (A.PrimitiveType(A.Rhythm), SRhythmAccess n) in
        L.build_call printf_func [| note_format_str ; t'; o'; r' |]
        "printf" builder
      | SCall ("printt", [e]) -> 
	      L.build_call printf_func [| tone_format_str ; (expr builder e) |]
	      "printf" builder
      | SCall ("printo", [e]) ->
        L.build_call printf_func [| octave_format_str ; (expr builder e) |]
        "printf" builder
      | SCall ("printr", [e]) ->
        L.build_call printf_func [| rhythm_format_str ; (expr builder e) |]
        "printf" builder
      | SCall ("playnote", [e]) -> let (_, SId n) = e in
        L.build_call play_note_func [| (lookup n) |] "play_note" builder
      | SCall ("bplaynote", [e1 ; e2]) -> let (_, SId n) = e1 in
        L.build_call bplay_note_func [| (lookup n) ; (expr builder e2) |] "bplay_note" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
	      let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	      let result = (match fdecl.styp with 
                        A.PrimitiveType(A.Void) -> ""
                      | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list llargs) result builder
    in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder = function
	SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder 
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.PrimitiveType(A.Void) -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
                              let bool_val = expr builder predicate in
                              let merge_bb = L.append_block context "merge" the_function in
                              let build_br_merge = L.build_br merge_bb in (* partial function *)
  
                              let then_bb = L.append_block context "then" the_function in
                                add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
                                build_br_merge;
  
                              let else_bb = L.append_block context "else" the_function in
                                add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
                                build_br_merge;
  
                                ignore(L.build_cond_br bool_val then_bb else_bb builder);
                                L.builder_at_end context merge_bb
  
      | SWhile (predicate, body) ->
                              let pred_bb = L.append_block context "while" the_function in
                                ignore(L.build_br pred_bb builder);
  
                              let body_bb = L.append_block context "while_body" the_function in
                                add_terminal (stmt (L.builder_at_end context body_bb) body)
                                (L.build_br pred_bb);
                          
                              let pred_builder = L.builder_at_end context pred_bb in
                              let bool_val = expr pred_builder predicate in
                          
                              let merge_bb = L.append_block context "merge" the_function in
                                ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
                                L.builder_at_end context merge_bb
      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
                                ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
                                
                                in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.PrimitiveType(A.Void) -> L.build_ret_void
      | A.PrimitiveType(A.Float) -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
