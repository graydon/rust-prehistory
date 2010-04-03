(*
 * LLVM translator.
 *)

open Common;;

let trans_crate
    (sem_cx:Semant.ctxt)
    (llctx:Llvm.llcontext)
    (sess:Session.sess)
    (crate:Ast.crate)
    : Llvm.llmodule =

  (* Translation of our node_ids into LLVM identifiers, which are strings. *)
  let next_anon_llid = ref 0 in
  let num_llid num klass = Printf.sprintf "%s%d" klass num in
  let anon_llid klass =
    let llid = num_llid !next_anon_llid klass in
    next_anon_llid := !next_anon_llid + 1;
    llid
  in
  let node_llid (node_id_opt:node_id option) : (string -> string) =
    match node_id_opt with
        None -> anon_llid
      | Some (Node num) -> num_llid num
  in

  (*
   * Returns a bogus value for use in stub code that hasn't been implemented
   * yet.
   *
   * TODO: On some joyous day, remove me.
   *)
  let bogus = Llvm.const_null (Llvm.i32_type llctx) in

  let ty_of = Hashtbl.find sem_cx.Semant.ctxt_all_item_types in

  let filename = Session.filename_of sess.Session.sess_in in
  let llmod = Llvm.create_module llctx filename in

  let trans_mach_ty (mty:ty_mach) : Llvm.lltype =
    let tycon =
      match mty with
          TY_u8 | TY_s8 -> Llvm.i8_type
        | TY_u16 | TY_s16 -> Llvm.i16_type
        | TY_u32 | TY_s32 -> Llvm.i32_type
        | TY_u64 | TY_s64 -> Llvm.i64_type
        | TY_f32 -> Llvm.float_type
        | TY_f64 -> Llvm.double_type
    in
    tycon llctx
  in

  let rec trans_ty (ty:Ast.ty) : Llvm.lltype =
    match ty with
        Ast.TY_any -> Llvm.opaque_type llctx
      | Ast.TY_nil -> Llvm.void_type llctx
      | Ast.TY_bool -> Llvm.i1_type llctx
      | Ast.TY_mach mty -> trans_mach_ty mty
      | Ast.TY_int -> Llvm.i32_type llctx (* FIXME: bignums? *)
      | Ast.TY_char -> Llvm.i32_type llctx
      | Ast.TY_str -> Llvm.pointer_type (Llvm.i8_type llctx)
      | Ast.TY_fn
            ({ Ast.sig_input_slots = ins; Ast.sig_output_slot = out }, _) ->
          let out_llty = trans_slot None out in
          let in_lltys = Array.map (trans_slot None) ins in
          Llvm.function_type out_llty in_lltys
      | Ast.TY_constrained (ty', _) -> trans_ty ty'
      | Ast.TY_tup _ | Ast.TY_vec _ | Ast.TY_rec _ | Ast.TY_tag _
            | Ast.TY_iso _ | Ast.TY_idx _ | Ast.TY_pred _ | Ast.TY_chan _
            | Ast.TY_port _ | Ast.TY_obj _ | Ast.TY_proc | Ast.TY_opaque _
            | Ast.TY_param _ | Ast.TY_named _ | Ast.TY_type ->
          Llvm.opaque_type llctx (* TODO *)

  (* Translates the type of a slot into the corresponding LLVM type. If the
   * id_opt parameter is specified, then the type will be fetched from the
   * context if it isn't stored with the slot. Otherwise, an untyped slot
   * produces an error. *)
  and trans_slot (id_opt:node_id option) (slot:Ast.slot) : Llvm.lltype =
    let ty =
      match (slot.Ast.slot_ty, id_opt) with
          (None, None) ->
            raise (Failure "llvm_trans: found untyped anonymous slot")
        | (None, Some id) -> ty_of id
        | (Some ty, _) -> ty
    in
    let base_llty = trans_ty ty in
    match slot.Ast.slot_mode with
        Ast.MODE_exterior _ | Ast.MODE_read_alias | Ast.MODE_write_alias ->
          Llvm.pointer_type base_llty
      | Ast.MODE_interior _ -> base_llty
  in

  let trans_fn
      (name:Ast.ident)
      ({ Ast.fn_body = (body:Ast.block) }:Ast.fn)
      (id:node_id)
      : unit =
    let llfn = Llvm.declare_function name (trans_ty (ty_of id)) llmod in

    (* LLVM requires that functions be grouped into basic blocks terminated by
     * terminator instructions, while our AST is less strict. So we have to do
     * a little trickery here to wrangle the statement sequence into LLVM's
     * format. *)

    let new_block id_opt =
      let llblock = Llvm.append_block llctx (node_llid id_opt "bb") llfn in
      let llbuilder = Llvm.builder_at_end llctx llblock in
      (llblock, llbuilder)
    in

    (* Translates a list of AST statements to a sequence of LLVM instructions
     * using the given LLVM builder. Returns the LLVM builder that can be used
     * to add instructions to the end of the chain created in this way, which
     * may not be the same builder that was passed in if extra basic blocks
     * were created along the way. *)
    let rec trans_stmts
        (id_opt:node_id option)
        (llbuilder:Llvm.llbuilder)
        (stmts:Ast.stmt list)
        : Llvm.llbuilder =
      let trans_literal
          (lit:Ast.lit)
          : Llvm.llvalue option =
        match lit with
            Ast.LIT_nil -> None
          | Ast.LIT_bool value ->
            Some (Llvm.const_int (Llvm.i1_type llctx) (if value then 1 else 0))
          | Ast.LIT_mach (mty, value, _) ->
            let llty = trans_mach_ty mty in
            Some (Llvm.const_of_int64 llty value (mach_is_signed mty))
          | Ast.LIT_int (value, _) ->
            (* TODO: bignums? *)
            Some (Llvm.const_of_int64 (Llvm.i32_type llctx) value true)
          | Ast.LIT_char ch ->
            Some (Llvm.const_int (Llvm.i32_type llctx) (Char.code ch))
          | Ast.LIT_custom _ -> Some bogus (* TODO *)
      in

      let trans_lval (lval:Ast.lval) : Llvm.llvalue =
        match lval with
            Ast.LVAL_base _ -> bogus (* TODO *)
          | Ast.LVAL_ext _ -> bogus (* TODO *)
      in

      let trans_atom atom =
        match atom with
            Ast.ATOM_literal { node = lit } -> trans_literal lit
          | Ast.ATOM_lval lval -> Some (trans_lval lval)
      in

      let trans_binary_expr
          ((op:Ast.binop), (lhs:Ast.atom), (rhs:Ast.atom))
          : Llvm.llvalue =
        (* Evaluate the operands in the proper order. *)
        let (lllhs_opt, llrhs_opt) =
          match op with
              Ast.BINOP_or | Ast.BINOP_and | Ast.BINOP_eq | Ast.BINOP_ne
                  | Ast.BINOP_lt | Ast.BINOP_le | Ast.BINOP_ge | Ast.BINOP_gt
                  | Ast.BINOP_lsl | Ast.BINOP_lsr | Ast.BINOP_asr
                  | Ast.BINOP_add | Ast.BINOP_sub | Ast.BINOP_mul
                  | Ast.BINOP_div | Ast.BINOP_mod ->
                (trans_atom lhs, trans_atom rhs)
            | Ast.BINOP_send ->
                let llrhs = trans_atom rhs in
                let lllhs = trans_atom lhs in
                (lllhs, llrhs)
        in
        let (lllhs, llrhs) =
          match (lllhs_opt, llrhs_opt) with
              (Some lllhs, Some llrhs) -> (lllhs, llrhs)
            | _ ->
              (* FIXME: Can this legitimately occur with send? *)
              raise (Failure "nil found in binary expression")
        in
        match op with
            Ast.BINOP_eq ->
              let llid = anon_llid "expr" in
              Llvm.build_icmp Llvm.Icmp.Eq lllhs llrhs llid llbuilder
          | _ -> bogus (* TODO *)
      in

      let trans_unary_expr _ = bogus in (* TODO *)

      let trans_expr (expr:Ast.expr) : Llvm.llvalue option =
        match expr with
            Ast.EXPR_binary binexp -> Some (trans_binary_expr binexp)
          | Ast.EXPR_unary unexp -> Some (trans_unary_expr unexp)
          | Ast.EXPR_atom atom -> trans_atom atom
      in

      match stmts with
          [] -> llbuilder
        | { node = head }::tail ->
            (* Translates the remaining statements as part of a new LLVM basic
             * block and returns that basic block, that basic block's builder,
             * along with a function that, given a builder, emits an
             * unconditional branch instruction to the new basic block. This
             * function is suitable for passing to trans_block. *)
            let trans_tail_in_new_block () =
              let (tail_llblock, tail_llbuilder) = new_block None in
              let final_llbuilder = trans_stmts None tail_llbuilder tail in
              let terminate llbuilder' =
                ignore (Llvm.build_br tail_llblock llbuilder')
              in
              (tail_llblock, final_llbuilder, terminate)
            in

            match head with
                Ast.STMT_if {
                  Ast.if_test = test;
                  Ast.if_then = if_then;
                  Ast.if_else = else_opt
                } ->
                  let llexpr =
                    match trans_expr test with
                        None -> raise (Failure "llvm: nil value as if test")
                      | Some llexpr -> llexpr
                  in
                  let (next_llblock, final_llbuilder, terminate) =
                    trans_tail_in_new_block ()
                  in
                  let then_llblock = trans_block if_then terminate in
                  begin
                    match else_opt with
                        None ->
                          ignore (Llvm.build_cond_br llexpr then_llblock
                            next_llblock llbuilder)
                      | Some if_else ->
                          let else_llblock = trans_block if_else terminate in
                          ignore (Llvm.build_cond_br llexpr then_llblock
                            else_llblock llbuilder)
                  end;
                  final_llbuilder
              | _ -> trans_stmts id_opt llbuilder tail (* TODO *)

    (* Translates an AST block to one or more LLVM basic blocks and returns the
     * first basic block. The supplied callback is expected to add a
     * terminator instruction. *)
    and trans_block
        ({ node = (stmts:Ast.stmt array); id = id }:Ast.block)
        (terminate:Llvm.llbuilder -> unit)
        : Llvm.llbasicblock =
      let (llblock, llbuilder) = new_block (Some id) in
      let llbuilder' = trans_stmts (Some id) llbuilder (Array.to_list stmts) in
      terminate llbuilder';
      llblock
    in

    ignore (trans_block body ignore);
  in

  let trans_mod_item
      (name:Ast.ident)
      { node = { Ast.decl_item = (item:Ast.mod_item') }; id = id }
      : unit =
    match item with
        Ast.MOD_ITEM_fn fn -> trans_fn name fn id
      | _ -> ()
  in

  try
    let crate' = crate.node in
    Hashtbl.iter trans_mod_item crate'.Ast.crate_items;
    llmod
  with e -> Llvm.dispose_module llmod; raise e
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)

