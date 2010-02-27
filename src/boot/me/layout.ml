open Semant;;
open Common;;

let log cx = Session.log "layout"
  cx.ctxt_sess.Session.sess_log_layout
  cx.ctxt_sess.Session.sess_log_out
;;

type slot_stack = Il.referent_ty Stack.t;;
type frame_blocks = slot_stack Stack.t;;

let layout_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  (*
   *   - Frames look, broadly, like this (growing downward):
   *
   *     +----------------------------+ <-- Rewind tail calls to here. If varargs are supported,
   *     |caller args                 |     must use memmove or similar "overlap-permitting" move,
   *     |...                         |     if supporting tail-calling.
   *     |...                         |
   *     +----------------------------+ <-- fp + abi_frame_base_sz + abi_implicit_args_sz
   *     |caller non-reg ABI operands |
   *     |possibly empty, if fastcall |
   *     |  - process pointer?        |
   *     |  - runtime pointer?        |
   *     |  - yield pc or delta?      |
   *     |  - yield slot addr?        |
   *     |  - ret slot addr?          |
   *     +----------------------------+ <-- fp + abi_frame_base_sz
   *     |return pc pushed by machine |
   *     |plus any callee-save stuff  |
   *     +----------------------------+ <-- fp
   *     |(optional gc info)          |
   *     |...                         |
   *     |frame-allocated stuff       |
   *     |determined in resolve       |
   *     |laid out in layout          |
   *     |...                         |
   *     |...                         |
   *     +----------------------------+ <-- fp - framesz
   *     |spills determined in ra     |
   *     |...                         |
   *     |...                         |
   *     +----------------------------+ <-- fp - (framesz + spillsz)
   *     |call space                  |
   *     |...                         |
   *     |...                         |
   *     +----------------------------+ <-- fp - (framesz + spillsz + callsz)
   *
   *   - the layout of a frame is offset from fp:
   *     in other words, frame_layout.layout_offset = -(frame_layout.layout_size)
   *
   *   - the layout of a slot in the function frame is offset from (fp-framesz)
   *
   *   - Slots are split into two classes:
   *
   *     #1 Those that are never aliased and fit in a word, so are
   *        vreg-allocated
   *
   *     #2 All others
   *
   *   - Non-aliased, word-fitting slots consume no frame space
   *     *yet*; they are given a generic value that indicates "try a
   *     vreg". The register allocator may spill them later, if it
   *     needs to, but that's not our concern.
   *
   *   - Aliased / too-big slots are frame-allocated, need to be
   *     laid out in the frame at fixed offsets, so need to be
   *     assigned Common.layout values.  (Is this true of aliased
   *     word-fitting? Can we not runtime-calculate the position of
   *     a spill slot? Meh.)
   *
   *   - The frame size is the maximum of all the block sizes contained
   *     within it.
   * 
   *   - Each call is examined and the size of the call tuple required
   *     for that call is calculated. The call size is the maximum of all
   *     such call tuples.
   *
   *)

  (* At the bottom of every frame, there's a frame-info pointer. *)
  let (frame_info_slot_sz:int64) =
    cx.ctxt_abi.Abi.abi_word_sz
  in

  let force_slot_to_mem (slot:Ast.slot) : bool =
    (* FIXME (bug 541559): For the time being we force any slot that
     * points into memory or is of opaque/code type to be stored in the
     * frame rather than in a vreg. This can probably be relaxed in the
     * future.
     *)
    let rec st_in_mem st =
      match st with
          Il.ValTy _ -> false
        | Il.AddrTy _ -> true

    and rt_in_mem rt =
      match rt with
          Il.ScalarTy st -> st_in_mem st
        | Il.StructTy rts
        | Il.UnionTy rts -> List.exists rt_in_mem (Array.to_list rts)
        | Il.OpaqueTy
        | Il.CodeTy -> true
        | Il.NilTy -> false
    in
      rt_in_mem (slot_referent_type cx.ctxt_abi slot)
  in

  let rty_sz rty = Il.referent_ty_size cx.ctxt_abi.Abi.abi_word_bits rty in
  let rty_layout rty = Il.referent_ty_layout cx.ctxt_abi.Abi.abi_word_bits rty in

  let layout_slot_ids
      (slot_accum:slot_stack)
      (upwards:bool)
      (vregs_ok:bool)
      (offset:int64)
      (slots:node_id array)
      : unit =
    let accum (off,align) id : (int64 * int64) =
      let slot = referent_to_slot cx id in
      let rt = slot_referent_type cx.ctxt_abi slot in
      let (elt_size, elt_align) = rty_layout rt in
        if vregs_ok
          && (i64_le elt_size cx.ctxt_abi.Abi.abi_word_sz)
          && (not (force_slot_to_mem slot))
          && (not (Hashtbl.mem cx.ctxt_slot_aliased id))
        then
          begin
            log cx "assigning slot #%d to vreg" (int_of_node id);
            htab_put cx.ctxt_slot_vregs id (ref None);
            (off,align)
          end
        else
          begin
            let elt_off = Il.align_to elt_align off in
            let frame_off =
              if upwards
              then elt_off
              else Int64.neg (Int64.add elt_off elt_size)
            in
              Stack.push (slot_referent_type cx.ctxt_abi slot) slot_accum;
              log cx "slot #%d offset: %Ld" (int_of_node id) frame_off;
              if (not (Hashtbl.mem cx.ctxt_slot_offsets id))
              then htab_put cx.ctxt_slot_offsets id frame_off;
              (Int64.add elt_off elt_size, i64_max elt_align align)
          end
    in
      ignore (Array.fold_left accum (offset, 0L) slots)
  in

  let layout_block (slot_accum:slot_stack) (offset:int64) (block:Ast.block) : unit =
    log cx "laying out block #%d at fp offset %Ld" (int_of_node block.id) offset;
    let block_slot_ids =
      Array.of_list (htab_vals (Hashtbl.find cx.ctxt_block_slots block.id))
    in
      layout_slot_ids slot_accum false true offset block_slot_ids
  in

  let layout_header (id:node_id) (input_slot_ids:node_id array) : unit =
    let offset =
      Int64.add
        cx.ctxt_abi.Abi.abi_frame_base_sz
        cx.ctxt_abi.Abi.abi_implicit_args_sz
    in
      log cx "laying out header for node #%d at fp offset %Ld" (int_of_node id) offset;
      layout_slot_ids (Stack.create()) true false offset input_slot_ids
  in

  let (frame_stack:(node_id * frame_blocks) Stack.t) = Stack.create() in

  let block_rty (block:slot_stack) : Il.referent_ty =
    Il.StructTy (Array.of_list (stk_elts_from_bot block))
  in

  let frame_rty (frame:frame_blocks) : Il.referent_ty =
    Il.StructTy (Array.of_list (List.map block_rty (stk_elts_from_bot frame)))
  in

  let update_frame_size _ =
    let (frame_id, frame_blocks) = Stack.top frame_stack in
    let sz = Int64.add frame_info_slot_sz (rty_sz (frame_rty frame_blocks)) in
    let curr = Hashtbl.find cx.ctxt_frame_sizes frame_id in
      log cx "extending frame #%d frame to size %Ld"
        (int_of_node frame_id) (i64_max curr sz) ;
      Hashtbl.replace cx.ctxt_frame_sizes frame_id (i64_max curr sz)
  in

  (* 
   * FIXME: this is a little aggressive for default callsz; it can be 
   * narrowed in frames with no drop glue and/or no indirect drop glue.
   *)

  let glue_callsz =
    let word = interior_slot Ast.TY_int in
    let glue_fn = mk_simple_ty_fn [| word; word |] in
      rty_sz (indirect_call_args_referent_type cx glue_fn Il.OpaqueTy)
  in

  let enter_frame id =
      Stack.push (id, (Stack.create())) frame_stack;
      htab_put cx.ctxt_frame_sizes id 0L;
      htab_put cx.ctxt_call_sizes id glue_callsz;
      htab_put cx.ctxt_spill_fixups id (new_fixup "frame spill fixup");
      htab_put cx.ctxt_frame_blocks id []
  in

  let leave_frame _ =
    ignore (Stack.pop frame_stack);
  in

  let visit_mod_item_pre n p i =
    let header_slot_ids hdr = Array.map (fun (sid,_) -> sid.id) hdr in
    begin
      match i.node with
          Ast.MOD_ITEM_fn fd ->
            enter_frame i.id;
            layout_header i.id
              (header_slot_ids fd.Ast.decl_item.Ast.fn_input_slots)

        | Ast.MOD_ITEM_pred pd ->
            enter_frame i.id;
            layout_header i.id
              (header_slot_ids
                 pd.Ast.decl_item.Ast.pred_input_slots)

        | Ast.MOD_ITEM_tag td ->
            let (header_slots, _, _) = td.Ast.decl_item in
              enter_frame i.id;
              layout_header i.id
                (Array.map (fun sid -> sid.id)
                   header_slots)

        | Ast.MOD_ITEM_mod {Ast.decl_item=(Some (hdr, _), _)} ->
            let ids = header_slot_ids hdr in
              layout_header i.id ids;
              Array.iter
                (fun id -> htab_put cx.ctxt_slot_is_module_state id ())
                ids

        | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in
  let visit_mod_item_post n p i =
    inner.Walk.visit_mod_item_post n p i;
    begin
      match i.node with
          Ast.MOD_ITEM_fn _ -> leave_frame ()
        | Ast.MOD_ITEM_pred _ -> leave_frame ()
        | Ast.MOD_ITEM_tag _ -> leave_frame ()
        | _ -> ()
    end
  in
  let visit_native_mod_item_pre n i =
    begin
      match i.node with
          Ast.NATIVE_fn nfn ->
            enter_frame i.id;
            Hashtbl.replace cx.ctxt_frame_sizes i.id frame_info_slot_sz;
            layout_header i.id
              (Array.map (fun (sid,_) -> sid.id)
                 nfn.Ast.native_fn_input_slots)
        | _ -> ()
    end;
    inner.Walk.visit_native_mod_item_pre n i
  in
  let visit_native_mod_item_post n i =
    inner.Walk.visit_native_mod_item_post n i;
    begin
      match i.node with
          Ast.NATIVE_fn _ -> leave_frame ()
        | _ -> ()
    end
  in
  let visit_block_pre b =
    let (frame_id, frame_blocks) = Stack.top frame_stack in
    let off =
      if Stack.is_empty frame_blocks
      then frame_info_slot_sz
      else (Int64.add frame_info_slot_sz (rty_sz (frame_rty frame_blocks)))
    in
    let block_slots = Stack.create() in
    let frame_block_ids = Hashtbl.find cx.ctxt_frame_blocks frame_id in
      Hashtbl.replace cx.ctxt_frame_blocks frame_id (b.id :: frame_block_ids);
      layout_block block_slots off b;
      Stack.push block_slots frame_blocks;
      update_frame_size ();
      inner.Walk.visit_block_pre b
  in
  let visit_block_post b =
    inner.Walk.visit_block_post b;
    (* FIXME (bug 541568): In earlier versions of this file, multiple
     * lexical blocks in the same frame would reuse space from one to
     * the next so long as they were not nested; The (commented-out)
     * code here supports that logic. Unfortunately since our marking
     * and unwinding strategy is very simplistic for now (analogous to
     * shadow stacks) we're going to have give lexical block in a frame
     * its own space in the frame, even if they seem like they *should*
     * be able to reuse space. This makes it possible to arrive at the
     * frame and work out which variables are live (and which frame
     * memory corresponds to them) w/o paying attention to the current
     * pc in the function; a greatly-simplifying assumption.
     * 
     * This is of course not optimal for the long term, but in the
     * longer term we'll have time to form proper DWARF CFI
     * records. We're in a hurry at the moment.  *)
    (*
      let stk = Stack.top block_stacks in
      ignore (Stack.pop stk)
    *)
  in

  (* Call-size calculation. *)

  let visit_stmt_pre (s:Ast.stmt) : unit =
    begin
      let callees =
        match s.node with
            Ast.STMT_call (_, lv, _)
          | Ast.STMT_spawn (_, _, lv, _) -> [| lv |]
          | Ast.STMT_check (_, calls) -> Array.map (fun (lv, _) -> lv) calls
          | _ -> [| |]
      in
        Array.iter
          begin
            fun (callee:Ast.lval) ->
              let lv_ty = lval_ty cx callee in
              let abi = cx.ctxt_abi in
              let static = lval_is_static cx callee in
              let closure = if static then None else Some Il.OpaqueTy in
              let rty = call_args_referent_type cx lv_ty closure in
              let sz = Il.referent_ty_size abi.Abi.abi_word_bits rty in
              let frame_id = fst (Stack.top frame_stack) in
              let curr = Hashtbl.find cx.ctxt_call_sizes frame_id in
                log cx "extending frame #%d call size to %Ld"
                  (int_of_node frame_id) (i64_max curr sz);
                Hashtbl.replace cx.ctxt_call_sizes frame_id (i64_max curr sz)
          end
          callees
    end;
    inner.Walk.visit_stmt_pre s
  in

    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post;
        Walk.visit_native_mod_item_pre = visit_native_mod_item_pre;
        Walk.visit_native_mod_item_post = visit_native_mod_item_post;

        Walk.visit_stmt_pre = visit_stmt_pre;
        Walk.visit_block_pre = visit_block_pre;
        Walk.visit_block_post = visit_block_post }
;;

let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  let path = Stack.create () in
  let passes =
    [|
      (layout_visitor cx
         Walk.empty_visitor)
    |];
  in
    run_passes cx "layout" path passes (log cx "%s") crate
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
