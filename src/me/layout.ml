open Semant;;
open Common;;

let log cx = Session.log "layout"
  cx.ctxt_sess.Session.sess_log_layout
  cx.ctxt_sess.Session.sess_log_out
;;


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

  let string_of_layout (ly:layout) : string =
    Printf.sprintf "sz=%Ld, off=%Ld, align=%Ld"
      ly.layout_size ly.layout_offset ly.layout_align
  in
  let layout_slot_ids (vregs_ok:bool) (offset:int64) (slots:node_id array) : layout =
    let layout_slot_id id =
      if Hashtbl.mem cx.ctxt_slot_layouts id
      then Some (Hashtbl.find cx.ctxt_slot_layouts id)
      else
        let slot = Hashtbl.find cx.ctxt_all_slots id in
        let layout = layout_slot cx.ctxt_abi 0L slot in
          if vregs_ok
            && (i64_le layout.layout_size cx.ctxt_abi.Abi.abi_word_sz)
            && (not (Hashtbl.mem cx.ctxt_slot_aliased id))
          then
            begin
              log cx "assigning slot #%d to vreg" (int_of_node id);
              htab_put cx.ctxt_slot_vregs id (ref None);
              None
            end
          else
            begin
              log cx "forming layout for slot #%d: %s" (int_of_node id) (string_of_layout layout);
              htab_put cx.ctxt_slot_layouts id layout;
              Some layout
            end
    in
    let layouts = arr_map_partial slots layout_slot_id  in
    let group_layout = pack offset layouts in
      for i = 0 to (Array.length layouts) - 1 do
        log cx "packed slot #%d layout to: %s" (int_of_node slots.(i)) (string_of_layout layouts.(i))
      done;
      group_layout
  in

  let layout_block (offset:int64) (block:Ast.block) : layout =
    log cx "laying out block #%d at fp offset %Ld" (int_of_node block.id) offset;
    let block_slots = Hashtbl.find cx.ctxt_block_slots block.id in
    let get_keyed_slot_ids key id accum = ((key, id) :: accum) in
    let keyed_slot_ids = Hashtbl.fold get_keyed_slot_ids block_slots [] in
    let sorted_keyed_slot_ids =
      (Array.of_list
         (Sort.list
            (fun (a, _) (b, _) -> a < b) keyed_slot_ids))
    in
      for i = 0 to (Array.length sorted_keyed_slot_ids) - 1 do
        let (key,sid) = sorted_keyed_slot_ids.(i) in
          log cx "block #%d entry %d: '%s' = slot #%d"
            (int_of_node block.id) i (Ast.fmt_to_str Ast.fmt_slot_key key) (int_of_node sid)
      done;
      let sorted_slot_ids = Array.map (fun (_,sid) -> sid) sorted_keyed_slot_ids in
      let layout = layout_slot_ids true offset sorted_slot_ids in
        (*
         * At this point the layout extends from [off,off+sz] but we need to adjust it
         * to cover [off-sz,off]. We also have to iterate through all the slot layouts
         * and adjust them down by sz.
         *)
      let sz = layout.layout_size in
      let sub_sz ly = ly.layout_offset <- Int64.sub ly.layout_offset sz in
        sub_sz layout;
        Array.iter
          (fun sid ->
             match htab_search cx.ctxt_slot_layouts sid with
                 None -> ()
               | Some ly -> sub_sz ly)
          sorted_slot_ids;
        log cx "block #%d total layout: %s" (int_of_node block.id) (string_of_layout layout);
        htab_put cx.ctxt_block_layouts block.id layout;
        layout;
  in

  let layout_header (id:node_id) (input_slot_ids:node_id array) : unit =
    let offset =
      Int64.add
        cx.ctxt_abi.Abi.abi_frame_base_sz
        cx.ctxt_abi.Abi.abi_implicit_args_sz
    in
      log cx "laying out header for node #%d at fp offset %Ld" (int_of_node id) offset;
      let layout = layout_slot_ids false offset input_slot_ids in
        log cx "node #%d header layout: %s" (int_of_node id) (string_of_layout layout);
        htab_put cx.ctxt_header_layouts id layout
  in

  let layout_prog (id:node_id) (prog:Ast.prog) : unit =
    log cx "laying out slots of prog #%d" (int_of_node id);
    let slot_ids =
      Array.of_list (List.map (fun sid -> sid.id)
                       (htab_vals prog.Ast.prog_slots))
    in
    let layout = layout_slot_ids false 0L slot_ids in
      log cx "prog #%d layout: %s" (int_of_node id) (string_of_layout layout);
      htab_put cx.ctxt_prog_layouts id layout
  in

  let (block_stacks:(layout Stack.t) Stack.t) = Stack.create () in
  let (frame_stack:node_id Stack.t) = Stack.create () in
  let update_frame_size _ =
    let sz = stk_fold
      (Stack.top block_stacks)
      (fun layout b -> Int64.add layout.layout_size b)
      0L
    in
    let frame_id = Stack.top frame_stack in
    let curr = Hashtbl.find cx.ctxt_frame_sizes frame_id in
      log cx "extending frame #%d frame to size %Ld"
        (int_of_node frame_id) (i64_max curr sz) ;
      Hashtbl.replace cx.ctxt_frame_sizes frame_id (i64_max curr sz)
  in

  let enter_frame id =
      Stack.push (Stack.create()) block_stacks;
      Stack.push id frame_stack;
      htab_put cx.ctxt_frame_sizes id 0L;
      htab_put cx.ctxt_call_sizes id 0L;
      htab_put cx.ctxt_spill_fixups id (new_fixup "frame spill fixup");
  in

  let leave_frame _ =
    ignore (Stack.pop frame_stack);
    ignore (Stack.pop block_stacks);
  in

  let visit_main_pre main = enter_frame main.id in
  let visit_fini_pre fini = enter_frame fini.id in
  let visit_init_pre init =
    let extended_input_slot_ids =
      Array.append [| init.node.Ast.init_proc_input.id |]
        (Array.map (fun (sid,_) -> sid.id) init.node.Ast.init_input_slots)
    in
      layout_header init.id extended_input_slot_ids;
      enter_frame init.id
  in

  let visit_main_post main = leave_frame() in
  let visit_init_post init = leave_frame() in
  let visit_fini_post fini = leave_frame() in

  let visit_mod_item_pre n p i =
    begin
      match i.node with
          Ast.MOD_ITEM_fn fd ->
            enter_frame i.id;
            layout_header i.id
              (Array.map (fun (sid,_) -> sid.id)
                 fd.Ast.decl_item.Ast.fn_input_slots)
        | Ast.MOD_ITEM_prog pd ->
            layout_prog i.id pd.Ast.decl_item
        | Ast.MOD_ITEM_pred _ ->
            enter_frame i.id
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
        | _ -> ()
    end
  in
  let visit_block_pre b =
    let stk = Stack.top block_stacks in
    let off =
      if Stack.is_empty stk
      then 0L
      else
        (* NB: blocks grow down, so inner blocks occur at offset *minus* size. *)
        Int64.sub
          (Stack.top stk).layout_offset
          (Stack.top stk).layout_size
    in
    let layout = layout_block off b in
      Stack.push layout stk;
      update_frame_size ();
      inner.Walk.visit_block_pre b
  in
  let visit_block_post b =
    inner.Walk.visit_block_post b;
    let stk = Stack.top block_stacks in
      ignore (Stack.pop stk)
  in

  (* Call-size calculation. *)

  let ty_fn_of_callee (id:node_id) : Ast.ty_fn =
    let referent = lval_to_referent cx id in
    let ty =
      if Hashtbl.mem cx.ctxt_all_items referent
      then Hashtbl.find cx.ctxt_all_item_types referent
      else slot_ty (Hashtbl.find cx.ctxt_all_slots referent)
    in
      match ty with
          Ast.TY_fn tfn -> tfn
        | _ -> err (Some id) "Non-function callee in call statement."
  in

  let visit_stmt_pre (s:Ast.stmt) : unit =
    begin
      match s.node with
          Ast.STMT_call (_, (Ast.LVAL_base nb), _) ->
            begin
              let abi = cx.ctxt_abi in
              let tfn = ty_fn_of_callee nb.id in
              let layout = pack 0L (layout_call_tup abi tfn) in
              let sz = layout.layout_size in
              let frame_id = Stack.top frame_stack in
              let curr = Hashtbl.find cx.ctxt_call_sizes frame_id in
                log cx "extending frame #%d call size to %Ld"
                  (int_of_node frame_id) (i64_max curr sz);
                Hashtbl.replace cx.ctxt_call_sizes frame_id (i64_max curr sz)
            end
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in

    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post;

        Walk.visit_init_pre = visit_init_pre;
        Walk.visit_main_pre = visit_main_pre;
        Walk.visit_fini_pre = visit_fini_pre;

        Walk.visit_init_post = visit_init_post;
        Walk.visit_main_post = visit_main_post;
        Walk.visit_fini_post = visit_fini_post;

        Walk.visit_stmt_pre = visit_stmt_pre;
        Walk.visit_block_pre = visit_block_pre;
        Walk.visit_block_post = visit_block_post }
;;

let process_crate
    (cx:ctxt)
    (items:Ast.mod_items)
    : unit =
  let passes =
    [|
      (layout_visitor cx
         Walk.empty_visitor)
    |];
  in
    run_passes cx passes (log cx "%s") items
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
