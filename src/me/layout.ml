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
   *     |...                         |
   *     |...                         |
   *     |...                         |
   *     +----------------------------+ <-- fp - framesz
   *     |spills determined in ra     |
   *     |...                         |
   *     |...                         |
   *     +----------------------------+ <-- fp - (framesz + spillsz)
   * 
   *   - Divide slots into two classes:
   * 
   *     #1 Those that are never aliased and fit in a word, so are
   *        vreg-allocated
   * 
   *     #2 All others
   * 
   *   - Lay out the frame in post-order, given what we now know wrt
   *     the slot types and aliasing:
   * 
   *     - Non-aliased, word-fitting slots consume no frame space
   *       *yet*; they are given a generic value that indicates "try a
   *       vreg". The register allocator may spill them later, if it
   *       needs to, but that's not our concern.
   * 
   *     - Aliased / too-big slots are frame-allocated, need to be
   *       laid out in the frame at fixed offsets, so need to be
   *       assigned Common.layout values.  (Is this true of aliased
   *       word-fitting? Can we not runtime-calculate the position of
   *       a spill slot? Meh.)
   * 
   *   - The frame size is the maximum of all the block sizes contained
   *     within it. 
   * 
   *)

  let string_of_layout (ly:layout) : string = 
    Printf.sprintf "sz=%Ld, off=%Ld, align=%Ld" 
      ly.layout_size ly.layout_offset ly.layout_align
  in
  let layout_slot_ids (offset:int64) (slots:node_id array) : layout = 
    let layout_slot_id id = 
      if Hashtbl.mem cx.ctxt_slot_layouts id
      then Hashtbl.find cx.ctxt_slot_layouts id
      else 
        let slot = Hashtbl.find cx.ctxt_all_slots id in
        let layout = layout_slot cx.ctxt_abi 0L slot in 
          log cx "forming layout for slot #%d: %s" id (string_of_layout layout);
          Hashtbl.add cx.ctxt_slot_layouts id layout;
          layout
    in
    let layouts = Array.map layout_slot_id slots in
    let group_layout = pack offset layouts in
      for i = 0 to (Array.length layouts) - 1 do
        log cx "packed slot #%d layout to: %s" slots.(i) (string_of_layout layouts.(i))
      done;
      group_layout
  in
    
  let layout_block (offset:int64) (block:Ast.block) : layout = 
    log cx "laying out block #%d at fp offset %Ld" block.id offset;
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
            block.id i (Ast.string_of_key key) sid
      done;
      let sorted_slot_ids = Array.map (fun (_,sid) -> sid) sorted_keyed_slot_ids in
      let layout = layout_slot_ids offset sorted_slot_ids in
        log cx "block #%d total layout: %s" block.id (string_of_layout layout);
        layout
  in
  let layout_fn (id:node_id) (fn:Ast.fn) : layout = 
    let offset = 
      Int64.add 
        cx.ctxt_abi.Abi.abi_frame_base_sz 
        cx.ctxt_abi.Abi.abi_implicit_args_sz 
    in
      log cx "laying out fn #%d at fp offset %Ld" id offset;
      let input_slot_ids = Array.map (fun (sid,_) -> sid.id) fn.Ast.fn_input_slots in
      let layout = layout_slot_ids offset input_slot_ids in
        log cx "fn #%d total layout: %s" id (string_of_layout layout);
        layout
  in
    
  let layout_prog (id:node_id) (prog:Ast.prog) : layout = 
    let offset = 0L in 
      log cx "laying out prog #%d at fp offset %Ld" id offset;
      let layout = 
        match prog.Ast.prog_main with 
            Some m -> layout_block offset m 
          | None -> new_layout offset 0L 0L
      in
        layout
  in
  let frame_layouts = Stack.create () in 
  let visit_mod_item_pre n p i = 
    begin
      match i.node with 
          Ast.MOD_ITEM_fn fd ->
            let layout = layout_fn i.id fd.Ast.decl_item in
              Stack.push layout frame_layouts
        | Ast.MOD_ITEM_prog pd ->
            let layout = layout_prog i.id pd.Ast.decl_item in
              Stack.push layout frame_layouts
        | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in
  let visit_mod_item_post n p i = 
    inner.Walk.visit_mod_item_post n p i;
    begin
      match i.node with 
          Ast.MOD_ITEM_fn fd ->
            ignore (Stack.pop frame_layouts)
        | Ast.MOD_ITEM_prog pd ->
            ignore (Stack.pop frame_layouts)
        | _ -> ()
    end
  in
  let visit_block_pre b = 
    ignore (layout_block 0L b);
    inner.Walk.visit_block_pre b
  in
  let visit_block_post b = 
    inner.Walk.visit_block_post b;
  in

    { inner with 
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post;
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
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
