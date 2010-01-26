(* Translation *)

open Semant;;
open Common;;

let log cx = Session.log "trans"
  cx.ctxt_sess.Session.sess_log_trans
  cx.ctxt_sess.Session.sess_log_out
;;

let arr_max a = (Array.length a) - 1;;

type intent =
    INTENT_init
  | INTENT_read
  | INTENT_write
;;

type quad_idx = int
;;

type mem_ctrl =
    MEM_rc_opaque of int
  | MEM_rc_struct
  | MEM_gc
  | MEM_interior

let trans_visitor
    (cx:ctxt)
    (path:Ast.name_component Stack.t)
    (inner:Walk.visitor)
    : Walk.visitor =

  let iflog thunk =
    if cx.ctxt_sess.Session.sess_log_trans
    then thunk ()
    else ()
  in

  let block_layouts = Stack.create () in
  let curr_file = ref None in
  let curr_stmt = ref None in

  let (abi:Abi.abi) = cx.ctxt_abi in
  let (word_sz:int64) = abi.Abi.abi_word_sz in
  let (word_bits:Il.bits) = abi.Abi.abi_word_bits in
  let (word_ty:Il.scalar_ty) = Il.ValTy word_bits in
  let (word_ty_mach:ty_mach) =
    match word_bits with
        Il.Bits8 -> TY_u8
      | Il.Bits16 -> TY_u16
      | Il.Bits32 -> TY_u32
      | Il.Bits64 -> TY_u64
  in
  let word_n (n:int) = Int64.mul word_sz (Int64.of_int n) in

  let imm_of_ty (i:int64) (tm:ty_mach) : Il.operand =
    Il.Imm (Asm.IMM i, tm)
  in

  let imm (i:int64) : Il.operand = imm_of_ty i word_ty_mach in
  let marker = imm 0xdeadbeefL in
  let one = imm 1L in
  let zero = imm 0L in
  let imm_true = one in
  let imm_false = zero in

  let nabi_indirect =
      match cx.ctxt_sess.Session.sess_targ with
          Linux_x86_elf -> false
        | _ -> true
  in
  let nabi_rust =
    { Abi.nabi_indirect = nabi_indirect;
      Abi.nabi_convention = Abi.CONV_rust }
  in

  let out_addr_disp = abi.Abi.abi_frame_base_sz in
  let arg0_disp = Int64.add abi.Abi.abi_frame_base_sz abi.Abi.abi_implicit_args_sz in
  let frame_fns_disp = word_n (-1) in

  let emitters = Stack.create () in
  let push_new_emitter _ =
    Stack.push
      (Il.new_emitter
         abi.Abi.abi_prealloc_quad
         abi.Abi.abi_is_2addr_machine)
      emitters
  in
  let pop_emitter _ = ignore (Stack.pop emitters) in
  let emitter _ = Stack.top emitters in
  let emit q = Il.emit (emitter()) q in
  let next_vreg _ = Il.next_vreg (emitter()) in
  let mark _ : quad_idx = (emitter()).Il.emit_pc in
  let patch_existing (jmp:quad_idx) (targ:quad_idx) : unit =
    Il.patch_jump (emitter()) jmp targ
  in
  let patch (i:quad_idx) : unit =
    Il.patch_jump (emitter()) i (mark());
    (* Insert a dead quad to ensure there's an otherwise-unused jump-target here. *)
    emit Il.Dead
  in

  let annotations _ =
    (emitter()).Il.emit_annotations
  in

  let annotate (str:string) =
    let e = emitter() in
      Hashtbl.add e.Il.emit_annotations e.Il.emit_pc str
  in

  let epilogue_jumps = Stack.create() in

  let path_name (_:unit) : string =
    Ast.fmt_to_str Ast.fmt_name (Walk.path_to_name path)
  in

  let based (reg:Il.reg) : Il.addr =
    Il.RegIn (reg, None)
  in

  let based_off (reg:Il.reg) (off:Asm.expr64) : Il.addr =
    Il.RegIn (reg, Some off)
  in

  let based_imm (reg:Il.reg) (imm:int64) : Il.addr =
    based_off reg (Asm.IMM imm)
  in

  let fp_imm (imm:int64) : Il.addr =
    based_imm abi.Abi.abi_fp_reg imm
  in

  let sp_imm (imm:int64) : Il.addr =
    based_imm abi.Abi.abi_sp_reg imm
  in

  let word_at (addr:Il.addr) : Il.cell =
    Il.Addr (addr, Il.ScalarTy (Il.ValTy word_bits))
  in

  let wordptr_at (addr:Il.addr) : Il.cell =
    Il.Addr (addr, Il.ScalarTy (Il.AddrTy (Il.ScalarTy (Il.ValTy word_bits))))
  in

  let addr_add (addr:Il.addr) (off:Asm.expr64) : Il.addr =
    let addto e = Asm.ADD (off, e) in
      match addr with
          Il.Abs e -> Il.Abs (addto e)
        | Il.RegIn (r, None) -> Il.RegIn (r, Some off)
        | Il.RegIn (r, Some e) -> Il.RegIn (r, Some (addto e))
        | Il.AbsIn (f, None) -> Il.AbsIn (f, Some off)
        | Il.AbsIn (f, Some e) -> Il.AbsIn (f, Some (addto e))
        | Il.Spill _ -> bug () "Adding offset to spill slot"
  in

  let addr_add_imm (addr:Il.addr) (imm:int64) : Il.addr =
    addr_add addr (Asm.IMM imm)
  in

  let mov (dst:Il.cell) (src:Il.operand) : unit =
    emit (Il.umov dst src)
  in

  let lea (dst:Il.cell) (src:Il.addr) : unit =
    emit (Il.lea dst src)
  in

  let ptr_at (addr:Il.addr) (pointee_ty:Ast.ty) : Il.typed_addr =
    (addr, Il.ScalarTy (Il.AddrTy (referent_type abi pointee_ty)))
  in

  let need_addr_cell (cell:Il.cell) : Il.typed_addr =
    match cell with
        Il.Addr a -> a
      | Il.Reg _ -> bug () "expected address cell, got non-address register cell"
  in

  let alias (ta:Il.typed_addr) : Il.operand =
    let addr, ty = ta in
    let vreg_cell = Il.next_vreg_cell (emitter()) (Il.AddrTy ty) in
      lea vreg_cell addr;
      Il.Cell vreg_cell
  in

  (* 
   * Note: alias_cell *requires* its cell to be in memory already, and should
   * only be used on slots you know to be memory-resident. Use 'aliasing' or 
   * 'via_memory' if you have a cell or operand you want in memory for a very
   * short period of time (the time spent by the code generated by the thunk).
   *)
  let alias_cell (c:Il.cell) : Il.operand =
    alias (need_addr_cell c)
  in

  let force_to_mem (src:Il.operand) : Il.typed_addr =
    let do_spill (t:Il.scalar_ty) =
      let s = (Il.next_spill (emitter())) in
      let spill_addr = Il.Spill s in
      let spill_ta = (spill_addr, Il.ScalarTy t) in
        mov (Il.Addr spill_ta) src;
        spill_ta
    in
    match src with
        Il.Cell (Il.Addr ta) -> ta
      | Il.Cell (Il.Reg (_, t)) -> do_spill t
      | Il.Imm _ -> do_spill (Il.ValTy word_bits)
  in

  let force_to_reg (op:Il.operand) : Il.typed_reg =
    let do_mov st =
      let tmp = next_vreg () in
      let regty = (tmp, st) in
        mov (Il.Reg regty) op;
        regty
    in
      match op with
          Il.Imm  (_, tm) -> do_mov (Il.ValTy (Il.bits_of_ty_mach tm))
        | Il.Cell (Il.Reg rt) -> rt
        | Il.Cell (Il.Addr (_, Il.ScalarTy st)) -> do_mov st
        | Il.Cell (Il.Addr (_, rt)) ->
            bug () "forcing non-scalar referent of type %s to register"
              (Il.string_of_referent_ty rt)
  in

  let via_memory (writeback:bool) (c:Il.cell) (thunk:Il.typed_addr -> unit) : unit =
    match c with
        Il.Addr ta -> thunk ta
      | Il.Reg _ ->
          let ta = force_to_mem (Il.Cell c) in
            begin
              thunk ta;
              if writeback
              then
                mov c (Il.Cell (Il.Addr ta))
            end
  in

  let aliasing (writeback:bool) (c:Il.cell) (thunk:Il.operand -> unit) : unit =
    via_memory writeback c (fun ta -> thunk (alias ta))
  in

  let pointee_type (ptr:Il.cell) : Il.referent_ty =
    match ptr with
        Il.Reg (_, (Il.AddrTy rt)) -> rt
      | Il.Addr (_, Il.ScalarTy (Il.AddrTy rt)) -> rt
      | _ ->
          bug () "taking pointee-type of non-address cell %s "
            (Il.string_of_cell abi.Abi.abi_str_of_hardreg ptr)
  in

  let deref (ptr:Il.cell) : Il.typed_addr =
    let (r, st) = force_to_reg (Il.Cell ptr) in
      match st with
          Il.AddrTy rt -> (based r, rt)
        | _ -> bug () "dereferencing non-address cell of type %s "
            (Il.string_of_scalar_ty st)
  in

  let deref_off (ptr:Il.cell) (off:Asm.expr64) : Il.typed_addr =
    let (r, st) = force_to_reg (Il.Cell ptr) in
      match st with
          Il.AddrTy rt -> (based_off r off, rt)
        | _ -> bug () "offset-dereferencing non-address cell of type %s "
            (Il.string_of_scalar_ty st)
  in

  let deref_imm (ptr:Il.cell) (imm:int64) : Il.typed_addr =
    deref_off ptr (Asm.IMM imm)
  in

  let pp_imm (imm:int64) : Il.typed_addr =
    deref_imm abi.Abi.abi_pp_cell imm
  in

  let cell_vreg_num (vr:(int option) ref) : int =
    match !vr with
        None ->
          let v = (Il.next_vreg_num (emitter())) in
            vr := Some v;
            v
      | Some v -> v
  in

  let slot_id_referent_type (slot_id:node_id) : Il.referent_ty =
    slot_referent_type abi (Hashtbl.find cx.ctxt_all_slots slot_id)
  in

  let cell_of_block_slot
      (slot_id:node_id)
      : Il.cell =
    let referent_type = slot_id_referent_type slot_id in
      match htab_search cx.ctxt_slot_vregs slot_id with
          Some vr ->
            let scalar_type =
              match referent_type with
                  Il.ScalarTy st -> st
                | Il.StructTy _ -> bugi cx slot_id "cannot treat structured referent as single operand"
                | Il.OpaqueTy -> bugi cx slot_id "cannot treat opaque referent as single operand"
                | Il.CodeTy ->  bugi cx slot_id "cannot treat code referent as single operand"
            in
              Il.Reg (Il.Vreg (cell_vreg_num vr), scalar_type)
        | None ->
            begin
              match htab_search cx.ctxt_slot_layouts slot_id with
                  None -> bugi cx slot_id "slot assigned to neither vreg nor layout"
                | Some layout ->
                    let disp = layout.layout_offset in
                      Il.Addr (fp_imm disp, referent_type)
            end
  in

  let iter_block_slots
      (block_id:node_id)
      (fn:Ast.slot_key -> node_id -> Ast.slot -> unit)
      : unit =
    let block_slots = Hashtbl.find cx.ctxt_block_slots block_id in
      Hashtbl.iter
        begin
          fun key slot_id ->
            let slot = Hashtbl.find cx.ctxt_all_slots slot_id in
              fn key slot_id slot
        end
        block_slots

  in

  let iter_frame_slots
      (fn_id:node_id)
      (fn:Ast.slot_key -> node_id -> Ast.slot -> unit)
      : unit =
    let blocks = Hashtbl.find cx.ctxt_frame_blocks fn_id in
      List.iter (fun block -> iter_block_slots block fn) blocks
  in

  let binop_to_jmpop (binop:Ast.binop) : Il.jmpop =
    match binop with
        Ast.BINOP_eq -> Il.JE
      | Ast.BINOP_ne -> Il.JNE
      | Ast.BINOP_lt -> Il.JL
      | Ast.BINOP_le -> Il.JLE
      | Ast.BINOP_ge -> Il.JGE
      | Ast.BINOP_gt -> Il.JG
      | _ -> bug () "Unhandled binop in binop_to_jmpop"
  in

  let rec trans_slot_lval_ext
      (base_ty:Ast.ty)
      (base_addr:Il.addr)
      (comp:Ast.lval_component)
      : (Il.cell * Ast.slot) =

    let displaced slot disp =
      Il.Addr (addr_add_imm base_addr disp,
               slot_referent_type abi slot)
    in

    match (base_ty, comp) with
        (Ast.TY_rec entries,
         Ast.COMP_named (Ast.COMP_ident id)) ->
          let layouts = layout_rec abi entries in
          let (slot, layout) = atab_find layouts id in
          let cell = displaced slot layout.layout_offset in
            (cell, slot)

      | (Ast.TY_tup entries,
         Ast.COMP_named (Ast.COMP_idx i)) ->
          let layouts = layout_tup abi entries in
          let slot = entries.(i) in
          let cell = displaced slot layouts.(i).layout_offset in
            (cell, slot)

      | (Ast.TY_vec slot,
         Ast.COMP_atom at) ->
          let atop = trans_atom at in
          let unit_sz = slot_sz abi slot in
          let reg = next_vreg () in
          let t = Il.Reg (reg, Il.ValTy word_bits) in
            emit (Il.binary Il.UMUL t atop (imm unit_sz));
            let (addr, _) = deref (Il.Addr (base_addr, Il.ScalarTy Il.voidptr_t)) in
            let elt_addr = trans_bounds_check addr (Il.Cell t) in
              (Il.Addr (elt_addr, slot_referent_type abi slot), slot)

      | (Ast.TY_str,
         Ast.COMP_atom at) ->
          let atop = trans_atom at in
          let unit_sz = 1L in
          let reg = next_vreg () in
          let t = Il.Reg (reg, Il.ValTy word_bits) in
          let slot = interior_slot (Ast.TY_mach TY_u8) in
            emit (Il.binary Il.UMUL t atop (imm unit_sz));
            let (addr, _) = deref (Il.Addr (base_addr, Il.ScalarTy Il.voidptr_t)) in
            let elt_addr = trans_bounds_check addr (Il.Cell t) in
              (Il.Addr (elt_addr, Il.ScalarTy (Il.ValTy Il.Bits8)), slot)

      | _ -> bug () "unhandled form of lval_ext in trans_slot_lval_ext"

  (* 
   * vec: operand holding ptr to vec.
   * mul_idx: index value * unit size.
   * return: ptr to element.
   *)
  and trans_bounds_check (vec:Il.addr) (mul_idx:Il.operand) : Il.addr =
    let (len:Il.cell) = word_at (addr_add_imm vec (word_n 2)) in
    let (base:Il.cell) = Il.Reg (next_vreg(), Il.voidptr_t) in
    let (elt_reg:Il.reg) = next_vreg () in
    let (elt:Il.cell) = Il.Reg (elt_reg, Il.voidptr_t) in
    let (diff:Il.cell) = Il.Reg (next_vreg (), Il.ValTy word_bits) in
      annotate "bounds check";
      lea base (addr_add_imm vec (word_n 3));
      emit (Il.binary Il.ADD elt (Il.Cell base) mul_idx);
      emit (Il.binary Il.SUB diff (Il.Cell elt) (Il.Cell base));
      let jmp = trans_compare Il.JB (Il.Cell diff) (Il.Cell len) in
        trans_cond_fail "bounds check" jmp;
        based elt_reg

  and trans_lval_full
      (lv:Ast.lval)
      (abs_ok:bool)
      (intent:intent)
      : (Il.cell * Ast.slot) =

    let return_fixup (fix:fixup) (slot:Ast.slot)
        : (Il.cell * Ast.slot) =
      let addr = (fixup_to_addr abs_ok fix
                    (slot_referent_type abi slot))
      in
        (Il.Addr (addr, (slot_referent_type abi slot)), slot)
    in

    let return_item (item:Ast.mod_item)
        : (Il.cell * Ast.slot) =
      let ty = Hashtbl.find cx.ctxt_all_item_types item.id in
      let slot = interior_slot ty
      in
        match item.node with
            Ast.MOD_ITEM_fn _ ->
              return_fixup (get_fn_fixup cx item.id) slot
          | Ast.MOD_ITEM_pred _ ->
              return_fixup (get_fn_fixup cx item.id) slot
          | Ast.MOD_ITEM_tag _ ->
              return_fixup (get_fn_fixup cx item.id) slot
          | Ast.MOD_ITEM_mod _ ->
              return_fixup (get_mod_fixup cx item.id) slot
          | _ ->
              bugi cx item.id
                "unhandled item type in trans_lval_full"
    in

    let return_native_item (item:Ast.native_mod_item)
        : (Il.cell * Ast.slot) =
      let ty = Hashtbl.find cx.ctxt_all_item_types item.id in
      let slot = interior_slot ty in
        match item.node with
            Ast.NATIVE_fn _ ->
              return_fixup (get_fn_fixup cx item.id) slot
          | _ ->
              bugi cx item.id
                "unhandled native item type in trans_lval_full"
    in

    let return_slot (_:node_id) (slot:Ast.slot) (slot_id:node_id)
        : (Il.cell * Ast.slot) =
      let cell = cell_of_block_slot slot_id in
        (cell, slot)
    in

    let rec trans_slot_lval_full lv intent =
      match lv with
          Ast.LVAL_ext (base, comp) ->
            let (base_cell, base_slot) = trans_slot_lval_full base intent in
            let base_cell' = deref_slot base_cell base_slot intent in
            let (addr, _) = need_addr_cell base_cell' in
              trans_slot_lval_ext (slot_ty base_slot) addr comp

        | Ast.LVAL_base nb ->
            let referent = lval_to_referent cx nb.id in
            let slot = lval_to_slot cx nb.id in
              return_slot nb.id slot referent
    in
      if lval_is_slot cx lv
      then trans_slot_lval_full lv intent
      else
        match intent with
            INTENT_read ->
              if lval_is_item cx lv
              then return_item (lval_item cx lv)
              else
                begin
                  assert (lval_is_native_item cx lv);
                  return_native_item (lval_native_item cx lv)
                end
          | _ -> err None "writing to item"


  and trans_lval (lv:Ast.lval) (intent:intent) : (Il.cell * Ast.slot) =
    trans_lval_full lv abi.Abi.abi_has_abs_data intent

  and trans_data_frag (d:data) (thunk:unit -> Asm.frag) : Il.operand =
    let fix =
      if Hashtbl.mem cx.ctxt_data d
      then
        let (fix, _) = Hashtbl.find cx.ctxt_data d in
          fix
      else
        let fix = new_fixup "data item fixup" in
        let frag = Asm.DEF (fix, thunk ()) in
          htab_put cx.ctxt_data d (fix, frag);
          fix
    in
      (* FIXME (bug 541552): wrong operand type. *)
      Il.Imm (Asm.M_POS fix, word_ty_mach)

  and trans_static_string (s:string) : Il.operand =
    trans_data_frag (DATA_str s) (fun _ -> Asm.ZSTRING s)

  and trans_type_info (t:Ast.ty) : Il.operand =
    (* FIXME: emit type-info table here. *)
    trans_data_frag (DATA_typeinfo t) (fun _ -> Asm.MARK)

  and trans_init_str (dst:Ast.lval) (s:string) : unit =
    (* Include null byte. *)
    let init_sz = Int64.of_int ((String.length s) + 1) in
    let static = trans_static_string s in
    let (dst, _) = trans_lval dst INTENT_init in
      trans_upcall "upcall_new_str" dst [| static; imm init_sz |]

  and trans_atom (atom:Ast.atom) : Il.operand =
    iflog
      begin
        fun _ ->
          annotate (Ast.fmt_to_str Ast.fmt_atom atom)
      end;

    match atom with
        Ast.ATOM_lval lv ->
          let (cell, slot) = trans_lval lv INTENT_read in
            Il.Cell (deref_slot cell slot INTENT_read)

      | Ast.ATOM_literal lit ->
          begin
            match lit.node with
                (* FIXME (bug 541565): handle nil properly. *)
                Ast.LIT_nil -> imm_false
              | Ast.LIT_bool false -> imm_false
              | Ast.LIT_bool true -> imm_true
                (* FIXME (bug 541566): handle char as exactly 32 bits, not word size. *)
              | Ast.LIT_char c -> imm (Int64.of_int (Char.code c))
              | Ast.LIT_int (i, _) -> imm i
              | Ast.LIT_mach (m, n, _) -> imm_of_ty n m

              | _ -> marker
          end

  and fixup_to_addr
      (abs_ok:bool)
      (fix:fixup)
      (referent_ty:Il.referent_ty)
      : Il.addr =
    let i = Asm.M_POS fix in
      if abs_ok
      then Il.Abs i
      else
        let ta = (Il.Abs i, referent_ty) in
        let (reg, _) = force_to_reg (alias ta) in
          Il.RegIn (reg, None)

  and annotate_quads (name:string) : unit =
    let e = emitter() in
    let quads = e.Il.emit_quads in
    let annotations = annotations() in
    log cx "emitted quads for %s:" name;
    for i = 0 to arr_max quads
    do
      if Hashtbl.mem annotations i
      then
        List.iter
          (fun a -> log cx "// %s" a)
          (List.rev (Hashtbl.find_all annotations i));
      log cx "[%6d]\t%s" i (Il.string_of_quad abi.Abi.abi_str_of_hardreg quads.(i));
    done

  and trans_glue_frame_entry (callsz:int64) (spill:fixup) : unit =
    let framesz = 0L in
      push_new_emitter ();
      iflog (fun _ -> annotate "prologue");
      abi.Abi.abi_emit_fn_prologue (emitter()) framesz spill callsz nabi_rust (upcall_fixup "upcall_grow_proc");
      iflog (fun _ -> annotate "finished prologue");

  and capture_emitted_glue (name:string) (fix:fixup) (spill:fixup) (g:glue) : unit =
    let e = emitter() in
      iflog (fun _ -> annotate_quads name);
      let code = { code_fixup = fix;
                   code_quads = e.Il.emit_quads;
                   code_vregs_and_spill = Some (e.Il.emit_next_vreg, spill) }
      in
        htab_put cx.ctxt_glue_code g code

  and trans_glue_frame_exit (name:string) (fix:fixup) (spill:fixup) (g:glue) : unit =
    iflog (fun _ -> annotate "epilogue");
    abi.Abi.abi_emit_fn_epilogue (emitter());
    capture_emitted_glue name fix spill g;
    pop_emitter ()

  and emit_exit_proc_glue (tsig:Ast.ty_sig) (fix:fixup) (g:glue) : unit =
    let spill = new_fixup "proc glue spill" in
      push_new_emitter ();
      (* 
       * We return-to-here in a synthetic frame we did not build; our job is
       * to drop the slots associated with tsig, which are already on the stack
       * as though we put them there in a call, then assume the 'exited' state.
       *)
      let arg_layouts = layout_fn_call_tup abi tsig in
      let in_slots = tsig.Ast.sig_input_slots in
        drop_arg_slots in_slots arg_layouts;
        iflog (fun _ -> annotate "assume 'exited' state");
        trans_void_upcall "upcall_exit" [| |];
        capture_emitted_glue "proc glue" fix spill g;
        pop_emitter ()

  and get_exit_proc_glue (tsig:Ast.ty_sig) : fixup =
    let g = GLUE_exit_proc tsig in
      match htab_search cx.ctxt_glue_code g with
          Some code -> code.code_fixup
        | None ->
            let fix = new_fixup "proc glue" in
              emit_exit_proc_glue tsig fix g;
              fix

  (* 
   * Mem-glue functions are either 'mark', 'drop' or 'free', they take
   * one pointer arg and reutrn nothing.
   *)

  and trans_mem_glue_frame_entry (n_outgoing_args:int) (spill:fixup) : unit =
    let isz = cx.ctxt_abi.Abi.abi_implicit_args_sz in
    let callsz = Int64.add isz (word_n n_outgoing_args) in
      trans_glue_frame_entry callsz spill

  and get_mem_glue (g:glue) (prefix:unit -> string) (inner:Il.addr -> unit) : fixup =
    match htab_search cx.ctxt_glue_code g with
        Some code -> code.code_fixup
      | None ->
          begin
            let prefix = prefix () in
            let fix = new_fixup ("glue: " ^ prefix) in
              (* 
               * Put a temporary code entry in the table to handle
               * recursive emit calls during the generation of the glue
               * function.
               *)
            let tmp_code = { code_fixup = fix;
                             code_quads = [| |];
                             code_vregs_and_spill = None } in
            let spill = new_fixup ("glue spill: " ^ prefix) in
              htab_put cx.ctxt_glue_code g tmp_code;
              trans_mem_glue_frame_entry 1 spill;
              let (arg:Il.addr) = fp_imm arg0_disp in
                inner arg;
                Hashtbl.remove cx.ctxt_glue_code g;
                trans_glue_frame_exit ("glue: " ^ prefix) fix spill g;
                fix
          end

  and get_typed_mem_glue (g:glue) (ty:Ast.ty) (prefix:unit -> string) (inner:Il.typed_addr -> unit) : fixup =
    get_mem_glue g prefix (fun addr -> inner (ptr_at addr ty))

  and trace_str b s =
    if b
    then
      begin
        let static = trans_static_string s in
          trans_void_upcall "upcall_trace_str" [| static |]
      end

  and trace_word b w =
    if b
    then
      trans_void_upcall "upcall_trace_word" [| Il.Cell w |]

  and get_drop_glue
      (ty:Ast.ty)
      (curr_iso:Ast.ty_iso option)
      : fixup =
    let g = GLUE_drop ty in
    let prefix _ = "drop " ^ (Ast.fmt_to_str Ast.fmt_ty ty) in
    let inner (arg:Il.typed_addr) =
      trace_str cx.ctxt_sess.Session.sess_trace_drop
        "in drop-glue, dropping";
      trace_word cx.ctxt_sess.Session.sess_trace_drop (Il.Addr arg);
      drop_ty ty (deref (Il.Addr arg)) curr_iso;
      trace_str cx.ctxt_sess.Session.sess_trace_drop
        "drop-glue complete";
    in
      get_typed_mem_glue g ty prefix inner

  and get_free_glue
      (ty:Ast.ty)
      (mctrl:mem_ctrl)
      (curr_iso:Ast.ty_iso option)
      : fixup =
    let g = GLUE_free ty in
    let prefix _ = "free " ^ (Ast.fmt_to_str Ast.fmt_ty ty) in
    let inner (arg:Il.typed_addr) =
      (* 
       * Free-glue assumes we're looking at a pointer to an 
       * exterior allocation with normal exterior layout. It's
       * just a way to move drop+free out of leaf code. 
       *)
      let (body_addr, _) = deref_imm (Il.Addr arg) exterior_rc_body_off in
      let vr = Il.next_vreg_cell (emitter()) Il.voidptr_t in
        lea vr body_addr;
        trace_str cx.ctxt_sess.Session.sess_trace_drop
          "in free-glue, calling drop-glue";
        trace_word cx.ctxt_sess.Session.sess_trace_drop vr;
        trans_call_mem_glue (get_drop_glue ty curr_iso) vr;
        trace_str cx.ctxt_sess.Session.sess_trace_drop
          "back in free-glue, calling free";
        if mctrl = MEM_gc
        then
          begin
            emit (Il.binary Il.SUB vr (Il.Cell vr)
                    (imm
                       (Int64.add exterior_rc_body_off
                          (word_n Abi.exterior_gc_malloc_return_adjustment))));
            trans_free vr
          end
        else
          trans_free (Il.Addr arg);
        trace_str cx.ctxt_sess.Session.sess_trace_drop
          "free-glue complete";
    in
      get_typed_mem_glue g ty prefix inner


  and get_mark_glue
      (ty:Ast.ty)
      (curr_iso:Ast.ty_iso option)
      : fixup =
    let g = GLUE_mark ty in
    let prefix _ = "mark " ^ (Ast.fmt_to_str Ast.fmt_ty ty) in
    let inner (arg:Il.typed_addr) = mark_ty ty (deref (Il.Addr arg)) curr_iso in
    let fix = get_typed_mem_glue g ty prefix inner in
      fix


  and trans_call_mem_glue (fix:fixup) (arg:Il.cell) : unit =
    let code = Il.CodeAddr (fixup_to_addr
                              abi.Abi.abi_has_abs_code
                              fix Il.OpaqueTy)
    in
    let tsig = { Ast.sig_input_slots = [| interior_slot (Ast.TY_mach word_ty_mach) |];
                 Ast.sig_input_constrs = [| |];
                 Ast.sig_output_slot = interior_slot Ast.TY_nil }
    in
    let arg_layouts = layout_fn_call_tup abi tsig in
    let arg_n n = sp_imm arg_layouts.(n).layout_offset in
    let arg_n_cell n = Il.Addr (arg_n n, Il.OpaqueTy) in
      (* Arg0 is omitted as there's no output-slot for gc glue. *)
      (* Arg1 is process-pointer, as usual. *)
      (* Arg2 is the sole pointer we pass in. Hard-wire its address here. *)
      mov (arg_n_cell 1) (Il.Cell abi.Abi.abi_pp_cell);
      mov (arg_n_cell 2) (Il.Cell arg);
      call_code code;

  (* trans_compare returns a quad number of the cjmp, which the caller
     patches to the cjmp destination.  *)
  and trans_compare
      (cjmp:Il.jmpop)
      (lhs:Il.operand)
      (rhs:Il.operand)
      : quad_idx list =
    (* FIXME: this is an x86-ism; abstract via ABI. *)
    emit (Il.cmp (Il.Cell (Il.Reg (force_to_reg lhs))) rhs);
    let jmp = mark() in
      emit (Il.jmp cjmp Il.CodeNone);
      [jmp]

  and trans_cond (invert:bool) (expr:Ast.expr) : quad_idx list =

    let anno _ =
      iflog
        begin
          fun _ ->
            annotate ((Ast.fmt_to_str Ast.fmt_expr expr) ^
                        ": cond, finale")
        end
    in

    match expr with
        Ast.EXPR_binary (binop, a, b) ->
          let lhs = trans_atom a in
          let rhs = trans_atom b in
          let cjmp = binop_to_jmpop binop in
          let cjmp' =
            if invert then
              match cjmp with
                  Il.JE -> Il.JNE
                | Il.JNE -> Il.JE
                | Il.JL -> Il.JGE
                | Il.JLE -> Il.JG
                | Il.JGE -> Il.JL
                | Il.JG -> Il.JLE
                | _ -> bug () "Unhandled inverse binop in trans_cond"
            else
              cjmp
          in
            anno ();
            trans_compare cjmp' lhs rhs

      | _ ->
          let bool_operand = trans_expr expr in
            anno ();
            trans_compare Il.JNE bool_operand
              (if invert then imm_true else imm_false)

  and trans_binary
      (binop:Ast.binop)
      (lhs:Il.operand)
      (rhs:Il.operand) : Il.operand =
    let arith op =
      let size = Il.operand_size lhs word_bits in
      let dst = Il.Reg (Il.next_vreg (emitter()), Il.ValTy size) in
        emit (Il.binary op dst lhs rhs);
        Il.Cell dst
    in
    match binop with
        Ast.BINOP_or -> arith Il.OR
      | Ast.BINOP_and -> arith Il.AND

      | Ast.BINOP_lsl -> arith Il.LSL
      | Ast.BINOP_lsr -> arith Il.LSR
      | Ast.BINOP_asr -> arith Il.ASR

      | Ast.BINOP_add -> arith Il.ADD
      | Ast.BINOP_sub -> arith Il.SUB

      (* FIXME (bug 541544): switch on type of operands, IMUL/IDIV/IMOD etc. *)
      | Ast.BINOP_mul -> arith Il.UMUL
      | Ast.BINOP_div -> arith Il.UDIV
      | Ast.BINOP_mod -> arith Il.UMOD

      | _ -> let dst = Il.Reg (Il.next_vreg (emitter()), Il.ValTy word_bits) in
          mov dst imm_true;
          let jmps = trans_compare (binop_to_jmpop binop) lhs rhs in
            mov dst imm_false;
            List.iter patch jmps;
            Il.Cell dst


  and trans_expr (expr:Ast.expr) : Il.operand =

    let anno _ =
      iflog
        begin
          fun _ ->
            annotate ((Ast.fmt_to_str Ast.fmt_expr expr) ^
                        ": plain exit, finale")
        end
    in

    match expr with

        Ast.EXPR_binary (binop, a, b) ->
          trans_binary binop (trans_atom a) (trans_atom b)

      | Ast.EXPR_unary (unop, a) ->
          let src = trans_atom a in
            (* FIXME (bug 541544): this has to change when we support other mach types. *)
          let dst = Il.Reg (Il.next_vreg (emitter()), Il.ValTy word_bits) in
          let op = match unop with
              Ast.UNOP_not -> Il.NOT
            | Ast.UNOP_neg -> Il.NEG
          in
            anno ();
            emit (Il.unary op dst src);
            Il.Cell dst

      | Ast.EXPR_atom a ->
          trans_atom a

  and trans_block (block:Ast.block) : unit =
    Stack.push (get_block_layout cx block.id) block_layouts;
    trace_str cx.ctxt_sess.Session.sess_trace_block
      "entering block";
    emit (Il.Enter (Hashtbl.find cx.ctxt_block_fixups block.id));
    Array.iter trans_stmt block.node;
    trace_str cx.ctxt_sess.Session.sess_trace_block
      "exiting block";
    iter_block_slots block.id
      begin
        fun slotkey slot_id slot ->
          (* FIXME (bug 541543): this is not going to free things in
           * the proper order; we need to analyze the decl order in an
           * earlier phase and thread it through to here.  *)
          iflog
            begin
              fun _ ->
                annotate
                  ("drop slot: " ^
                     (Ast.fmt_to_str Ast.fmt_slot_key slotkey))
            end;
          trace_str cx.ctxt_sess.Session.sess_trace_drop
            ("dropping slot " ^ (Ast.fmt_to_str Ast.fmt_slot_key slotkey));
          let cell = cell_of_block_slot slot_id in
            drop_slot cell slot None
      end;
    emit Il.Leave;
    ignore (Stack.pop block_layouts);
    trace_str cx.ctxt_sess.Session.sess_trace_block
      "exited block";

  and trans_native_thunk (nabi:Abi.nabi) (lib:import_lib) (name:string) (ret:Il.cell) (args:Il.operand array) : unit =
    abi.Abi.abi_emit_native_call_in_thunk (emitter()) ret nabi (Semant.import cx lib name) args;

  and upcall_fixup (name:string) : fixup =
    Semant.import cx LIB_rustrt name;

  and trans_upcall (name:string) (ret:Il.cell) (args:Il.operand array) : unit =
    abi.Abi.abi_emit_native_call (emitter()) ret nabi_rust (upcall_fixup name) args;

  and trans_void_upcall (name:string) (args:Il.operand array) : unit =
    abi.Abi.abi_emit_native_void_call (emitter()) nabi_rust (upcall_fixup name) args;

  and trans_log_int (a:Ast.atom) : unit =
    trans_void_upcall "upcall_log_int" [| (trans_atom a) |]

  and trans_log_str (a:Ast.atom) : unit =
    trans_void_upcall "upcall_log_str" [| (trans_atom a) |]

  and trans_spawn
      ((*initialising*)_:bool)
      (dst:Ast.lval)
      (fn_lval:Ast.lval)
      (args:Ast.atom array)
      : unit =
    let (proc_cell, _) = trans_lval dst INTENT_init in
    let (fn_cell, fn_slot) = trans_lval fn_lval INTENT_read in
    let tsig =
      match fn_slot.Ast.slot_ty with
          Some (Ast.TY_fn (tsig, _)) -> tsig
        | _ -> bug () "spawned-function slot has wrong type"
    in
    let in_slots = tsig.Ast.sig_input_slots in
    let arg_layouts = layout_fn_call_tup abi tsig in
    let callsz = (pack 0L arg_layouts).layout_size in
    let exit_proc_glue_fixup = get_exit_proc_glue tsig in
    let exit_proc_glue_addr = (fixup_to_addr
                                 abi.Abi.abi_has_abs_data
                                 exit_proc_glue_fixup Il.CodeTy) in
    let exit_proc_glue_cell = Il.Addr (exit_proc_glue_addr, Il.CodeTy) in

      iflog (fun _ -> annotate "spawn proc: copy args");
      copy_fn_args proc_cell in_slots arg_layouts args [||];
      iflog (fun _ -> annotate "spawn proc: upcall");
      trans_upcall "upcall_new_proc" proc_cell
        [|
          alias_cell exit_proc_glue_cell;
          alias_cell fn_cell;
          imm callsz |];
      ()

  and trans_cond_fail (str:string) (fwd_jmps:quad_idx list) : unit =
    let (filename, line, _) =
      match !curr_stmt with
          None -> ("<none>", 0, 0)
        | Some stmt_id ->
            match (Session.get_span cx.ctxt_sess stmt_id) with
                None -> ("<none>", 0, 0)
              | Some sp -> sp.lo
    in
      iflog (fun _ -> annotate ("condition-fail: " ^ str));
      trans_void_upcall "upcall_fail"
        [|
          trans_static_string str;
          trans_static_string filename;
          imm (Int64.of_int line)
        |];
      List.iter patch fwd_jmps

  and trans_check_expr (e:Ast.expr) : unit =
    let fwd_jmps = trans_cond false e in
      trans_cond_fail (Ast.fmt_to_str Ast.fmt_expr e) fwd_jmps

  and trans_malloc (dst:Il.cell) (nbytes:int64) : unit =
    trans_upcall "upcall_malloc" dst [| imm nbytes |]

  and trans_free (src:Il.cell) : unit =
    trans_void_upcall "upcall_free" [| Il.Cell src |]

  and trans_send (chan:Ast.lval) (src:Ast.lval) : unit =
    let (srccell, _) = trans_lval src INTENT_read in
      aliasing false srccell
        begin
          fun src_alias ->
            trans_void_upcall "upcall_send" [| trans_atom (Ast.ATOM_lval chan); src_alias |];
        end

  and trans_recv (initialising:bool) (dst:Ast.lval) (chan:Ast.lval) : unit =
    let intent = if initialising then INTENT_init else INTENT_write in
    let (dstcell, _) = trans_lval dst intent in
      aliasing true dstcell
        begin
          fun dst_alias ->
            trans_void_upcall "upcall_recv" [| dst_alias; trans_atom (Ast.ATOM_lval chan) |];
        end

  and trans_init_port (dst:Ast.lval) : unit =
    let (dstcell, dst_slot) = trans_lval dst INTENT_init in
    let unit_ty = match slot_ty dst_slot with
        Ast.TY_port t -> t
      | _ -> bug () "init dst of port-init has non-port type"
    in
    let unit_sz = ty_sz abi unit_ty in
      trans_upcall "upcall_new_port" dstcell [| imm unit_sz |]

  and trans_del_port (port:Il.cell) : unit =
    trans_void_upcall "upcall_del_port" [| Il.Cell port |]

  and trans_kill_proc (proc:Il.cell) : unit =
    trans_void_upcall "upcall_kill" [| Il.Cell proc |]

  (*
   * A vec is implicitly exterior: every slot vec[T] is 1 word and
   * points to a refcounted structure. That structure has 3 words with
   * defined meaning at the beginning; data follows the header.
   *
   *   word 0: refcount or gc control word
   *   word 1: allocated size of data
   *   word 2: initialised size of data
   *   word 3...N: data
   * 
   * This 3-word prefix is shared with strings, we factor the common
   * part out for reuse in string code.
   *)

  and trans_init_vec (dst:Ast.lval) (atoms:Ast.atom array) : unit =
    let (dstcell, dst_slot) = trans_lval dst INTENT_init in
    let unit_slot = match slot_ty dst_slot with
        Ast.TY_vec s -> s
      | _ -> bug () "init dst of vec-init has non-vec type"
    in
    let unit_sz = slot_sz abi unit_slot in
    let n_inits = Array.length atoms in
    let init_sz = Int64.mul unit_sz (Int64.of_int n_inits) in
    let padded_sz = Int64.add init_sz (word_n 3) in
    let alloc_sz = next_power_of_two padded_sz in
      trans_malloc dstcell alloc_sz;
      mov (Il.Addr (deref_imm dstcell (word_n 0))) one;
      mov (Il.Addr (deref_imm dstcell (word_n 1))) (imm alloc_sz);
      mov (Il.Addr (deref_imm dstcell (word_n 2))) (imm init_sz);
      Array.iteri
        begin
          fun i atom ->
            let off = Int64.add (word_n 3) (Int64.mul (Int64.of_int i) unit_sz) in
            let cell = Il.Addr (deref_imm dstcell off) in
              trans_init_slot_from_atom cell unit_slot atom
        end
        atoms

  and next_power_of_two (x:int64) : int64 =
    let xr = ref (Int64.sub x 1L) in
      xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 1);
      xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 2);
      xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 4);
      xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 8);
      xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 16);
      xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 32);
      Int64.add 1L (!xr)

  and exterior_rc_body_off : int64 = word_n Abi.exterior_rc_slot_field_body
  and exterior_gc_body_off : int64 = word_n Abi.exterior_gc_slot_field_body

  and exterior_ctrl_cell (cell:Il.cell) (off:int) : Il.cell =
    let (rc_addr, _) = deref_imm cell (word_n off) in
    word_at rc_addr

  and exterior_rc_cell (cell:Il.cell) : Il.cell =
    exterior_ctrl_cell cell Abi.exterior_rc_slot_field_refcnt

  and exterior_gc_ctrl_cell (cell:Il.cell) : Il.cell =
    exterior_ctrl_cell cell Abi.exterior_gc_slot_field_ctrl

  and exterior_gc_next_cell (cell:Il.cell) : Il.cell =
    exterior_ctrl_cell cell Abi.exterior_gc_slot_field_next

  and exterior_gc_allocation_size (slot:Ast.slot) : int64 =
    let layout = layout_ty abi 0L (slot_ty slot) in
      (Int64.add layout.layout_size (word_n Abi.exterior_gc_header_size))

  and exterior_rc_allocation_size (slot:Ast.slot) : int64 =
    let layout = layout_ty abi 0L (slot_ty slot) in
      (Int64.add layout.layout_size (word_n Abi.exterior_rc_header_size))


  and ty_is_structured (t:Ast.ty) : bool =
    let fold = ty_fold_bool_or false in
    let fold = { fold with
                   ty_fold_tup = (fun _ -> true);
                   ty_fold_vec = (fun _ -> true);
                   ty_fold_rec = (fun _ -> true);
                   ty_fold_tag = (fun _ -> true);
                   ty_fold_iso = (fun _ -> true);
                   ty_fold_idx = (fun _ -> true) }
    in
      fold_ty fold t


  and slot_mem_ctrl (slot:Ast.slot) : mem_ctrl =
    let ty = slot_ty slot in
      if type_is_mutable ty
      then
        match slot.Ast.slot_mode with
            Ast.MODE_exterior _ -> MEM_gc
          | _ -> MEM_interior
      else
        match ty with
            Ast.TY_port _ -> MEM_rc_opaque Abi.port_field_refcnt
          | Ast.TY_chan _ -> MEM_rc_opaque Abi.chan_field_refcnt
          | Ast.TY_proc -> MEM_rc_opaque Abi.proc_field_refcnt
              (* Vecs and strs are pseudo-exterior. *)
          | Ast.TY_vec _ -> MEM_rc_struct
          | Ast.TY_str -> MEM_rc_opaque Abi.exterior_rc_slot_field_refcnt
          | _ ->
              match slot.Ast.slot_mode with
                  Ast.MODE_exterior _ when ty_is_structured (slot_ty slot) ->
                    MEM_rc_struct
                | Ast.MODE_exterior _ ->
                    MEM_rc_opaque Abi.exterior_rc_slot_field_refcnt
                | _ ->
                    MEM_interior

  and iter_rec_slots
      (ta:Il.typed_addr)
      (entries:Ast.ty_rec)
      (f:Il.cell -> Ast.slot -> (Ast.ty_iso option) -> unit)
      (curr_iso:Ast.ty_iso option)
      : unit =
    let (addr, t) = ta in
    let layouts = layout_rec abi entries in
      begin
        match t with
            Il.StructTy elts ->
              assert (Array.length layouts = Array.length elts)
          | _ -> bug () "iter_rec_slots of non-struct referent ty"
      end;
      Array.iteri
        begin
          fun i (_, (_, layout)) ->
            let (_, sub_slot) = entries.(i) in
            let disp = layout.layout_offset in
            let cell = wordptr_at (addr_add_imm addr disp) in
              f cell sub_slot curr_iso
        end
        layouts

  and iter_tup_slots
      (ta:Il.typed_addr)
      (slots:Ast.ty_tup)
      (f:Il.cell -> Ast.slot -> (Ast.ty_iso option) -> unit)
      (curr_iso:Ast.ty_iso option)
      : unit =
    let (addr, t) = ta in
    let layouts = layout_tup abi slots in
      begin
        match t with
            Il.StructTy elts ->
              assert (Array.length layouts = Array.length elts)
          | _ -> bug () "iter_tup_slots of non-struct referent ty"
      end;
      Array.iteri
        begin
          fun i layout ->
            let sub_slot = slots.(i) in
            let disp = layout.layout_offset in
            let cell = wordptr_at (addr_add_imm addr disp) in
              f cell sub_slot curr_iso
        end
        layouts

  and iter_tag_slots
        (ta:Il.typed_addr)
        (ttag:Ast.ty_tag)
        (f:Il.cell -> Ast.slot -> (Ast.ty_iso option) -> unit)
        (curr_iso:Ast.ty_iso option)
        : unit =
    let tag_keys = sorted_htab_keys ttag in
    let (tag:Il.cell) = Il.Reg (next_vreg(), word_ty) in
    let (addr, _) = ta in
    let tup_addr = addr_add_imm addr word_sz in
      mov tag (Il.Cell (word_at addr));
      for i = 0 to arr_max tag_keys do
        (* FIXME (bug 541540): A table-switch would be better. *)
        (iflog (fun _ -> annotate ("tag case #" ^ (string_of_int i))));
        let jmps = trans_compare Il.JNE (Il.Cell tag) (imm (Int64.of_int i)) in
        let ttup = Hashtbl.find ttag tag_keys.(i) in
          iter_tup_slots (tup_addr, referent_type abi (Ast.TY_tup ttup)) ttup f curr_iso;
          List.iter patch jmps
      done

  and get_iso_tag tiso =
    tiso.Ast.iso_group.(tiso.Ast.iso_index)

  and iter_ty_slots
        (ty:Ast.ty)
        (addr:Il.typed_addr)
        (f:Il.cell -> Ast.slot -> (Ast.ty_iso option) -> unit)
        (curr_iso:Ast.ty_iso option)
        : unit =
        (* 
         * FIXME: this will require some reworking if we support
         * rec, tag or tup slots that fit in a vreg. It requires 
         * addrs presently.
         *)
        match ty with
            Ast.TY_rec entries -> iter_rec_slots addr entries f curr_iso
          | Ast.TY_tup slots -> iter_tup_slots addr slots f curr_iso
          | Ast.TY_tag tag -> iter_tag_slots addr tag f curr_iso
          | Ast.TY_iso tiso ->
              let ttag = get_iso_tag tiso in
                iter_tag_slots addr ttag f (Some tiso)
          | _ -> ()

  and drop_ty
      (ty:Ast.ty)
      (addr:Il.typed_addr)
      (curr_iso:Ast.ty_iso option)
      : unit =
      iter_ty_slots ty addr drop_slot curr_iso

  and mark_ty
      (ty:Ast.ty)
      (addr:Il.typed_addr)
      (curr_iso:Ast.ty_iso option)
      : unit =
    iter_ty_slots ty addr mark_slot curr_iso

  and free_ty
        (ty:Ast.ty)
        (cell:Il.cell)
        : unit =
    match ty with
        Ast.TY_port _ -> trans_del_port cell
      | Ast.TY_chan _ -> trans_del_port cell
      | Ast.TY_proc -> trans_kill_proc cell
      | _ -> trans_free cell

  and maybe_iso
      (curr_iso:Ast.ty_iso option)
      (t:Ast.ty)
      : Ast.ty =
    match (curr_iso, t) with
        (Some iso, Ast.TY_idx n) ->
          Ast.TY_iso { iso with Ast.iso_index = n }
      | (None, Ast.TY_idx _) ->
          bug () "TY_idx outside TY_iso"
      | _ -> t

  and maybe_enter_iso
      (t:Ast.ty)
      (curr_iso:Ast.ty_iso option)
      : Ast.ty_iso option =
    match t with
        Ast.TY_iso tiso -> Some tiso
      | _ -> curr_iso

  and mark_slot
      (cell:Il.cell)
      (slot:Ast.slot)
      (curr_iso:Ast.ty_iso option)
      : unit =
    let ty = slot_ty slot in
      match slot_mem_ctrl slot with
          MEM_gc ->
            (iflog (fun _ -> annotate ("mark GC slot " ^
                                         (Ast.fmt_to_str Ast.fmt_slot slot))));
            log cx "marking MEM_gc slot: %a" Ast.sprintf_slot slot;
            emit (Il.cmp (Il.Cell cell) zero);
            let null_cell_jump = mark () in
              emit (Il.jmp Il.JE Il.CodeNone);
              let gc_word = exterior_gc_ctrl_cell cell in
              let tmp = Il.next_vreg_cell (emitter()) Il.voidptr_t in
                (* if this has been marked already, jump to exit.*)
                emit (Il.binary Il.AND tmp (Il.Cell gc_word) one);
                let already_marked_jump = mark () in
                  emit (Il.jmp Il.JNZ Il.CodeNone);
                  (* Set mark bit in allocation header. *)
                  emit (Il.binary Il.OR gc_word (Il.Cell gc_word) one);
                  (* Iterate over exterior slots marking outgoing links. *)
                  let (body_addr, _) = deref_imm cell exterior_gc_body_off in
                  let ty = maybe_iso curr_iso ty in
                  let curr_iso = maybe_enter_iso ty curr_iso in
                    lea tmp body_addr;
                    trans_call_mem_glue (get_mark_glue ty curr_iso) tmp;
                    patch null_cell_jump;
                    patch already_marked_jump

        | MEM_interior when ty_is_structured ty ->
            (iflog (fun _ -> annotate ("mark interior slot " ^
                                         (Ast.fmt_to_str Ast.fmt_slot slot))));
            let (addr, _) = need_addr_cell cell in
            let tmp = Il.next_vreg_cell (emitter()) Il.voidptr_t in
            let ty = maybe_iso curr_iso ty in
            let curr_iso = maybe_enter_iso ty curr_iso in
              lea tmp addr;
              trans_call_mem_glue (get_mark_glue ty curr_iso) tmp

        | _ -> ()

  and drop_slot
      (cell:Il.cell)
      (slot:Ast.slot)
      (curr_iso:Ast.ty_iso option)
      : unit =
    let null_check _ =
      emit (Il.cmp (Il.Cell cell) zero);
      let j = mark() in
        emit (Il.jmp Il.JE Il.CodeNone);
        j
    in
    let drop_refcount_and_cmp rc =
      (iflog (fun _ -> annotate ("drop refcount and maybe free slot " ^
                                   (Ast.fmt_to_str Ast.fmt_slot slot))));
      emit (Il.binary Il.SUB rc (Il.Cell rc) one);
      emit (Il.cmp (Il.Cell rc) zero);
      let j = mark () in
        emit (Il.jmp Il.JNE Il.CodeNone);
        j
    in
    let ty = slot_ty slot in
    let mctrl = slot_mem_ctrl slot in
      match mctrl with
          MEM_rc_opaque rc_off ->
            (* Refcounted opaque objects we handle without glue functions. *)
            let null_jmp = null_check () in
            let (rc_addr, _) = deref_imm cell (word_n rc_off) in
            let rc = word_at rc_addr in
            let j = drop_refcount_and_cmp rc in
              free_ty ty cell;
              (* Null the slot out to prevent double-free if the frame unwinds. *)
              mov cell zero;
              patch j;
              patch null_jmp

        | MEM_gc
        | MEM_rc_struct ->
            (* Refcounted "structured exterior" objects we handle via glue functions. *)

            (* 
             * 'GC memory' is treated similarly, just happens to have
             * an extra couple cells on the front.
             *)

            (* FIXME (bug 541542): check to see that the exterior has
             * further exterior members; if it doesn't we can elide the
             * call to the glue function.  *)
            let null_jmp = null_check () in
            let rc = exterior_rc_cell cell in
            let j = drop_refcount_and_cmp rc in
            let ty = maybe_iso curr_iso ty in
            let curr_iso = maybe_enter_iso ty curr_iso in
              trans_call_mem_glue (get_free_glue ty mctrl curr_iso) cell;
              (* Null the slot out to prevent double-free if the frame unwinds. *)
              mov cell zero;
              patch j;
              patch null_jmp

        | MEM_interior when ty_is_structured ty ->
            (iflog (fun _ -> annotate ("drop interior slot " ^
                                         (Ast.fmt_to_str Ast.fmt_slot slot))));
            let (addr, _) = need_addr_cell cell in
            let vr = Il.next_vreg_cell (emitter()) Il.voidptr_t in
            let ty = maybe_iso curr_iso ty in
            let curr_iso = maybe_enter_iso ty curr_iso in
              lea vr addr;
              trans_call_mem_glue (get_drop_glue ty curr_iso) vr

        | MEM_interior ->
            (* Interior allocation of all-interior value: nothing to do. *)
            ()

  and exterior_body_off (slot:Ast.slot) : int64 =
      match slot_mem_ctrl slot with
          MEM_gc -> exterior_gc_body_off
        | MEM_rc_struct -> exterior_rc_body_off
        | MEM_rc_opaque _
        | MEM_interior -> bug () "exterior_body_off of MEM_interior"

  (* Returns the offset of the slot-body in the initialized allocation. *)
  and init_exterior_slot (cell:Il.cell) (slot:Ast.slot) : unit =
      match slot_mem_ctrl slot with
          MEM_gc ->
            iflog (fun _ -> annotate "init GC exterior: malloc");
            let sz = exterior_gc_allocation_size slot in
              (* 
               * Malloc and then immediately shift down to point to
               * the pseudo-rc cell.
               *)
              trans_malloc cell sz;
              emit (Il.binary Il.ADD cell (Il.Cell cell)
                      (imm (word_n Abi.exterior_gc_malloc_return_adjustment)));

              iflog (fun _ -> annotate "init GC exterior: load control word");
              let ctrl = exterior_gc_ctrl_cell cell in
              let fix = get_drop_glue (slot_ty slot) None in
              let tmp = Il.next_vreg_cell (emitter()) Il.voidptr_t in
              let rc = exterior_rc_cell cell in
                mov rc one;
                lea tmp (Il.Abs (Asm.M_POS fix));
                mov ctrl (Il.Cell tmp);
                iflog (fun _ -> annotate "init GC exterior: load next-pointer");
                let next = exterior_gc_next_cell cell in
                  mov next (Il.Cell (Il.Addr (pp_imm (word_n Abi.proc_field_gc_alloc_chain))));

        | MEM_rc_opaque rc_off ->
            iflog (fun _ -> annotate "init RC exterior: malloc");
            let sz = exterior_rc_allocation_size slot in
              trans_malloc cell sz;
              iflog (fun _ -> annotate "init RC exterior: load refcount");
              let rc = exterior_ctrl_cell cell rc_off in
                mov rc one

        | MEM_rc_struct ->
            iflog (fun _ -> annotate "init RC exterior: malloc");
            let sz = exterior_rc_allocation_size slot in
              trans_malloc cell sz;
              iflog (fun _ -> annotate "init RC exterior: load refcount");
              let rc = exterior_rc_cell cell in
                mov rc one

        | MEM_interior -> bug () "init_exterior_slot of MEM_interior"

  and intent_str i =
    match i with
        INTENT_init -> "init"
      | INTENT_write -> "write"
      | INTENT_read -> "read"

  and deref_exterior (intent:intent) (cell:Il.cell) (slot:Ast.slot) : Il.typed_addr =
    let body_ty =
      match pointee_type cell with
          Il.StructTy parts
            when (Array.length parts == 2) &&
              (parts.(0) = Il.ScalarTy word_ty) -> parts.(1)
        | ty -> bug () "Dereferencing exterior cell with bad IL type: %s"
            (Il.string_of_referent_ty ty)
    in
      iflog (fun _ -> annotate ("deref exterior: " ^
                                  (intent_str intent) ^ ", " ^
                                  (Il.string_of_cell
                                     abi.Abi.abi_str_of_hardreg cell)));
      let (addr, _) =
        match intent with
            INTENT_init ->
              init_exterior_slot cell slot;
              deref_imm cell (exterior_body_off slot)

          | INTENT_write ->
              deref_imm cell (exterior_body_off slot)

          | INTENT_read ->
              deref_imm cell (exterior_body_off slot)
      in
        (addr, body_ty)


  and deref_slot (cell:Il.cell) (slot:Ast.slot) (intent:intent) : Il.cell =
    match slot.Ast.slot_mode with
        Ast.MODE_interior _ -> cell
      | Ast.MODE_exterior _ -> Il.Addr (deref_exterior intent cell slot)
      | Ast.MODE_read_alias
      | Ast.MODE_write_alias ->
          match intent with
              INTENT_init -> cell
            | _ -> Il.Addr (deref cell)


  and get_struct_referent_tys (t:Il.referent_ty) : Il.referent_ty array =
    match t with
        Il.StructTy tys -> tys
      | Il.ScalarTy _ -> failwith "expected structural referent type, got scalar"
      | Il.OpaqueTy -> failwith "expected structural referent type, got opaque"
      | Il.CodeTy -> failwith "expected structural referent type, got code"


  (* FIXME (bug 541541): this should permit f.e. copying (foo,bar) <-
   * (@foo,@bar), but presently it walks through both structures via
   * the layouts for the destination slots.  *)
  and trans_copy_structural
      (initialising:bool)
      (layouts:layout array)
      (dst_ta:Il.typed_addr) (dst_slots:Ast.ty_tup)
      (src_ta:Il.typed_addr) (src_slots:Ast.ty_tup)
      : unit =
    let (dst_addr, dst_rt) = dst_ta in
    let (src_addr, _) = src_ta in
    let dst_tys = get_struct_referent_tys dst_rt in
    let src_tys = get_struct_referent_tys dst_rt in
      assert (src_tys = dst_tys);
      assert (Array.length src_tys = Array.length layouts);
      Array.iteri
        begin
          fun i layout ->
            let disp = layout.layout_offset in
            let sub_dst = addr_add_imm dst_addr disp in
            let sub_src = addr_add_imm src_addr disp in
            let sub_dst_cell = Il.Addr (sub_dst, dst_tys.(i)) in
            let sub_src_cell = Il.Addr (sub_src, src_tys.(i)) in
              assert (slot_ty src_slots.(i) = slot_ty dst_slots.(i));
              trans_copy_slot
                initialising
                sub_dst_cell dst_slots.(i)
                sub_src_cell src_slots.(i)
        end
        layouts


  and trans_copy_rec
      (initialising:bool)
      (dst_ta:Il.typed_addr) (dst_entries:Ast.ty_rec)
      (src_ta:Il.typed_addr) (src_entries:Ast.ty_rec)
      : unit =
    let layouts = layout_rec abi dst_entries in
    let layouts' = Array.map (fun (_, (_, layout)) -> layout) layouts in
    let dst_slots = Array.map (fun (_, slot) -> slot) dst_entries in
    let src_slots = Array.map (fun (_, slot) -> slot) src_entries in
      trans_copy_structural
        initialising layouts'
        dst_ta dst_slots
        src_ta src_slots


  and trans_copy_tup
      (initialising:bool)
      (dst_ta:Il.typed_addr) (dst_slots:Ast.ty_tup)
      (src_ta:Il.typed_addr) (src_slots:Ast.ty_tup)
      : unit =
    let layouts = layout_tup abi dst_slots in
      trans_copy_structural
        initialising layouts
        dst_ta dst_slots
        src_ta src_slots


  and trans_copy_tag
      (initialising:bool)
      (dst_ta:Il.typed_addr) (dst_tag:Ast.ty_tag)
      (src_ta:Il.typed_addr) (src_tag:Ast.ty_tag)
      : unit =
    let tag_keys = sorted_htab_keys dst_tag in
    let (src_tag_val:Il.cell) = Il.Reg (next_vreg(), word_ty) in
    let (dst_addr, _) = dst_ta in
    let (src_addr, _) = src_ta in
    let dst_tup_addr = addr_add_imm dst_addr word_sz in
    let src_tup_addr = addr_add_imm src_addr word_sz in
      mov src_tag_val (Il.Cell (word_at src_addr));
      mov (word_at dst_addr) (Il.Cell src_tag_val);
      for i = 0 to arr_max tag_keys do
        (* FIXME (bug 541540): A table-switch would be better. *)
        (iflog (fun _ -> annotate ("tag case #" ^ (string_of_int i))));
        let jmps = trans_compare Il.JNE (Il.Cell src_tag_val) (imm (Int64.of_int i)) in
        let dst_ttup = Hashtbl.find dst_tag tag_keys.(i) in
        let src_ttup = Hashtbl.find src_tag tag_keys.(i) in
        let dst_rty = referent_type abi (Ast.TY_tup dst_ttup) in
        let src_rty = referent_type abi (Ast.TY_tup src_ttup) in
          trans_copy_tup
            initialising
            (dst_tup_addr, dst_rty) dst_ttup
            (src_tup_addr, src_rty) src_ttup;
          List.iter patch jmps
      done

  and trans_copy_pair
      (_(*initialising*):bool)
      (dst_ta:Il.typed_addr)
      (src_ta:Il.typed_addr)
      : unit =
    (* FIXME: adjust refcounts on non-null bound value carried along. *)
    let (dst_addr, _) = dst_ta in
    let (src_addr, _) = src_ta in
      mov (word_at dst_addr) (Il.Cell (word_at src_addr));
      let dst_addr = addr_add_imm dst_addr (word_n 1) in
      let src_addr = addr_add_imm src_addr (word_n 1) in
        mov (word_at dst_addr) (Il.Cell (word_at src_addr));

  and trans_copy_slot
      (initialising:bool)
      (dst:Il.cell) (dst_slot:Ast.slot)
      (src:Il.cell) (src_slot:Ast.slot)
      : unit =
    let anno (weight:string) : unit =
      iflog
        begin
          fun _ ->
            annotate
              (Printf.sprintf "%sweight copy: %a <- %a"
                 weight
                 Ast.sprintf_slot dst_slot
                 Ast.sprintf_slot src_slot)
        end;
    in
    let lightweight_rc src_rc =
      (* Lightweight copy: twiddle refcounts, move pointer. *)
      anno "refcounted light";
      emit (Il.binary Il.ADD src_rc (Il.Cell src_rc) one);
      if not initialising
      then
        drop_slot dst dst_slot None;
      mov dst (Il.Cell src)
    in

      assert (slot_ty src_slot = slot_ty dst_slot);
      match (slot_mem_ctrl src_slot,
             slot_mem_ctrl dst_slot) with
        | (MEM_rc_opaque src_rc_off, MEM_rc_opaque _) ->
            lightweight_rc (exterior_ctrl_cell src src_rc_off)

        | (MEM_gc, MEM_gc)
        | (MEM_rc_struct, MEM_rc_struct) ->
            lightweight_rc (exterior_rc_cell src)

      | _ ->
          (* Heavyweight copy: duplicate the referent. *)
          anno "heavy";
          trans_copy_slot_heavy initialising
            dst dst_slot src src_slot

  (* NB: heavyweight copying here does not mean "producing a deep
   * clone of the entire data tree rooted at the src operand". It means
   * "replicating a single level of the tree". You usually only need to
   * heavyweight copy if you're preparing to write to a sub-component
   * of an lval with refcount > 1.
   * 
   * If you're dereferencing a path x.y.z in order to write to the z
   * component, you may have to make 0..2 heavyweight copies:
   * 
   *   - let x' = if x.rc > 1 then heavy_copy(x) else x
   *   - let y' = if x'.y.rc > 1 then heavy_copy(x'.y) else x'.y
   *   - let z' = if y'.z.rc > 1 then heavy_copy(y'.z) else y'.z
   * 
   * There is no general-recursion entailed in performing a heavy
   * copy. There is only "one level" to each heavy copy call.
   * 
   * In other words, this is a lightweight copy:
   * 
   *    [dstptr]  <-copy-  [srcptr]
   *         \              |
   *          \             |
   *        [some record.rc++]
   *             |
   *           [some other record]
   * 
   * Whereas this is a heavyweight copy:
   * 
   *    [dstptr]  <-copy-  [srcptr]
   *       |                  |
   *       |                  |
   *  [some record]       [some record]
   *             |          |
   *           [some other record]
   * 
   *)

  and trans_copy_slot_heavy
      (initialising:bool)
      (dst:Il.cell) (dst_slot:Ast.slot)
      (src:Il.cell) (src_slot:Ast.slot)
      : unit =
    assert (slot_ty src_slot = slot_ty dst_slot);
    let dst_intent =
      if initialising
      then INTENT_init
      else INTENT_write
    in
      iflog (fun _ ->
               annotate ("heavy copy: slot preparation"));
      let dst = deref_slot dst dst_slot dst_intent in
      let src = deref_slot src src_slot INTENT_read in
        iflog (fun _ ->
                 annotate ("heavy copy: referent data"));

        match (dst, slot_ty dst_slot,
               src, slot_ty src_slot) with
            (Il.Addr dst_ta, Ast.TY_rec dst_entries,
             Il.Addr src_ta, Ast.TY_rec src_entries) ->
              trans_copy_rec
                initialising
                dst_ta dst_entries
                src_ta src_entries

          | (Il.Addr dst_ta, Ast.TY_tup dst_slots,
             Il.Addr src_ta, Ast.TY_tup src_slots) ->
              trans_copy_tup
                initialising
                dst_ta dst_slots
                src_ta src_slots

          | (Il.Addr dst_ta, Ast.TY_tag dst_tag,
             Il.Addr src_ta, Ast.TY_tag src_tag) ->
              trans_copy_tag
                initialising
                dst_ta dst_tag
                src_ta src_tag

          | (Il.Addr dst_ta, Ast.TY_iso dst_iso,
             Il.Addr src_ta, Ast.TY_iso src_iso) ->
              let src_tag = get_iso_tag src_iso in
              let dst_tag = get_iso_tag dst_iso in
                trans_copy_tag
                  initialising
                  dst_ta dst_tag
                  src_ta src_tag

          | (Il.Addr dst_ta, Ast.TY_fn _,
             Il.Addr src_ta, Ast.TY_fn _)
          | (Il.Addr dst_ta, Ast.TY_pred _,
             Il.Addr src_ta, Ast.TY_pred _)
          | (Il.Addr dst_ta, Ast.TY_mod _,
             Il.Addr src_ta, Ast.TY_mod _) ->
              (*
               * FIXME: will need to split out TY_mod when module type
               * conversion (thus structural rearrangement) is part of
               * 1st-class mod copying.
               *)
              trans_copy_pair
                initialising
                dst_ta src_ta

          | (_, t, _, _) when (i64_le (ty_sz abi t) word_sz) ->
              mov dst (Il.Cell src)

          | (_, t, _, _) ->
              bug () "unhandled form of heavyweight copy: %s" (Ast.fmt_to_str Ast.fmt_ty t)


  and trans_copy
      (initialising:bool)
      (dst:Ast.lval)
      (src:Ast.expr)
      (binop_opt:Ast.binop option) : unit =
    let dst_intent =
      if initialising
      then INTENT_init
      else INTENT_write
    in
    let (dst_cell, dst_slot) = trans_lval dst dst_intent in
      match binop_opt with
          None ->
            begin
              match src with
                  (Ast.EXPR_binary _)
                | (Ast.EXPR_unary _)
                | (Ast.EXPR_atom (Ast.ATOM_literal _)) ->
                    (*
                     * Translations of these expr types yield vregs,
                     * so copy is just MOV into the lval.
                     *)
                  let src_operand = trans_expr src in
                    mov (deref_slot dst_cell dst_slot INTENT_read) src_operand

              | Ast.EXPR_atom (Ast.ATOM_lval src_lval) ->
                  (* Possibly-large structure copying *)
                  let (src_cell, src_slot) = trans_lval src_lval INTENT_read in
                    trans_copy_slot
                      initialising
                      dst_cell dst_slot
                      src_cell src_slot
          end
      | Some binop ->
          ignore (trans_binary binop
            (Il.Cell (deref_slot dst_cell dst_slot INTENT_read))
            (trans_expr src));
          ()


  and trans_init_structural_from_atoms
      (dst:Il.cell) (dst_slots:Ast.slot array)
      (atoms:Ast.atom array)
      : unit =
    let layouts = layout_tup abi dst_slots in
    assert (Array.length layouts == Array.length atoms);
    let (dst_addr, _) = need_addr_cell dst in
    Array.iteri
      begin
        fun i layout ->
          let disp = layout.layout_offset in
          let sub_dst = addr_add_imm dst_addr disp in
          let sub_slot = dst_slots.(i) in
          let sub_rtype = slot_referent_type abi sub_slot in
          let sub_dst_cell = Il.Addr (sub_dst, sub_rtype) in
            trans_init_slot_from_atom sub_dst_cell sub_slot atoms.(i)
      end
      layouts


  and trans_init_slot_from_atom
      (dst:Il.cell) (dst_slot:Ast.slot)
      (atom:Ast.atom)
      : unit =
    match atom with
      | Ast.ATOM_literal _ ->
          let src = trans_atom atom in
            begin
              match dst_slot.Ast.slot_mode with
                  Ast.MODE_read_alias
                | Ast.MODE_write_alias ->
                    mov dst (alias (force_to_mem src))
                | _ -> mov (deref_slot dst dst_slot INTENT_init) src
            end
      | Ast.ATOM_lval src_lval ->
          let (src, src_slot) = trans_lval src_lval INTENT_read in
            trans_init_slot_from_cell dst dst_slot src src_slot

  and trans_init_slot_from_cell
      (dst:Il.cell) (dst_slot:Ast.slot)
      (src:Il.cell) (src_slot:Ast.slot)
      : unit =
    assert (slot_ty src_slot = slot_ty dst_slot);
    match dst_slot.Ast.slot_mode with
        Ast.MODE_read_alias
      | Ast.MODE_write_alias -> mov dst (alias_cell src)
      | _ ->
          trans_copy_slot
            true
            dst dst_slot
            src src_slot

  and trans_call_fn
      (initialising:bool)
      (dst:Ast.lval)
      (flv:Ast.lval)
      (args:Ast.atom array)
      : unit =
    let (dst_cell, _) = trans_lval dst INTENT_init in
    let (fn_cell, fn_slot) =
      trans_lval_full flv
        abi.Abi.abi_has_abs_code
        INTENT_read
    in
    let tfn =
      match slot_ty fn_slot with
          Ast.TY_fn fty -> fty
        | _ -> bug () "Calling non-function."
    in
    let (tsig, _) = tfn in
    let in_slots = tsig.Ast.sig_input_slots in
    let arg_layouts = layout_fn_call_tup abi tsig in
      trans_call initialising (fun _ -> Ast.sprintf_lval () flv)
        dst_cell fn_cell in_slots arg_layouts args [||]

  and trans_call_pred_and_check
      (constr:Ast.constr)
      (flv:Ast.lval)
      (args:Ast.atom array)
      : unit =
    let dst_cell = Il.Addr (force_to_mem imm_false) in
    let (fn_cell, fn_slot) =
      trans_lval_full flv
        abi.Abi.abi_has_abs_code
        INTENT_read
    in
    let tpred =
      match slot_ty fn_slot with
          Ast.TY_pred tpred -> tpred
        | _ -> bug () "Calling non-predicate."
    in
    let (in_slots, _) = tpred in
    let arg_layouts = layout_pred_call_tup abi tpred in
      iflog (fun _ -> annotate "predicate call");
      trans_call true (fun _ -> Ast.sprintf_lval () flv)
        dst_cell fn_cell in_slots arg_layouts args [||];
      iflog (fun _ -> annotate "predicate check/fail");
      let jmp = trans_compare Il.JE (Il.Cell dst_cell) imm_true in
      let errstr = Printf.sprintf "predicate check: %a"
        Ast.sprintf_constr constr
      in
        trans_cond_fail errstr jmp


  and trans_arg0 (arg_cell:Il.cell) (output_cell:Il.cell) : unit =
    (* Emit arg0 of any call: the output slot. *)
    iflog (fun _ -> annotate "fn-call arg 0: output slot");
    trans_init_slot_from_cell
      arg_cell (word_write_alias_slot abi)
      output_cell (word_slot abi)

  and trans_arg1 (arg_cell:Il.cell) : unit =
    (* Emit arg1 or any call: the process pointer. *)
    iflog (fun _ -> annotate "fn-call arg 1: process pointer");
    trans_init_slot_from_cell
      arg_cell (word_slot abi)
      abi.Abi.abi_pp_cell (word_slot abi)

  and trans_argN (arg_cell:Il.cell) (arg_slot:Ast.slot) (arg:Ast.atom) : unit =
    trans_init_slot_from_atom
      arg_cell arg_slot
      arg

  and code_of_cell (cell:Il.cell) : Il.code =
    match cell with
        Il.Addr (a, _) -> Il.CodeAddr a
      | _ -> bug () "loading code from register"

  and copy_fn_args
      (output_cell:Il.cell)
      (arg_slots:Ast.slot array)
      (arg_layouts:layout array)
      (args:Ast.atom array)
      (extra_args:Il.operand array)
      : unit =
    assert (Array.length args == Array.length arg_slots);
    let n_layouts = Array.length arg_layouts in
    let n_extras = Array.length extra_args in
      assert (n_layouts == ((Array.length args) + 2));
      trans_arg0 (implicit_arg_cell arg_layouts 0) output_cell;
      trans_arg1 (implicit_arg_cell arg_layouts 1);
      Array.iteri
        begin
          fun i slot ->
            iflog (fun _ ->
                     annotate
                       (Printf.sprintf "fn-call arg %d of %d (+ %d extra)"
                          i n_layouts n_extras));
            trans_argN (arg_cell arg_slots arg_layouts i) slot args.(i)
        end
        arg_slots;
      Array.iteri
        begin
          fun i operand ->
            iflog (fun _ ->
                     annotate (Printf.sprintf "fn-call extra-arg %d of %d"
                                 i n_extras));
            mov (extra_arg_cell arg_layouts i) operand
        end
        extra_args

  and call_code (code:Il.code) : unit =
    let vr = Il.next_vreg_cell (emitter()) Il.voidptr_t in
      emit (Il.call vr code);

  and trans_call
      ((*initialising*)_:bool)
      (logname:(unit -> string))
      (output_cell:Il.cell)
      (callee_cell:Il.cell)
      (arg_slots:Ast.slot array)
      (arg_layouts:layout array)
      (args:Ast.atom array)
      (extra_args:Il.operand array)
      : unit =
      iflog (fun _ -> annotate (Printf.sprintf "copy args for call to %s" (logname ())));
      copy_fn_args output_cell arg_slots arg_layouts args  extra_args;
      iflog (fun _ -> annotate (Printf.sprintf "call %s" (logname ())));
      (* FIXME (bug 541535 ): we need to actually handle writing to an
       * already-initialised slot. Currently we blindly assume we're
       * initialising, overwrite the slot; this is ok if we're writing
       * to an interior output slot, but we'll leak any exteriors as we
       * do that.  *)
      call_code (code_of_cell callee_cell);
      drop_arg_slots arg_slots arg_layouts

  and implicit_arg_cell
      (arg_layouts:layout array)
      (arg:int)
      : Il.cell =
    assert (arg < 2);
    let arg_addr = sp_imm arg_layouts.(arg).layout_offset in
    let arg_referent_ty = Il.ScalarTy (Il.voidptr_t) in
      Il.Addr (arg_addr, arg_referent_ty)

  and arg_cell
      (arg_slots:Ast.slot array)
      (arg_layouts:layout array)
      (arg:int)
      : Il.cell =
    (* NB: first 2 layouts account for implicit args. *)
    let arg_addr = sp_imm arg_layouts.(2 + arg).layout_offset in
    let arg_referent_ty = slot_referent_type abi arg_slots.(arg) in
      Il.Addr (arg_addr, arg_referent_ty)

  and extra_arg_cell
      (arg_layouts:layout array)
      (arg:int)
      : Il.cell =
    let n_args = Array.length arg_layouts in
    let extra_args_start =
      if n_args > 0
      then
        let last_arg_layout = arg_layouts.(n_args - 1) in
          Int64.add
            last_arg_layout.layout_offset
            last_arg_layout.layout_size
      else
        word_n 2
    in
    let arg_off = Int64.add extra_args_start (word_n arg) in
    let arg_addr = sp_imm arg_off in
    let arg_referent_ty = Il.ScalarTy (Il.voidptr_t) in
      Il.Addr (arg_addr, arg_referent_ty)

  and drop_arg_slots
      (arg_slots:Ast.slot array)
      (arg_layouts:layout array)
      : unit =
    for i = 0 to arr_max arg_slots do
      iflog (fun _ -> annotate (Printf.sprintf "drop arg %d" i));
      drop_slot (arg_cell arg_slots arg_layouts i) arg_slots.(i) None
    done


  and trans_stmt (stmt:Ast.stmt) : unit =
    (* Helper to localize errors by stmt, at minimum. *)
    try
      iflog
        begin
          fun _ ->
            annotate (Ast.fmt_to_str Ast.fmt_stmt_body stmt)
        end;
      curr_stmt := Some stmt.id;
      trans_stmt_full stmt;
      curr_stmt := None
    with
        Semant_err (None, msg) -> raise (Semant_err ((Some stmt.id), msg))


  and maybe_init (id:node_id) (action:string) (dst:Ast.lval) : bool =
    let b = Hashtbl.mem cx.ctxt_copy_stmt_is_init id in
    let act = if b then ("initialising-" ^ action) else action in
      iflog
        (fun _ ->
           annotate (Printf.sprintf "%s on dst lval %a" act Ast.sprintf_lval dst));
      b

  and trans_stmt_full (stmt:Ast.stmt) : unit =
    match stmt.node with

        Ast.STMT_log a ->
          begin
            match atom_type cx a with
                Ast.TY_str -> trans_log_str a
              | Ast.TY_int -> trans_log_int a
              | _ -> bugi cx stmt.id "unimplemented logging type"
          end

      | Ast.STMT_check_expr e ->
          begin
            match expr_type cx e with
                Ast.TY_bool -> trans_check_expr e
              | _ -> bugi cx stmt.id "check expr on non-bool"
          end

      | Ast.STMT_send (chan,src) ->
          trans_send chan src

      | Ast.STMT_spawn (dst, plv, args) ->
          trans_spawn (maybe_init stmt.id "spawn" dst) dst plv args

      | Ast.STMT_recv (dst, chan) ->
          trans_recv (maybe_init stmt.id "recv" dst) dst chan

      | Ast.STMT_copy (dst, e_src, binop_opt) ->
          trans_copy (maybe_init stmt.id "copy" dst) dst e_src binop_opt

      | Ast.STMT_call (dst, flv, args) ->
          trans_call_fn (maybe_init stmt.id "call" dst) dst flv args

      | Ast.STMT_init_rec (dst, atab) ->
          let (slot_cell, slot) = trans_lval dst INTENT_init in
          let dst_slots =
            match slot_ty slot with
                Ast.TY_rec trec -> (Array.map (fun (_, slot) -> slot) trec)
              | _ -> bugi cx stmt.id "non-rec destination type in stmt_init_rec"
          in
          let atoms = Array.map (fun (_, _, atom) -> atom) atab in
          let dst_cell = deref_slot slot_cell slot INTENT_init in
            trans_init_structural_from_atoms dst_cell dst_slots atoms


      | Ast.STMT_init_tup (dst, mode_atoms) ->
          let (slot_cell, slot) = trans_lval dst INTENT_init in
          let dst_slots =
            match slot_ty slot with
                Ast.TY_tup ttup -> ttup
              | _ -> bugi cx stmt.id "non-tup destination type in stmt_init_tup"
          in
          let atoms = Array.map (fun (_, atom) -> atom) mode_atoms in
          let dst_cell = deref_slot slot_cell slot INTENT_init in
            trans_init_structural_from_atoms dst_cell dst_slots atoms


      | Ast.STMT_init_str (dst, s) ->
          trans_init_str dst s

      | Ast.STMT_init_vec (dst, _, atoms) ->
          trans_init_vec dst atoms

      | Ast.STMT_init_port dst ->
          trans_init_port dst

      | Ast.STMT_init_chan (dst, port) ->
          let (dst_cell, dst_slot) =
            trans_lval dst INTENT_init
          in
            begin
              match port with
                  None ->
                    mov dst_cell imm_false
                | Some p ->
                    let (src_cell, _) = trans_lval p INTENT_read in
                    let src_slot = interior_slot (slot_ty dst_slot) in
                      trans_copy_slot true
                        dst_cell dst_slot
                        src_cell src_slot
            end

      | Ast.STMT_block block ->
          trans_block block

      | Ast.STMT_while sw ->
          let (head_stmts, head_expr) = sw.Ast.while_lval in
          let fwd_jmp = mark () in
            emit (Il.jmp Il.JMP Il.CodeNone);
            let block_begin = mark () in
              trans_block sw.Ast.while_body;
              patch fwd_jmp;
              Array.iter trans_stmt head_stmts;
              let back_jmps = trans_cond false head_expr in
                List.iter (fun j -> patch_existing j block_begin) back_jmps;

      | Ast.STMT_if si ->
          let skip_thn_jmps = trans_cond true si.Ast.if_test in
            trans_block si.Ast.if_then;
            begin
              match si.Ast.if_else with
                  None -> List.iter patch skip_thn_jmps
                | Some els ->
                    let skip_els_jmp = mark () in
                      begin
                        emit (Il.jmp Il.JMP Il.CodeNone);
                        List.iter patch skip_thn_jmps;
                        trans_block els;
                        patch skip_els_jmp
                      end
            end

      | Ast.STMT_check (preds, calls) ->
          Array.iteri
            (fun i (fn, args) -> trans_call_pred_and_check preds.(i) fn args)
            calls

      | Ast.STMT_ret (proto_opt, atom_opt) ->
          begin
          match proto_opt with
              None ->
                begin
                  begin
                    match atom_opt with
                        None -> ()
                      | Some at ->
                          let (dst_addr, _) = deref (wordptr_at (fp_imm out_addr_disp)) in
                          let atom_ty = atom_type cx at in
                          let dst_slot = interior_slot atom_ty in
                          let dst_ty = referent_type abi atom_ty in
                          let dst_cell = Il.Addr (dst_addr, dst_ty) in
                            trans_init_slot_from_atom dst_cell dst_slot at
                  end;
                  Stack.push (mark()) (Stack.top epilogue_jumps);
                end;
                emit (Il.jmp Il.JMP Il.CodeNone)
            | Some _ -> ()
          end

      | Ast.STMT_decl _ -> ()

      | _ -> bugi cx stmt.id "unhandled form of statement in trans_stmt"
  in

  let capture_emitted_quads (fix:fixup) (node:node_id) : unit =
    let e = emitter() in
    let n_vregs = e.Il.emit_next_vreg in
    let quads = e.Il.emit_quads in
    let name = path_name () in
    let f = match !curr_file with
        None -> bugi cx node "Missing file scope when capturing quads."
      | Some f -> f
    in
    let item_code = Hashtbl.find cx.ctxt_file_code f in
      begin
        iflog (fun _ -> annotate_quads name);
        let vr_s =
          match htab_search cx.ctxt_spill_fixups node with
              None -> (assert (n_vregs = 0); None)
            | Some spill -> Some (n_vregs, spill)
        in
        let code = { code_fixup = fix;
                     code_quads = quads;
                     code_vregs_and_spill = vr_s }
        in
          htab_put item_code node code;
          htab_put cx.ctxt_all_item_code node code
      end
  in

  let get_frame_glue_fns (fnid:node_id) : Il.operand =
    let get_frame_glue glue prefix inner =
      let path = path_name() in
        get_mem_glue glue
          (fun _ -> prefix ^ " frame: " ^ path)
          begin
            fun addr ->
              iter_frame_slots fnid
                begin
                  fun key slot_id slot ->
                    match htab_search cx.ctxt_slot_layouts slot_id with
                        None -> ()
                      | Some layout ->
                          let referent_type = slot_id_referent_type slot_id in
                          let disp = layout.layout_offset in
                          let fp_cell = Il.Addr (addr, (Il.ScalarTy (Il.AddrTy referent_type))) in
                          let slot_cell = Il.Addr (deref_imm fp_cell disp) in
                            inner key slot_id slot slot_cell
                end
          end
    in
    trans_data_frag (DATA_frame_glue_fns fnid)
      begin
        fun _ ->
          let mark_frame_glue_fixup =
            get_frame_glue (GLUE_mark_frame fnid) "mark"
              begin
                fun _ _ slot slot_cell ->
                  mark_slot slot_cell slot None
              end
          in
          let drop_frame_glue_fixup =
            get_frame_glue (GLUE_drop_frame fnid) "drop"
              begin
                fun _ _ slot slot_cell ->
                  drop_slot slot_cell slot None
              end
          in
          let reloc_frame_glue_fixup =
            get_frame_glue (GLUE_reloc_frame fnid) "reloc"
              begin
                fun _ _ _ _ ->
                  ()
              end
          in
            Asm.SEQ
              [|
               (* 
                * NB: this must match the struct-offsets given in ABI
                * & rust runtime library.
                *)
                Asm.WORD (word_ty_mach, Asm.M_POS mark_frame_glue_fixup);
                Asm.WORD (word_ty_mach, Asm.M_POS drop_frame_glue_fixup);
                Asm.WORD (word_ty_mach, Asm.M_POS reloc_frame_glue_fixup);
              |]
      end
  in

  let trans_frame_entry (fnid:node_id) : unit =
    let frame_fns = get_frame_glue_fns fnid in
    let framesz = get_framesz cx fnid in
    let callsz = get_callsz cx fnid in
    let spill_fixup = Hashtbl.find cx.ctxt_spill_fixups fnid in
      Stack.push (Stack.create()) epilogue_jumps;
      push_new_emitter ();
      iflog (fun _ -> annotate "prologue");
      abi.Abi.abi_emit_fn_prologue (emitter())
                                   framesz
                                   spill_fixup
                                   callsz
                                   nabi_rust
                                   (upcall_fixup "upcall_grow_proc");
      mov (word_at (fp_imm frame_fns_disp)) frame_fns;
      iflog (fun _ -> annotate "finished prologue");
  in

  let trans_frame_exit (fnid:node_id) : unit =
    Stack.iter patch (Stack.pop epilogue_jumps);
    iflog (fun _ -> annotate "epilogue");
    abi.Abi.abi_emit_fn_epilogue (emitter());
    capture_emitted_quads (get_fn_fixup cx fnid) fnid;
    pop_emitter ()
  in

  let trans_fn (fnid:node_id) (body:Ast.block) : unit =
    trans_frame_entry fnid;
    trans_block body;
    trans_frame_exit fnid;
  in

  let trans_tag
      (n:Ast.ident)
      (tagid:node_id)
      (tag:(Ast.header_tup * Ast.ty_tag * node_id))
      : unit =
    trans_frame_entry tagid;
    trace_str cx.ctxt_sess.Session.sess_trace_tag
      ("in tag constructor " ^ n);
    let (header_tup, _, _) = tag in
    let ctor_ty = Hashtbl.find cx.ctxt_all_item_types tagid in
    let ttag =
      match ctor_ty with
          Ast.TY_fn ({Ast.sig_output_slot={Ast.slot_ty=Some (Ast.TY_tag ttag)}}, _) -> ttag
        | Ast.TY_fn ({Ast.sig_output_slot={Ast.slot_ty=Some (Ast.TY_iso tiso)}}, _) ->
            get_iso_tag tiso
        | _ -> bugi cx tagid "unexpected type for tag constructor"
    in
    let slots = Array.map (fun sloti -> Hashtbl.find cx.ctxt_all_slots sloti.id) header_tup in
    let tag_keys = sorted_htab_keys ttag in
    let i = ref (-1) in
      begin
        for j = 0 to arr_max tag_keys do
          if tag_keys.(j) = (Ast.NAME_base (Ast.BASE_ident n))
          then i := j
        done;
        if (!i) = -1
        then bugi cx tagid "error sorting tag";
      end;
      let _ = log cx "tag variant: %s -> tag value #%d" n (!i) in
      let (out_addr, _) = deref (wordptr_at (fp_imm out_addr_disp)) in
      let tag_cell = word_at out_addr in
      let rty = referent_type abi (Ast.TY_tup slots) in
      let src_ta = (fp_imm arg0_disp, rty) in
      let dst_ta = ((addr_add_imm out_addr word_sz), rty) in
        (* A clever compiler will inline this. We are not clever. *)
        iflog (fun _ -> annotate (Printf.sprintf "write tag #%d" (!i)));
        mov tag_cell (imm (Int64.of_int (!i)));
        iflog (fun _ -> annotate "copy tag-content tuple");
        trans_copy_tup true
          dst_ta slots
          src_ta slots;
        trace_str cx.ctxt_sess.Session.sess_trace_tag
          ("finished tag constructor " ^ n);
        trans_frame_exit tagid;
  in

  let trans_native_fn (fnid:node_id) (nfn:Ast.native_fn) : unit =
    let nabi =
      match (Abi.string_to_nabi nfn.Ast.native_fn_abi nabi_indirect) with
          Some n -> n
        | None -> (err None "invalid abi specification")
    in
      (* FIXME (bug 541532): This is a gross hack. The library should
         come from the frontend. *)
    let lib =
      match nabi.Abi.nabi_convention with
          Abi.CONV_rust -> LIB_rustrt
        | Abi.CONV_cdecl -> LIB_c
    in
    let name =
      (Ast.fmt_to_str Ast.fmt_name (Hashtbl.find cx.ctxt_all_item_names fnid))
    in
    let args =
      (Array.init (Array.length nfn.Ast.native_fn_input_slots)
         (fun (n:int) ->
            (Il.Cell (word_at (sp_imm (Int64.add (Int64.of_int n) (word_n 3)))))))
    in
      push_new_emitter ();
      let e = (emitter()) in
        trans_native_thunk nabi lib name (word_at (sp_imm (word_n 1))) args;
        Il.emit e Il.Ret;
        if e.Il.emit_next_vreg != 0
        then bug () "%s uses nonzero vregs" name;
        capture_emitted_quads (get_fn_fixup cx fnid) fnid;
        pop_emitter ();
  in

  let trans_mod (id:node_id) (m:Ast.mod_items) : unit =
    log cx "emitting %d-entry mod table for %s" (Hashtbl.length m) (path_name());
    let item_pairs =
      Array.map
        begin
          fun ident ->
            let item = Hashtbl.find m ident in
            let fix =
              match item.node with
                  Ast.MOD_ITEM_fn _
                | Ast.MOD_ITEM_pred _
                | Ast.MOD_ITEM_tag _ -> get_fn_fixup cx item.id
                | Ast.MOD_ITEM_mod _ -> get_mod_fixup cx item.id
                | Ast.MOD_ITEM_opaque_type td
                | Ast.MOD_ITEM_public_type td ->
                    let t = td.Ast.decl_item in
                      ignore (trans_type_info t);
                      let (fix, _) = Hashtbl.find cx.ctxt_data (DATA_typeinfo t) in
                        fix
            in
              Asm.SEQ
                [| Asm.WORD (word_ty_mach, Asm.M_POS fix);
                   Asm.WORD (word_ty_mach, Asm.IMM 0L) |]
        end
        (sorted_htab_keys m)
    in
    let fix = get_mod_fixup cx id in
    let frag = Asm.DEF (fix, Asm.SEQ item_pairs) in
      htab_put cx.ctxt_data (DATA_mod_table id) (fix, frag)
  in

  let enter_file_for i =
    if Hashtbl.mem cx.ctxt_item_files i.id
    then begin
      match !curr_file with
          None -> curr_file := Some i.id
        | Some _ -> bugi cx i.id "Existing source file on file-scope entry."
    end
  in

  let leave_file_for i =
    if Hashtbl.mem cx.ctxt_item_files i.id
    then begin
      match !curr_file with
          None -> bugi cx i.id "Missing source file on file-scope exit."
        | Some _ -> curr_file := None
    end
  in

  let visit_mod_item_pre n p i =
    enter_file_for i;
    begin
      match i.node with
          Ast.MOD_ITEM_fn f ->
            if path_name() = cx.ctxt_main_name
            then
              begin
                log cx "emitting main exit-proc glue for %s" cx.ctxt_main_name;
                let main_tsig =
                  {
                    Ast.sig_input_slots = [| |];
                    Ast.sig_input_constrs = [| |];
                    Ast.sig_output_slot = interior_slot Ast.TY_nil;
                  }
                in
                  emit_exit_proc_glue
                    main_tsig
                    cx.ctxt_main_exit_proc_glue_fixup
                    GLUE_exit_main_proc;
              end;
            trans_fn i.id f.Ast.decl_item.Ast.fn_body

        | Ast.MOD_ITEM_pred p -> trans_fn i.id p.Ast.decl_item.Ast.pred_body
        | Ast.MOD_ITEM_tag t -> trans_tag n i.id t.Ast.decl_item
        | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in

  let visit_mod_item_post n p i =
    inner.Walk.visit_mod_item_post n p i;
    begin
      match i.node with
          Ast.MOD_ITEM_mod m -> trans_mod i.id m.Ast.decl_item
        | _ -> ()
    end;
    leave_file_for i
  in

  let visit_native_mod_item_pre n i =
    enter_file_for i;
    begin
      match i.node with
          Ast.NATIVE_fn nfn -> trans_native_fn i.id nfn
        | _ -> ()
    end;
    inner.Walk.visit_native_mod_item_pre n i
  in

  let visit_native_mod_item_post n i =
    inner.Walk.visit_native_mod_item_post n i;
    leave_file_for i
  in


  let visit_crate_post _ =
    let emit_aux_global_glue cx glue glue_name fix fn =
      push_new_emitter ();
      let e = emitter() in
        fn e;
        iflog (fun _ -> annotate_quads glue_name);
        if e.Il.emit_next_vreg != 0
        then bug () "%s uses nonzero vregs" glue_name;
        pop_emitter();
        let code =
          { code_fixup = fix;
            code_quads = e.Il.emit_quads;
            code_vregs_and_spill = None }
        in
          htab_put cx.ctxt_glue_code glue code
    in
    let global_glue_fns =
      (cx.ctxt_global_glue_fixup,
       Asm.DEF
         (cx.ctxt_global_glue_fixup,
          Asm.SEQ [|
            (* 
             * NB: this must match the struct-offsets given in ABI
             * & rust runtime library.
             *)
            Asm.WORD (word_ty_mach, Asm.M_POS cx.ctxt_c_to_proc_fixup);
            Asm.WORD (word_ty_mach, Asm.M_POS cx.ctxt_main_exit_proc_glue_fixup);
            Asm.WORD (word_ty_mach, Asm.M_POS cx.ctxt_unwind_fixup);
            Asm.WORD (word_ty_mach, Asm.M_POS cx.ctxt_yield_fixup);
          |]))
    in

      (* Emit additional glue we didn't do elsewhere. *)
      emit_aux_global_glue cx GLUE_C_to_proc
        "c-to-proc glue"
        cx.ctxt_c_to_proc_fixup
        cx.ctxt_abi.Abi.abi_c_to_proc;

      emit_aux_global_glue cx GLUE_yield
        "yield glue"
        cx.ctxt_yield_fixup
        cx.ctxt_abi.Abi.abi_yield;

      emit_aux_global_glue cx GLUE_unwind
        "unwind glue"
        cx.ctxt_unwind_fixup
        (fun e -> cx.ctxt_abi.Abi.abi_unwind
           e nabi_rust (upcall_fixup "upcall_exit"));

      htab_put cx.ctxt_data
        DATA_global_glue_fns global_glue_fns
  in

    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post;
        Walk.visit_native_mod_item_pre = visit_native_mod_item_pre;
        Walk.visit_native_mod_item_post = visit_native_mod_item_post;
        Walk.visit_crate_post = visit_crate_post;
    }
;;


let fixup_assigning_visitor
    (cx:ctxt)
    (path:Ast.name_component Stack.t)
    (inner:Walk.visitor)
    : Walk.visitor =

  let path_name (_:unit) : string =
    Ast.fmt_to_str Ast.fmt_name (Walk.path_to_name path)
  in

  let enter_file_for i =
    if Hashtbl.mem cx.ctxt_item_files i.id
    then
      begin
        htab_put cx.ctxt_file_fixups i.id (new_fixup (path_name()));
        if not (Hashtbl.mem cx.ctxt_file_code i.id)
        then htab_put cx.ctxt_file_code i.id (Hashtbl.create 0);
      end
  in

  let visit_mod_item_pre n p i =
    enter_file_for i;
    begin
      match i.node with

          Ast.MOD_ITEM_pred _
        | Ast.MOD_ITEM_tag _ ->
            htab_put cx.ctxt_fn_fixups i.id
              (new_fixup (path_name()));

        | Ast.MOD_ITEM_fn _ ->
            begin
              let path = path_name () in
              let fixup =
                if path = cx.ctxt_main_name
                then cx.ctxt_main_fn_fixup
                else new_fixup path
              in
                htab_put cx.ctxt_fn_fixups i.id fixup;
            end

        | Ast.MOD_ITEM_mod _ ->
            htab_put cx.ctxt_mod_fixups i.id
              (new_fixup (path_name()));

        | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in

  let visit_native_mod_item_pre n i =
    enter_file_for i;
    begin
      match i.node with
          Ast.NATIVE_fn _ ->
            htab_put cx.ctxt_fn_fixups i.id
              (new_fixup ((path_name()) ^ " native thunk"));
        | _ -> ()
    end;
    inner.Walk.visit_native_mod_item_pre n i
  in

  let visit_block_pre b =
    htab_put cx.ctxt_block_fixups b.id (new_fixup "lexical block");
    inner.Walk.visit_block_pre b
  in

  { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_block_pre = visit_block_pre;
        Walk.visit_native_mod_item_pre = visit_native_mod_item_pre }


let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  let path = Stack.create () in
  let passes =
    [|
      (fixup_assigning_visitor cx path
         Walk.empty_visitor);
      (trans_visitor cx path
         Walk.empty_visitor)
    |];
  in
    log cx "translating crate with main function %s" cx.ctxt_main_name;
    run_passes cx path passes (log cx "%s") crate;
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
