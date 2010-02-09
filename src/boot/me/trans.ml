(* Translation *)

open Semant;;
open Common;;

let log cx = Session.log "trans"
  cx.ctxt_sess.Session.sess_log_trans
  cx.ctxt_sess.Session.sess_log_out
;;

let arr_max a = (Array.length a) - 1;;

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
  let (word_slot:Ast.slot) = word_slot abi in
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
  let nil_ptr = Il.Mem ((Il.Abs (Asm.IMM 0L)), Il.NilTy) in

  let table_of_fixups (fixups:fixup array) : Asm.frag =
    Asm.SEQ
      (Array.map
         (fun fix -> Asm.WORD (word_ty_mach, Asm.M_POS fix)) fixups)
  in

  let nabi_indirect =
      match cx.ctxt_sess.Session.sess_targ with
          Linux_x86_elf -> false
        | _ -> true
  in
  let nabi_rust =
    { Abi.nabi_indirect = nabi_indirect;
      Abi.nabi_convention = Abi.CONV_rust }
  in

  let out_mem_disp = abi.Abi.abi_frame_base_sz in
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
  let next_vreg_cell t = Il.next_vreg_cell (emitter()) t in
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

  let based (reg:Il.reg) : Il.mem =
    Il.RegIn (reg, None)
  in

  let based_off (reg:Il.reg) (off:Asm.expr64) : Il.mem =
    Il.RegIn (reg, Some off)
  in

  let based_imm (reg:Il.reg) (imm:int64) : Il.mem =
    based_off reg (Asm.IMM imm)
  in

  let fp_imm (imm:int64) : Il.mem =
    based_imm abi.Abi.abi_fp_reg imm
  in

  let sp_imm (imm:int64) : Il.mem =
    based_imm abi.Abi.abi_sp_reg imm
  in

  let word_at (mem:Il.mem) : Il.cell =
    Il.Mem (mem, Il.ScalarTy (Il.ValTy word_bits))
  in

  let wordptr_at (mem:Il.mem) : Il.cell =
    Il.Mem (mem, Il.ScalarTy (Il.AddrTy (Il.ScalarTy (Il.ValTy word_bits))))
  in

  let mov (dst:Il.cell) (src:Il.operand) : unit =
    emit (Il.umov dst src)
  in

  let lea (dst:Il.cell) (src:Il.mem) : unit =
    emit (Il.lea dst src)
  in

  let ptr_at (mem:Il.mem) (pointee_ty:Ast.ty) : Il.cell =
    Il.Mem (mem, Il.ScalarTy (Il.AddrTy (referent_type abi pointee_ty)))
  in

  let need_scalar_ty (rty:Il.referent_ty) : Il.scalar_ty =
    match rty with
        Il.ScalarTy s -> s
      | _ -> bug () "expected ScalarTy"
  in

  let need_mem_cell (cell:Il.cell) : Il.typed_mem =
    match cell with
        Il.Mem a -> a
      | Il.Reg _ -> bug () "expected address cell, got non-address register cell"
  in

  let get_element_ptr (mem_cell:Il.cell) (i:int) : Il.cell =
    match mem_cell with
        Il.Mem (mem, Il.StructTy elts) when i >= 0 && i < (Array.length elts) ->
          assert ((Array.length elts) != 0);
          begin
            let elts_before = Array.sub elts 0 i in
            let elt_rty = elts.(i) in
            let elts_before_size = Il.referent_ty_size word_bits (Il.StructTy elts_before) in
            let elt_align = Il.referent_ty_align word_bits elt_rty in
            let elt_off = Il.align_to elt_align elts_before_size in
              Il.Mem (Il.mem_off_imm mem elt_off, elt_rty)
          end

      | _ -> bug () "get_element_ptr %d on cell %s" i
          (Il.string_of_cell abi.Abi.abi_str_of_hardreg mem_cell)
  in

  let get_variant_ptr (mem_cell:Il.cell) (i:int) : Il.cell =
    match mem_cell with
        Il.Mem (mem, Il.UnionTy elts) when i >= 0 && i < (Array.length elts) ->
          assert ((Array.length elts) != 0);
          Il.Mem (mem, elts.(i))

      | _ -> bug () "get_variant_ptr %d on cell %s" i
          (Il.string_of_cell abi.Abi.abi_str_of_hardreg mem_cell)
  in

  (* 
   * Note: alias *requires* its cell to be in memory already, and should
   * only be used on slots you know to be memory-resident. Use 'aliasing' or 
   * 'via_memory' if you have a cell or operand you want in memory for a very
   * short period of time (the time spent by the code generated by the thunk).
   *)

  let alias (cell:Il.cell) : Il.cell =
    let mem, ty = need_mem_cell cell in
    let vreg_cell = next_vreg_cell (Il.AddrTy ty) in
      begin
        match ty with
            Il.NilTy -> ()
          | _ -> lea vreg_cell mem
      end;
      vreg_cell
  in

  let force_to_mem (src:Il.operand) : Il.typed_mem =
    let do_spill (t:Il.scalar_ty) =
      let s = (Il.next_spill (emitter())) in
      let spill_mem = Il.Spill s in
      let spill_ta = (spill_mem, Il.ScalarTy t) in
        mov (Il.Mem spill_ta) src;
        spill_ta
    in
    match src with
        Il.Cell (Il.Mem ta) -> ta
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
        | Il.Cell (Il.Mem (_, Il.ScalarTy st)) -> do_mov st
        | Il.Cell (Il.Mem (_, rt)) ->
            bug () "forcing non-scalar referent of type %s to register"
              (Il.string_of_referent_ty rt)
  in

  let via_memory (writeback:bool) (c:Il.cell) (thunk:Il.cell -> unit) : unit =
    match c with
        Il.Mem _ -> thunk c
      | Il.Reg _ ->
          let mem_c = Il.Mem (force_to_mem (Il.Cell c)) in
            thunk mem_c;
            if writeback
            then
              mov c (Il.Cell mem_c)
  in

  let aliasing (writeback:bool) (c:Il.cell) (thunk:Il.cell -> unit) : unit =
    via_memory writeback c (fun c -> thunk (alias c))
  in

  let pointee_type (ptr:Il.cell) : Il.referent_ty =
    match ptr with
        Il.Reg (_, (Il.AddrTy rt)) -> rt
      | Il.Mem (_, Il.ScalarTy (Il.AddrTy rt)) -> rt
      | _ ->
          bug () "taking pointee-type of non-address cell %s "
            (Il.string_of_cell abi.Abi.abi_str_of_hardreg ptr)
  in

  let deref (ptr:Il.cell) : Il.cell =
    let (r, st) = force_to_reg (Il.Cell ptr) in
      match st with
          Il.AddrTy rt -> Il.Mem (based r, rt)
        | _ -> bug () "dereferencing non-address cell of type %s "
            (Il.string_of_scalar_ty st)
  in

  let deref_off (ptr:Il.cell) (off:Asm.expr64) : Il.cell =
    let (r, st) = force_to_reg (Il.Cell ptr) in
      match st with
          Il.AddrTy rt -> Il.Mem (based_off r off, rt)
        | _ -> bug () "offset-dereferencing non-address cell of type %s "
            (Il.string_of_scalar_ty st)
  in

  let deref_imm (ptr:Il.cell) (imm:int64) : Il.cell =
    deref_off ptr (Asm.IMM imm)
  in

  let pp_imm (imm:int64) : Il.cell =
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
            begin
              match referent_type with
                  Il.ScalarTy st -> Il.Reg (Il.Vreg (cell_vreg_num vr), st)
                | Il.NilTy -> nil_ptr
                | Il.StructTy _ -> bugi cx slot_id "cannot treat structured referent as single operand"
                | Il.UnionTy _ -> bugi cx slot_id "cannot treat union referent as single operand"
                | Il.OpaqueTy -> bugi cx slot_id "cannot treat opaque referent as single operand"
                | Il.CodeTy ->  bugi cx slot_id "cannot treat code referent as single operand"
            end
        | None ->
            begin
              match htab_search cx.ctxt_slot_layouts slot_id with
                  None -> bugi cx slot_id "slot assigned to neither vreg nor layout"
                | Some layout ->
                    let disp = layout.layout_offset in
                      Il.Mem (fp_imm disp, referent_type)
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
      (frame_id:node_id)
      (fn:Ast.slot_key -> node_id -> Ast.slot -> unit)
      : unit =
    let blocks = Hashtbl.find cx.ctxt_frame_blocks frame_id in
      List.iter (fun block -> iter_block_slots block fn) blocks
  in

  let iter_frame_and_arg_slots
      (frame_id:node_id)
      (fn:Ast.slot_key -> node_id -> Ast.slot -> unit)
      : unit =
    iter_frame_slots frame_id fn;
    match htab_search cx.ctxt_frame_args frame_id with
        None -> ()
      | Some ls ->
          List.iter
            begin
              fun slot_id ->
                let key = Hashtbl.find cx.ctxt_slot_keys slot_id in
                let slot = Hashtbl.find cx.ctxt_all_slots slot_id in
                  fn key slot_id slot
            end
            ls
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
      (cell:Il.cell)
      (comp:Ast.lval_component)
      : (Il.cell * Ast.slot) =

    match (base_ty, comp) with
        (Ast.TY_rec entries,
         Ast.COMP_named (Ast.COMP_ident id)) ->
          let i = arr_idx (Array.map fst entries) id in
            (get_element_ptr cell i, snd entries.(i))

      | (Ast.TY_tup entries,
         Ast.COMP_named (Ast.COMP_idx i)) ->
          (get_element_ptr cell i, entries.(i))

      | (Ast.TY_vec slot,
         Ast.COMP_atom at) ->
          let atop = trans_atom at in
          let unit_sz = slot_sz abi slot in
          let reg = next_vreg () in
          let t = Il.Reg (reg, Il.ValTy word_bits) in
            emit (Il.binary Il.UMUL t atop (imm unit_sz));
            let (mem, _) = need_mem_cell (deref cell) in
            let elt_mem = trans_bounds_check mem (Il.Cell t) in
              (Il.Mem (elt_mem, slot_referent_type abi slot), slot)

      | (Ast.TY_str,
         Ast.COMP_atom at) ->
          let atop = trans_atom at in
          let unit_sz = 1L in
          let reg = next_vreg () in
          let t = Il.Reg (reg, Il.ValTy word_bits) in
          let slot = interior_slot (Ast.TY_mach TY_u8) in
            emit (Il.binary Il.UMUL t atop (imm unit_sz));
            let (mem, _) = need_mem_cell (deref cell) in
            let elt_mem = trans_bounds_check mem (Il.Cell t) in
              (Il.Mem (elt_mem, Il.ScalarTy (Il.ValTy Il.Bits8)), slot)

      | (Ast.TY_mod (_, mtis),
         Ast.COMP_named (Ast.COMP_ident id)) ->
          let sorted_idents = sorted_htab_keys mtis in
          let i = arr_idx sorted_idents id in
            (* A mod is a pair of pointers [mod_table, binding];
             * we dereference the first cell of this pair and then
             * return the address of the Nth table-item. Each table
             * item is itself a pair. *)
          let (table_mem, _) = need_mem_cell (deref (get_element_ptr cell 0)) in
          let off = word_n (i * 2) in
          let item_mem = Il.mem_off_imm table_mem off in
          let item_ty = ty_of_mod_type_item (Hashtbl.find mtis id) in
          let item_referent_ty = referent_type abi item_ty in
            (Il.Mem (item_mem, item_referent_ty), interior_slot item_ty)

      | _ -> bug () "unhandled form of lval_ext in trans_slot_lval_ext"

  (* 
   * vec: operand holding ptr to vec.
   * mul_idx: index value * unit size.
   * return: ptr to element.
   *)
  and trans_bounds_check (vec:Il.mem) (mul_idx:Il.operand) : Il.mem =
    let (len:Il.cell) = word_at (Il.mem_off_imm vec (word_n 2)) in
    let (base:Il.cell) = Il.Reg (next_vreg(), Il.voidptr_t) in
    let (elt_reg:Il.reg) = next_vreg () in
    let (elt:Il.cell) = Il.Reg (elt_reg, Il.voidptr_t) in
    let (diff:Il.cell) = Il.Reg (next_vreg (), Il.ValTy word_bits) in
      annotate "bounds check";
      lea base (Il.mem_off_imm vec (word_n 3));
      emit (Il.binary Il.ADD elt (Il.Cell base) mul_idx);
      emit (Il.binary Il.SUB diff (Il.Cell elt) (Il.Cell base));
      let jmp = trans_compare Il.JB (Il.Cell diff) (Il.Cell len) in
        trans_cond_fail "bounds check" jmp;
        based elt_reg

  and trans_lval_full
      (initializing:bool)
      (lv:Ast.lval)
      (abs_ok:bool)
      : (Il.cell * Ast.slot) =

    let return_fixup (fix:fixup) (slot:Ast.slot)
        : (Il.cell * Ast.slot) =
      let rty = slot_referent_type abi slot in
      let mem = fixup_to_mem abs_ok fix rty in
        (Il.Mem (mem, rty), slot)
    in

    let return_item (item:Ast.mod_item)
        : (Il.cell * Ast.slot) =
      let ty = Hashtbl.find cx.ctxt_all_item_types item.id in
      let slot = interior_slot ty in
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

    let rec trans_slot_lval_full (initializing:bool) lv =
      match lv with
          Ast.LVAL_ext (base, comp) ->
            let (base_cell, base_slot) = trans_slot_lval_full initializing base in
            let base_cell' = deref_slot initializing base_cell base_slot in
              trans_slot_lval_ext (slot_ty base_slot) base_cell' comp

        | Ast.LVAL_base nb ->
            let referent = lval_to_referent cx nb.id in
            let slot = lval_to_slot cx nb.id in
              return_slot nb.id slot referent
    in
      if lval_is_slot cx lv
      then trans_slot_lval_full initializing lv
      else
        if initializing
        then err None "init item"
        else
          if lval_is_item cx lv
          then return_item (lval_item cx lv)
          else
            begin
              assert (lval_is_native_item cx lv);
              return_native_item (lval_native_item cx lv)
            end

  and trans_lval_maybe_init (initializing:bool) (lv:Ast.lval) : (Il.cell * Ast.slot) =
    trans_lval_full initializing lv abi.Abi.abi_has_abs_data

  and trans_lval_init (lv:Ast.lval) : (Il.cell * Ast.slot) =
    trans_lval_maybe_init true lv

  and trans_lval (lv:Ast.lval) : (Il.cell * Ast.slot) =
    trans_lval_maybe_init false lv

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
    let (dst, _) = trans_lval_init dst in
      trans_upcall "upcall_new_str" dst [| static; imm init_sz |]

  and trans_atom (atom:Ast.atom) : Il.operand =
    iflog
      begin
        fun _ ->
          annotate (Ast.fmt_to_str Ast.fmt_atom atom)
      end;

    match atom with
        Ast.ATOM_lval lv ->
          let (cell, slot) = trans_lval lv in
            Il.Cell (deref_slot false cell slot)

      | Ast.ATOM_literal lit ->
          begin
            match lit.node with
                Ast.LIT_nil -> Il.Cell (nil_ptr)
              | Ast.LIT_bool false -> imm_false
              | Ast.LIT_bool true -> imm_true
                (* FIXME (bug 541566): handle char as exactly 32 bits, not word size. *)
              | Ast.LIT_char c -> imm (Int64.of_int (Char.code c))
              | Ast.LIT_int (i, _) -> imm i
              | Ast.LIT_mach (m, n, _) -> imm_of_ty n m

              | _ -> marker
          end

  and fixup_to_mem
      (abs_ok:bool)
      (fix:fixup)
      (referent_ty:Il.referent_ty)
      : Il.mem =
    let i = Asm.M_POS fix in
      if abs_ok
      then Il.Abs i
      else
        let c = Il.Mem (Il.Abs i, referent_ty) in
        let (reg, _) = force_to_reg (Il.Cell (alias c)) in
          Il.RegIn (reg, None)

  and code_fixup_to_mem (fix:fixup) : Il.mem =
    fixup_to_mem abi.Abi.abi_has_abs_code fix Il.CodeTy

  and fixup_to_code (fix:fixup) : Il.code =
    Il.CodeMem (code_fixup_to_mem fix)

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
      let in_slots = tsig.Ast.sig_input_slots in
        drop_arg_slots in_slots;
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

  and emit_new_mod_glue (mod_id:node_id) (hdr:Ast.ty_mod_header) (fix:fixup) (g:glue) : unit =
    let spill = new_fixup "new-mod glue spill" in
      trans_mem_glue_frame_entry 0 spill;

      let (slots, _) = hdr in
      let ty = Ast.TY_tup slots in
      let src_rty = slot_referent_type abi (interior_slot ty) in
      let binding_ptr_rty = slot_referent_type abi (exterior_slot ty) in
      let sz = exterior_rc_allocation_size (exterior_slot ty) in

      let mod_ty = Hashtbl.find cx.ctxt_all_item_types mod_id in
      let mod_cell = deref (ptr_at (fp_imm out_mem_disp) mod_ty) in

        (* 
         * pair_mem is now the pair [item,binding*]
         *)
      let item_ptr_cell = get_element_ptr mod_cell 0 in
      let item_fixup = get_mod_fixup cx mod_id in
      let item_mem = fixup_to_mem abi.Abi.abi_has_abs_data item_fixup Il.OpaqueTy in
      let binding_ptr_cell = get_element_ptr mod_cell 1 in

        (* Load first cell of pair with static item mem addr.*)
      let tmp = next_vreg_cell Il.voidptr_t in
        lea tmp item_mem;
        mov item_ptr_cell (Il.Cell tmp);

        (* Load second cell of pair with pointer to fresh binding tuple.*)
        trans_malloc binding_ptr_cell sz;

        (* Copy args into the binding tuple. *)
        let dst_ptr = next_vreg_cell (need_scalar_ty binding_ptr_rty) in
          mov dst_ptr (Il.Cell binding_ptr_cell);
          let dst = deref dst_ptr in
          let refcnt_cell = get_element_ptr dst 0 in
          let body_cell = get_element_ptr dst 1 in
          let src_ta = (fp_imm arg0_disp, src_rty) in

            mov refcnt_cell one;

            trans_copy_tup true body_cell (Il.Mem src_ta) slots;
            trans_glue_frame_exit "new-mod glue" fix spill g;


  and get_new_mod_glue (mod_id:node_id) (hdr:Ast.ty_mod_header) : fixup =
    let g = GLUE_new_module mod_id in
      match htab_search cx.ctxt_glue_code g with
          Some code -> code.code_fixup
        | None ->
            let fix = new_fixup "new-mod glue" in
              emit_new_mod_glue mod_id hdr fix g;
              fix


  (* FIXME (bug 544925): this should eventually use tail calling logic *)

  (* FIXME: here's some pseudocode for emit_new_bind_glue:
   *   1. save the fn ptr in a tmp vreg
   *   2. save the env ptr in a tmp vreg
   *   3. copy the new and saved args into a new arg tuple
   *      (via trans_copy_slot, which also does refcount logic)
   *   4. call into the fn ptr
   *      (try to reuse trans_call_fn)
   *)
  and emit_new_bind_glue
      ((*bind_id*)_:node_id)
      ((*fn_sig*)_:Ast.ty_sig)
      ((*bound_args*)_:bool array)
      ((*fix*)_:fixup)
      ((*g*)_:glue)
      : unit =
    bug () "not yet implemented"


  (* FIXME: abstract out the memoization logic between get_new_mod_glue and get_new_bind_glue *)
  and get_new_bind_glue (bind_id:node_id) (fn_sig:Ast.ty_sig) (bound_args:bool array) : fixup =
    let g = GLUE_new_bind bind_id in
      match htab_search cx.ctxt_glue_code g with
          Some code -> code.code_fixup
        | None ->
            let fix = new_fixup "new-bind glue" in
              emit_new_bind_glue bind_id fn_sig bound_args fix g;
              fix


  (* 
   * Mem-glue functions are either 'mark', 'drop' or 'free', they take
   * one pointer arg and return nothing.
   *)

  and trans_mem_glue_frame_entry (n_outgoing_args:int) (spill:fixup) : unit =
    let isz = cx.ctxt_abi.Abi.abi_implicit_args_sz in
    let callsz = Int64.add isz (word_n n_outgoing_args) in
      trans_glue_frame_entry callsz spill

  and get_mem_glue (g:glue) (prefix:unit -> string) (inner:Il.mem -> unit) : fixup =
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
              let (arg:Il.mem) = fp_imm arg0_disp in
                inner arg;
                Hashtbl.remove cx.ctxt_glue_code g;
                trans_glue_frame_exit ("glue: " ^ prefix) fix spill g;
                fix
          end

  and get_typed_mem_glue
      (g:glue)
      (ty:Ast.ty)
      (prefix:unit -> string)
      (inner:Il.cell -> unit)
      : fixup =
    get_mem_glue g prefix (fun mem -> inner (ptr_at mem ty))

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
    let inner (arg:Il.cell) =
      trace_str cx.ctxt_sess.Session.sess_trace_drop
        "in drop-glue, dropping";
      trace_word cx.ctxt_sess.Session.sess_trace_drop arg;
      drop_ty ty (deref arg) curr_iso;
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
    let inner (arg:Il.cell) =
      (* 
       * Free-glue assumes we're looking at a pointer to an 
       * exterior allocation with normal exterior layout. It's
       * just a way to move drop+free out of leaf code. 
       *)
      let (body_mem, _) = need_mem_cell (deref_imm arg exterior_rc_body_off) in
      let vr = next_vreg_cell Il.voidptr_t in
        lea vr body_mem;
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
          trans_free arg;
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
    let inner (arg:Il.cell) = mark_ty ty (deref arg) curr_iso in
    let fix = get_typed_mem_glue g ty prefix inner in
      fix

  and get_clone_glue
      (ty:Ast.ty)
      (curr_iso:Ast.ty_iso option)
      : fixup =
    let g = GLUE_clone ty in
    let prefix _ = "clone " ^ (Ast.fmt_to_str Ast.fmt_ty ty) in
    let inner (arg:Il.cell) =
      let dst = deref (ptr_at (fp_imm out_mem_disp) ty) in
      let src = deref arg in
        (* FIXME: Gross hack here: we know rt is one-past arg in clone glue args. *)
      let rt = word_at (fp_imm (Int64.add arg0_disp (word_n 1))) in
        clone_ty rt ty dst src curr_iso
    in
    let fix = get_typed_mem_glue g ty prefix inner in
      fix

  and trans_call_clone_glue (dst:Il.cell) (fix:fixup) (arg:Il.cell) (rt:Il.cell) : unit =
    let code = fixup_to_code fix in
    let arg_tup = arg_tup_cell [| word_slot; word_slot |] in
      (* Arg0 is target of clone, to which we write. *)
      (* Arg1 is process-pointer, as usual. *)
      (* Arg2 is the address of the slot we're cloning. *)
      (* Arg3 is the address of the runtime into which we're to clone various things. *)
      aliasing true dst
        begin
          fun dst ->
            mov (get_element_ptr arg_tup 0) (Il.Cell dst);
            mov (get_element_ptr arg_tup 1) (Il.Cell abi.Abi.abi_pp_cell);
            mov (get_element_ptr arg_tup 2) (Il.Cell arg);
            mov (get_element_ptr arg_tup 3) (Il.Cell rt);
            call_code code
        end

  and trans_call_mem_glue (fix:fixup) (arg:Il.cell) : unit =
    let code = fixup_to_code fix in
    let arg_tup = arg_tup_cell [| word_slot |] in
      (* Arg0 we ignore, not present. *)
      (* Arg1 is process-pointer, as usual. *)
      (* Arg2 is the sole pointer we pass in. *)
      mov (get_element_ptr arg_tup 1) (Il.Cell abi.Abi.abi_pp_cell);
      mov (get_element_ptr arg_tup 2) (Il.Cell arg);
      call_code code

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
      let bits = Il.operand_bits word_bits lhs in
      let dst = Il.Reg (Il.next_vreg (emitter()), Il.ValTy bits) in
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
    trace_str cx.ctxt_sess.Session.sess_trace_block
      "exited block";

  and trans_native_thunk
      (nabi:Abi.nabi)
      (lib:import_lib)
      (name:string)
      (ret:Il.cell)
      (args:Il.operand array)
      : unit =
    abi.Abi.abi_emit_native_call_in_thunk (emitter())
      ret nabi (Semant.import cx lib name) args;

  and upcall_fixup (name:string) : fixup =
    Semant.import cx LIB_rustrt name;

  and trans_upcall
      (name:string)
      (ret:Il.cell)
      (args:Il.operand array)
      : unit =
    abi.Abi.abi_emit_native_call (emitter())
      ret nabi_rust (upcall_fixup name) args;

  and trans_void_upcall
      (name:string)
      (args:Il.operand array)
      : unit =
    abi.Abi.abi_emit_native_void_call (emitter())
      nabi_rust (upcall_fixup name) args;

  and trans_log_int (a:Ast.atom) : unit =
    trans_void_upcall "upcall_log_int" [| (trans_atom a) |]

  and trans_log_str (a:Ast.atom) : unit =
    trans_void_upcall "upcall_log_str" [| (trans_atom a) |]

  and trans_spawn
      ((*initializing*)_:bool)
      (dst:Ast.lval)
      (realm:Ast.realm)
      (fn_lval:Ast.lval)
      (args:Ast.atom array)
      : unit =
    let (proc_cell, _) = trans_lval_init dst in
    let (fn_cell, fn_slot) = trans_lval fn_lval in
    let tsig =
      match fn_slot.Ast.slot_ty with
          Some (Ast.TY_fn (tsig, _)) -> tsig
        | _ -> bug () "spawned-function slot has wrong type"
    in
    let in_slots = tsig.Ast.sig_input_slots in
    let in_tup = arg_tup_cell in_slots in
    let callsz = Il.referent_ty_size word_bits (snd (need_mem_cell in_tup)) in
    let exit_proc_glue_fixup = get_exit_proc_glue tsig in
    let exit_proc_glue_mem = code_fixup_to_mem exit_proc_glue_fixup in
    let exit_proc_glue_cell = Il.Mem (exit_proc_glue_mem, Il.CodeTy) in

      iflog (fun _ -> annotate "spawn proc: copy args");

      match realm with
          Ast.REALM_thread ->
            begin
              let clone_rt = next_vreg_cell Il.voidptr_t in
                trans_upcall "upcall_new_rt" clone_rt [|  |];
                copy_fn_args (Some clone_rt) proc_cell in_slots args [| |];
                trans_upcall "upcall_spawn_thread" proc_cell
                  [|
                    Il.Cell clone_rt;
                    Il.Cell (alias exit_proc_glue_cell);
                    Il.Cell (alias fn_cell);
                    imm callsz
                  |];
            end
         | _ ->
            begin
              copy_fn_args None proc_cell in_slots args [||];
              trans_upcall "upcall_spawn_local" proc_cell
                [|
                  Il.Cell (alias exit_proc_glue_cell);
                  Il.Cell (alias fn_cell);
                  imm callsz
                |];
            end;
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

  and trans_yield () : unit =
    trans_void_upcall "upcall_yield" [| |];

  and trans_join (proc:Ast.lval) : unit =
    trans_void_upcall "upcall_join" [| trans_atom (Ast.ATOM_lval proc) |]

  and trans_send (chan:Ast.lval) (src:Ast.lval) : unit =
    let (srccell, _) = trans_lval src in
      aliasing false srccell
        begin
          fun src_alias ->
            trans_void_upcall "upcall_send" [| trans_atom (Ast.ATOM_lval chan);
                                               Il.Cell src_alias |];
        end

  and trans_recv (initializing:bool) (dst:Ast.lval) (chan:Ast.lval) : unit =
    let (dstcell, _) = trans_lval_maybe_init initializing dst in
      aliasing true dstcell
        begin
          fun dst_alias ->
            trans_void_upcall "upcall_recv" [| Il.Cell dst_alias;
                                               trans_atom (Ast.ATOM_lval chan) |];
        end

  and trans_init_port (dst:Ast.lval) : unit =
    let (dstcell, dst_slot) = trans_lval_init dst in
    let unit_ty = match slot_ty dst_slot with
        Ast.TY_port t -> t
      | _ -> bug () "init dst of port-init has non-port type"
    in
    let unit_sz = ty_sz abi unit_ty in
      trans_upcall "upcall_new_port" dstcell [| imm unit_sz |]

  and trans_del_port (port:Il.cell) : unit =
    trans_void_upcall "upcall_del_port" [| Il.Cell port |]

  and trans_init_chan (dst:Ast.lval) (port:Ast.lval) : unit =
    let (dstcell, _) = trans_lval_init dst
    in
      trans_upcall "upcall_new_chan" dstcell [| trans_atom (Ast.ATOM_lval port) |]

  and trans_del_chan (chan:Il.cell) : unit =
    trans_void_upcall "upcall_del_chan" [| Il.Cell chan |]

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
    let (dstcell, dst_slot) = trans_lval_init dst in
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
      mov (deref_imm dstcell (word_n 0)) one;
      mov (deref_imm dstcell (word_n 1)) (imm alloc_sz);
      mov (deref_imm dstcell (word_n 2)) (imm init_sz);
      Array.iteri
        begin
          fun i atom ->
            let off = Int64.add (word_n 3) (Int64.mul (Int64.of_int i) unit_sz) in
            let cell = deref_imm dstcell off in
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
    let (rc_mem, _) = need_mem_cell (deref_imm cell (word_n off)) in
    word_at rc_mem

  and exterior_rc_cell (cell:Il.cell) : Il.cell =
    exterior_ctrl_cell cell Abi.exterior_rc_slot_field_refcnt

  and exterior_gc_ctrl_cell (cell:Il.cell) : Il.cell =
    exterior_ctrl_cell cell Abi.exterior_gc_slot_field_ctrl

  and exterior_gc_next_cell (cell:Il.cell) : Il.cell =
    exterior_ctrl_cell cell Abi.exterior_gc_slot_field_next

  and exterior_gc_allocation_size (slot:Ast.slot) : int64 =
    (Int64.add
       (ty_sz  abi (slot_ty slot))
       (word_n Abi.exterior_gc_header_size))

  and exterior_rc_allocation_size (slot:Ast.slot) : int64 =
      (Int64.add
         (ty_sz abi (slot_ty slot))
         (word_n Abi.exterior_rc_header_size))


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
      (dst_cell:Il.cell)
      (src_cell:Il.cell)
      (entries:Ast.ty_rec)
      (f:Il.cell -> Il.cell -> Ast.slot -> (Ast.ty_iso option) -> unit)
      (curr_iso:Ast.ty_iso option)
      : unit =
    iter_tup_slots dst_cell src_cell (Array.map snd entries) f curr_iso

  and iter_tup_slots
      (dst_cell:Il.cell)
      (src_cell:Il.cell)
      (slots:Ast.ty_tup)
      (f:Il.cell -> Il.cell -> Ast.slot -> (Ast.ty_iso option) -> unit)
      (curr_iso:Ast.ty_iso option)
      : unit =
    Array.iteri
      begin
        fun i slot ->
          f (get_element_ptr dst_cell i)
            (get_element_ptr src_cell i)
            slot curr_iso
      end
      slots

  and iter_tag_slots
        (dst_cell:Il.cell)
        (src_cell:Il.cell)
        (ttag:Ast.ty_tag)
        (f:Il.cell -> Il.cell -> Ast.slot -> (Ast.ty_iso option) -> unit)
        (curr_iso:Ast.ty_iso option)
        : unit =
      let tag_keys = sorted_htab_keys ttag in
      let src_tag = get_element_ptr src_cell 0 in
      let dst_tag = get_element_ptr src_cell 0 in
      let src_union = get_element_ptr src_cell 1 in
      let dst_union = get_element_ptr dst_cell 1 in
      let tmp = next_vreg_cell word_ty in
        f dst_tag src_tag word_slot curr_iso;
        mov tmp (Il.Cell src_tag);
        Array.iteri
          begin
            fun i key ->
              (iflog (fun _ -> annotate ("tag case #" ^ (string_of_int i)
                                         ^ " == " ^ (Ast.fmt_to_str Ast.fmt_name key))));
              let jmps = trans_compare Il.JNE (Il.Cell tmp) (imm (Int64.of_int i)) in
              let ttup = Hashtbl.find ttag key in
                iter_tup_slots
                  (get_variant_ptr dst_union i)
                  (get_variant_ptr src_union i)
                  ttup f curr_iso;
                List.iter patch jmps
          end
          tag_keys

  and get_iso_tag tiso =
    tiso.Ast.iso_group.(tiso.Ast.iso_index)

  and iter_ty_slots_full
        (ty:Ast.ty)
        (dst_cell:Il.cell)
        (src_cell:Il.cell)
        (f:Il.cell -> Il.cell -> Ast.slot -> (Ast.ty_iso option) -> unit)
        (curr_iso:Ast.ty_iso option)
        : unit =
        (* 
         * FIXME: this will require some reworking if we support
         * rec, tag or tup slots that fit in a vreg. It requires 
         * addrs presently.
         *)
        match ty with
            Ast.TY_rec entries ->
              iter_rec_slots dst_cell src_cell entries f curr_iso

          | Ast.TY_tup slots ->
              iter_tup_slots dst_cell src_cell slots f curr_iso

          | Ast.TY_tag tag ->
              iter_tag_slots dst_cell src_cell tag f curr_iso

          | Ast.TY_iso tiso ->
              let ttag = get_iso_tag tiso in
                iter_tag_slots dst_cell src_cell ttag f (Some tiso)

          | Ast.TY_fn _
          | Ast.TY_pred _
          | Ast.TY_mod _ ->
              (* TY_fn and TY_mod are stored as pairs, one of which
               * points to an item and one of which is a (possible)
               * pointer to an exterior allocation.
               *)
              let src_binding_field_cell = get_element_ptr src_cell 1 in
              let dst_binding_field_cell = get_element_ptr dst_cell 1 in
                emit (Il.cmp (Il.Cell src_binding_field_cell) zero);
                let null_jmp = mark() in
                  emit (Il.jmp Il.JE Il.CodeNone);
                  (* Call thunk if we have a src binding. *)
                  (* 
                   * FIXME (bug 543738): this is completely wrong, need a second thunk that
                   * generates code to make use of a runtime type descriptor extracted from a
                   * binding tuple. For now this only works by accident.
                   *)
                  (f dst_binding_field_cell
                     src_binding_field_cell
                     (exterior_slot Ast.TY_int) curr_iso);
                  patch null_jmp

          | _ -> ()

  (* 
   * This just calls iter_ty_slots_full with your cell as both src and
   * dst, with an adaptor function that discards the dst slots of the
   * parallel traversal and and calls your provided function on the
   * passed-in src slots.
   *)
  and iter_ty_slots
      (ty:Ast.ty)
      (cell:Il.cell)
      (f:Il.cell -> Ast.slot -> (Ast.ty_iso option) -> unit)
      (curr_iso:Ast.ty_iso option)
      : unit =
    iter_ty_slots_full ty cell cell
      (fun _ src_cell slot curr_iso -> f src_cell slot curr_iso)
      curr_iso

  and drop_ty
      (ty:Ast.ty)
      (cell:Il.cell)
      (curr_iso:Ast.ty_iso option)
      : unit =
    iter_ty_slots ty cell drop_slot curr_iso

  and mark_ty
      (ty:Ast.ty)
      (cell:Il.cell)
      (curr_iso:Ast.ty_iso option)
      : unit =
    iter_ty_slots ty cell mark_slot curr_iso

  and clone_ty
      (rt:Il.cell)
      (ty:Ast.ty)
      (dst:Il.cell)
      (src:Il.cell)
      (curr_iso:Ast.ty_iso option)
      : unit =
    match ty with
        Ast.TY_chan _ ->
          trans_upcall "upcall_clone_chan" dst [| (Il.Cell src); (Il.Cell rt) |]
      | Ast.TY_proc
      | Ast.TY_port _
      | _ when type_is_mutable ty
          -> bug () "cloning mutable type"
      | _ when i64_le (ty_sz abi ty) word_sz
          -> mov dst (Il.Cell src)
      | _ -> iter_ty_slots_full ty dst src (clone_slot rt) curr_iso

  and free_ty
      (ty:Ast.ty)
      (cell:Il.cell)
      : unit =
    match ty with
        Ast.TY_port _ -> trans_del_port cell
      | Ast.TY_chan _ -> trans_del_chan cell
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
              let tmp = next_vreg_cell Il.voidptr_t in
                (* if this has been marked already, jump to exit.*)
                emit (Il.binary Il.AND tmp (Il.Cell gc_word) one);
                let already_marked_jump = mark () in
                  emit (Il.jmp Il.JNZ Il.CodeNone);
                  (* Set mark bit in allocation header. *)
                  emit (Il.binary Il.OR gc_word (Il.Cell gc_word) one);
                  (* Iterate over exterior slots marking outgoing links. *)
                  let (body_mem, _) = need_mem_cell (deref_imm cell exterior_gc_body_off) in
                  let ty = maybe_iso curr_iso ty in
                  let curr_iso = maybe_enter_iso ty curr_iso in
                    lea tmp body_mem;
                    trans_call_mem_glue (get_mark_glue ty curr_iso) tmp;
                    patch null_cell_jump;
                    patch already_marked_jump

        | MEM_interior when ty_is_structured ty ->
            (iflog (fun _ -> annotate ("mark interior slot " ^
                                         (Ast.fmt_to_str Ast.fmt_slot slot))));
            let (mem, _) = need_mem_cell cell in
            let tmp = next_vreg_cell Il.voidptr_t in
            let ty = maybe_iso curr_iso ty in
            let curr_iso = maybe_enter_iso ty curr_iso in
              lea tmp mem;
              trans_call_mem_glue (get_mark_glue ty curr_iso) tmp

        | _ -> ()

  and clone_slot
      (rt:Il.cell)
      (dst:Il.cell)
      (src:Il.cell)
      (dst_slot:Ast.slot)
      (curr_iso:Ast.ty_iso option)
      : unit =
    let ty = slot_ty dst_slot in
      match dst_slot.Ast.slot_mode with
          Ast.MODE_exterior _ ->
            let ty = maybe_iso curr_iso ty in
            let curr_iso = maybe_enter_iso ty curr_iso in
            let dst = deref_slot true dst dst_slot in
            let glue_fix = get_clone_glue (slot_ty dst_slot) curr_iso in
              trans_call_clone_glue dst glue_fix src rt

        | Ast.MODE_read_alias
        | Ast.MODE_write_alias -> bug () "cloning into alias slot"
        | Ast.MODE_interior _ -> clone_ty rt ty dst src curr_iso

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
            let (rc_mem, _) = need_mem_cell (deref_imm cell (word_n rc_off)) in
            let rc = word_at rc_mem in
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
            let (mem, _) = need_mem_cell cell in
            let vr = next_vreg_cell Il.voidptr_t in
            let ty = maybe_iso curr_iso ty in
            let curr_iso = maybe_enter_iso ty curr_iso in
              lea vr mem;
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
              let tmp = next_vreg_cell Il.voidptr_t in
              let rc = exterior_rc_cell cell in
                mov rc one;
                lea tmp (Il.Abs (Asm.M_POS fix));
                mov ctrl (Il.Cell tmp);
                iflog (fun _ -> annotate "init GC exterior: load next-pointer");
                let next = exterior_gc_next_cell cell in
                  mov next (Il.Cell (pp_imm (word_n Abi.proc_field_gc_alloc_chain)));

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

  and deref_exterior (initializing:bool) (cell:Il.cell) (slot:Ast.slot) : Il.typed_mem =
    let body_ty =
      match pointee_type cell with
          Il.StructTy parts
            when (Array.length parts == 2) &&
              (parts.(0) = Il.ScalarTy word_ty) -> parts.(1)
        | ty -> bug () "Dereferencing exterior cell with bad IL type: %s"
            (Il.string_of_referent_ty ty)
    in
      iflog (fun _ -> annotate ("deref exterior: " ^
                                  (if initializing
                                   then "init"
                                   else "access") ^ ", " ^
                                  (Il.string_of_cell
                                     abi.Abi.abi_str_of_hardreg cell)));
      if initializing
      then init_exterior_slot cell slot;
      let (mem, _) =
        need_mem_cell (deref_imm cell (exterior_body_off slot))
      in
        (mem, body_ty)

  and deref_slot (initializing:bool) (cell:Il.cell) (slot:Ast.slot) : Il.cell =
    match slot.Ast.slot_mode with
        Ast.MODE_interior _ -> cell
      | Ast.MODE_exterior _ -> Il.Mem (deref_exterior initializing cell slot)
      | Ast.MODE_read_alias
      | Ast.MODE_write_alias ->
          if initializing
          then cell
          else deref cell

  and trans_copy_tup
      (initializing:bool)
      (dst:Il.cell)
      (src:Il.cell)
      (slots:Ast.ty_tup)
      : unit =
    Array.iteri
      begin
        fun i slot ->
          let sub_dst_cell = get_element_ptr dst i in
          let sub_src_cell = get_element_ptr src i in
            trans_copy_slot initializing sub_dst_cell slot sub_src_cell slot None
      end
      slots

  and trans_copy_slot
      (initializing:bool)
      (dst:Il.cell) (dst_slot:Ast.slot)
      (src:Il.cell) (src_slot:Ast.slot)
      (curr_iso:Ast.ty_iso option)
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
      if not initializing
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
          (* Heavyweight copy: duplicate 1 level of the referent. *)
          anno "heavy";
          trans_copy_slot_heavy initializing
            dst dst_slot src src_slot curr_iso

  (* NB: heavyweight copying here does not mean "producing a deep
   * clone of the entire data tree rooted at the src operand". It means
   * "replicating a single level of the tree".
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
      (initializing:bool)
      (dst:Il.cell) (dst_slot:Ast.slot)
      (src:Il.cell) (src_slot:Ast.slot)
      (curr_iso:Ast.ty_iso option)
      : unit =
    assert (slot_ty src_slot = slot_ty dst_slot);
    iflog (fun _ ->
             annotate ("heavy copy: slot preparation"));

    let ty = slot_ty src_slot in
    let ty = maybe_iso curr_iso ty in
    let curr_iso = maybe_enter_iso ty curr_iso in
    let dst = deref_slot initializing dst dst_slot in
    let src = deref_slot false src src_slot in
      iflog (fun _ ->
               annotate ("heavy copy: referent data"));
      if (i64_le (ty_sz abi ty) word_sz)
      then mov dst (Il.Cell src)
      else
        iter_ty_slots_full ty dst src
          (fun dst src slot curr_iso ->
             trans_copy_slot initializing dst slot src slot curr_iso)
          curr_iso

  and trans_copy
      (initializing:bool)
      (dst:Ast.lval)
      (src:Ast.expr)
      (binop_opt:Ast.binop option) : unit =
    let (dst_cell, dst_slot) = trans_lval_maybe_init initializing dst in
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
                    mov (deref_slot false dst_cell dst_slot) src_operand

              | Ast.EXPR_atom (Ast.ATOM_lval src_lval) ->
                  (* Possibly-large structure copying *)
                  let (src_cell, src_slot) = trans_lval src_lval in
                    trans_copy_slot
                      initializing
                      dst_cell dst_slot
                      src_cell src_slot
                      None
          end
      | Some binop ->
          ignore (trans_binary binop
            (Il.Cell (deref_slot false dst_cell dst_slot))
            (trans_expr src));
          ()


  and trans_init_structural_from_atoms
      (dst:Il.cell)
      (dst_slots:Ast.slot array)
      (atoms:Ast.atom array)
      : unit =
    Array.iteri
      begin
        fun i atom ->
          trans_init_slot_from_atom
            (get_element_ptr dst i)
            dst_slots.(i)
            atom
      end
      atoms


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
                    mov dst (Il.Cell (alias (Il.Mem (force_to_mem src))))
                | _ -> mov (deref_slot true dst dst_slot) src
            end
      | Ast.ATOM_lval src_lval ->
          let (src, src_slot) = trans_lval src_lval in
            trans_init_slot_from_cell dst dst_slot src src_slot


  and trans_init_slot_from_atom_clone
      (rt:Il.cell)
      (dst:Il.cell) (dst_slot:Ast.slot)
      (atom:Ast.atom)
      : unit =
    match dst_slot.Ast.slot_mode with
        Ast.MODE_read_alias
      | Ast.MODE_write_alias ->
          bug () "attempting to clone alias cell"
      | _ ->
          begin
            match atom with
              | Ast.ATOM_literal _ ->
                  let src = trans_atom atom in
                    mov (deref_slot true dst dst_slot) src

              | Ast.ATOM_lval src_lval ->
                  let (src, _) = trans_lval src_lval in
                    clone_slot rt dst src dst_slot None
          end


  and trans_init_slot_from_cell
      (dst:Il.cell) (dst_slot:Ast.slot)
      (src:Il.cell) (src_slot:Ast.slot)
      : unit =
    assert (slot_ty src_slot = slot_ty dst_slot);
    match dst_slot.Ast.slot_mode with
        Ast.MODE_read_alias
      | Ast.MODE_write_alias -> mov dst (Il.Cell (alias src))
      | _ ->
          trans_copy_slot
            true
            dst dst_slot
            src src_slot
            None

  and trans_call_fn
      (initializing:bool)
      (cx:ctxt)
      (dst:Ast.lval)
      (flv:Ast.lval)
      (tsig:Ast.ty_sig)
      (args:Ast.atom array)
      : unit =
    let (dst_cell, _) = trans_lval_maybe_init initializing dst in
    let (fn_cell, _) = trans_callee cx flv in
    let in_slots = tsig.Ast.sig_input_slots in
    let direct = lval_is_direct_fn cx flv in
    let logname () = Ast.sprintf_lval () flv in
    let extra_args =
      if direct then
        [||]
      else
        begin
          iflog (fun _ -> annotate (Printf.sprintf "get env ptr for extra args in call to %s" (logname ())));
          let env_ptr = Il.Cell (deref (get_element_ptr fn_cell 2)) in
            iflog (fun _ -> annotate (Printf.sprintf "get proxy ptr for extra args in call to %s" (logname ())));
            let proxy_ptr = Il.Cell (deref (get_element_ptr fn_cell 3)) in
              [| env_ptr; proxy_ptr |]
        end
    in
      trans_call initializing (lval_is_direct_fn cx flv) logname
        dst_cell fn_cell in_slots args extra_args

  and trans_callee
      (cx:ctxt)
      (flv:Ast.lval)
      : (Il.cell * Ast.ty) =
    (* direct call to item *)
    if lval_is_item cx flv then
      let fn_item = lval_item cx flv in
      let fn_mem = code_fixup_to_mem (get_fn_fixup cx fn_item.id) in
        (Il.Mem (fn_mem, Il.CodeTy), Hashtbl.find cx.ctxt_all_item_types fn_item.id)
    (* direct call to native item *)
    else if lval_is_native_item cx flv then
      let fn_item = lval_native_item cx flv in
      let fn_mem = code_fixup_to_mem (get_fn_fixup cx fn_item.id) in
        (Il.Mem (fn_mem, Il.CodeTy), Hashtbl.find cx.ctxt_all_item_types fn_item.id)
    (* indirect call to computed slot *)
    else
      let (cell, slot) = trans_lval_full false flv abi.Abi.abi_has_abs_code in
        (cell, slot_ty slot)


  and trans_call_mod
      (initializing:bool)
      (dst:Ast.lval)
      (flv:Ast.lval)
      (tmod_hdr:Ast.ty_mod_header)
      (args:Ast.atom array)
      : unit =
    let (dst_cell, _) = trans_lval_maybe_init initializing dst
    in
    let item = lval_item cx flv in
    let item_ty = Hashtbl.find cx.ctxt_all_item_types item.id in
    let glue_fixup =
      match item_ty with
          Ast.TY_mod (Some hdr, _) ->
            get_new_mod_glue item.id hdr
        | _ -> err None "call to unexpected form of module"
    in
    let fn_mem = code_fixup_to_mem glue_fixup in
    let fn_cell = Il.Mem (fn_mem, Il.CodeTy) in
    let (in_slots, _) = tmod_hdr in
      trans_call initializing true (fun _ -> Ast.sprintf_lval () flv)
        dst_cell fn_cell in_slots args [||]

  and trans_call_pred
      (dst_cell:Il.cell)
      (flv:Ast.lval)
      (args:Ast.atom array)
      : unit =
    let (fn_cell, fn_ty) = trans_callee cx flv in
    let tpred =
      match fn_ty with
          Ast.TY_pred tpred -> tpred
        | _ -> bug () "Calling non-predicate."
    in
    let (in_slots, _) = tpred in
      iflog (fun _ -> annotate "predicate call");
      (* FIXME: extra_args if indirect *)
      trans_call true (lval_is_direct_fn cx flv) (fun _ -> Ast.sprintf_lval () flv)
        dst_cell fn_cell in_slots args [||];

  and trans_call_pred_and_check
      (constr:Ast.constr)
      (flv:Ast.lval)
      (args:Ast.atom array)
      : unit =
    let dst_cell = Il.Mem (force_to_mem imm_false) in
      trans_call_pred dst_cell flv args;
      iflog (fun _ -> annotate "predicate check/fail");
      let jmp = trans_compare Il.JE (Il.Cell dst_cell) imm_true in
      let errstr = Printf.sprintf "predicate check: %a"
        Ast.sprintf_constr constr
      in
        trans_cond_fail errstr jmp


  (* FIXME: here's some pseudocode for trans_bind_fn:
   *   1. create the glue function for this lval id, which should:
   *   2. create the env data structure on the heap, which should:
   *      a. have a gc control word iff any of the args are mutable
   *      b. have a refcount word
   *      c. have an inlined tuple of all args that are not _
   *   3. mov dst[0] <- fn ptr
   *   4. mov dst[1] <- env ptr
   *)

  (*
   * Closure representation (always on heap):
   * 
   *  +----+
   *  | rc |
   *  +----+
   *  |    |---> [code]
   *  +----+                 +---+---...---+
   *  |    |---> args vector |   |         |
   *  +----+                 +---+---...---+
   *  |    |---> next closure
   *  +----+
   * 
   *)

  and trans_bind_fn
      (initializing:bool)
      ((*direct*)_:bool)
      (bind_id:node_id)
      (dst:Ast.lval)
      (flv:Ast.lval)
      ((*tsig*)_:Ast.ty_sig)
      (args:Ast.atom option array)
      : unit =
    let (*(dst_cell, _)*)_ = trans_lval_maybe_init initializing dst in
    let ((*fn_cell*)_, fn_slot) = trans_lval_full false flv abi.Abi.abi_has_abs_code in
    let fn_ty = slot_ty fn_slot in
    let bound_args = Array.map (fun arg -> match arg with Some _ -> true | None -> false) args in
    let (*glue_fixup*)_ =
      match fn_ty with
          Ast.TY_fn (fn_sig, _) ->
            get_new_bind_glue bind_id fn_sig bound_args
        (* FIXME: should work for TY_pred too *)
        | _ -> err None "bind of unexpected form of function"
    in
    bug () "trans_bind_fn not yet implemented"


  and trans_arg0 (arg_cell:Il.cell) (output_cell:Il.cell) : unit =
    (* Emit arg0 of any call: the output slot. *)
    iflog (fun _ -> annotate "fn-call arg 0: output slot");
    trans_init_slot_from_cell
      arg_cell (word_write_alias_slot abi)
      output_cell word_slot

  and trans_arg1 (arg_cell:Il.cell) : unit =
    (* Emit arg1 or any call: the process pointer. *)
    iflog (fun _ -> annotate "fn-call arg 1: process pointer");
    trans_init_slot_from_cell
      arg_cell word_slot
      abi.Abi.abi_pp_cell word_slot

  and trans_argN
      (clone_rt:Il.cell option)
      (arg_cell:Il.cell)
      (arg_slot:Ast.slot)
      (arg:Ast.atom)
      : unit =
    match clone_rt with
        None ->
          trans_init_slot_from_atom arg_cell arg_slot arg
      | Some rt ->
          trans_init_slot_from_atom_clone rt arg_cell arg_slot arg

  and code_of_cell (cell:Il.cell) : Il.code =
    match cell with
        Il.Mem (a, Il.CodeTy) -> Il.CodeMem a
      | Il.Mem (_, ty) ->
          bug () "expected code cell, found cell of type %s" (Il.string_of_referent_ty ty)
      | _ -> bug () "loading code from register"

  and copy_fn_args
      (clone_rt:Il.cell option)
      (output_cell:Il.cell)
      (arg_slots:Ast.slot array)
      (args:Ast.atom array)
      (extra_args:Il.operand array)
      : unit =
    assert (Array.length args == Array.length arg_slots);
    let n_args = Array.length args in
    let n_extras = Array.length extra_args in
    let arg_tup = arg_tup_cell arg_slots in
      trans_arg0 (get_element_ptr arg_tup 0) output_cell;
      trans_arg1 (get_element_ptr arg_tup 1);
      Array.iteri
        begin
          fun i slot ->
            iflog (fun _ ->
                     annotate
                       (Printf.sprintf "fn-call arg %d of %d (+ %d extra)"
                          i n_args n_extras));
            trans_argN clone_rt (get_element_ptr arg_tup (2+i)) slot args.(i)
        end
        arg_slots;
      Array.iteri
        begin
          fun i operand ->
            iflog (fun _ ->
                     annotate (Printf.sprintf "fn-call extra-arg %d of %d"
                                 i n_extras));
            mov (extra_arg_cell arg_tup i) operand
        end
        extra_args

  and call_code (code:Il.code) : unit =
    let vr = next_vreg_cell Il.voidptr_t in
      emit (Il.call vr code);


  and trans_callee_code
      (callee_cell:Il.cell)
      (direct:bool)
      (logname:unit -> string)
      : Il.cell =
    if direct then
      callee_cell
    else
      begin
        iflog (fun _ -> annotate (Printf.sprintf "extract closure mem for call to %s" (logname ())));
        let closure_cell = deref (get_element_ptr callee_cell 0) in
          iflog (fun _ -> annotate (Printf.sprintf "extract code mem for call to %s" (logname ())));
          deref (get_element_ptr closure_cell 1)
      end


  and trans_call
      ((*initializing*)_:bool)
      (direct:bool)
      (logname:(unit -> string))
      (output_cell:Il.cell)
      (callee_cell:Il.cell)
      (arg_slots:Ast.slot array)
      (args:Ast.atom array)
      (extra_args:Il.operand array)
      : unit =
    let callee_code_cell = trans_callee_code callee_cell direct logname in
      iflog (fun _ -> annotate (Printf.sprintf "copy args for call to %s" (logname ())));
      copy_fn_args None output_cell arg_slots args extra_args;
      iflog (fun _ -> annotate (Printf.sprintf "call %s" (logname ())));
      (* FIXME (bug 541535 ): we need to actually handle writing to an
       * already-initialised slot. Currently we blindly assume we're
       * initializing, overwrite the slot; this is ok if we're writing
       * to an interior output slot, but we'll leak any exteriors as we
       * do that.  *)
      call_code (code_of_cell callee_code_cell);
      drop_arg_slots arg_slots

  and arg_tup_cell
      (arg_slots:Ast.slot array)
      : Il.cell =
    let mem = sp_imm 0L in
    let ty = Ast.TY_tup (fn_call_tup abi arg_slots) in
    let rty = referent_type abi ty in
      Il.Mem (mem, rty)

  and extra_arg_cell
      (arg_tup:Il.cell)
      (arg:int)
      : Il.cell =
    let (_, rty) = need_mem_cell arg_tup in
    let extra_args_start = Il.referent_ty_size word_bits rty in
    let arg_off = Int64.add extra_args_start (word_n arg) in
    let arg_mem = sp_imm arg_off in
    let arg_referent_ty = Il.ScalarTy (Il.voidptr_t) in
      Il.Mem (arg_mem, arg_referent_ty)

  and drop_arg_slots
      (arg_slots:Ast.slot array)
      : unit =
    let arg_tup = arg_tup_cell arg_slots in
    for i = 0 to arr_max arg_slots do
      iflog (fun _ -> annotate (Printf.sprintf "drop arg %d" i));
      drop_slot (get_element_ptr arg_tup (2+i)) arg_slots.(i) None
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
    let act = if b then ("initializing-" ^ action) else action in
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

      | Ast.STMT_yield ->
          trans_yield ()

      | Ast.STMT_join proc ->
          trans_join proc

      | Ast.STMT_send (chan,src) ->
          trans_send chan src

      | Ast.STMT_spawn (dst, realm, plv, args) ->
          trans_spawn (maybe_init stmt.id "spawn" dst) dst realm plv args

      | Ast.STMT_recv (dst, chan) ->
          trans_recv (maybe_init stmt.id "recv" dst) dst chan

      | Ast.STMT_copy (dst, e_src, binop_opt) ->
          trans_copy (maybe_init stmt.id "copy" dst) dst e_src binop_opt

      | Ast.STMT_call (dst, flv, args) ->
          begin
            let init = maybe_init stmt.id "call" dst in
            match lval_ty cx flv with
                Ast.TY_fn (tsig, _) ->
                  trans_call_fn init cx dst flv tsig args

              | Ast.TY_pred _ ->
                  let (dst_cell, _) = trans_lval_maybe_init init dst
                  in
                    trans_call_pred dst_cell flv args

              | Ast.TY_mod (Some tmod_hdr, _) ->
                    trans_call_mod init dst flv tmod_hdr args

              | _ -> bug () "Calling unexpected lval."
          end

      | Ast.STMT_bind (dst, flv, args) ->
          begin
            let init = maybe_init stmt.id "bind" dst in
              match lval_ty cx flv with
                  Ast.TY_fn (tsig, _) ->
                    trans_bind_fn init (lval_is_direct_fn cx flv) stmt.id dst flv tsig args
                      (* FIXME (bug 544382): implement bind for modules *)
                | _ -> bug () "Binding unexpected lval."
          end

      | Ast.STMT_init_rec (dst, atab) ->
          let (slot_cell, slot) = trans_lval_init dst in
          let dst_slots =
            match slot_ty slot with
                Ast.TY_rec trec -> (Array.map (fun (_, slot) -> slot) trec)
              | _ -> bugi cx stmt.id "non-rec destination type in stmt_init_rec"
          in
          let atoms = Array.map (fun (_, _, atom) -> atom) atab in
          let dst_cell = deref_slot true slot_cell slot in
            trans_init_structural_from_atoms dst_cell dst_slots atoms


      | Ast.STMT_init_tup (dst, mode_atoms) ->
          let (slot_cell, slot) = trans_lval_init dst in
          let dst_slots =
            match slot_ty slot with
                Ast.TY_tup ttup -> ttup
              | _ -> bugi cx stmt.id "non-tup destination type in stmt_init_tup"
          in
          let atoms = Array.map (fun (_, atom) -> atom) mode_atoms in
          let dst_cell = deref_slot true slot_cell slot in
            trans_init_structural_from_atoms dst_cell dst_slots atoms


      | Ast.STMT_init_str (dst, s) ->
          trans_init_str dst s

      | Ast.STMT_init_vec (dst, _, atoms) ->
          trans_init_vec dst atoms

      | Ast.STMT_init_port dst ->
          trans_init_port dst

      | Ast.STMT_init_chan (dst, port) ->
          begin
            match port with
                None ->
                  let (dst_cell, _) =
                    trans_lval_init dst
                  in
                    mov dst_cell imm_false
              | Some p ->
                  trans_init_chan dst p
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
                          let (dst_mem, _) = need_mem_cell (deref (wordptr_at (fp_imm out_mem_disp))) in
                          let atom_ty = atom_type cx at in
                          let dst_slot = interior_slot atom_ty in
                          let dst_ty = referent_type abi atom_ty in
                          let dst_cell = Il.Mem (dst_mem, dst_ty) in
                            trans_init_slot_from_atom dst_cell dst_slot at
                  end;
                  Stack.push (mark()) (Stack.top epilogue_jumps);
                end;
                emit (Il.jmp Il.JMP Il.CodeNone)
            | Some _ -> ()
          end

      | Ast.STMT_decl _ -> ()

      | _ -> bugi cx stmt.id "unhandled form of statement in trans_stmt %a" Ast.sprintf_stmt stmt
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
            fun mem ->
              iter_frame_and_arg_slots fnid
                begin
                  fun key slot_id slot ->
                    match htab_search cx.ctxt_slot_layouts slot_id with
                        None -> ()
                      | Some layout ->
                          let referent_type = slot_id_referent_type slot_id in
                          let disp = layout.layout_offset in
                          let fp_cell = Il.Mem (mem, (Il.ScalarTy (Il.AddrTy referent_type))) in
                          let slot_cell = deref_imm fp_cell disp in
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
            table_of_fixups
              [|
               (* 
                * NB: this must match the struct-offsets given in ABI
                * & rust runtime library.
                *)
                mark_frame_glue_fixup;
                drop_frame_glue_fixup;
                reloc_frame_glue_fixup;
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
    let i = arr_idx tag_keys (Ast.NAME_base (Ast.BASE_ident n)) in
      let _ = log cx "tag variant: %s -> tag value #%d" n i in
      let out_cell = deref (ptr_at (fp_imm out_mem_disp) (Ast.TY_tag ttag)) in
      let tag_cell = get_element_ptr out_cell 0 in
      let union_cell = get_element_ptr out_cell 1 in
      let dst = get_variant_ptr union_cell i in
      let src = Il.Mem (fp_imm arg0_disp, snd (need_mem_cell dst)) in
        (* A clever compiler will inline this. We are not clever. *)
        iflog (fun _ -> annotate (Printf.sprintf "write tag #%d" i));
        mov tag_cell (imm (Int64.of_int i));
        iflog (fun _ -> annotate "copy tag-content tuple");
        trans_copy_tup true dst src slots;
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
    let name = Hashtbl.find cx.ctxt_all_item_names fnid in
    let name =
      match name with
          Ast.NAME_base (Ast.BASE_ident id) -> id
        | Ast.NAME_ext (_, Ast.COMP_ident id) -> id
        | _ -> err (Some fnid) "Native fn %a lacks simple identifier-name" Ast.sprintf_name name
    in
    let args =
      (Array.init (Array.length nfn.Ast.native_fn_input_slots)
         (fun (n:int) ->
            (Il.Cell (word_at (sp_imm (Int64.add (word_n n) (word_n 3)))))))
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
    let pair_with_nil fix =
      Asm.SEQ
        [| Asm.WORD (word_ty_mach, Asm.M_POS fix);
           Asm.WORD (word_ty_mach, Asm.IMM 0L) |]
    in
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
              pair_with_nil fix
        end
        (sorted_htab_keys m)
    in
    let table_fix = new_fixup "mod table" in
    let pair_fix = get_mod_fixup cx id in
    let table_frag = Asm.DEF (table_fix, Asm.SEQ item_pairs) in
    let pair_frag = Asm.DEF (pair_fix, pair_with_nil table_fix) in
      htab_put cx.ctxt_data (DATA_mod_table id) (table_fix, table_frag);
      htab_put cx.ctxt_data (DATA_mod_pair id) (pair_fix, pair_frag)
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
          Ast.MOD_ITEM_mod m -> trans_mod i.id (snd m.Ast.decl_item)
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
          table_of_fixups
            [|
            (* 
             * NB: this must match the struct-offsets given in ABI
             * & rust runtime library.
             *)
              cx.ctxt_c_to_proc_fixup;
              cx.ctxt_main_exit_proc_glue_fixup;
              cx.ctxt_unwind_fixup;
              cx.ctxt_yield_fixup;
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
