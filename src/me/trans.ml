(* Translation *)

open Semant;;
open Common;;

let log cx = Session.log "trans"
  cx.ctxt_sess.Session.sess_log_trans
  cx.ctxt_sess.Session.sess_log_out
;;

let iflog cx thunk =
  if cx.ctxt_sess.Session.sess_log_trans
  then thunk ()
  else ()
;;

let marker = Il.Imm (Asm.IMM 0xdeadbeefL);;
let imm_true = Il.Imm (Asm.IMM 1L);;
let imm_false = Il.Imm (Asm.IMM 0L);;
let badlab = Il.Label (-1);;


type intent =
    INTENT_init
  | INTENT_read
  | INTENT_write
  | INTENT_drop
;;

let trans_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  let path = Stack.create () in
  let block_layouts = Stack.create () in
  let file = ref None in

  let (abi:Abi.abi) = cx.ctxt_abi in
  let (word_mem:Il.mem) = abi.Abi.abi_word_mem in
  let (word_sz:int64) = abi.Abi.abi_word_sz in

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
  let emit op dst lhs rhs = Il.emit (emitter()) op dst lhs rhs in
  let next_vreg _ = Il.next_vreg (emitter()) in
  let mark _ : int = (emitter()).Il.emit_pc in
  let patch (i:int) : unit =
    (emitter()).Il.emit_quads.(i)
    <- { (emitter()).Il.emit_quads.(i)
         with Il.quad_lhs = Il.Label (mark ()) };
    (* Insert a dead quad to ensure there's an otherwise-unused patch target here. *)
    emit Il.DEAD Il.Nil Il.Nil Il.Nil
  in

  let epilogue_jumps = Stack.create() in

  let path_name (_:unit) : string =
    String.concat "." (stk_elts_from_bot path)
  in

  let word_at_reg_off (reg:Il.reg option) (off:Asm.expr64) : Il.operand =
    Il.Mem (word_mem, reg, off)
  in

  let word_at_fp_off (imm:int64) : Il.operand =
    word_at_reg_off (Some abi.Abi.abi_fp_reg) (Asm.IMM imm)
  in

  let word_at_sp_off (imm:int64) : Il.operand =
    word_at_reg_off (Some abi.Abi.abi_sp_reg) (Asm.IMM imm)
  in

  let word_at_reg_off_imm
      (reg:Il.reg option)
      (off:Asm.expr64)
      (imm:int64)
      : Il.operand =
    word_at_reg_off reg (Asm.ADD (off, Asm.IMM imm))
  in

  let get_reg_off
      (op:Il.operand)
      : (Il.reg option * Asm.expr64) =
    match op with
        Il.Mem (_, reg, op) -> (reg, op)
      | _ -> err None "Expected reg/off memory operand"
  in

  let mov (dst:Il.operand) (src:Il.operand) : unit =
    emit Il.UMOV dst src Il.Nil
  in

  let lea (dst:Il.operand) (src:Il.operand) : unit =
    match src with
        Il.Mem _ -> emit Il.LEA dst src Il.Nil
      | _ -> err None "LEA on non-memory operand"
  in

  let alias (src:Il.operand) : Il.operand =
    let addr = Il.Reg (Il.next_vreg (emitter())) in
      lea addr src;
      addr
  in

  let deref (mem:Il.operand) : Il.operand =
    let reg = next_vreg() in
    let res = word_at_reg_off (Some reg) (Asm.IMM 0L) in
      mov (Il.Reg reg) mem;
      res
  in

  let deref_off (mem:Il.operand) (off:int64) : Il.operand =
    let reg = next_vreg() in
    let res = word_at_reg_off (Some reg) (Asm.IMM off) in
      mov (Il.Reg reg) mem;
      res
  in

  let force_to_reg (op:Il.operand) : Il.reg =
    let do_mov _ =
      let tmp = next_vreg () in
        mov (Il.Reg tmp) op;
        tmp
    in
      match op with
          Il.Reg r -> r
        | Il.Imm _ -> do_mov ()
        | Il.Mem (mem, _, _) when mem = word_mem -> do_mov ()
        | _ -> err None "unhandled operand type in force_to_reg"
  in

  let rec atom_type (at:Ast.atom) : Ast.ty =
    match at with
        Ast.ATOM_literal {node=(Ast.LIT_str _); id=_} -> Ast.TY_str
      | Ast.ATOM_literal {node=(Ast.LIT_int _); id=_} -> Ast.TY_int
      | Ast.ATOM_literal {node=(Ast.LIT_bool _); id=_} -> Ast.TY_bool
      | Ast.ATOM_literal {node=(Ast.LIT_char _); id=_} -> Ast.TY_char
      | Ast.ATOM_literal {node=(Ast.LIT_nil); id=_} -> Ast.TY_nil
      | Ast.ATOM_literal _ -> err None "unhandled form of literal in atom_type"
      | Ast.ATOM_lval (Ast.LVAL_base nb) ->
          let slot = lval_to_slot cx nb.id in
            begin
              match slot.Ast.slot_ty with
                  None -> err (Some nb.id) "name refers to untyped slot, in atom_type"
                | Some t -> t
            end
      | Ast.ATOM_lval (Ast.LVAL_ext (base, comp)) ->
          let base_ty = atom_type (Ast.ATOM_lval base) in
          let need_ty topt =
            match topt with
                None -> err None "missing type in lval-ext"
              | Some s -> s
          in
            match (base_ty, comp) with
                (Ast.TY_rec elts, Ast.COMP_named (Ast.COMP_ident id)) ->
                  begin
                    match atab_search elts id with
                        Some slot -> need_ty slot.Ast.slot_ty
                      | None -> err None "unknown record-member '%s'" id
                  end

              | (Ast.TY_tup elts, Ast.COMP_named (Ast.COMP_idx i)) ->
                  if 0 <= i && i < (Array.length elts)
                  then need_ty elts.(i).Ast.slot_ty
                  else err None "out-of-range tuple index %d" i

              | (_,_) -> err None "unhandled form of lval-ext"
  in


  let trans_lval_ext
      (base_slot:Ast.slot)
      (base_reg:Il.reg option)
      (base_off:Asm.expr64)
      (comp:Ast.lval_component)
      : (Il.operand * Ast.slot) =

      match (base_slot.Ast.slot_ty, comp) with
          (Some (Ast.TY_rec entries),
           Ast.COMP_named (Ast.COMP_ident id)) ->
            let layouts = layout_rec abi entries in
            let (slot, layout) = atab_find layouts id in
            let disp = layout.layout_offset in
            let oper = (word_at_reg_off_imm
                          base_reg base_off disp)
            in
              (oper, slot)

        | (Some (Ast.TY_tup entries),
           Ast.COMP_named (Ast.COMP_idx i)) ->
            let layouts = layout_tup abi entries in
            let slot = entries.(i) in
            let disp = layouts.(i).layout_offset in
            let oper = (word_at_reg_off_imm
                          base_reg base_off disp)
            in
              (oper, slot)

        | _ -> err None "unhandled form of lval_ext in trans_lval_ext"
  in

  let cell_vreg_num (vr:(int option) ref) : int =
    match !vr with
        None ->
          let v = (Il.next_vreg_num (emitter())) in
            vr := Some v;
            v
      | Some v -> v
  in

  let operand_of_block_slot
      (slotid:node_id)
      : Il.operand =
    match htab_search cx.ctxt_slot_vregs slotid with
        Some vr ->
          Il.Reg (Il.Vreg (cell_vreg_num vr))
      | None ->
          begin
            match htab_search cx.ctxt_slot_layouts slotid with
                None -> err (Some slotid) "slot assigned to neither vreg nor layout"
              | Some layout ->
                  let disp = layout.layout_offset in
                    word_at_fp_off disp
          end
  in

  let rec trans_lval_full
      (lv:Ast.lval)
      (pcrel_ok:bool)
      (imm_ok:bool)
      (intent:intent)
      : (Il.operand * Ast.slot) =

    let return_fixup (fix:fixup) (slot:Ast.slot)
        : (Il.operand * Ast.slot) =
      if pcrel_ok
      then (Il.Pcrel fix, slot)
      else
        let imm = (Il.Imm (Asm.M_POS fix)) in
          if imm_ok
          then (imm, slot)
          else (Il.Reg (force_to_reg imm), slot)
    in

    let return_item (item:Ast.mod_item') (referent:node_id)
        : (Il.operand * Ast.slot) =
      let ty = Hashtbl.find cx.ctxt_all_item_types referent in
      let slot = { Ast.slot_mode = Ast.MODE_interior;
                   Ast.slot_ty = Some ty }
      in
        match item with
            Ast.MOD_ITEM_fn _ ->
              return_fixup (get_fn_fixup cx referent) slot
          | Ast.MOD_ITEM_prog _ ->
              return_fixup (get_prog_fixup cx referent) slot
          | _ ->
              err (Some referent)
                "unhandled item type in trans_lval_full"
    in

    let return_slot (slot:Ast.slot) (referent:node_id)
        : (Il.operand * Ast.slot) =
      let operand = operand_of_block_slot referent in
        (operand, slot)
    in

      match lv with
          Ast.LVAL_ext (base, comp) ->
            let (base_operand, base_slot) = trans_lval_full base false true intent in
            let base_operand' = deref_slot base_operand base_slot intent in
            let (base_reg, base_off) = get_reg_off base_operand' in
              trans_lval_ext base_slot base_reg base_off comp

        | Ast.LVAL_base nb ->
            let referent = lval_to_referent cx nb.id in
              begin
                match htab_search cx.ctxt_all_items referent with
                    Some item -> return_item item referent
                  | None ->
                      let slot = lval_to_slot cx nb.id in
                        return_slot slot referent
              end

  and trans_lval (lv:Ast.lval) (intent:intent) : (Il.operand * Ast.slot) =
    trans_lval_full lv false false intent

  and trans_atom (atom:Ast.atom) : Il.operand =
    match atom with
        Ast.ATOM_lval lv ->
          let (operand, slot) = trans_lval lv INTENT_read in
            deref_slot operand slot INTENT_read

      | Ast.ATOM_literal lit ->
          begin
            match lit.node with
                Ast.LIT_nil ->
                  Il.Nil

              | Ast.LIT_bool false ->
                  Il.Imm (Asm.IMM 0L)

              | Ast.LIT_bool true ->
                  Il.Imm (Asm.IMM 1L)

              | Ast.LIT_char c ->
                  Il.Imm (Asm.IMM (Int64.of_int (Char.code c)))

              | Ast.LIT_int (bi, s) ->
                  Il.Imm (Asm.IMM (Int64.of_int (Big_int.int_of_big_int bi)))

              | Ast.LIT_str s ->
                  let strfix = new_fixup "string fixup" in
                  let str = Asm.DEF (strfix, Asm.ZSTRING s) in
                    cx.ctxt_data_items <- str :: cx.ctxt_data_items;
                    (Il.Imm (Asm.M_POS strfix))

              | _ -> marker
          end

  and trans_expr (expr:Ast.expr) : Il.operand =

    match expr with

        Ast.EXPR_binary (binop, a, b) ->
          let lhs = trans_atom a in
          let rhs = trans_atom b in
          let dst = Il.Reg (Il.next_vreg (emitter())) in
          let arith op =
            emit op dst lhs rhs;
            dst
          in
          let rela cjmp =
            emit Il.CMP Il.Nil (Il.Reg (force_to_reg lhs)) rhs;
            mov dst imm_true;
            let j = mark () in
              emit cjmp Il.Nil badlab Il.Nil;
              mov dst imm_false;
              patch j;
              dst
          in
            begin
              match binop with
                  Ast.BINOP_or -> arith Il.OR
                | Ast.BINOP_and -> arith Il.AND

                | Ast.BINOP_lsl -> arith Il.LSL
                | Ast.BINOP_lsr -> arith Il.LSR
                | Ast.BINOP_asr -> arith Il.ASR

                | Ast.BINOP_add -> arith Il.ADD
                | Ast.BINOP_sub -> arith Il.SUB

                (* FIXME: switch on type of operands, IMUL/IDIV/IMOD etc. *)
                | Ast.BINOP_mul -> arith Il.UMUL
                | Ast.BINOP_div -> arith Il.UDIV
                | Ast.BINOP_mod -> arith Il.UMOD

                | Ast.BINOP_eq -> rela Il.JE
                | Ast.BINOP_ne -> rela Il.JNE
                | Ast.BINOP_lt -> rela Il.JL
                | Ast.BINOP_le -> rela Il.JLE
                | Ast.BINOP_ge -> rela Il.JGE
                | Ast.BINOP_gt -> rela Il.JG

                | _ -> err None "Unhandled binop of expr in trans_expr"
            end

      | Ast.EXPR_unary (unop, a) ->
          let src = trans_atom a in
          let dst = Il.Reg (Il.next_vreg (emitter())) in
          let op = match unop with
              Ast.UNOP_not -> Il.NOT
            | Ast.UNOP_neg -> Il.NEG
          in
            emit op dst src Il.Nil;
            dst

      | Ast.EXPR_atom a ->
          trans_atom a

  and trans_block (block:Ast.block) : unit =
    Stack.push (get_block_layout cx block.id) block_layouts;
    Array.iter trans_stmt block.node;
    let block_slots = Hashtbl.find cx.ctxt_block_slots block.id in
      log cx "end of block, translating slot-frees:";
      (* 
       * FIXME: this is not going to free things in the proper order; 
       * we need to analyze the decl order in an earlier phase and thread
       * it through to here. 
       *)
      Hashtbl.iter
        begin
          fun slotkey slotid ->
            log cx "slot free: %a" Ast.sprintf_slot_key slotkey;
            let slot = Hashtbl.find cx.ctxt_all_slots slotid in
            let operand = operand_of_block_slot slotid in
              trans_drop_slot operand slot
        end
        block_slots;
      ignore (Stack.pop block_layouts)


  and trans_upcall (u:Abi.upcall) (args:Il.operand array) : unit =
    abi.Abi.abi_emit_upcall (emitter()) u args cx.ctxt_proc_to_c_fixup;

  and trans_log_int (a:Ast.atom) : unit =
    trans_upcall Abi.UPCALL_log_int [| (trans_atom a) |]

  and trans_log_str (a:Ast.atom) : unit =
    trans_upcall Abi.UPCALL_log_str [| (trans_atom a) |]

  and trans_spawn
      (dst:Ast.lval)
      (plv:Ast.lval)
      (args:Ast.atom array)
      : unit =
    let (dstop, _) = trans_lval dst INTENT_write in
      trans_upcall Abi.UPCALL_spawn [|  (alias dstop); (trans_atom (Ast.ATOM_lval plv)) |];
      (* FIXME: call the init here. *)
      trans_upcall Abi.UPCALL_sched [| dstop |]

  and trans_check_expr (a:Ast.atom) : unit =
    trans_upcall Abi.UPCALL_check_expr [| (trans_atom a) |]

  and trans_malloc (dst:Il.operand) (nbytes:int64) : unit =
    trans_upcall Abi.UPCALL_malloc [| (alias dst); Il.Imm (Asm.IMM nbytes) |]

  and trans_free (src:Il.operand) : unit =
    trans_upcall Abi.UPCALL_free [| src |]

  and exterior_refcount_cell operand =
    word_at_reg_off (Some (force_to_reg operand)) (Asm.IMM 0L)

  and trans_send (chan:Ast.lval) (src:Ast.atom) : unit =
    trans_upcall Abi.UPCALL_send [| (trans_atom (Ast.ATOM_lval chan)); (trans_atom src) |]

  and trans_recv (dst:Ast.lval) (chan:Ast.lval) : unit =
    let (dstop, _) = trans_lval dst INTENT_write in
      trans_upcall Abi.UPCALL_recv [| (alias dstop); (trans_atom (Ast.ATOM_lval chan)) |]

  and trans_new_port (dst:Ast.lval) : unit =
    let (dstop, _) = trans_lval dst INTENT_write in
    trans_upcall Abi.UPCALL_new_port [| (alias dstop) |]

  and trans_del_port (port:Ast.lval) : unit =
      trans_upcall Abi.UPCALL_del_port [| (trans_atom (Ast.ATOM_lval port)) |]

  and trans_new_chan (dst:Ast.lval) (port:Ast.lval option) : unit =
    let portop =
      match port with
          None -> Il.Imm (Asm.IMM 0L)
        | Some p -> trans_atom (Ast.ATOM_lval p)
    in
    let (dstop, _) = trans_lval dst INTENT_write in
    trans_upcall Abi.UPCALL_new_chan [| (alias dstop); portop |]

  and trans_del_chan (chan:Ast.lval) : unit =
      trans_upcall Abi.UPCALL_del_chan [| (trans_atom (Ast.ATOM_lval chan)) |]


  and drop_rec_entries
      (reg:Il.reg option)
      (off:Asm.expr64)
      (entries:Ast.ty_rec)
      : unit =
    let layouts = layout_rec abi entries in
      Array.iteri
        begin
          fun i (_, (_, layout)) ->
            let (_, sub_slot) = entries.(i) in
              match sub_slot.Ast.slot_mode with
                  Ast.MODE_exterior ->
                    begin
                      let disp = layout.layout_offset in
                      let operand = word_at_reg_off_imm reg off disp in
                      let rc = exterior_refcount_cell operand in
                      drop_exterior_refcount_and_maybe_free rc operand sub_slot
                    end
                | _ -> ()
        end
        layouts

  and drop_tup_slots
      (reg:Il.reg option)
      (off:Asm.expr64)
      (slots:Ast.ty_tup)
      : unit =
    let layouts = layout_tup abi slots in
      Array.iteri
        begin
          fun i layout ->
            let sub_slot = slots.(i) in
              match sub_slot.Ast.slot_mode with
                  Ast.MODE_exterior ->
                    begin
                      let disp = layout.layout_offset in
                      let operand = word_at_reg_off_imm reg off disp in
                      let rc = exterior_refcount_cell operand in
                      drop_exterior_refcount_and_maybe_free rc operand sub_slot
                    end
                | _ -> ()
        end
        layouts


  and drop_exterior_refcount_and_maybe_free
      (rc:Il.operand)
      (operand:Il.operand)
      (slot:Ast.slot)
      : unit =
    let zero = Il.Imm (Asm.IMM 0L) in
    let one = Il.Imm (Asm.IMM 1L) in
      emit Il.SUB rc rc one;
      emit Il.CMP Il.Nil rc zero;
      let j = mark () in
        emit Il.JNE Il.Nil badlab Il.Nil;
        begin
          let (reg, off) = get_reg_off operand in
          let off = Asm.ADD (Asm.IMM word_sz, off) in
            match slot_ty slot with
                Ast.TY_rec entries -> drop_rec_entries reg off entries
              | Ast.TY_tup slots -> drop_tup_slots reg off slots
              | _ -> ()
        end;
        trans_free operand;
        patch j;


  and trans_drop_slot
      (operand:Il.operand)
      (slot:Ast.slot)
      : unit =
    match slot.Ast.slot_mode with
        Ast.MODE_exterior ->
          let rc = exterior_refcount_cell operand in
            drop_exterior_refcount_and_maybe_free rc operand slot
      | _ ->
          begin
            (* FIXME: this will require some reworking if we support
             * rec or tup slots that fit in a vreg. It forces to mem
             * presently. *)
            match slot_ty slot with
                Ast.TY_rec entries ->
                  let (reg, off) = get_reg_off operand in
                    drop_rec_entries reg off entries
              | Ast.TY_tup slots ->
                  let (reg, off) = get_reg_off operand in
                    drop_tup_slots reg off slots
                | _ -> ()
            end


  and init_exterior_slot operand slot =
    let layout = layout_slot cx.ctxt_abi 0L slot in
    let one = Il.Imm (Asm.IMM 1L) in
      trans_malloc operand (Int64.add layout.layout_size word_sz);
      (* Reload rc; operand changed underfoot. *)
      let rc = exterior_refcount_cell operand in
        mov rc one


  and deref_exterior intent operand slot =
    let one = Il.Imm (Asm.IMM 1L) in
      match intent with
          INTENT_init ->
            init_exterior_slot operand slot;
            deref_off operand word_sz

        | INTENT_write ->
            let rc = exterior_refcount_cell operand in
            emit Il.CMP Il.Nil rc one;
            let j = mark () in
              emit Il.JE Il.Nil badlab Il.Nil;
              let src = Il.Reg (Il.next_vreg (emitter())) in
                drop_exterior_refcount_and_maybe_free rc operand slot;
                (* 
                 * Calling trans_copy_slot_deep in 'initializing' mode 
                 * will init the slot for us. Don't double-init.
                 *)
                trans_copy_slot_deep true operand slot src slot;
                patch j;
                deref_off operand word_sz

        | INTENT_read ->
            deref_off operand word_sz

        | INTENT_drop ->
            let rc = exterior_refcount_cell operand in
            drop_exterior_refcount_and_maybe_free rc operand slot;
            Il.Nil

  and deref_slot (operand:Il.operand) (slot:Ast.slot) (intent:intent) : Il.operand =
    match slot.Ast.slot_mode with
        Ast.MODE_interior -> operand
      | Ast.MODE_read_alias -> deref operand
      | Ast.MODE_write_alias -> deref operand
      | Ast.MODE_exterior -> deref_exterior intent operand slot


  and trans_copy_rec
      (initializing:bool)
      (dst_reg:Il.reg option) (dst_off:Asm.expr64) (dst_entries:Ast.ty_rec)
      (src_reg:Il.reg option) (src_off:Asm.expr64) (src_entries:Ast.ty_rec)
      : unit =
    let layouts = layout_rec abi dst_entries in
      Array.iteri
        begin
          fun i (_, (_, layout)) ->
            let disp = layout.layout_offset in
            let sub_dst = word_at_reg_off_imm dst_reg dst_off disp in
            let sub_src = word_at_reg_off_imm src_reg src_off disp in
            let (_, sub_dst_slot) = dst_entries.(i) in
            let (_, sub_src_slot) = src_entries.(i) in
              trans_copy_slot
                initializing
                sub_dst sub_dst_slot
                sub_src sub_src_slot
        end
        layouts


  and trans_copy_tup
      (initializing:bool)
      (dst_reg:Il.reg option) (dst_off:Asm.expr64) (dst_slots:Ast.ty_tup)
      (src_reg:Il.reg option) (src_off:Asm.expr64) (src_slots:Ast.ty_tup)
      : unit =
    let layouts = layout_tup abi dst_slots in
      Array.iteri
        begin
          fun i layout ->
            let disp = layout.layout_offset in
            let sub_src = word_at_reg_off_imm src_reg src_off disp in
            let sub_dst = word_at_reg_off_imm dst_reg dst_off disp in
              trans_copy_slot
                initializing
                sub_dst dst_slots.(i)
                sub_src src_slots.(i)
        end
        layouts


  and trans_copy_slot
      (initializing:bool)
      (dst:Il.operand) (dst_slot:Ast.slot)
      (src:Il.operand) (src_slot:Ast.slot)
      : unit =
    assert (slot_ty src_slot = slot_ty dst_slot);
    if dst_slot.Ast.slot_mode = Ast.MODE_exterior &&
      src_slot.Ast.slot_mode = Ast.MODE_exterior
    then
      (* Shallow copy: move pointer, bump refcount. *)
      begin
        if not initializing
        then
          let dst_rc = exterior_refcount_cell dst in
            drop_exterior_refcount_and_maybe_free dst_rc dst dst_slot
        else
          ();
        log cx "shallow-copy of exterior slots: %a <- %a"
          Ast.sprintf_slot dst_slot
          Ast.sprintf_slot src_slot;
        let src_rc = exterior_refcount_cell src in
          emit Il.ADD src_rc src_rc (Il.Imm (Asm.IMM 1L));
          mov dst src;
      end
    else
      (* Deep copy: recursive copying. *)
      trans_copy_slot_deep initializing dst dst_slot src src_slot


  and trans_copy_slot_deep
      (initializing:bool)
      (dst:Il.operand) (dst_slot:Ast.slot)
      (src:Il.operand) (src_slot:Ast.slot)
      : unit =
    assert (slot_ty src_slot = slot_ty dst_slot);
    let dst_intent =
      if initializing
      then INTENT_init
      else INTENT_write
    in
    let dst = deref_slot dst dst_slot dst_intent in
    let src = deref_slot src src_slot INTENT_read in
      match (dst, dst_slot.Ast.slot_ty,
             src, src_slot.Ast.slot_ty) with
          (Il.Mem (_, dst_reg, dst_off), Some (Ast.TY_rec dst_entries),
           Il.Mem (_, src_reg, src_off), Some (Ast.TY_rec src_entries)) ->
            trans_copy_rec
              initializing
              dst_reg dst_off dst_entries
              src_reg src_off src_entries

        | (Il.Mem (_, dst_reg, dst_off), Some (Ast.TY_tup dst_slots),
           Il.Mem (_, src_reg, src_off), Some (Ast.TY_tup src_slots)) ->
            trans_copy_tup
              initializing
              dst_reg dst_off dst_slots
              src_reg src_off src_slots

        | _ ->
            mov dst src


  and trans_copy
      (initializing:bool)
      (dst:Ast.lval)
      (src:Ast.expr) : unit =
    let dst_intent =
      if initializing
      then INTENT_init
      else INTENT_write
    in
    match src with
        (Ast.EXPR_binary _)
      | (Ast.EXPR_unary _)
      | (Ast.EXPR_atom (Ast.ATOM_literal _)) ->
          (* 
           * Translations of these expr types yield vregs, 
           * so copy is just MOV into the lval. 
           *)
          let (dst_operand, dst_slot) = trans_lval dst dst_intent in
          let src_operand = trans_expr src in
            mov (deref_slot dst_operand dst_slot INTENT_read) src_operand

      | Ast.EXPR_atom (Ast.ATOM_lval src_lval) ->
          (* Possibly-large structure copying *)
          let (dst_operand, dst_slot) = trans_lval dst dst_intent in
          let (src_operand, src_slot) = trans_lval src_lval INTENT_read in
            trans_copy_slot
              initializing
              dst_operand dst_slot
              src_operand src_slot


  and trans_init_slot_from_atom
      (dst:Il.operand) (dst_slot:Ast.slot)
      (atom:Ast.atom)
      : unit =
    match atom with
      | (Ast.ATOM_literal _) ->
          let src = trans_atom atom in
            mov (deref_slot dst dst_slot INTENT_init) src
      | Ast.ATOM_lval src_lval ->
          let (src, src_slot) = trans_lval src_lval INTENT_read in
            trans_init_slot dst dst_slot src src_slot


  and trans_init_slot
      (dst:Il.operand) (dst_slot:Ast.slot)
      (src:Il.operand) (src_slot:Ast.slot)
      : unit =
    assert (slot_ty src_slot = slot_ty dst_slot);
    match dst_slot.Ast.slot_mode with
        Ast.MODE_read_alias
      | Ast.MODE_write_alias -> mov dst (alias src)
      | _ ->
          trans_copy_slot
            true
            dst dst_slot
            src src_slot

  and trans_call
        (dst:Ast.lval)
        (flv:Ast.lval)
        (args:Ast.atom array)
        : unit =
      let vr = Il.Reg (Il.next_vreg (emitter())) in
      let (outmem, _) = trans_lval dst INTENT_write in
      let (fv, fslot) = (trans_lval_full flv
                           abi.Abi.abi_has_pcrel_jumps
                           abi.Abi.abi_has_imm_jumps
                           INTENT_read) in
      let tfn =
        match slot_ty fslot with
            Ast.TY_fn fty -> fty
          | _ -> err None "Calling non-function."
      in
      let (tsig, _) = tfn in
      let fslots = tsig.Ast.sig_input_slots in
      let arg_layouts = layout_call_tup abi tfn in
      let word_slot = word_slot abi in
        assert ((Array.length arg_layouts) == ((Array.length args) + 2));
        for i = 0 to (Array.length arg_layouts) - 1 do
          let dst_operand = word_at_sp_off arg_layouts.(i).layout_offset in
            if i == 0
            then
              begin
                (* Emit arg0: the output slot. *)
                trans_init_slot
                  dst_operand (word_write_alias_slot abi)
                  outmem word_slot
              end
            else
              if i == 1
              then
                (* Emit arg1: the process pointer. *)
                trans_init_slot
                  dst_operand word_slot
                  abi.Abi.abi_pp_operand word_slot
              else
                begin
                  log cx "copying formal arg %d, slot %s"
                    (i-2) (Ast.fmt_to_str Ast.fmt_slot fslots.(i-2));
                  trans_init_slot_from_atom
                    dst_operand fslots.(i-2)
                    args.(i-2)
                end
        done;
        emit Il.CCALL vr fv Il.Nil;
        for i = 2 to (Array.length arg_layouts) - 1 do
          let operand = word_at_sp_off arg_layouts.(i).layout_offset in
            trans_drop_slot operand fslots.(i-2)
        done


  and trans_stmt (stmt:Ast.stmt) : unit =
    (* Helper to localize errors by stmt, at minimum. *)
    try
      trans_stmt_full stmt
    with
        Semant_err (None, msg) -> raise (Semant_err ((Some stmt.id), msg))


  and trans_stmt_full (stmt:Ast.stmt) : unit =
    match stmt.node with

        Ast.STMT_log a ->
          begin
            match atom_type a with
                Ast.TY_str -> trans_log_str a
              | Ast.TY_int -> trans_log_int a
              | _ -> err (Some stmt.id) "unimplemented logging type"
          end

      | Ast.STMT_check_expr a ->
          begin
            match atom_type a with
                Ast.TY_bool -> trans_check_expr a
              | _ -> err (Some stmt.id) "check expr on non-bool"
          end

      | Ast.STMT_spawn (dst, plv, args) -> trans_spawn dst plv args
      | Ast.STMT_send (chan,src) -> trans_send chan src
      | Ast.STMT_recv (dst,chan) -> trans_recv dst chan

      | Ast.STMT_copy (lv_dst, e_src) ->
          if Hashtbl.mem cx.ctxt_copy_stmt_is_init stmt.id
          then
            begin
              iflog cx
                (fun _ ->
                   log cx "initializing-copy on dst lval %a"
                     Ast.sprintf_lval lv_dst);
              trans_copy true lv_dst e_src
            end
          else
              trans_copy false lv_dst e_src

      | Ast.STMT_init_rec (dst, atab) ->
          Array.iter
            begin
              fun (ident, atom) ->
                let lval = (Ast.LVAL_ext
                              (dst, (Ast.COMP_named
                                       (Ast.COMP_ident ident))))
                in
                let expr = Ast.EXPR_atom atom in
                  trans_copy true lval expr
            end
            atab

      | Ast.STMT_init_tup (dst, atoms) ->
          Array.iteri
            begin
              fun i atom ->
                let lval = (Ast.LVAL_ext
                              (dst, (Ast.COMP_named
                                       (Ast.COMP_idx i))))
                in
                let expr = Ast.EXPR_atom atom in
                  trans_copy true lval expr
            end
            atoms

      | Ast.STMT_init_port dst ->
          trans_new_port dst

      | Ast.STMT_init_chan (dst, port) ->
          trans_new_chan dst port

      | Ast.STMT_block block ->
          trans_block block

      | Ast.STMT_while sw ->
          let back_jmp_target = mark () in
          let (head_stmts, head_atom) = sw.Ast.while_lval in
            Array.iter trans_stmt head_stmts;
            let v = trans_atom head_atom in
              emit Il.CMP Il.Nil v imm_false;
              let fwd_jmp_quad = mark () in
                emit Il.JE Il.Nil badlab Il.Nil;
                trans_block sw.Ast.while_body;
                emit Il.JMP Il.Nil (Il.Label back_jmp_target) Il.Nil;
                patch fwd_jmp_quad

      | Ast.STMT_if si ->
          let v = trans_atom si.Ast.if_test in
            emit Il.CMP Il.Nil v imm_true;
            let skip_thn_clause_jmp = mark () in
              emit Il.JNE Il.Nil badlab Il.Nil;
              trans_block si.Ast.if_then;
              begin
                match si.Ast.if_else with
                    None -> patch skip_thn_clause_jmp
                  | Some els ->
                      let skip_els_clause_jmp = mark () in
                        emit Il.JMP Il.Nil badlab Il.Nil;
                        patch skip_thn_clause_jmp;
                        trans_block els;
                        patch skip_els_clause_jmp
              end

      | Ast.STMT_check _ -> ()

      | Ast.STMT_call (dst, flv, args) -> trans_call dst flv args


      | Ast.STMT_ret (proto_opt, atom_opt) ->
          begin
          match proto_opt with
              None ->
                begin
                  begin
                    match atom_opt with
                        None -> ()
                      | Some at ->
                          let disp = abi.Abi.abi_frame_base_sz in
                          let dst = deref (word_at_fp_off disp) in
                          let src = trans_atom at in
                            mov dst src
                  end;
                  Stack.push (mark()) (Stack.top epilogue_jumps);
                end;
                emit Il.JMP Il.Nil badlab Il.Nil
            | Some _ -> ()
          end

      | Ast.STMT_decl _ -> ()

      | _ -> err (Some stmt.id) "unhandled form of statement in trans_stmt"
  in

  let capture_emitted_quads (node:node_id) : unit =
    let e = emitter() in
    let n_vregs = e.Il.emit_next_vreg in
    let quads = e.Il.emit_quads in
    let name = path_name () in
    let f = match !file with
        None -> err (Some node) "Missing file scope when capturing quads."
      | Some f -> f
    in
    let file_list =
      begin
        if not (Hashtbl.mem cx.ctxt_texts f)
        then htab_put cx.ctxt_texts f (ref []);
        Hashtbl.find cx.ctxt_texts f
      end
    in
      begin
        log cx "emitted quads for %s:" name;
        for i = 0 to (Array.length quads) - 1
        do
          log cx "[%6d]\t%s" i (Il.string_of_quad abi.Abi.abi_str_of_hardreg quads.(i));
        done;
        let text = { text_node = node;
                     text_quads = quads;
                     text_n_vregs = n_vregs }
        in
          file_list := text :: (!file_list)
      end
  in

  let trans_fn (fnid:node_id) (body:Ast.block) : unit =
    let framesz = get_framesz cx fnid in
    let callsz = get_callsz cx fnid in
    let spill_fixup = Hashtbl.find cx.ctxt_spill_fixups fnid in
    Stack.push (Stack.create()) epilogue_jumps;
      push_new_emitter ();
      abi.Abi.abi_emit_fn_prologue (emitter()) framesz spill_fixup callsz;
      trans_block body;
      Stack.iter patch (Stack.pop epilogue_jumps);
      abi.Abi.abi_emit_fn_epilogue (emitter());
      capture_emitted_quads fnid;
      pop_emitter ()
  in

  let trans_prog_block (b:Ast.block) (ncomp:string) : fixup =
    let _ = Stack.push ncomp path in
    let fix = new_fixup (path_name ()) in
    let framesz = get_framesz cx b.id in
    let callsz = get_callsz cx b.id in
    let spill_fixup = Hashtbl.find cx.ctxt_spill_fixups b.id in
      push_new_emitter ();
      let dst = Il.Reg (Il.next_vreg (emitter())) in
        Il.emit_full (emitter()) (Some fix) Il.DEAD Il.Nil Il.Nil Il.Nil;
        abi.Abi.abi_emit_main_prologue (emitter()) b framesz spill_fixup callsz;
        trans_block b;
        abi.Abi.abi_emit_proc_state_change (emitter()) Abi.STATE_exiting;
        emit Il.CCALL dst (Il.Pcrel cx.ctxt_proc_to_c_fixup) Il.Nil;
        capture_emitted_quads b.id;
        pop_emitter ();
        ignore (Stack.pop path);
        fix
  in

  let trans_prog (progid:node_id) (p:Ast.prog) : unit =
    let _ = log cx "translating program: %s" (path_name()) in
    let init =
      match p.Ast.prog_init with
          None -> Asm.IMM 0L
        | Some init ->
            begin
              trans_fn init.id init.node.Ast.init_body;
              Asm.M_POS (get_fn_fixup cx init.id)
            end
    in
    let main =
      match p.Ast.prog_main with
          None -> Asm.IMM 0L
        | Some main -> Asm.M_POS (trans_prog_block main "main")
    in
    let fini =
      match p.Ast.prog_fini with
          None -> Asm.IMM 0L
        | Some fini -> Asm.M_POS (trans_prog_block fini "fini")
    in
    let prog =
      let fixup = get_prog_fixup cx progid in
      (* FIXME: extract prog layout from ABI. *)
      Asm.DEF (fixup,
               Asm.SEQ [| Asm.WORD (TY_u32, init);
                          Asm.WORD (TY_u32, main);
                          Asm.WORD (TY_u32, fini) |])
    in
      cx.ctxt_data_items <- prog :: cx.ctxt_data_items
  in

  let rec trans_mod_item
      (item:Ast.mod_item)
      : unit =
    begin
      match item.node with
          Ast.MOD_ITEM_fn f -> trans_fn item.id f.Ast.decl_item.Ast.fn_body
        | Ast.MOD_ITEM_prog p -> trans_prog item.id p.Ast.decl_item
        | _ -> ()
    end
  in

  let visit_mod_item_pre n p i =
    if Hashtbl.mem cx.ctxt_item_files i.id
    then begin
      match !file with
          None -> file := Some i.id
        | Some _ -> err (Some i.id) "Existing source file on file-scope entry."
    end;
    Stack.push n path;
    trans_mod_item i;
    inner.Walk.visit_mod_item_pre n p i
  in
  let visit_mod_item_post n p i =
    inner.Walk.visit_mod_item_post n p i;
    ignore (Stack.pop path);
    if Hashtbl.mem cx.ctxt_item_files i.id
    then begin
      match !file with
          None -> err (Some i.id) "Missing source file on file-scope exit."
        | Some _ -> file := None
    end;
  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post
    }
;;


let fixup_assigning_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =

  let path = Stack.create () in

  let path_name (_:unit) : string =
    String.concat "." (stk_elts_from_bot path)
  in

  let visit_mod_item_pre n p i =
    Stack.push n path;
    begin
      if Hashtbl.mem cx.ctxt_item_files i.id
      then
        htab_put cx.ctxt_file_fixups i.id (new_fixup (path_name()))
      else
        match i.node with
            Ast.MOD_ITEM_fn _ ->
              htab_put cx.ctxt_fn_fixups i.id (new_fixup (path_name()))
          | Ast.MOD_ITEM_prog prog ->
              begin
                let path = path_name() in
                let prog_fixup =
                  if path = cx.ctxt_main_name
                  then cx.ctxt_main_prog
                  else (new_fixup path)
                in
                  log cx "defining '%s' to mod item '%s'" prog_fixup.fixup_name (path_name());
                  htab_put cx.ctxt_prog_fixups i.id prog_fixup;
                  match prog.Ast.decl_item.Ast.prog_init with
                      None -> ()
                    | Some init ->
                        begin
                          (* Treat an init like a fn for purposes of code generation. *)
                          htab_put cx.ctxt_fn_fixups init.id
                            (new_fixup ((path_name()) ^ ".init"))
                        end
              end
          | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in

  let visit_mod_item_post n p i =
    inner.Walk.visit_mod_item_post n p i;
    ignore (Stack.pop path)
  in
  { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post;
    }

let emit_c_to_proc_glue cx =
  let e = Il.new_emitter
    cx.ctxt_abi.Abi.abi_prealloc_quad
    cx.ctxt_abi.Abi.abi_is_2addr_machine
  in
    cx.ctxt_abi.Abi.abi_c_to_proc e cx.ctxt_c_to_proc_fixup;
    if e.Il.emit_next_vreg != 0
    then err None "c-to-proc glue uses nonzero vregs"
    else cx.ctxt_anon_text_quads <-
      (e.Il.emit_quads) :: cx.ctxt_anon_text_quads
;;


let emit_proc_to_c_glue cx =
  let e = Il.new_emitter
    cx.ctxt_abi.Abi.abi_prealloc_quad
    cx.ctxt_abi.Abi.abi_is_2addr_machine
  in
    cx.ctxt_abi.Abi.abi_proc_to_c e cx.ctxt_proc_to_c_fixup;
    if e.Il.emit_next_vreg != 0
    then err None "proc-to-c glue uses nonzero vregs"
    else cx.ctxt_anon_text_quads <-
      (e.Il.emit_quads) :: cx.ctxt_anon_text_quads
;;

let trans_crate
    (cx:ctxt)
    (items:Ast.mod_items)
    : (file_grouped_texts * Asm.item list * fixup) =
  let passes =
    [|
      (fixup_assigning_visitor cx
         Walk.empty_visitor);
      (trans_visitor cx
         Walk.empty_visitor)
    |];
  in
    log cx "translating crate with main program %s" cx.ctxt_main_name;
    run_passes cx passes (log cx "%s") items;
    emit_c_to_proc_glue cx;
    emit_proc_to_c_glue cx;
    (cx.ctxt_texts, cx.ctxt_data_items, cx.ctxt_main_prog)
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
