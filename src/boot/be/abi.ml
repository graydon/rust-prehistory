
(*
 * The 'abi' structure is pretty much just a grab-bag of machine
 * dependencies and structure-layout information. Part of the latter
 * is shared with trans and semant.
 *
 * Make some attempt to factor it as time goes by.
 *)

(* Word offsets for structure fields in rust.h, and elsewhere in compiler. *)

let rc_base_field_refcnt = 0;;

let proc_field_refcnt = rc_base_field_refcnt;;
let proc_field_stk = proc_field_refcnt + 1;;
let proc_field_runtime_sp = proc_field_stk + 1;;
let proc_field_rust_sp = proc_field_runtime_sp + 1;;
let proc_field_gc_alloc_chain = proc_field_rust_sp + 1;;
let proc_field_rt = proc_field_gc_alloc_chain + 1;;
let n_visible_proc_fields = proc_field_rt + 1;;

let frame_glue_fns_field_mark = 0;;
let frame_glue_fns_field_drop = 1;;
let frame_glue_fns_field_reloc = 2;;

let exterior_rc_slot_field_refcnt = 0;;
let exterior_rc_slot_field_body = 1;;

let exterior_gc_slot_field_next = (-2);;
let exterior_gc_slot_field_ctrl = (-1);;
let exterior_gc_slot_field_refcnt = 0;;
let exterior_gc_slot_field_body = 1;;

let exterior_rc_header_size = 1;;
let exterior_gc_header_size = 3;;

let exterior_gc_malloc_return_adjustment = 2;;

let port_field_refcnt = rc_base_field_refcnt;;

let chan_field_refcnt = rc_base_field_refcnt;;

let stk_field_valgrind_id = 0 + 1;;
let stk_field_limit = stk_field_valgrind_id + 1;;
let stk_field_data = stk_field_limit + 1;;

let binding_size = 2;;
let binding_field_item = 0;;
let binding_field_binding = 1;;

let general_code_alignment = 16;;

let tydesc_field_size = 0;;
let tydesc_field_align = 1;;
let tydesc_field_copy_glue = 2;;
let tydesc_field_drop_glue = 3;;
let tydesc_field_free_glue = 4;;

let calltup_elt_out_ptr = 0;;
let calltup_elt_proc_ptr = 1;;
let calltup_elt_ty_params = 2;;
let calltup_elt_args = 3;;
let calltup_elt_extra_args = 4;;

let extra_args_elt_closure = 0;;

type abi =
  {
    abi_word_sz: int64;
    abi_word_bits: Il.bits;
    abi_word_ty: Common.ty_mach;

    abi_is_2addr_machine: bool;
    abi_has_pcrel_data: bool;
    abi_has_pcrel_code: bool;

    abi_n_hardregs: int;
    abi_str_of_hardreg: (int -> string);

    abi_prealloc_quad: (Il.quad' -> Il.quad');
    abi_emit_fn_prologue: (Il.emitter -> int64 -> Common.fixup -> int64 -> Common.nabi -> Common.fixup -> unit);
    abi_emit_fn_epilogue: (Il.emitter -> unit);
    abi_emit_fn_tail_call: (Il.emitter -> int64 -> int64 -> Il.code -> int64 -> unit);

    abi_clobbers: (Il.quad -> Il.hreg list);

    abi_emit_native_call: (Il.emitter -> Il.cell -> Common.nabi -> Common.fixup -> Il.operand array -> unit);
    abi_emit_native_void_call: (Il.emitter -> Common.nabi -> Common.fixup -> Il.operand array -> unit);
    abi_emit_native_call_in_thunk: (Il.emitter -> Il.cell -> Common.nabi -> Il.operand -> Il.operand array -> unit);
    abi_emit_inline_memcpy: (Il.emitter -> int64 -> Il.reg -> Il.reg -> Il.reg -> bool -> unit);

    (* Global glue. *)
    abi_c_to_proc: (Il.emitter -> unit);
    abi_yield: (Il.emitter -> unit);
    abi_unwind: (Il.emitter -> Common.nabi -> Common.fixup -> unit);
    abi_get_next_pc_thunk: ((Il.reg * Common.fixup * (Il.emitter -> unit)) option);

    abi_sp_reg: Il.reg;
    abi_fp_reg: Il.reg;
    abi_dwarf_fp_reg: int;
    abi_pp_cell: Il.cell;
    abi_implicit_args_sz: int64;
    abi_frame_base_sz: int64;
    abi_spill_slot: (int64 -> Il.spill -> Il.mem);
  }
;;

let load_fixup_addr
    (e:Il.emitter)
    (out_reg:Il.reg)
    (fix:Common.fixup)
    (rty:Il.referent_ty)
    : unit =

  let cell = Il.Reg (out_reg, Il.AddrTy rty) in
  let op = Il.ImmPtr (fix, rty) in
    Il.emit e (Il.lea cell op);
;;

let load_fixup_codeptr
    (e:Il.emitter)
    (out_reg:Il.reg)
    (fixup:Common.fixup)
    (has_pcrel_code:bool)
    (indirect:bool)
    : Il.code =
  if indirect
  then
    begin
      load_fixup_addr e out_reg fixup (Il.ScalarTy (Il.AddrTy Il.CodeTy));
      Il.CodePtr (Il.Cell (Il.Mem (Il.RegIn (out_reg, None),
                                   Il.ScalarTy (Il.AddrTy Il.CodeTy))))
    end
  else
    if has_pcrel_code
    then (Il.CodePtr (Il.ImmPtr (fixup, Il.CodeTy)))
    else
      begin
        load_fixup_addr e out_reg fixup Il.CodeTy;
        Il.CodePtr (Il.Cell (Il.Reg (out_reg, Il.AddrTy Il.CodeTy)))
      end
;;


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
