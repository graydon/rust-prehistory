
(* 
 * The 'abi' structure is pretty much just a grab-bag of machine
 * dependencies.  Make some attempt to factor it as time goes by.  
 *)

type nabi_conv =
    CONV_rust
  | CONV_cdecl
;;

type nabi = { nabi_indirect: bool;
              nabi_convention: nabi_conv }

let string_to_nabi (a:string) (indirect:bool) : nabi option =
  match a with
      "cdecl" -> (Some { nabi_indirect = indirect;
                         nabi_convention = CONV_cdecl })
    | "rust" -> (Some { nabi_indirect = indirect;
                        nabi_convention = CONV_rust })
    | _ -> None
;;

(* Word offsets for structure fields in rust.h. *)

let proc_field_stk = 0;;
let proc_field_runtime_sp = proc_field_stk + 1;;
let proc_field_rust_sp = proc_field_runtime_sp + 1;;
let proc_field_refcnt = proc_field_rust_sp + 1;;
let proc_field_gc_alloc_chain = proc_field_refcnt + 1;;

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

let port_field_refcnt = 0;;
let chan_field_refcnt = 0;;

let stk_field_valgrind_id = 0 + 1;;
let stk_field_limit = stk_field_valgrind_id + 1;;
let stk_field_data = stk_field_limit + 1;;

let binding_size = 2;;
let binding_field_item = 0;;
let binding_field_binding = 1;;

type abi =
  {
    abi_word_sz: int64;
    abi_word_bits: Il.bits;
    abi_word_ty: Common.ty_mach;

    abi_is_2addr_machine: bool;
    abi_has_absin_data: bool;
    abi_has_absin_code: bool;
    abi_has_abs_data: bool;
    abi_has_abs_code: bool;

    abi_n_hardregs: int;
    abi_str_of_hardreg: (int -> string);

    abi_prealloc_quad: (Il.quad' -> Il.quad');
    abi_emit_fn_prologue: (Il.emitter -> int64 -> Common.fixup -> int64 -> nabi -> Common.fixup -> unit);
    abi_emit_fn_epilogue: (Il.emitter -> unit);

    abi_clobbers: (Il.quad -> Il.hreg list);

    abi_emit_native_call: (Il.emitter -> Il.cell -> nabi -> Common.fixup -> Il.operand array -> unit);
    abi_emit_native_void_call: (Il.emitter -> nabi -> Common.fixup -> Il.operand array -> unit);
    abi_emit_native_call_in_thunk: (Il.emitter -> Il.cell -> nabi -> Common.fixup -> Il.operand array -> unit);

    (* Global glue. *)
    abi_c_to_proc: (Il.emitter -> unit);
    abi_yield: (Il.emitter -> unit);
    abi_unwind: (Il.emitter -> nabi -> Common.fixup -> unit);

    abi_sp_reg: Il.reg;
    abi_fp_reg: Il.reg;
    abi_dwarf_fp_reg: int;
    abi_pp_cell: Il.cell;
    abi_implicit_args_sz: int64;
    abi_frame_base_sz: int64;
    abi_spill_slot: (int64 -> Il.spill -> Il.addr);
  }


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
