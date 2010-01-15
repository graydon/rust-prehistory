
(* 
 * The 'abi' structure is pretty much just a grab-bag of machine
 * dependencies.  Make some attempt to factor it as time goes by.  
 *)


type proc_state =
    STATE_running
  | STATE_calling_c
  | STATE_failing
  | STATE_blocked_exited
  | STATE_blocked_reading
  | STATE_blocked_writing
;;

type upcall =
    UPCALL_log_int
  | UPCALL_log_str
  | UPCALL_new_proc
  | UPCALL_del_proc
  | UPCALL_fail
  | UPCALL_malloc
  | UPCALL_free
  | UPCALL_new_port
  | UPCALL_del_port
  | UPCALL_send
  | UPCALL_recv
  | UPCALL_native
  | UPCALL_new_str
  | UPCALL_grow_proc
  | UPCALL_trace_word
  | UPCALL_trace_str
;;

type nabi =
    NABI_rust
  | NABI_cdecl
;;

(* NB: all these numbers must be kept in sync with runtime. *)
let proc_state_to_code (st:proc_state) : int64 =
  match st with
      STATE_running -> 0L
    | STATE_calling_c -> 1L
    | STATE_failing -> 2L
    | STATE_blocked_exited -> 3L
    | STATE_blocked_reading -> 4L
    | STATE_blocked_writing -> 5L
;;

let upcall_to_code (u:upcall) : int64 =
  match u with
    UPCALL_log_int -> 0L
  | UPCALL_log_str -> 1L
  | UPCALL_new_proc -> 2L
  | UPCALL_del_proc -> 3L
  | UPCALL_fail -> 4L
  | UPCALL_malloc -> 5L
  | UPCALL_free -> 6L
  | UPCALL_new_port -> 7L
  | UPCALL_del_port -> 8L
  | UPCALL_send -> 9L
  | UPCALL_recv -> 10L
  | UPCALL_native -> 11L
  | UPCALL_new_str -> 12L
  | UPCALL_grow_proc -> 13L
  | UPCALL_trace_word -> 14L
  | UPCALL_trace_str -> 15L
;;

let nabi_to_code (a:string) : int64 option =
  match a with
    "cdecl" -> (Some 0L)
  | "rust" -> (Some 1L)
  | _ -> None
;;

let string_to_nabi (a:string) : nabi option =
  match a with
    "cdecl" -> (Some NABI_cdecl)
  | "rust" -> (Some NABI_rust)
  | _ -> None
;;

(* Word offsets for structure fields in rust.h. *)

let proc_field_rt = 0;;
let proc_field_stk = proc_field_rt + 1;;
let proc_field_fn = proc_field_stk + 1;;
let proc_field_runtime_sp = proc_field_fn + 1;;
let proc_field_rust_sp = proc_field_runtime_sp + 1;;
let proc_field_state = proc_field_rust_sp + 1;;
let proc_field_idx = proc_field_state + 1;;
let proc_field_refcnt = proc_field_idx + 1;;
let proc_field_chans = proc_field_refcnt + 1;;
let proc_field_gc_alloc_chain = proc_field_chans + 1;;
let proc_field_upcall_code = proc_field_gc_alloc_chain + 1;;
let proc_field_upcall_args = proc_field_upcall_code + 1;;

let max_upcall_args = 8;;

let global_glue_fns_c_to_proc_glue = 0;;
let global_glue_fns_unwind_glue = 1;;

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
let chan_field_refcnt = 1;;

let stk_field_prev = 0;;
let stk_field_next = stk_field_prev + 1;;
let stk_field_valgrind_id = stk_field_next + 1;;
let stk_field_prev_fp = stk_field_valgrind_id + 1;;
let stk_field_prev_sp = stk_field_prev_fp + 1;;
let stk_field_limit = stk_field_prev_sp + 1;;
let stk_field_data = stk_field_limit + 1;;

type abi =
  {
    abi_word_sz: int64;
    abi_word_bits: Il.bits;
    abi_word_ty: Common.ty_mach;

    abi_is_2addr_machine: bool;
    abi_has_pcrel_data: bool;
    abi_has_pcrel_code: bool;
    abi_has_abs_data: bool;
    abi_has_abs_code: bool;

    abi_n_hardregs: int;
    abi_str_of_hardreg: (int -> string);

    abi_prealloc_quad: (Il.quad' -> Il.quad');
    abi_emit_fn_prologue: (Il.emitter -> int64 -> int64 -> Common.fixup -> int64 -> Common.fixup -> unit);
    abi_emit_fn_epilogue: (Il.emitter -> unit);
    abi_emit_main_prologue: (Il.emitter -> Ast.block -> int64 -> Common.fixup -> int64 -> unit);

    abi_clobbers: (Il.quad -> Il.hreg list);

    abi_emit_proc_state_change: (Il.emitter -> proc_state -> unit);
    abi_emit_upcall: (Il.emitter -> upcall -> Il.operand array -> Common.fixup -> unit);
    abi_emit_c_call: (Il.emitter -> nabi -> Common.fixup -> Il.operand array -> unit);

    (* Global glue. *)
    abi_c_to_proc: (Il.emitter -> unit);
    abi_proc_to_c: (Il.emitter -> unit);
    abi_unwind: (Il.emitter -> Common.fixup -> unit);

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
