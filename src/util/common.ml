(* 
 * This module goes near the *bottom* of the dependency DAG, and holds basic
 * types shared across all phases of the compiler.
 *)

type filename = string
type pos = (filename * int * int) 
type span = {lo: pos; hi: pos}
type 'a spanned = { node: 'a; span: span }

type abi_pseudo_reg = 
	FP (* frame pointer *)
  | PP (* process pointer *)
  | CP (* crate pointer *)
  | RP (* runtime pointer *)
;;

let fmt_span span = 
	let (filename, line0, col0) = span.lo in 
	let (_, line1, col1) = span.hi in 
	Printf.sprintf "%s:%d:%d - %d:%d" filename line0 col0 line1 col1
;;

type fixup = 
	{ fixup_name: string;
	  mutable fixup_file_pos: int option;
	  mutable fixup_file_sz: int option;
	  mutable fixup_mem_pos: int64 option;
	  mutable fixup_mem_sz: int64 option }
;;

let new_fixup (s:string) 
	: fixup = 
  { fixup_name = s;
	fixup_file_pos = None;
	fixup_file_sz = None;
	fixup_mem_pos = None;
	fixup_mem_sz = None }
;;

type layout = 
    { 
      mutable layout_size: int64;
      mutable layout_offset: int64;
      mutable layout_align: int64; 
    }
;;

let new_layout _ = 
  { layout_size = 0L; 
    layout_offset = 0L;
    layout_align = 0L }
;;
