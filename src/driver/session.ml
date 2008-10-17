(* 
 * This module goes at the *bottom* of the dependency DAG, and holds logging, 
 * option, and global-state machinery for a single run of the compiler.
 *)

type filename = string
type pos = (filename * int * int) 
type span = {lo: pos; hi: pos}

let fmt_span span = 
	let (filename, line0, col0) = span.lo in 
	let (_, line1, col1) = span.hi in 
	Printf.sprintf "%s:%d:%d - %d:%d" filename line0 col0 line1 col1
;;

type sess_fmt = 
	Linux_x86_elf
  | Win32_x86_pe
;;

type sess = 
{
  mutable sess_crate: filename;
  mutable sess_out: filename;
  mutable sess_fmt: sess_fmt;
  mutable sess_log_lex: bool;
  mutable sess_log_parse: bool;
  mutable sess_log_env: bool;
  mutable sess_log_type: bool;
  mutable sess_log_trans: bool;
  mutable sess_log_reg: bool;
  mutable sess_log_insn: bool;
  mutable sess_log_obj: bool;
  mutable sess_log_out: out_channel;
}
;;
