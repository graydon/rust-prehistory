
open Common;;

let (targ:Common.target) = 
  match Sys.os_type with 
	  "Unix" -> Linux_x86_elf
	| "Win32" -> Win32_x86_pe
	| "Cygwin" -> Win32_x86_pe
	| _ -> Linux_x86_elf
;;

let (sess:Session.sess) = 
  {
	Session.sess_crate = "";
	(* FIXME: need something fancier here for unix sub-flavours. *)
	Session.sess_targ = targ;
	Session.sess_out = 
	  ("rust_out" ^ (match targ with 
						 Linux_x86_elf -> ""
					   | Win32_x86_pe -> ".exe"));
	Session.sess_log_lex = false;
	Session.sess_log_parse = false;
	Session.sess_log_resolve = false;
	Session.sess_log_type = false;
	Session.sess_log_trans = false;
	Session.sess_log_ra = false;
	Session.sess_log_insn = false;
	Session.sess_log_asm = false;
	Session.sess_log_obj = false;   
	Session.sess_log_out = stderr;
    Session.sess_failed = false;
  }

let argspecs = 
  [
	("-t", Arg.Symbol (["linux-x86-elf"; "win32-x86-pe"], 
					   fun s -> (sess.Session.sess_targ <- 
								  (match s with 
									   "win32-x86-pe" -> Win32_x86_pe
									 | _ -> Linux_x86_elf))),
	 ("target (default: " ^ (match sess.Session.sess_targ with 
										Win32_x86_pe -> "win32-x86-pe"
									  | Linux_x86_elf -> "linux-x86-elf") ^ ")"));
	("-o", Arg.String (fun s -> sess.Session.sess_out <- s),
	 "output filename (default: " ^ sess.Session.sess_out ^ ")");
	("-llex", Arg.Unit (fun _ -> sess.Session.sess_log_lex <- true), "log lexing");
	("-lparse", Arg.Unit (fun _ -> sess.Session.sess_log_parse <- true), "log parsing");
	("-lresolve", Arg.Unit (fun _ -> sess.Session.sess_log_resolve <- true), "log resolution");
	("-ltype", Arg.Unit (fun _ -> sess.Session.sess_log_type <- true), "log type checking");
	("-ltrans", Arg.Unit (fun _ -> sess.Session.sess_log_trans <- true), "log intermediate translation");
	("-lra", Arg.Unit (fun _ -> sess.Session.sess_log_ra <- true), "log register allocation");
	("-linsn", Arg.Unit (fun _ -> sess.Session.sess_log_insn <- true), "log instruction selection");
	("-lasm", Arg.Unit (fun _ -> sess.Session.sess_log_asm <- true), "log assembly");
	("-lobj", Arg.Unit (fun _ -> sess.Session.sess_log_obj <- true), "log object file generation")
  ]
;;

let exit_if_failed _ = 
  if sess.Session.sess_failed 
  then exit 1 
  else ()
;;

Arg.parse 
  argspecs 
  (fun arg -> sess.Session.sess_crate <- arg)
  ("usage: " ^ Sys.argv.(0) ^ " [options] CRATE_FILE.rc\n%!")
;;

let _ =
  if sess.Session.sess_out = "" 
  then (Printf.fprintf stderr "Error: no output file specified\n%!"; exit 1)
  else ()
;;
  
let _ = 
  if sess.Session.sess_crate = "" 
  then (Printf.fprintf stderr "Error: empty crate filename\n%!"; exit 1)
  else ()
;;


let (crate_items:Ast.mod_items) = Ll1parser.parse_crate sess Lexer.token;;
let _ = exit_if_failed ()
;;

let (abi:Abi.abi) = X86.abi;;

let _ = Resolve.resolve_crate sess abi crate_items;;
let _ = exit_if_failed ()
;;

let ((text_items:(string, (Il.quads * int)) Hashtbl.t), 
     (data_items:Asm.item list)) = Trans.trans_crate sess abi crate_items;;
let _ = exit_if_failed ()
;;

let (text_quads:Il.quads list) = 
  (* FIXME: get the frame size from the translation. *)
  let frame_sz = 64L in 
    Hashtbl.fold 
      (fun name (quads,n_vregs) accum -> (Ra.reg_alloc sess quads n_vregs abi frame_sz) :: accum) 
      text_items [];;
let _ = exit_if_failed ()
;;

let (code:Asm.item) = Asm.SEQ (Array.of_list (List.map (X86.select_insns sess) text_quads));;
let _ = exit_if_failed ()
;;

let _ = match sess.Session.sess_targ with 
	Win32_x86_pe -> Pe.emit_file sess code
  | Linux_x86_elf -> Elf.emit_file sess code
;;

(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
