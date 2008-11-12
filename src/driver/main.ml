
open Common;;

let fmt = 
  match Sys.os_type with 
	  "Unix" -> Session.Linux_x86_elf
	| "Win32" -> Session.Win32_x86_pe
	| "Cygwin" -> Session.Win32_x86_pe
	| _ -> Session.Linux_x86_elf
;;

let sess:Session.sess = 
  {
	Session.sess_crate = "";
	(* FIXME: need something fancier here for unix sub-flavours. *)
	Session.sess_fmt = fmt;
	Session.sess_out = 
	  ("rust_out" ^ (match fmt with 
						 Session.Linux_x86_elf -> ""
					   | Session.Win32_x86_pe -> ".exe"));
	Session.sess_log_lex = false;
	Session.sess_log_parse = false;
	Session.sess_log_resolve = false;
	Session.sess_log_type = false;
	Session.sess_log_trans = false;
	Session.sess_log_reg = false;
	Session.sess_log_insn = false;
	Session.sess_log_obj = false;
	Session.sess_log_out = stderr;
  }

let argspecs = 
  [
	("-f", Arg.Symbol (["linux-x86-elf"; "win32-x86-pe"], 
					  fun s -> (sess.Session.sess_fmt <- 
								  (match s with 
									   "win32-x86-pe" -> Session.Win32_x86_pe
									 | _ -> Session.Linux_x86_elf))),
	 ("output format (default: " ^ (match sess.Session.sess_fmt with 
										Session.Win32_x86_pe -> "win32-x86-pe"
									  | Session.Linux_x86_elf -> "linux-x86-elf") ^ ")"));
	("-o", Arg.String (fun s -> sess.Session.sess_out <- s),
	 "output filename (default: " ^ sess.Session.sess_out ^ ")");
	("-llex", Arg.Unit (fun _ -> sess.Session.sess_log_lex <- true), "log lexing");
	("-lparse", Arg.Unit (fun _ -> sess.Session.sess_log_parse <- true), "log parsing");
	("-lresolve", Arg.Unit (fun _ -> sess.Session.sess_log_resolve <- true), "log resolution");
	("-ltype", Arg.Unit (fun _ -> sess.Session.sess_log_type <- true), "log type checking");
	("-ltrans", Arg.Unit (fun _ -> sess.Session.sess_log_trans <- true), "log intermediate translation");
	("-lreg", Arg.Unit (fun _ -> sess.Session.sess_log_reg <- true), "log register allocation");
	("-linsn", Arg.Unit (fun _ -> sess.Session.sess_log_insn <- true), "log instruction selection");
	("-lobj", Arg.Unit (fun _ -> sess.Session.sess_log_obj <- true), "log object file generation")
  ]
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


let crate_items = 
  Ll1parser.parse_crate sess Lexer.token 
;;

let _ = 
  try 
	Resolve.resolve_crate sess crate_items
  with
	  Semant.Semant_err (spano, str) -> 
		match spano with 
			None -> Printf.fprintf stderr "semantic error: %s\n%!" str
		  | Some span -> 			  
			  Printf.fprintf stderr "%s:E:%s\n%!" (fmt_span span) str
;;

let emit = Trans.trans_crate crate_items
let _ = Il.print_quads emit.Il.emit_quads
let _ = Ra.reg_alloc emit
let _ = Il.print_quads emit.Il.emit_quads
;;

let pick q = 
  try 
    X86.select_insn q
  with
      X86.Unrecognized -> 
        Printf.fprintf stderr "unrecognized quad: %s\n%!" (Il.string_of_quad q);
        Asm.MARK
;;
          
let code = (Asm.SEQ (Array.map pick emit.Il.emit_quads))        
;;

let _ = match sess.Session.sess_fmt with 
	Session.Win32_x86_pe -> Pe.emit_file sess.Session.sess_out code
  | Session.Linux_x86_elf -> Elf.emit_file sess.Session.sess_out code
;;

(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
