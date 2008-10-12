
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
	Session.sess_log_env = false;
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
	("-lenv", Arg.Unit (fun _ -> sess.Session.sess_log_env <- true), "log environment construction");
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
  ("usage: " ^Sys.argv.(0) ^ " [options] CRATE_FILE.rc\n")
;;

let _ =
  if sess.Session.sess_out = "" 
  then (Printf.printf "Error: no output file specified\n"; exit 1)
  else ()
;;
  
let _ = 
  if sess.Session.sess_crate = "" 
  then (Printf.printf "Error: empty crate filename\n"; exit 1)
  else ()
;;


let crate_items = 
  Ll1parser.parse_crate sess Lexer.token 
;;

let _ = 
  try 
	Semant.resolve_mod_items (Semant.root_ctxt sess) crate_items
  with
	  Semant.Semant_err (spano, str) -> 
		match spano with 
			None -> Printf.printf "semantic error: %s\n" str
		  | Some span -> 			  
			  Printf.printf "%s:E:%s\n" (Session.fmt_span span) str
;;

let _ = 
  match sess.Session.sess_fmt with 
	  Session.Win32_x86_pe -> Pe.emit_testfile sess.Session.sess_out
	| Session.Linux_x86_elf -> Elf.emit_testfile sess.Session.sess_out
;;
