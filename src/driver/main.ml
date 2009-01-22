
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
	Session.sess_log_alias = false;
	Session.sess_log_auto = false;
	Session.sess_log_type = false;
	Session.sess_log_layout = false;
	Session.sess_log_trans = false;
	Session.sess_log_ra = false;
	Session.sess_log_insn = false;
	Session.sess_log_asm = false;
	Session.sess_log_obj = false;   
	Session.sess_log_out = stderr;
    Session.sess_failed = false;
    Session.sess_spans = Hashtbl.create 0;
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
	("-lalias", Arg.Unit (fun _ -> sess.Session.sess_log_alias <- true), "log alias analysis");
	("-lauto", Arg.Unit (fun _ -> sess.Session.sess_log_auto <- true), "log auto type-inference");
	("-ltype", Arg.Unit (fun _ -> sess.Session.sess_log_type <- true), "log type checking");
	("-llayout", Arg.Unit (fun _ -> sess.Session.sess_log_layout <- true), "log frame layout");
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

(* Semantic passes. *)
let sem_cx = Semant.new_ctxt sess abi
;;

let _ = 
  begin
      Array.iter 
        (fun proc -> 
           proc sem_cx crate_items;
           exit_if_failed ())
        [| Resolve.process_crate;
           Alias.process_crate;
           Auto.process_crate;
           Layout.process_crate |]
  end
;;

let ((text_items:(string, (node_id * Il.quads * int)) Hashtbl.t), 
     (data_items:Asm.item list),
     (entry_prog_fixup:fixup)) = Trans.trans_crate sem_cx crate_items;;
let _ = exit_if_failed ()
;;

let (text_quads:Il.quads list) = 
  let ra_quads name (node, quads, n_vregs) accum = 
    let frame_sz = Hashtbl.find sem_cx.Semant.ctxt_frame_sizes node in
    let (quads', n_spills) = Ra.reg_alloc sess quads n_vregs abi frame_sz in 
      (Hashtbl.find sem_cx.Semant.ctxt_spill_fixups node).fixup_mem_sz <-
        Some (Int64.mul 
                (Int64.of_int n_spills) 
                abi.Abi.abi_ptr_sz);                                                              
      quads' :: accum
  in
    Hashtbl.fold ra_quads text_items []
;;
let _ = exit_if_failed ()
;;

let (all_quads:Il.quads list) = text_quads @ sem_cx.Semant.ctxt_anon_text_items;;
let (code:Asm.item) = Asm.SEQ (Array.of_list (List.map (X86.select_insns sess) all_quads));;
let _ = exit_if_failed ()
;;

let (data:Asm.item) = Asm.SEQ (Array.of_list data_items)
;;

let _ = match sess.Session.sess_targ with 
	Win32_x86_pe -> Pe.emit_file sess code data entry_prog_fixup sem_cx.Semant.ctxt_c_to_proc_fixup
  | Linux_x86_elf -> Elf.emit_file sess code data entry_prog_fixup
;;

(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
