
open Common;;

let (targ:Common.target) =
  match Sys.os_type with
      "Unix" ->
        (* FIXME: this is an absurd heuristic. *)
        if Sys.file_exists "/System/Library"
        then MacOS_x86_macho
        else Linux_x86_elf
    | "Win32" -> Win32_x86_pe
    | "Cygwin" -> Win32_x86_pe
    | _ -> Linux_x86_elf
;;

let (sess:Session.sess) =
  {
    Session.sess_in = "";
    (* FIXME: need something fancier here for unix sub-flavours. *)
    Session.sess_targ = targ;
    Session.sess_out =
      ("rust_out" ^ (match targ with
                         Linux_x86_elf -> ""
                       | MacOS_x86_macho -> ""
                       | Win32_x86_pe -> ".exe"));
    Session.sess_log_lex = false;
    Session.sess_log_parse = false;
    Session.sess_log_ast = false;
    Session.sess_log_resolve = false;
    Session.sess_log_alias = false;
    Session.sess_log_auto = false;
    Session.sess_log_type = false;
    Session.sess_log_mode = false;
    Session.sess_log_typestate = false;
    Session.sess_log_layout = false;
    Session.sess_log_trans = false;
    Session.sess_log_dwarf = false;
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
                                     | "macos-x86-macho" -> MacOS_x86_macho
                                     | _ -> Linux_x86_elf))),
     ("target (default: " ^ (match sess.Session.sess_targ with
                                        Win32_x86_pe -> "win32-x86-pe"
                                      | Linux_x86_elf -> "linux-x86-elf"
                                      | MacOS_x86_macho -> "macos-x86-macho"
                            ) ^ ")"));
    ("-o", Arg.String (fun s -> sess.Session.sess_out <- s),
     "output filename (default: " ^ sess.Session.sess_out ^ ")");
    ("-llex", Arg.Unit (fun _ -> sess.Session.sess_log_lex <- true), "log lexing");
    ("-lparse", Arg.Unit (fun _ -> sess.Session.sess_log_parse <- true), "log parsing");
    ("-last", Arg.Unit (fun _ -> sess.Session.sess_log_ast <- true), "log post-parse AST");
    ("-lresolve", Arg.Unit (fun _ -> sess.Session.sess_log_resolve <- true), "log resolution");
    ("-lalias", Arg.Unit (fun _ -> sess.Session.sess_log_alias <- true), "log alias analysis");
    ("-lauto", Arg.Unit (fun _ -> sess.Session.sess_log_auto <- true), "log auto type-inference");
    ("-ltype", Arg.Unit (fun _ -> sess.Session.sess_log_type <- true), "log type checking");
    ("-lmode", Arg.Unit (fun _ -> sess.Session.sess_log_mode <- true), "log mode checking");
    ("-ltypestate", Arg.Unit (fun _ -> sess.Session.sess_log_typestate <- true), "log typestate checking");
    ("-llayout", Arg.Unit (fun _ -> sess.Session.sess_log_layout <- true), "log frame layout");
    ("-ltrans", Arg.Unit (fun _ -> sess.Session.sess_log_trans <- true), "log intermediate translation");
    ("-ldwarf", Arg.Unit (fun _ -> sess.Session.sess_log_dwarf <- true), "log DWARF record generation");
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
  (fun arg -> sess.Session.sess_in <- arg)
  ("usage: " ^ Sys.argv.(0) ^ " [options] (CRATE_FILE.rc|SOURCE_FILE.rs)\n%!")
;;

let _ =
  if sess.Session.sess_out = ""
  then (Printf.fprintf stderr "Error: no output file specified\n%!"; exit 1)
  else ()
;;

let _ =
  if sess.Session.sess_in = ""
  then (Printf.fprintf stderr "Error: empty input filename\n%!"; exit 1)
  else ()
;;


let (crate:Ast.crate) =
  if Filename.check_suffix sess.Session.sess_in ".rc"
  then Ll1parser.parse_crate sess Lexer.token
  else
    if Filename.check_suffix sess.Session.sess_in ".rs"
    then Ll1parser.parse_srcfile sess Lexer.token
    else
      begin
        Printf.fprintf stderr
          "Error: unrecognized input file type: %s\n%!"
          sess.Session.sess_in;
        exit 1
      end
;;
let _ = exit_if_failed ()
;;

if sess.Session.sess_log_ast
then
  begin
    Printf.fprintf stderr "Post-parse AST:\n%!";
    Format.set_margin 80;
    Ast.fmt_crate Format.err_formatter crate;
    Printf.fprintf stderr "\n%!";
  end

let list_to_seq ls = Asm.SEQ (Array.of_list ls);;
let (abi:Abi.abi) = X86.abi;;
let (select_insns:(Il.quads -> Asm.item)) = X86.select_insns sess;;

(* Semantic passes. *)
let sem_cx = Semant.new_ctxt sess abi crate
;;

let _ =
  begin
      Array.iter
        (fun proc ->
           proc sem_cx crate.Ast.crate_items;
           exit_if_failed ())
        [| Resolve.process_crate;
           Alias.process_crate;
           Auto.process_crate;
           Type.process_crate;
           Mode.process_crate;
           Typestate.process_crate;
           Layout.process_crate |]
  end
;;


(* Primary translation from AST -> IL quads. *)
let ((file_texts:Semant.file_grouped_texts),
     (data_items:Asm.item list),
     (entry_prog_fixup:fixup)) = Trans.trans_crate sem_cx crate.Ast.crate_items;;
let _ = exit_if_failed ()
;;
let (data:Asm.item) = Asm.SEQ (Array.of_list data_items)
;;


(* Tying up various knots, allocating registers and selecting instructions. *)
let (text_items:Asm.item list) =
  let accum = ref [] in
  let process_file file_id texts =
    let process_text text =
      let frame_sz =
        Hashtbl.find
          sem_cx.Semant.ctxt_frame_sizes
          text.Semant.text_node
      in
      let (quads', n_spills) =
        Ra.reg_alloc sess
          text.Semant.text_quads
          text.Semant.text_n_vregs
          abi frame_sz
      in
      let insns = select_insns quads' in
        begin
          let spill_fix = Hashtbl.find
            sem_cx.Semant.ctxt_spill_fixups
            text.Semant.text_node
          in
            spill_fix.fixup_mem_sz <-
              Some (Int64.mul
                      (Int64.of_int n_spills)
                      abi.Abi.abi_word_sz);
            match htab_search
              sem_cx.Semant.ctxt_fn_fixups
              text.Semant.text_node
            with
                Some fn_fix -> Asm.DEF (fn_fix, insns)
              | None -> insns
        end
    in
    let file_items = List.map process_text (!texts) in
    let file_fix = Hashtbl.find sem_cx.Semant.ctxt_file_fixups file_id in
      accum := (Asm.DEF (file_fix, list_to_seq file_items)) :: (!accum)
  in
    Hashtbl.iter process_file file_texts;
    !accum
;;
let _ = exit_if_failed ()
;;


let (anon_text_item:Asm.item) =
  list_to_seq (List.map select_insns sem_cx.Semant.ctxt_anon_text_quads);;
let (code:Asm.item) = list_to_seq (anon_text_item :: text_items);;
let _ = exit_if_failed ()
;;


(* Emitting Dwarf and PE/ELF/Macho. *)
let (dwarf:Dwarf.debug_records) = Dwarf.process_crate sem_cx crate.Ast.crate_items;;
let _ = exit_if_failed ()
;;

let emitter =
  match sess.Session.sess_targ with
      Win32_x86_pe -> Pe.emit_file
    | MacOS_x86_macho -> Macho.emit_file
    | Linux_x86_elf -> Elf.emit_file
;;

emitter sess code data dwarf entry_prog_fixup sem_cx.Semant.ctxt_c_to_proc_fixup;;
let _ = exit_if_failed ()

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
