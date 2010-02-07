
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
    Session.sess_log_typestate = false;
    Session.sess_log_mode = false;
    Session.sess_log_mutable = false;
    Session.sess_log_gc = false;
    Session.sess_log_layout = false;
    Session.sess_log_itype = false;
    Session.sess_log_trans = false;
    Session.sess_log_dwarf = false;
    Session.sess_log_ra = false;
    Session.sess_log_insn = false;
    Session.sess_log_asm = false;
    Session.sess_log_obj = false;
    Session.sess_log_out = stderr;
    Session.sess_trace_block = false;
    Session.sess_trace_drop = false;
    Session.sess_trace_tag = false;
    Session.sess_failed = false;
    Session.sess_spans = Hashtbl.create 0;
    (* FIXME (bug 541579): For the time being, our dwarf output appears to do more
       harm than good on OSX.  On other platforms it's the other way
       around. Ancient OSX gdb perhaps?  *)
    Session.sess_emit_dwarf = (not (targ = MacOS_x86_macho));
  }
;;

let dump_file s =
  let ar = Asm.new_asm_reader s in
    Printf.printf "mapped file: %s\n" s;
    let get_sections =
      match sess.Session.sess_targ with
          Win32_x86_pe -> Pe.get_sections
        | MacOS_x86_macho -> Macho.get_sections
        | Linux_x86_elf -> Elf.get_sections
    in
      ignore (get_sections ar);
      exit 0
;;

let argspecs =
  [
    ("-t", Arg.Symbol (["linux-x86-elf"; "win32-x86-pe"; "macos-x86-macho"],
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
    ("-ltypestate", Arg.Unit (fun _ -> sess.Session.sess_log_typestate <- true), "log typestate checking");
    ("-lmode", Arg.Unit (fun _ -> sess.Session.sess_log_mode <- true), "log mode checking");
    ("-lmutable", Arg.Unit (fun _ -> sess.Session.sess_log_mutable <- true), "log mutability checking");
    ("-lgc", Arg.Unit (fun _ -> sess.Session.sess_log_gc <- true), "log gc pointer analysis");
    ("-llayout", Arg.Unit (fun _ -> sess.Session.sess_log_layout <- true), "log frame layout");
    ("-ltrans", Arg.Unit (fun _ -> sess.Session.sess_log_trans <- true), "log intermediate translation");
    ("-litype", Arg.Unit (fun _ -> sess.Session.sess_log_itype <- true;
                            Il.log_iltypes := true), "log IL types");
    ("-ldwarf", Arg.Unit (fun _ -> sess.Session.sess_log_dwarf <- true), "log DWARF record generation");
    ("-lra", Arg.Unit (fun _ -> sess.Session.sess_log_ra <- true), "log register allocation");
    ("-linsn", Arg.Unit (fun _ -> sess.Session.sess_log_insn <- true), "log instruction selection");
    ("-lasm", Arg.Unit (fun _ -> sess.Session.sess_log_asm <- true), "log assembly");
    ("-lobj", Arg.Unit (fun _ -> sess.Session.sess_log_obj <- true), "log object file generation");
    ("-tblock", Arg.Unit (fun _ -> sess.Session.sess_trace_block <- true), "emit block-boundary tracing code");
    ("-tdrop", Arg.Unit (fun _ -> sess.Session.sess_trace_drop <- true), "emit slot-drop tracing code");
    ("-ttag", Arg.Unit (fun _ -> sess.Session.sess_trace_tag <- true), "emit tag-construction tracing code");
    ("-tall", Arg.Unit (fun _ ->
                          sess.Session.sess_trace_block <- true;
                          sess.Session.sess_trace_drop <- true;
                          sess.Session.sess_trace_tag <- true ),
     "emit all tracing code");
    ("-dwarf", Arg.Unit (fun _ -> sess.Session.sess_emit_dwarf <- true), "emit DWARF info (default false)");
    ("-dump", Arg.String dump_file, "dump DWARF info in compiled file")
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
  then Parser.parse_crate sess Lexer.token
  else
    if Filename.check_suffix sess.Session.sess_in ".rs"
    then Parser.parse_srcfile sess Lexer.token
    else
      begin
        Printf.fprintf stderr
          "Error: unrecognized input file type: %s\n%!"
          sess.Session.sess_in;
        exit 1
      end
;;

exit_if_failed ()
;;

if sess.Session.sess_log_ast
then
  begin
    Printf.fprintf stderr "Post-parse AST:\n%!";
    Format.set_margin 80;
    Printf.fprintf stderr "%s\n!" (Ast.fmt_to_str Ast.fmt_crate crate)
  end

let list_to_seq ls = Asm.SEQ (Array.of_list ls);;
let (abi:Abi.abi) = X86.abi;;
let (select_insns:(Il.quads -> Asm.frag)) = X86.select_insns sess;;

(* Semantic passes. *)
let sem_cx = Semant.new_ctxt sess abi crate.node
;;


let main_pipeline _ =
  let _ =
    Array.iter
      (fun proc ->
         proc sem_cx crate;
         exit_if_failed ())
      [| Resolve.process_crate;
         Alias.process_crate;
         Auto.process_crate;
         Type.process_crate;
         Typestate.process_crate;
         Mode.process_crate;
         Mutable.process_crate;
         Gc.process_crate;
         Layout.process_crate;
         Trans.process_crate |]
  in

  (* Tying up various knots, allocating registers and selecting instructions. *)
  let process_code (frame_sz:int64) (code:Semant.code) : Asm.frag =
    let frag =
      match code.Semant.code_vregs_and_spill with
          None -> select_insns code.Semant.code_quads
        | Some (n_vregs, spill_fix) ->
            let (quads', n_spills) =
              Ra.reg_alloc sess
                code.Semant.code_quads
                n_vregs abi frame_sz
            in
            let insns = select_insns quads' in
              begin
                spill_fix.fixup_mem_sz <-
                  Some (Int64.mul
                          (Int64.of_int n_spills)
                          abi.Abi.abi_word_sz);
                insns
              end
    in
      Asm.DEF (code.Semant.code_fixup, frag)
  in

  let process_node_code (node:node_id) (code:Semant.code) : Asm.frag =
    let frame_sz =
      Hashtbl.find
        sem_cx.Semant.ctxt_frame_sizes
        node
    in
      process_code frame_sz code
  in

  let (file_frags:Asm.frag) =
    let process_file file_id frag_code =
      let file_fix = Hashtbl.find sem_cx.Semant.ctxt_file_fixups file_id in
        Asm.DEF (file_fix, list_to_seq (reduce_hash_to_list process_node_code frag_code))
    in
      list_to_seq (reduce_hash_to_list process_file sem_cx.Semant.ctxt_file_code)
  in

    exit_if_failed ();
    let (glue_frags:Asm.frag) =
      let process_glue _ code = process_code 0L code in
        list_to_seq (reduce_hash_to_list process_glue sem_cx.Semant.ctxt_glue_code)
    in

      exit_if_failed ();
      let code = Asm.SEQ [| file_frags; glue_frags |] in
      let data = list_to_seq (reduce_hash_to_list
                                (fun _ (_, i) -> i) sem_cx.Semant.ctxt_data)
      in
      (* Emitting Dwarf and PE/ELF/Macho. *)
      let (dwarf:Dwarf.debug_records) = Dwarf.process_crate sem_cx crate in

        exit_if_failed ();
        let emitter =
          match sess.Session.sess_targ with
              Win32_x86_pe -> Pe.emit_file
            | MacOS_x86_macho -> Macho.emit_file
            | Linux_x86_elf -> Elf.emit_file
        in
          emitter sess code data sem_cx dwarf;
          exit_if_failed ()
;;


main_pipeline ();;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
