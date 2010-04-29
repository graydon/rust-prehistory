
open Common;;

let _ =
  Gc.set { (Gc.get()) with
             Gc.space_overhead = 400; }
;;

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

let (abi:Abi.abi) = X86.abi;;

let (sess:Session.sess) =
  {
    Session.sess_in = None;
    Session.sess_out = None;
    Session.sess_library_mode = false;
    Session.sess_alt_backend = false;
    (* FIXME: need something fancier here for unix sub-flavours. *)
    Session.sess_targ = targ;
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
    Session.sess_log_gctype = false;
    Session.sess_log_layout = false;
    Session.sess_log_itype = false;
    Session.sess_log_trans = false;
    Session.sess_log_dwarf = false;
    Session.sess_log_ra = false;
    Session.sess_log_insn = false;
    Session.sess_log_asm = false;
    Session.sess_log_obj = false;
    Session.sess_log_out = stdout;
    Session.sess_trace_block = false;
    Session.sess_trace_drop = false;
    Session.sess_trace_tag = false;
    Session.sess_failed = false;
    Session.sess_spans = Hashtbl.create 0;
    Session.sess_report_timing = false;
    Session.sess_report_gc = false;
    Session.sess_report_deps = false;
    Session.sess_timings = Hashtbl.create 0;
  }
;;

let ar_cache = Hashtbl.create 0 ;;
let sects_cache = Hashtbl.create 0;;
let meta_cache = Hashtbl.create 0;;
let die_cache = Hashtbl.create 0;;

let get_ar (filename:filename) : Asm.asm_reader option =
  htab_search_or_add ar_cache filename
    begin
      fun _ ->
        let sniff =
          match sess.Session.sess_targ with
              Win32_x86_pe -> Pe.sniff
            | MacOS_x86_macho -> Macho.sniff
            | Linux_x86_elf -> Elf.sniff
        in
          sniff sess filename
    end
;;


let get_sects (filename:filename) :
    (Asm.asm_reader * ((string,(int*int)) Hashtbl.t)) option =
  htab_search_or_add sects_cache filename
    begin
      fun _ ->
        match get_ar filename with
            None -> None
          | Some ar ->
              let get_sections =
                match sess.Session.sess_targ with
                    Win32_x86_pe -> Pe.get_sections
                  | MacOS_x86_macho -> Macho.get_sections
                  | Linux_x86_elf -> Elf.get_sections
              in
                Some (ar, (get_sections sess ar))
    end
;;

let get_meta (filename:filename) : ((Ast.ident * string) array) option =
  htab_search_or_add meta_cache filename
    begin
      fun _ ->
        match get_sects filename with
            None -> None
          | Some (ar, sects) ->
              match htab_search sects ".note.rust" with
                  Some (off, _) ->
                    ar.Asm.asm_seek off;
                    Some (Asm.read_rust_note ar)
                | None -> None
    end
;;

let get_dies_opt (filename:filename) : ((int * ((int,Dwarf.die) Hashtbl.t)) option) =
  htab_search_or_add die_cache filename
    begin
      fun _ ->
        match get_sects filename with
            None -> None
          | Some (ar, sects) ->
              let abbrevs = Dwarf.read_abbrevs sess ar (Hashtbl.find sects ".debug_abbrev") in
              let dies = Dwarf.read_dies sess ar (Hashtbl.find sects ".debug_info")  abbrevs in
                ar.Asm.asm_close ();
                Some dies
    end
;;

let get_dies (filename:filename) : (int * ((int,Dwarf.die) Hashtbl.t)) =
  match get_dies_opt filename with
      None ->
        Printf.fprintf stderr "Error: bad crate file: %s\n%!" filename;
        exit 1
    | Some dies -> dies
;;

let get_mod (filename:filename) (nref:node_id ref) (oref:opaque_id ref) : Ast.mod_items =
  let dies = get_dies filename in
  let items = Hashtbl.create 0 in
    Dwarf.extract_mod_items nref oref abi items dies;
    items
;;

let infer_lib_name (ident:filename) : filename =
  match sess.Session.sess_targ with
      Win32_x86_pe -> ident ^ ".dll"
    | MacOS_x86_macho -> "lib" ^ ident ^ ".dylib"
    | Linux_x86_elf -> "lib" ^ ident ^ ".so"
;;

let default_output_filename (sess:Session.sess) : filename option =
  match sess.Session.sess_in with
      None -> None
    | Some fname ->
        let base = Filename.chop_extension (Filename.basename fname) in
        let out =
          if sess.Session.sess_library_mode
          then
            infer_lib_name base
          else
            base ^ (match sess.Session.sess_targ with
                        Linux_x86_elf -> ""
                      | MacOS_x86_macho -> ""
                      | Win32_x86_pe -> ".exe")
        in
          Some out
;;

let set_default_output_filename (sess:Session.sess) : unit =
  match sess.Session.sess_out with
      None -> (sess.Session.sess_out <- default_output_filename sess)
    | _ -> ()
;;


let dump_sig (filename:filename) : unit =
  let items = get_mod filename (ref (Node 0)) (ref (Opaque 0)) in
    Printf.fprintf stdout "%s\n%!" (Ast.fmt_to_str Ast.fmt_mod_items items);
    exit 0
;;

let dump_meta (filename:filename) : unit =
  begin
    match get_meta filename with
        None -> Printf.fprintf stderr "Error: bad crate file: %s\n%!" filename
      | Some meta ->
          Array.iter
            begin
              fun (k,v) ->
                Printf.fprintf stdout "%s = %S\n%!" k v;
            end
            meta
  end;
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
    ("-o", Arg.String (fun s -> sess.Session.sess_out <- Some s),
     "output filename (default: " ^ (Session.filename_of sess.Session.sess_out) ^ ")");
    ("-shared", Arg.Unit (fun _ -> sess.Session.sess_library_mode <- true), "compile a shared-library crate");
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
    ("-lgctype", Arg.Unit (fun _ -> sess.Session.sess_log_gctype <- true), "log gc type analysis");
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
    ("-rtime", Arg.Unit (fun _ -> sess.Session.sess_report_timing <- true), "report timing of compiler phases");
    ("-rgc", Arg.Unit (fun _ -> sess.Session.sess_report_gc <- true), "report gc behavior of compiler");
    ("-rsig", Arg.String dump_sig, "report type-signature from DWARF info in compiled file, then exit");
    ("-rmeta", Arg.String dump_meta, "report metadata from DWARF info in compiled file, then exit");
    ("-rdeps", Arg.Unit (fun _ -> sess.Session.sess_report_deps <- true), "report dependencies of input, then exit");
  ] @ (Glue.alt_argspecs sess)
;;

let exit_if_failed _ =
  if sess.Session.sess_failed
  then exit 1
  else ()
;;

Arg.parse
  argspecs
  (fun arg -> sess.Session.sess_in <- (Some arg))
  ("usage: " ^ Sys.argv.(0) ^ " [options] (CRATE_FILE.rc|SOURCE_FILE.rs)\n%!")
;;

let _ = set_default_output_filename  sess
;;

let _ =
  if sess.Session.sess_out = None
  then (Printf.fprintf stderr "Error: no output file specified\n%!"; exit 1)
  else ()
;;

let _ =
  if sess.Session.sess_in = None
  then (Printf.fprintf stderr "Error: empty input filename\n%!"; exit 1)
  else ()
;;


let (crate:Ast.crate) =
  Session.time_inner "parse" sess
    begin
      fun _ ->
        let infile = Session.filename_of sess.Session.sess_in in
        let crate =
          if Filename.check_suffix infile ".rc"
          then Cexp.parse_crate_file sess get_mod infer_lib_name
          else
            if Filename.check_suffix infile ".rs"
            then Cexp.parse_src_file sess get_mod infer_lib_name
            else
              begin
                Printf.fprintf stderr
                  "Error: unrecognized input file type: %s\n%!"
                  infile;
                exit 1
              end
        in
          if sess.Session.sess_report_deps
          then
            let outfile = (Session.filename_of sess.Session.sess_out) in
            let depfile =
              match sess.Session.sess_targ with
                  Linux_x86_elf
                | MacOS_x86_macho -> outfile ^ ".d"
                | Win32_x86_pe -> (Filename.chop_extension outfile) ^ ".d"
            in
              begin
                Array.iter
                  begin
                    fun out ->
                      Printf.fprintf stdout "%s: \\\n" out;
                      Hashtbl.iter
                        (fun _ file -> Printf.fprintf stdout "    %s \\\n" file)
                        crate.node.Ast.crate_files;
                      Printf.fprintf stdout "\n"
                  end
                  [| outfile; depfile|];
                exit 0
              end
          else
            crate
    end
;;

exit_if_failed ()
;;

if sess.Session.sess_log_ast
then
  begin
    Printf.fprintf stdout "Post-parse AST:\n%!";
    Format.set_margin 80;
    Printf.fprintf stdout "%s\n%!" (Ast.fmt_to_str Ast.fmt_crate crate)
  end

let list_to_seq ls = Asm.SEQ (Array.of_list ls);;
let select_insns (quads:Il.quads) : Asm.frag =
  Session.time_inner "insn" sess
    (fun _ -> X86.select_insns sess quads)
;;


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
         Dead.process_crate;
         Type.process_crate;
         Typestate.process_crate;
         Mode.process_crate;
         Mutable.process_crate;
         Gctype.process_crate;
         Loop.process_crate;
         Layout.process_crate;
         Trans.process_crate |]
  in

  (* Tying up various knots, allocating registers and selecting instructions. *)
  let process_code _ (code:Semant.code) : Asm.frag =
    let frag =
      match code.Semant.code_vregs_and_spill with
          None -> select_insns code.Semant.code_quads
        | Some (n_vregs, spill_fix) ->
            let (quads', n_spills) =
              (Session.time_inner "RA" sess
                 (fun _ ->
                    Ra.reg_alloc sess
                      code.Semant.code_quads
                      code.Semant.code_spill_disp
                      n_vregs abi))
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
      Asm.ALIGN_FILE (Abi.general_code_alignment,
                      Asm.DEF (code.Semant.code_fixup, frag))
  in

  let (file_frags:Asm.frag) =
    let process_file file_id frag_code =
      let file_fix = Hashtbl.find sem_cx.Semant.ctxt_file_fixups file_id in
        Asm.DEF (file_fix, list_to_seq (reduce_hash_to_list process_code frag_code))
    in
      list_to_seq (reduce_hash_to_list process_file sem_cx.Semant.ctxt_file_code)
  in

    exit_if_failed ();
    let (glue_frags:Asm.frag) =
      list_to_seq (reduce_hash_to_list process_code sem_cx.Semant.ctxt_glue_code)
    in

      exit_if_failed ();
      let code = Asm.SEQ [| file_frags; glue_frags |] in
      let data = list_to_seq (reduce_hash_to_list
                                (fun _ (_, i) -> i) sem_cx.Semant.ctxt_data)
      in
      (* Emitting Dwarf and PE/ELF/Macho. *)
      let (dwarf:Dwarf.debug_records) =
        Session.time_inner "dwarf" sess
          (fun _ -> Dwarf.process_crate sem_cx crate)
      in

        exit_if_failed ();
        let emitter =
          match sess.Session.sess_targ with
              Win32_x86_pe -> Pe.emit_file
            | MacOS_x86_macho -> Macho.emit_file
            | Linux_x86_elf -> Elf.emit_file
        in
          Session.time_inner "emit" sess
            (fun _ -> emitter sess crate code data sem_cx dwarf);
          exit_if_failed ()
;;

(* And we're off! *)
if sess.Session.sess_alt_backend
then Glue.alt_pipeline sess sem_cx crate
else main_pipeline ()
;;

if sess.Session.sess_report_timing
then
  begin
    Printf.fprintf stdout "timing:\n\n";
    Array.iter
      begin
        fun name ->
          Printf.fprintf stdout "%20s: %f\n" name
            (Hashtbl.find sess.Session.sess_timings name)
      end
      (sorted_htab_keys sess.Session.sess_timings)
  end;
;;

if sess.Session.sess_report_gc
then Gc.print_stat stdout;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
