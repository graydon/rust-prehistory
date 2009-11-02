open Asm;;
open Common;;

(* Mach-O writer. *)

let (cpu_arch_abi64:int64) = 0x01000000L
;;

let (mh_magic:int64) = 0xfeedfaceL
;;

let cpu_subtype_intel (f:int64) (m:int64) : int64 =
  Int64.add f (Int64.shift_left m 4)
;;

type cpu_type =
    (* Maybe support more later. *)
    CPU_TYPE_X86
  | CPU_TYPE_X86_64
  | CPU_TYPE_ARM
  | CPU_TYPE_POWERPC
;;

type cpu_subtype =
    (* Maybe support more later. *)
    CPU_SUBTYPE_X86_ALL
  | CPU_SUBTYPE_X86_64_ALL
  | CPU_SUBTYPE_ARM_ALL
  | CPU_SUBTYPE_POWERPC_ALL
;;

type file_type =
    MH_OBJECT
  | MH_EXECUTE
  | MH_FVMLIB
  | MH_CORE
  | MH_PRELOAD
  | MH_DYLIB
  | MH_DYLINKER
  | MH_BUNDLE
  | MH_DYLIB_STUB
  | MH_DSYM
;;

let file_type_code (ft:file_type) : int64 =
  match ft with
      MH_OBJECT ->0x1L      (* object *)
    | MH_EXECUTE -> 0x2L    (* executable *)
    | MH_FVMLIB -> 0x3L     (* fixed-VM shared lib *)
    | MH_CORE -> 0x4L       (* core *)
    | MH_PRELOAD -> 0x5L    (* preloaded executable *)
    | MH_DYLIB -> 0x6L      (* dynamic lib *)
    | MH_DYLINKER -> 0x7L   (* dynamic linker *)
    | MH_BUNDLE -> 0x8L     (* bundle *)
    | MH_DYLIB_STUB -> 0x9L (* shared lib stub *)
    | MH_DSYM -> 0xaL       (* debuginfo only *)
;;

type file_flag =
    MH_NOUNDEFS
  | MH_INCRLINK
  | MH_DYLDLINK
  | MH_BINDATLOAD
  | MH_PREBOUND
  | MH_SPLIT_SEGS
  | MH_LAZY_INIT
  | MH_TWOLEVEL
  | MH_FORCE_FLAT
  | MH_NOMULTIDEFS
  | MH_NOFIXPREBINDING
  | MH_PREBINDABLE
  | MH_ALLMODSBOUND
  | MH_SUBSECTIONS_VIA_SYMBOLS
  | MH_CANONICAL
  | MH_WEAK_DEFINES
  | MH_BINDS_TO_WEAK
  | MH_ALLOW_STACK_EXECUTION
  | MH_ROOT_SAFE
  | MH_SETUID_SAFE
  | MH_NO_REEXPORTED_DYLIBS
  | MH_PIE
;;

let file_flag_code (ff:file_flag) : int64 =
  match ff with
      MH_NOUNDEFS -> 0x1L
    | MH_INCRLINK -> 0x2L
    | MH_DYLDLINK -> 0x4L
    | MH_BINDATLOAD -> 0x8L
    | MH_PREBOUND -> 0x10L
    | MH_SPLIT_SEGS -> 0x20L
    | MH_LAZY_INIT -> 0x40L
    | MH_TWOLEVEL -> 0x80L
    | MH_FORCE_FLAT -> 0x100L
    | MH_NOMULTIDEFS -> 0x200L
    | MH_NOFIXPREBINDING -> 0x400L
    | MH_PREBINDABLE -> 0x800L
    | MH_ALLMODSBOUND -> 0x1000L
    | MH_SUBSECTIONS_VIA_SYMBOLS -> 0x2000L
    | MH_CANONICAL -> 0x4000L
    | MH_WEAK_DEFINES -> 0x8000L
    | MH_BINDS_TO_WEAK -> 0x10000L
    | MH_ALLOW_STACK_EXECUTION -> 0x20000L
    | MH_ROOT_SAFE -> 0x40000L
    | MH_SETUID_SAFE -> 0x80000L
    | MH_NO_REEXPORTED_DYLIBS -> 0x100000L
    | MH_PIE -> 0x200000L
;;


type vm_prot =
    VM_PROT_NONE
  | VM_PROT_READ
  | VM_PROT_WRITE
  | VM_PROT_EXECUTE
;;


type load_command =
    LC_SEGMENT
  | LC_SYMTAB
  | LC_SYMSEG
  | LC_THREAD
  | LC_UNIXTHREAD
  | LC_LOADFVMLIB
  | LC_IDFVMLIB
  | LC_IDENT
  | LC_FVMFILE
  | LC_PREPAGE
  | LC_DYSYMTAB
  | LC_LOAD_DYLIB
  | LC_ID_DYLIB
  | LC_LOAD_DYLINKER
  | LC_ID_DYLINKER
  | LC_PREBOUND_DYLIB
  | LC_ROUTINES
  | LC_SUB_FRAMEWORK
  | LC_SUB_UMBRELLA
  | LC_SUB_CLIENT
  | LC_SUB_LIBRARY
  | LC_TWOLEVEL_HINTS
  | LC_PREBIND_CKSUM
  | LC_LOAD_WEAK_DYLIB
  | LC_SEGMENT_64
  | LC_ROUTINES_64
  | LC_UUID
  | LC_RPATH
  | LC_CODE_SIGNATURE
  | LC_SEGMENT_SPLIT_INFO
  | LC_REEXPORT_DYLIB
  | LC_LAZY_LOAD_DYLIB
  | LC_ENCRYPTION_INFO
;;


let cpu_type_code (cpu:cpu_type) : int64 =
  match cpu with
      CPU_TYPE_X86 -> 7L
    | CPU_TYPE_X86_64 -> Int64.logor 7L cpu_arch_abi64
    | CPU_TYPE_ARM -> 12L
    | CPU_TYPE_POWERPC -> 18L
;;

let cpu_subtype_code (cpu:cpu_subtype) : int64 =
  match cpu with
      CPU_SUBTYPE_X86_ALL -> 3L
    | CPU_SUBTYPE_X86_64_ALL -> 3L
    | CPU_SUBTYPE_ARM_ALL -> 0L
    | CPU_SUBTYPE_POWERPC_ALL -> 0L
;;


let vm_prot_code (vmp:vm_prot) : int64 =
  match vmp with
    VM_PROT_NONE -> 0L
  | VM_PROT_READ -> 1L
  | VM_PROT_WRITE -> 2L
  | VM_PROT_EXECUTE -> 4L
;;


let lc_req_dyld = 0x80000000L;;

let load_command_code (lc:load_command) =
  match lc with
    | LC_SEGMENT -> 0x1L
    | LC_SYMTAB -> 0x2L
    | LC_SYMSEG -> 0x3L
    | LC_THREAD -> 0x4L
    | LC_UNIXTHREAD -> 0x5L
    | LC_LOADFVMLIB -> 0x6L
    | LC_IDFVMLIB -> 0x7L
    | LC_IDENT -> 0x8L
    | LC_FVMFILE -> 0x9L
    | LC_PREPAGE -> 0xaL
    | LC_DYSYMTAB -> 0xbL
    | LC_LOAD_DYLIB -> 0xcL
    | LC_ID_DYLIB -> 0xdL
    | LC_LOAD_DYLINKER -> 0xeL
    | LC_ID_DYLINKER -> 0xfL
    | LC_PREBOUND_DYLIB -> 0x10L
    | LC_ROUTINES -> 0x11L
    | LC_SUB_FRAMEWORK -> 0x12L
    | LC_SUB_UMBRELLA -> 0x13L
    | LC_SUB_CLIENT -> 0x14L
    | LC_SUB_LIBRARY -> 0x15L
    | LC_TWOLEVEL_HINTS -> 0x16L
    | LC_PREBIND_CKSUM -> 0x17L
    | LC_LOAD_WEAK_DYLIB -> Int64.logor lc_req_dyld 0x18L
    | LC_SEGMENT_64 -> 0x19L
    | LC_ROUTINES_64 -> 0x1aL
    | LC_UUID -> 0x1bL
    | LC_RPATH -> Int64.logor lc_req_dyld 0x1cL
    | LC_CODE_SIGNATURE -> 0x1dL
    | LC_SEGMENT_SPLIT_INFO -> 0x1eL
    | LC_REEXPORT_DYLIB -> Int64.logor lc_req_dyld 0x1fL
    | LC_LAZY_LOAD_DYLIB -> 0x20L
    | LC_ENCRYPTION_INFO -> 0x21L
;;


let fixed_sz_string (sz:int) (str:string) : frag =
  if String.length str > sz
  then STRING (String.sub str 0 sz)
  else SEQ [| STRING str; PAD (sz - (String.length str)) |]
;;

let macho_section_command
    (seg_name:string)
    (sect:(string * int * fixup))
    : frag =
  let (sect_name, sect_align, sect_fixup) = sect in
    SEQ [|
      fixed_sz_string 16 sect_name;
      fixed_sz_string 16 seg_name;
      WORD (TY_u32, M_POS sect_fixup);
      WORD (TY_u32, M_SZ sect_fixup);
      WORD (TY_u32, F_POS sect_fixup);
      WORD (TY_u32, IMM (Int64.of_int sect_align));
      WORD (TY_u32, IMM 0L); (* reloff *)
      WORD (TY_u32, IMM 0L); (* nreloc *)
      WORD (TY_u32, IMM 0L); (* flags *)
      WORD (TY_u32, IMM 0L); (* reserved1 *)
      WORD (TY_u32, IMM 0L); (* reserved2 *)
  |]
;;

let macho_segment_command
    (seg_name:string)
    (seg_fixup:fixup)
    (maxprot:vm_prot list)
    (initprot:vm_prot list)
    (sects:(string * int * fixup) array)
    : frag =

  let cmd_fixup = new_fixup "segment command" in
  let cmd =
    SEQ [|
      WORD (TY_u32, IMM (load_command_code LC_SEGMENT));
      WORD (TY_u32, F_SZ cmd_fixup);
      fixed_sz_string 16 seg_name;
      WORD (TY_u32, M_POS seg_fixup);
      WORD (TY_u32, M_SZ seg_fixup);
      WORD (TY_u32, F_POS seg_fixup);
      WORD (TY_u32, F_SZ seg_fixup);
      WORD (TY_u32, IMM (fold_flags vm_prot_code maxprot));
      WORD (TY_u32, IMM (fold_flags vm_prot_code initprot));
      WORD (TY_u32, IMM (Int64.of_int (Array.length sects)));
      WORD (TY_u32, IMM 0L); (* Flags? *)
    |]
  in
    DEF (cmd_fixup,
         SEQ [|
           cmd;
           SEQ (Array.map (macho_section_command seg_name) sects);
         |])
;;

let macho_thread_command
    (entry:fixup)
    : frag =
  let cmd_fixup = new_fixup "thread command" in
  let x86_THREAD_STATE32 = 1L in
  let regs =
    [|
      WORD (TY_u32, IMM 0x0L); (* eax *)
      WORD (TY_u32, IMM 0x0L); (* ebx *)
      WORD (TY_u32, IMM 0x0L); (* ecx *)
      WORD (TY_u32, IMM 0x0L); (* edx *)

      WORD (TY_u32, IMM 0x0L); (* edi *)
      WORD (TY_u32, IMM 0x0L); (* esi *)
      WORD (TY_u32, IMM 0x0L); (* ebp *)
      WORD (TY_u32, IMM 0x0L); (* esp *)

      WORD (TY_u32, IMM 0x0L); (* ss *)
      WORD (TY_u32, IMM 0x0L); (* eflags *)
      WORD (TY_u32, M_POS entry); (* eip *)
      WORD (TY_u32, IMM 0x0L); (* cs *)

      WORD (TY_u32, IMM 0x0L); (* ds *)
      WORD (TY_u32, IMM 0x0L); (* es *)
      WORD (TY_u32, IMM 0x0L); (* fs *)
      WORD (TY_u32, IMM 0x0L); (* gs *)
    |]
  in
  let cmd =
    SEQ [|
      WORD (TY_u32, IMM (load_command_code LC_UNIXTHREAD));
      WORD (TY_u32, F_SZ cmd_fixup);
      WORD (TY_u32, IMM x86_THREAD_STATE32); (* "flavour" *)
      WORD (TY_u32, IMM (Int64.of_int (Array.length regs)));
      SEQ regs
    |]
  in
    DEF (cmd_fixup, cmd)
;;

let macho_dylinker_command : frag =
  let cmd_fixup = new_fixup "dylinker command" in
  let str_fixup = new_fixup "dylinker lc_str fixup" in
  let cmd =
    SEQ
      [|
        WORD (TY_u32, IMM (load_command_code LC_LOAD_DYLINKER));
        WORD (TY_u32, F_SZ cmd_fixup);

        (* see definition of lc_str; these things are weird. *)
        WORD (TY_u32, SUB (F_POS (str_fixup), F_POS (cmd_fixup)));
        DEF (str_fixup, ZSTRING "/usr/lib/dyld");
        ALIGN_FILE (4, MARK);
      |]
  in
    DEF (cmd_fixup, cmd);
;;

let macho_header_32
    (cpu:cpu_type)
    (sub:cpu_subtype)
    (ftype:file_type)
    (flags:file_flag list)
    (loadcmds:frag array) : frag =
  let load_commands_fixup = new_fixup "load commands" in
  let cmds = DEF (load_commands_fixup, SEQ loadcmds) in
    SEQ
    [|
      WORD (TY_u32, IMM mh_magic);
      WORD (TY_u32, IMM (cpu_type_code cpu));
      WORD (TY_u32, IMM (cpu_subtype_code sub));
      WORD (TY_u32, IMM (file_type_code ftype));
      WORD (TY_u32, IMM (Int64.of_int (Array.length loadcmds)));
      WORD (TY_u32, F_SZ load_commands_fixup);
      WORD (TY_u32, IMM (fold_flags file_flag_code flags));
      cmds
    |]
;;

let emit_file
    (sess:Session.sess)
    (code:Asm.frag)
    (data:Asm.frag)
    (dwarf:Dwarf.debug_records)
    (entry_prog_fixup:fixup)
    (c_to_proc_fixup:fixup)
    : unit =

  (* FIXME: alignment? *)

  let text_sect_align_log2 = 2 in
  let data_sect_align_log2 = 2 in
  let jump_table_sect_align_log2 = 6 in
  let dyld_sect_align_log2 = 2 in

  let seg_align = 0x1000 in
  let text_sect_align = 2 lsl text_sect_align_log2 in
  let data_sect_align = 2 lsl data_sect_align_log2 in
  let jump_table_sect_align = 2 lsl jump_table_sect_align_log2 in
  let dyld_sect_align = 2 lsl dyld_sect_align_log2 in

  let align_both align i =
    ALIGN_FILE (align,
                (ALIGN_MEM (align, i)))
  in

  let def_aligned a f i =
    align_both a
      (SEQ [| DEF(f, i);
              (align_both a MARK)|])
  in

  (* Sections in the text segment. *)
  let text_section_fixup = new_fixup "__text section" in
  let text_section = def_aligned text_sect_align text_section_fixup code in

  (* Sections in the data segment. *)
  let data_section_fixup = new_fixup "__data section" in
  let const_section_fixup = new_fixup "__const section" in
  let bss_section_fixup = new_fixup "__bss section" in
  let data_section = def_aligned data_sect_align data_section_fixup data in
  let const_section = def_aligned data_sect_align const_section_fixup (SEQ [| |]) in
  let bss_section = def_aligned data_sect_align bss_section_fixup (SEQ [| |]) in

  (* Sections in the import segment. *)
  let jump_table_section_fixup = new_fixup "__jump_table section" in
  let jump_table_section =
    def_aligned jump_table_sect_align
      jump_table_section_fixup (SEQ [| |])
  in

  (* Segments. *)
  let zero_segment_fixup = new_fixup "__PAGEZERO segment" in
  let zero_segment = align_both seg_align
    (SEQ [| MEMPOS 0L; DEF (zero_segment_fixup, PAD 0x1000) |])
  in

  let text_segment_fixup = new_fixup "__TEXT segment" in
  let text_segment = def_aligned seg_align text_segment_fixup
    (SEQ [|
       text_section
     |])
  in

  let data_segment_fixup = new_fixup "__DATA segment" in
  let data_segment = def_aligned seg_align data_segment_fixup
    (SEQ [|
       data_section;
       const_section;
       bss_section;
     |])
  in

  let import_segment_fixup = new_fixup "__IMPORT segment" in
  let import_segment = def_aligned seg_align import_segment_fixup
    (SEQ [|
       jump_table_section
     |])
  in

  let load_commands =
    [|
      macho_segment_command "__PAGEZERO" zero_segment_fixup
        [] [] [||];

      macho_segment_command "__TEXT" text_segment_fixup
        [VM_PROT_READ; VM_PROT_EXECUTE]
        [VM_PROT_READ; VM_PROT_EXECUTE]
        [|
          ("__text", text_sect_align_log2, text_section_fixup)
        |];

      macho_segment_command "__DATA" data_segment_fixup
        [VM_PROT_READ; VM_PROT_WRITE]
        [VM_PROT_READ; VM_PROT_WRITE]
        [|
          ("__data", data_sect_align_log2, data_section_fixup);
          ("__const", data_sect_align_log2, const_section_fixup);
          ("__bss", data_sect_align_log2, bss_section_fixup)
        |];

      macho_segment_command "__IMPORT" text_segment_fixup
        [VM_PROT_READ; VM_PROT_WRITE]
        [VM_PROT_READ; VM_PROT_WRITE]
        [|
          ("__jump_table", jump_table_sect_align_log2, jump_table_section_fixup)
        |];

      macho_dylinker_command;

      (* FIXME: the thread command should point eip to the glue code
         entrypoint, not the entry-prog fixup. That's a *prog*, it's
         in the data section. This is not right at all! *)
      macho_thread_command entry_prog_fixup
    |]
  in

  let header_and_commands =
    macho_header_32
      CPU_TYPE_X86
      CPU_SUBTYPE_X86_ALL
      MH_EXECUTE
      []
      load_commands
  in

  let segments =
    SEQ [|
      zero_segment;
      text_segment;
      data_segment;
      import_segment
    |]
  in
  let all_frags = SEQ [| header_and_commands; segments |] in

  let buf = Buffer.create 16 in
  let out = open_out_bin sess.Session.sess_out in
    resolve_frag sess all_frags;
    lower_frag ~lsb0: true ~buf ~it: all_frags;
    Buffer.output_buffer out buf;
    flush out;
    close_out out
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
