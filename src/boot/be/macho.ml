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

type sect_type =
    S_REGULAR
  | S_ZEROFILL
  | S_CSTRING_LITERALS
  | S_4BYTE_LITERALS
  | S_8BYTE_LITERALS
  | S_LITERAL_POINTERS
  | S_NON_LAZY_SYMBOL_POINTERS
  | S_LAZY_SYMBOL_POINTERS
  | S_SYMBOL_STUBS
  | S_MOD_INIT_FUNC_POINTERS
  | S_MOD_TERM_FUNC_POINTERS
  | S_COALESCED
  | S_GB_ZEROFILL
  | S_INTERPOSING
  | S_16BYTE_LITERALS
  | S_DTRACE_DOF
  | S_LAZY_DYLIB_SYMBOL_POINTERS
;;

let sect_type_code (s:sect_type) : int64 =
  match s with
    S_REGULAR -> 0x0L
  | S_ZEROFILL -> 0x1L
  | S_CSTRING_LITERALS -> 0x2L
  | S_4BYTE_LITERALS -> 0x3L
  | S_8BYTE_LITERALS -> 0x4L
  | S_LITERAL_POINTERS -> 0x5L
  | S_NON_LAZY_SYMBOL_POINTERS -> 0x6L
  | S_LAZY_SYMBOL_POINTERS -> 0x7L
  | S_SYMBOL_STUBS -> 0x8L
  | S_MOD_INIT_FUNC_POINTERS -> 0x9L
  | S_MOD_TERM_FUNC_POINTERS -> 0xaL
  | S_COALESCED -> 0xbL
  | S_GB_ZEROFILL -> 0xcL
  | S_INTERPOSING -> 0xdL
  | S_16BYTE_LITERALS -> 0xeL
  | S_DTRACE_DOF -> 0xfL
  | S_LAZY_DYLIB_SYMBOL_POINTERS -> 0x10L
;;


let macho_section_command
    (seg_name:string)
    (sect:(string * int * sect_type * fixup))
    : frag =
  let (sect_name, sect_align, sect_type, sect_fixup) = sect in
    SEQ [|
      fixed_sz_string 16 sect_name;
      fixed_sz_string 16 seg_name;
      WORD (TY_u32, M_POS sect_fixup);
      WORD (TY_u32, M_SZ sect_fixup);
      WORD (TY_u32, F_POS sect_fixup);
      WORD (TY_u32, IMM (Int64.of_int sect_align));
      WORD (TY_u32, IMM 0L); (* reloff *)
      WORD (TY_u32, IMM 0L); (* nreloc *)
      WORD (TY_u32, IMM (sect_type_code sect_type)); (* flags *)
      WORD (TY_u32, IMM 0L); (* reserved1 *)
      WORD (TY_u32, IMM 0L); (* reserved2 *)
  |]
;;

let macho_segment_command
    (seg_name:string)
    (seg_fixup:fixup)
    (maxprot:vm_prot list)
    (initprot:vm_prot list)
    (sects:(string * int * sect_type * fixup) array)
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

let macho_dylib_command (dylib:string) : frag =

  let cmd_fixup = new_fixup "dylib command" in
  let str_fixup = new_fixup "dylib lc_str fixup" in
  let cmd =
    SEQ
      [|
        WORD (TY_u32, IMM (load_command_code LC_LOAD_DYLIB));
        WORD (TY_u32, F_SZ cmd_fixup);

        (* see definition of lc_str; these things are weird. *)
        WORD (TY_u32, SUB (F_POS (str_fixup), F_POS (cmd_fixup)));

        WORD (TY_u32, IMM 0L); (* timestamp *)
        WORD (TY_u32, IMM 0L); (* current_version *)
        WORD (TY_u32, IMM 0L); (* compatibility_version *)

        (* Payload-and-alignment of an lc_str goes at end of command. *)
        DEF (str_fixup, ZSTRING dylib);
        ALIGN_FILE (4, MARK);

      |]
  in
    DEF (cmd_fixup, cmd)
;;


let macho_symtab_command
    (symtab_fixup:fixup)
    (nsyms:int64)
    (strtab_fixup:fixup)
    : frag =
  let cmd_fixup = new_fixup "symtab command" in
  let cmd =
    SEQ
      [|
        WORD (TY_u32, IMM (load_command_code LC_SYMTAB));
        WORD (TY_u32, F_SZ cmd_fixup);
        
        WORD (TY_u32, F_POS symtab_fixup); (* symoff *)
        WORD (TY_u32, IMM nsyms);          (* nsyms *)

        WORD (TY_u32, F_POS strtab_fixup); (* stroff *)
        WORD (TY_u32, F_SZ strtab_fixup);  (* strsz *)
      |]
  in
    DEF (cmd_fixup, cmd)
;;

let macho_dysymtab_command
    (defined_syms_index:int64)
    (defined_syms_count:int64)
    (undefined_syms_index:int64)
    (undefined_syms_count:int64)
    (indirect_symtab_fixup:fixup)  : frag =
  let cmd_fixup = new_fixup "dysymtab command" in
  let cmd =
    SEQ
      [|
        WORD (TY_u32, IMM (load_command_code LC_DYSYMTAB));
        WORD (TY_u32, F_SZ cmd_fixup);
        
        WORD (TY_u32, IMM 0L); (* ilocalsym *)
        WORD (TY_u32, IMM 0L); (* nlocalsym *)

        WORD (TY_u32, IMM defined_syms_index); (* iextdefsym *)
        WORD (TY_u32, IMM defined_syms_count); (* nextdefsym *)

        WORD (TY_u32, IMM undefined_syms_index); (* iundefsym *)
        WORD (TY_u32, IMM undefined_syms_count); (* nundefsym *)

        WORD (TY_u32, IMM 0L); (* tocoff *)
        WORD (TY_u32, IMM 0L); (* ntoc *)

        WORD (TY_u32, IMM 0L); (* modtaboff *)
        WORD (TY_u32, IMM 0L); (* nmodtab *)

        WORD (TY_u32, IMM 0L); (* extrefsymoff *)
        WORD (TY_u32, IMM 0L); (* nextrefsyms *)

        WORD (TY_u32, F_POS indirect_symtab_fixup); (* indirectsymoff *)
        WORD (TY_u32, IMM undefined_syms_count);    (* nindirectsyms *)

        WORD (TY_u32, IMM 0L); (* extreloff *)
        WORD (TY_u32, IMM 0L); (* nextrel *)

        WORD (TY_u32, IMM 0L); (* locreloff *)
        WORD (TY_u32, IMM 0L); (* nlocrel *)
      |]
  in
    DEF (cmd_fixup, cmd)
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
    (sem:Semant.ctxt)
    (dwarf:Dwarf.debug_records)
    : unit =

  (* FIXME: alignment? *)

  let mh_execute_header_fixup = new_fixup "__mh_execute header" in

  let nxargc_fixup = new_fixup "_NXArgc" in
  let nxargv_fixup = new_fixup "_NXArgv" in
  let progname_fixup = new_fixup "___progname" in
  let environ_fixup = new_fixup "_environ" in
  let exit_fixup = new_fixup "_exit" in
  let rust_start_fixup = new_fixup "_rust_start" in

  let start_fixup = new_fixup "start function entry" in

  let text_sect_align_log2 = 2 in
  let data_sect_align_log2 = 2 in

  let seg_align = 0x1000 in
  let text_sect_align = 2 lsl text_sect_align_log2 in
  let data_sect_align = 2 lsl data_sect_align_log2 in

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
  let nl_symbol_ptr_section_fixup = new_fixup "__nl_symbol_ptr section" in

  let data_section = def_aligned data_sect_align data_section_fixup data in
  let const_section = def_aligned data_sect_align const_section_fixup (SEQ [| |]) in
  let bss_section = def_aligned data_sect_align bss_section_fixup (SEQ [| |]) in


  (* String, symbol and parallel "nonlazy-pointer" tables. *)
  let symtab_fixup = new_fixup "symtab" in
  let strtab_fixup = new_fixup "strtab" in


  let sect_symbol_nlist_entry (sect_index:int) (fixup_to_use:fixup) : (frag * fixup) =
    let strtab_entry_fixup = new_fixup "strtab entry" in
      (SEQ
         [|
           WORD (TY_u32, SUB ((F_POS strtab_entry_fixup), (F_POS strtab_fixup)));
           BYTE 0xf;              (* n_type == N_SECT | N_EXT *)
           BYTE sect_index;       (* n_sect == NO_SECT *)
           WORD (TY_u16, IMM 0L); (* n_desc == unused *)
           WORD (TY_u32, M_POS (fixup_to_use));
         |], strtab_entry_fixup)
  in

  let indirect_symbol_nlist_entry (dylib_index:int) : (frag * fixup) =
    let strtab_entry_fixup = new_fixup "strtab entry" in
      (SEQ
         [|
           WORD (TY_u32, SUB ((F_POS strtab_entry_fixup), (F_POS strtab_fixup)));
           BYTE 1;                (* n_type == N_UNDEF | N_EXT *)
           BYTE 0;                (* n_sect == NO_SECT *)
           WORD (TY_u16, IMM (Int64.of_int (dylib_index lsl 8)));
           (* n_desc == REFERENCE_FLAG_UNDEFINED_NON_LAZY *)
           WORD (TY_u32, IMM 0L); (* n_value == unused *)
         |], strtab_entry_fixup)
  in

  let absolute_symbol_nlist_entry (fixup_to_use:fixup) : (frag * fixup) =
    let strtab_entry_fixup = new_fixup "strtab entry" in
      (SEQ
         [|
           WORD (TY_u32, SUB ((F_POS strtab_entry_fixup), (F_POS strtab_fixup)));
           BYTE 1;                (* n_type == N_ABS | N_EXT *)
           BYTE 0;                (* n_sect == NO_SECT *)
           WORD (TY_u16, IMM 2L); (* n_desc == REFERENCE_FLAG_DEFINED *)
           WORD (TY_u32, M_POS fixup_to_use);
         |], strtab_entry_fixup)
  in

  let (symbols:(string * (frag * fixup)) array) =
    [|
      ("_rust_start", indirect_symbol_nlist_entry 1);
      ("_exit", indirect_symbol_nlist_entry 2);
      ("_NXArgc", sect_symbol_nlist_entry 2 nxargc_fixup);
      ("_NXArgv", sect_symbol_nlist_entry 2 nxargv_fixup);
      ("_environ", sect_symbol_nlist_entry 2 environ_fixup);
      ("___progname", sect_symbol_nlist_entry 2 progname_fixup);
      ("__mh_execute_header", absolute_symbol_nlist_entry mh_execute_header_fixup);
    |]
  in

  let indirect_symbols =
    [|
      rust_start_fixup;
      exit_fixup
    |]
  in
  let indirect_symtab_fixup = new_fixup "indirect symbol table" in
  let indirect_symtab =
    DEF (indirect_symtab_fixup,
         SEQ (Array.mapi
                (fun i _ -> WORD(TY_u32, IMM (Int64.of_int i)))
                indirect_symbols))
  in

  let nl_symbol_ptr_section =
    def_aligned data_sect_align nl_symbol_ptr_section_fixup
      (SEQ (Array.map
              (fun fix -> DEF(fix, WORD(TY_u32, IMM 0L)))
              indirect_symbols))
  in
  let strtab = DEF (strtab_fixup,
                    SEQ (Array.map
                           (fun (name, (_, fix)) -> DEF(fix, ZSTRING name))
                           symbols))
  in
  let symtab = DEF (symtab_fixup,
                    SEQ (Array.map (fun (_, (frag, _)) -> frag) symbols))
  in


  (* Segments. *)
  let zero_segment_fixup = new_fixup "__PAGEZERO segment" in
  let text_segment_fixup = new_fixup "__TEXT segment" in
  let data_segment_fixup = new_fixup "__DATA segment" in
  let linkedit_segment_fixup = new_fixup "__LINKEDIT segment" in

  let load_commands =
    [|
      macho_segment_command "__PAGEZERO" zero_segment_fixup
        [] [] [||];

      macho_segment_command "__TEXT" text_segment_fixup
        [VM_PROT_READ; VM_PROT_EXECUTE]
        [VM_PROT_READ; VM_PROT_EXECUTE]
        [|
          ("__text", text_sect_align_log2, S_REGULAR, text_section_fixup)
        |];

      macho_segment_command "__DATA" data_segment_fixup
        [VM_PROT_READ; VM_PROT_WRITE]
        [VM_PROT_READ; VM_PROT_WRITE]
        [|
          ("__data", data_sect_align_log2, S_REGULAR, data_section_fixup);
          ("__const", data_sect_align_log2, S_REGULAR, const_section_fixup);
          ("__bss", data_sect_align_log2, S_REGULAR, bss_section_fixup);
          ("__nl_symbol_ptr", data_sect_align_log2,
           S_NON_LAZY_SYMBOL_POINTERS, nl_symbol_ptr_section_fixup)
        |];

      macho_segment_command "__LINKEDIT" linkedit_segment_fixup
        [VM_PROT_READ]
        [VM_PROT_READ]
        [|
        |];

      macho_symtab_command
        symtab_fixup (Int64.of_int (Array.length symbols)) strtab_fixup;

      (* These index-and-count numbers must match the 'symbols' table above.*)
      macho_dysymtab_command 2L 5L 0L 2L indirect_symtab_fixup;

      macho_dylinker_command;

      macho_dylib_command "librustrt.dylib";

      macho_dylib_command "/usr/lib/libSystem.B.dylib";

      macho_thread_command start_fixup
    |]
  in

  let header_and_commands =
    macho_header_32
      CPU_TYPE_X86
      CPU_SUBTYPE_X86_ALL
      MH_EXECUTE
      [ MH_BINDATLOAD; MH_DYLDLINK; MH_TWOLEVEL ]
      load_commands
  in

  let objfile_start e =
    Il.emit_full e (Some start_fixup) Il.Dead;

    (* zero marks the bottom of the frame chain. *)
    Il.emit e (Il.Push (X86.imm (Asm.IMM 0L)));
    Il.emit e (Il.umov (X86.rc X86.ebp) (X86.ro X86.esp));

    (* 16-byte align stack for SSE. *)
    Il.emit e (Il.binary Il.AND (X86.rc X86.esp) (X86.ro X86.esp)
                 (X86.imm (Asm.IMM 0xfffffffffffffff0L)));

    (* Store argc. *)
    Il.emit e (Il.umov (X86.rc X86.ebx) (X86.c (X86.word_n (Il.Hreg X86.ebp) 1)));
    Il.emit e (Il.umov (X86.word_at_abs (Asm.M_POS nxargc_fixup)) (X86.ro X86.ebx));

    (* Store argv. *)
    Il.emit e (Il.lea (X86.rc X86.ecx) (Il.Based (Il.Hreg X86.ebp, Some (X86.word_off_n 2))));
    Il.emit e (Il.umov (X86.word_at_abs (Asm.M_POS nxargv_fixup)) (X86.ro X86.ebx));

    (* Calculte and store envp. *)
    Il.emit e (Il.binary Il.ADD (X86.rc X86.ebx) (X86.ro X86.ebx) (X86.imm (Asm.IMM 1L)));
    Il.emit e (Il.binary Il.UMUL (X86.rc X86.ebx) (X86.ro X86.ebx) (X86.imm (Asm.IMM X86.word_sz)));
    Il.emit e (Il.binary Il.ADD (X86.rc X86.ebx) (X86.ro X86.ebx) (X86.ro X86.ecx));
    Il.emit e (Il.umov (X86.word_at_abs (Asm.M_POS environ_fixup)) (X86.ro X86.ebx));

    (* Push 16 bytes to preserve SSE alignment. *)
    Il.emit e (Il.Push (X86.imm (Asm.IMM 0L)));
    Il.emit e (Il.Push (X86.imm (Asm.M_POS sem.Semant.ctxt_c_to_proc_fixup)));
    Il.emit e (Il.Push (X86.imm (Asm.M_POS sem.Semant.ctxt_main_exit_proc_glue_fixup)));
    Il.emit e (Il.Push (X86.imm (Asm.M_POS sem.Semant.ctxt_main_fn_fixup)));
    Il.emit e (Il.call (X86.rc X86.eax) (Il.CodeAddr (Il.Abs (Asm.M_POS rust_start_fixup))));
    Il.emit e (Il.Pop (X86.rc X86.ecx));
    Il.emit e (Il.Pop (X86.rc X86.ecx));
    Il.emit e (Il.Pop (X86.rc X86.ecx));
    Il.emit e (Il.Pop (X86.rc X86.ecx));
    Il.emit e (Il.Push (X86.ro X86.eax));
    Il.emit e (Il.call (X86.rc X86.eax) (Il.CodeAddr (Il.Abs (Asm.M_POS exit_fixup))));
    Il.emit e (Il.Pop (X86.rc X86.ecx));
    Il.emit e Il.Ret;
  in

  let text_segment =
    let e = X86.new_emitter () in
      objfile_start e;
      def_aligned seg_align text_segment_fixup
        (SEQ [|
           DEF (mh_execute_header_fixup, header_and_commands);
           X86.frags_of_emitted_quads sess e;
           text_section;
           align_both seg_align MARK;
         |]);
  in

  let zero_segment = align_both seg_align
    (SEQ [| MEMPOS 0L; DEF (zero_segment_fixup, SEQ [| MEMPOS 0x1000L; MARK |] ) |])
  in

  let data_segment = def_aligned seg_align data_segment_fixup
    (SEQ [|
       DEF(nxargc_fixup, WORD (TY_u32, IMM 0L));
       DEF(nxargv_fixup, WORD (TY_u32, IMM 0L));
       DEF(environ_fixup, WORD (TY_u32, IMM 0L));
       DEF(progname_fixup, WORD (TY_u32, IMM 0L));
       data_section;
       const_section;
       bss_section;
       nl_symbol_ptr_section
     |])
  in

  let linkedit_segment = def_aligned seg_align linkedit_segment_fixup
    (SEQ [|
       symtab;
       strtab;
       indirect_symtab;
     |])
  in

  let segments =
    SEQ [|
      zero_segment;
      text_segment;
      data_segment;
      linkedit_segment;
    |]
  in

  let buf = Buffer.create 16 in
  let out = open_out_bin sess.Session.sess_out in
    resolve_frag sess segments;
    lower_frag ~lsb0: true ~buf ~it: segments;
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
