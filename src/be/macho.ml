open Asm;;
open Common;;


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


let macho_header_32 (cpu:cpu_type) (sub:cpu_subtype) =
  SEQ
    [|
      WORD (TY_u32, IMM mh_magic);
      WORD (TY_u32, IMM (cpu_type_code cpu));
      WORD (TY_u32, IMM (cpu_subtype_code sub));
    |]

let macho_section name item = 
  SEQ [| |]
;;

let macho_segment name sections = 
  SEQ [| |]
;;

let fixed_sz_string (sz:int) (str:string) : item =
  if String.length str > sz
  then STRING (String.sub str 0 sz)
  else SEQ [| STRING str; PAD (sz - (String.length str)) |]
;;

let macho_section
    (seg_name:string)
    (sect:(string * int * fixup))
    : item =
  let (sect_name, sect_align, sect_fixup) = sect in
    SEQ [|
      fixed_sz_string 16 sect_name;
      fixed_sz_string 16 seg_name;
      WORD (TY_u32, M_POS sect_fixup);
      WORD (TY_u32, F_SZ sect_fixup);
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
    : item =

  let cmd_fixup = new_fixup "segment command" in
  let cmd = 
    DEF (cmd_fixup,
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
         |])
  in
    SEQ [|
      cmd;
      SEQ (Array.map (macho_section seg_name) sects);
    |]
;;

let emit_file
    (sess:Session.sess)
    (code:Asm.item)
    (data:Asm.item)
    (dwarf:Dwarf.debug_records)
    (entry_prog_fixup:fixup)
    (c_to_proc_fixup:fixup)
    : unit =

  (* Sections in the text segment. *)
  let text_section_fixup = new_fixup "__text section" in
  let text_section = DEF (text_section_fixup, code) in

  (* Sections in the data segment. *)
  let data_section_fixup = new_fixup "__data section" in
  let const_section_fixup = new_fixup "__const section" in
  let bss_section_fixup = new_fixup "__bss section" in
  let data_section = DEF (data_section_fixup, data) in
  let const_section = DEF (const_section_fixup, SEQ [| |]) in
  let bss_section = DEF (bss_section_fixup, SEQ [| |]) in

  (* Sections in the import segment. *)
  let jump_table_section_fixup = new_fixup "__jump_table section" in
  let jump_table_section = DEF (jump_table_section_fixup, SEQ [| |]) in

  (* Segments. *)
  let text_segment_fixup = new_fixup "__TEXT segment" in
  let text_segment = DEF (text_segment_fixup, SEQ [| text_section |]) in

  let data_segment_fixup = new_fixup "__DATA segment" in
  let data_segment = DEF (data_segment_fixup, 
                          SEQ [| 
                            data_section; 
                            const_section;
                            bss_section;
                          |]) 
  in

  let import_segment_fixup = new_fixup "__IMPORT segment" in
  let import_segment = DEF (import_segment_fixup, SEQ [| jump_table_section |]) in

  let header = macho_header_32 CPU_TYPE_X86 CPU_SUBTYPE_X86_ALL in

  (* FIXME: Alignment? *)
  let sect_align = 4096 in

  let load_commands = 
    SEQ [| 
      macho_segment_command "__TEXT" text_segment_fixup 
        [VM_PROT_READ; VM_PROT_EXECUTE]
        [VM_PROT_READ; VM_PROT_EXECUTE]
        [|
          ("__text", sect_align, text_section_fixup)
        |];

      macho_segment_command "__DATA" data_segment_fixup 
        [VM_PROT_READ; VM_PROT_WRITE]
        [VM_PROT_READ; VM_PROT_WRITE]
        [|
          ("__data", sect_align, data_section_fixup);
          ("__const", sect_align, const_section_fixup);
          ("__bss", sect_align, bss_section_fixup)
        |];

      macho_segment_command "__IMPORT" text_segment_fixup 
        [VM_PROT_READ; VM_PROT_WRITE]
        [VM_PROT_READ; VM_PROT_WRITE]
        [|
          ("__jump_table", sect_align, jump_table_section_fixup)
        |];
    |] 
  in

  let segments = SEQ [| text_segment; data_segment; import_segment |] in
  let all_items = SEQ [| header; load_commands; segments |] in

  let buf = Buffer.create 16 in
  let out = open_out_bin sess.Session.sess_out in
    resolve_item sess all_items;
    lower_item ~lsb0: true ~buf ~it: all_items;
    Buffer.output_buffer out buf;
    flush out;
    close_out out
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
