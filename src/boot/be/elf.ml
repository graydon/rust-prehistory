(*
 * Module for writing System V ELF files.
 *
 * FIXME: Presently heavily infected with x86 and elf32 specificities,
 * though they are reasonably well marked. Needs to be refactored to
 * depend on abi fields if it's to be usable for other elf
 * configurations.
 *)

open Asm;;
open Common;;


(* Fixed sizes of structs involved in elf32 spec. *)
let elf32_ehsize = 52L;;
let elf32_phentsize = 32L;;
let elf32_shentsize = 40L;;
let elf32_symsize = 16L;;
let elf32_rela_entsz = 0xcL;;

type ei_class =
    ELFCLASSNONE
  | ELFCLASS32
  | ELFCLASS64
;;


type ei_data =
    ELFDATANONE
  | ELFDATA2LSB
  | ELFDATA2MSB
;;


let elf_identification ei_class ei_data =
  SEQ
    [|
      STRING "\x7fELF";
      BYTES
        [|
          (match ei_class with  (* EI_CLASS *)
               ELFCLASSNONE -> 0
             | ELFCLASS32 -> 1
             | ELFCLASS64 -> 2);
          (match ei_data with   (* EI_DATA *)
               ELFDATANONE -> 0
             | ELFDATA2LSB -> 1
             | ELFDATA2MSB -> 2);
          1;                    (* EI_VERSION = EV_CURRENT *)
          0;                    (* EI_PAD #7 *)
          0;                    (* EI_PAD #8 *)
          0;                    (* EI_PAD #9 *)
          0;                    (* EI_PAD #A *)
          0;                    (* EI_PAD #B *)
          0;                    (* EI_PAD #C *)
          0;                    (* EI_PAD #D *)
          0;                    (* EI_PAD #E *)
          0;                    (* EI_PAD #F *)
        |]
    |]
;;


type e_type =
    ET_NONE
  | ET_REL
  | ET_EXEC
  | ET_DYN
  | ET_CORE
;;


type e_machine =
    (* Maybe support more later. *)
    EM_NONE
  | EM_386
  | EM_X86_64
;;


type e_version =
    EV_NONE
  | EV_CURRENT
;;


let elf32_header
    ~(ei_data:ei_data)
    ~(e_type:e_type)
    ~(e_machine:e_machine)
    ~(e_version:e_version)
    ~(e_entry_fixup:fixup)
    ~(e_phoff_fixup:fixup)
    ~(e_shoff_fixup:fixup)
    ~(e_phnum:int64)
    ~(e_shnum:int64)
    ~(e_shstrndx:int64)
    : frag =
  let elf_header_fixup = new_fixup "elf header" in
    DEF
      (elf_header_fixup,
       SEQ [| elf_identification ELFCLASS32 ei_data;
              WORD (TY_u16, (IMM (match e_type with
                                      ET_NONE -> 0L
                                    | ET_REL -> 1L
                                    | ET_EXEC -> 2L
                                    | ET_DYN -> 3L
                                    | ET_CORE -> 4L)));
              WORD (TY_u16, (IMM (match e_machine with
                                      EM_NONE -> 0L
                                    | EM_386 -> 3L
                                    | EM_X86_64 -> 62L)));
              WORD (TY_u32, (IMM (match e_version with
                                      EV_NONE -> 0L
                                    | EV_CURRENT -> 1L)));
              WORD (TY_u32, (M_POS e_entry_fixup));
              WORD (TY_u32, (F_POS e_phoff_fixup));
              WORD (TY_u32, (F_POS e_shoff_fixup));
              WORD (TY_u32, (IMM 0L)); (* e_flags *)
              WORD (TY_u16, (IMM elf32_ehsize));
              WORD (TY_u16, (IMM elf32_phentsize));
              WORD (TY_u16, (IMM e_phnum));
              WORD (TY_u16, (IMM elf32_shentsize));
              WORD (TY_u16, (IMM e_shnum));
              WORD (TY_u16, (IMM e_shstrndx));
           |])
;;


type sh_type =
    SHT_NULL
  | SHT_PROGBITS
  | SHT_SYMTAB
  | SHT_STRTAB
  | SHT_RELA
  | SHT_HASH
  | SHT_DYNAMIC
  | SHT_NOTE
  | SHT_NOBITS
  | SHT_REL
  | SHT_SHLIB
  | SHT_DYNSYM
;;


type sh_flags =
    SHF_WRITE
  | SHF_ALLOC
  | SHF_EXECINSTR
;;


let section_header
    ~(shstring_table_fixup:fixup)
    ~(shname_string_fixup:fixup)
    ~(sh_type:sh_type)
    ~(sh_flags:sh_flags list)
    ~(section_fixup:fixup option)
    ~(sh_addralign:int64)
    ~(sh_entsize:int64)
    ~(sh_link:int64 option)
    : frag =
  SEQ
    [|
      WORD (TY_s32, (SUB
                       ((F_POS shname_string_fixup),
                        (F_POS shstring_table_fixup))));
      WORD (TY_u32, (IMM (match sh_type with
                              SHT_NULL -> 0L
                            | SHT_PROGBITS -> 1L
                            | SHT_SYMTAB -> 2L
                            | SHT_STRTAB -> 3L
                            | SHT_RELA -> 4L
                            | SHT_HASH -> 5L
                            | SHT_DYNAMIC -> 6L
                            | SHT_NOTE -> 7L
                            | SHT_NOBITS -> 8L
                            | SHT_REL -> 9L
                            | SHT_SHLIB -> 10L
                            | SHT_DYNSYM -> 11L)));
      WORD (TY_u32, (IMM (fold_flags
                            (fun f -> match f with
                                 SHF_WRITE -> 0x1L
                               | SHF_ALLOC -> 0x2L
                               | SHF_EXECINSTR -> 0x4L) sh_flags)));
      WORD (TY_u32, (match section_fixup with
                         None -> (IMM 0L)
                       | Some s -> (M_POS s)));
      WORD (TY_u32, (match section_fixup with
                         None -> (IMM 0L)
                       | Some s -> (F_POS s)));
      WORD (TY_u32, (match section_fixup with
                         None -> (IMM 0L)
                       | Some s -> (F_SZ s)));
      WORD (TY_u32, (IMM (match sh_link with
                              None -> 0L
                            | Some i -> i)));
      WORD (TY_u32, (IMM 0L)); (* sh_info *)
      WORD (TY_u32, (IMM sh_addralign));
      WORD (TY_u32, (IMM sh_entsize));
    |]
;;


type p_type =
    PT_NULL
  | PT_LOAD
  | PT_DYNAMIC
  | PT_INTERP
  | PT_NOTE
  | PT_SHLIB
  | PT_PHDR
;;


type p_flag =
    PF_X
  | PF_W
  | PF_R
;;


let program_header
    ~(p_type:p_type)
    ~(segment_fixup:fixup)
    ~(p_flags:p_flag list)
    ~(p_align:int64)
    : frag =
  SEQ
    [|
      WORD (TY_u32, (IMM (match p_type with
                              PT_NULL -> 0L
                            | PT_LOAD -> 1L
                            | PT_DYNAMIC -> 2L
                            | PT_INTERP -> 3L
                            | PT_NOTE -> 4L
                            | PT_SHLIB -> 5L
                            | PT_PHDR -> 6L)));
      WORD (TY_u32, (F_POS segment_fixup));
      WORD (TY_u32, (M_POS segment_fixup));
      WORD (TY_u32, (M_POS segment_fixup)); (* IMM 0L); p_paddr, 0 on most archs *)
      WORD (TY_u32, (F_SZ segment_fixup));
      WORD (TY_u32, (M_SZ segment_fixup));
      WORD (TY_u32, (IMM (fold_flags
                            (fun f ->
                               match f with
                                   PF_X -> 0x1L
                                 | PF_W -> 0x2L
                                 | PF_R -> 0x4L)
                            p_flags)));
      WORD (TY_u32, (IMM p_align));
    |]
;;


type st_bind =
    STB_LOCAL
  | STB_GLOBAL
  | STB_WEAK
;;


type st_type =
    STT_NOTYPE
  | STT_OBJECT
  | STT_FUNC
  | STT_SECTION
  | STT_FILE
;;


(* Special symbol-section indices *)
let shn_UNDEF   = 0L;;
let shn_ABS     = 0xfff1L;;
let shn_ABS     = 0xfff2L;;


let symbol
    ~(string_table_fixup:fixup)
    ~(name_string_fixup:fixup)
    ~(sym_target_fixup:fixup option)
    ~(st_bind:st_bind)
    ~(st_type:st_type)
    ~(st_shndx:int64)
    : frag =
  let st_bind_num =
    match st_bind with
        STB_LOCAL -> 0L
      | STB_GLOBAL -> 1L
      | STB_WEAK -> 2L
  in
  let st_type_num =
    match st_type with
        STT_NOTYPE -> 0L
      | STT_OBJECT -> 1L
      | STT_FUNC -> 2L
      | STT_SECTION -> 3L
      | STT_FILE -> 4L
  in
    SEQ
      [|
        WORD (TY_u32, (SUB
                         ((F_POS name_string_fixup),
                          (F_POS string_table_fixup))));
        WORD (TY_u32, (match sym_target_fixup with
                           None -> (IMM 0L)
                         | Some f -> (M_POS f)));
        WORD (TY_u32, (match sym_target_fixup with
                           None -> (IMM 0L)
                         | Some f -> (M_SZ f)));
        WORD (TY_u8,           (* st_info *)
              (OR
                 ((SLL ((IMM st_bind_num), 4)),
                  (AND ((IMM st_type_num), (IMM 0xfL))))));
        WORD (TY_u8, (IMM 0L)); (* st_other *)
        WORD (TY_u16, (IMM st_shndx));
      |]
;;

type d_tag =
    DT_NULL
  | DT_NEEDED
  | DT_PLTRELSZ
  | DT_PLTGOT
  | DT_HASH
  | DT_STRTAB
  | DT_SYMTAB
  | DT_RELA
  | DT_RELASZ
  | DT_RELAENT
  | DT_STRSZ
  | DT_SYMENT
  | DT_INIT
  | DT_FINI
  | DT_SONAME
  | DT_RPATH
  | DT_SYMBOLIC
  | DT_REL
  | DT_RELSZ
  | DT_RELENT
  | DT_PLTREL
  | DT_DEBUG
  | DT_TEXTREL
  | DT_JMPREL
  | DT_BIND_NOW
  | DT_INIT_ARRAY
  | DT_FINI_ARRAY
  | DT_INIT_ARRAYSZ
  | DT_FINI_ARRAYSZ
  | DT_RUNPATH
  | DT_FLAGS
  | DT_ENCODING
  | DT_PREINIT_ARRAY
  | DT_PREINIT_ARRAYSZ
;;

type elf32_dyn = (d_tag * expr64);;

let elf32_num_of_dyn_tag tag =
  match tag with
      DT_NULL -> 0L
    | DT_NEEDED -> 1L
    | DT_PLTRELSZ -> 2L
    | DT_PLTGOT -> 3L
    | DT_HASH -> 4L
    | DT_STRTAB -> 5L
    | DT_SYMTAB -> 6L
    | DT_RELA -> 7L
    | DT_RELASZ -> 8L
    | DT_RELAENT -> 9L
    | DT_STRSZ -> 10L
    | DT_SYMENT -> 11L
    | DT_INIT -> 12L
    | DT_FINI -> 13L
    | DT_SONAME -> 14L
    | DT_RPATH -> 15L
    | DT_SYMBOLIC -> 16L
    | DT_REL -> 17L
    | DT_RELSZ -> 18L
    | DT_RELENT -> 19L
    | DT_PLTREL -> 20L
    | DT_DEBUG -> 21L
    | DT_TEXTREL -> 22L
    | DT_JMPREL -> 23L
    | DT_BIND_NOW -> 24L
    | DT_INIT_ARRAY -> 25L
    | DT_FINI_ARRAY -> 26L
    | DT_INIT_ARRAYSZ -> 27L
    | DT_FINI_ARRAYSZ -> 28L
    | DT_RUNPATH -> 29L
    | DT_FLAGS -> 30L
    | DT_ENCODING -> 31L
    | DT_PREINIT_ARRAY -> 32L
    | DT_PREINIT_ARRAYSZ -> 33L
;;

let elf32_dyn_frag d =
  let (tag, expr) = d in
  let tagval = elf32_num_of_dyn_tag tag in
    SEQ [| WORD (TY_u32, (IMM tagval)); WORD (TY_u32, expr) |]
;;

type elf32_386_reloc_type =
    R_386_NONE
  | R_386_32
  | R_386_PC32
  | R_386_GOT32
  | R_386_PLT32
  | R_386_COPY
  | R_386_GLOB_DAT
  | R_386_JMP_SLOT
  | R_386_RELATIVE
  | R_386_GOTOFF
  | R_386_GOTPC
;;


type elf32_386_rela =
    { elf32_386_rela_type: elf32_386_reloc_type;
      elf32_386_rela_offset: expr64;
      elf32_386_rela_sym: expr64;
      elf32_386_rela_addend: expr64 }
;;

let elf32_386_rela_frag r =
  let type_val =
    match r.elf32_386_rela_type with
        R_386_NONE -> 0L
      | R_386_32 -> 1L
      | R_386_PC32 -> 2L
      | R_386_GOT32 -> 3L
      | R_386_PLT32 -> 4L
      | R_386_COPY -> 5L
      | R_386_GLOB_DAT -> 6L
      | R_386_JMP_SLOT -> 7L
      | R_386_RELATIVE -> 8L
      | R_386_GOTOFF -> 9L
      | R_386_GOTPC -> 10L
  in
  let info_expr =
    WORD (TY_u32,
          (OR
             (SLL ((r.elf32_386_rela_sym), 8),
              AND ((IMM 0xffL), (IMM type_val)))))
  in
    SEQ [| WORD (TY_u32, r.elf32_386_rela_offset);
           info_expr;
           WORD (TY_u32, r.elf32_386_rela_addend) |]
;;

let elf32_linux_x86_file
    ~(sess:Session.sess)
    ~(entry_name:string)
    ~(text_frags:(string option, frag) Hashtbl.t)
    ~(data_frags:(string option, frag) Hashtbl.t)
    ~(rodata_frags:(string option, frag) Hashtbl.t)
    ~(import_fixups:(string, fixup) Hashtbl.t)
    ~(dwarf:Dwarf.debug_records)
    ~(sem:Semant.ctxt)
    ~(needed_libs:string array)
    : frag =

  (* Procedure Linkage Tables (PLTs), Global Offset Tables
   * (GOTs), and the relocations that set them up:
   *
   * The PLT goes in a section called .plt and GOT in a section called
   * .got. The portion of the GOT that holds PLT jump slots goes in a
   * section called .got.plt. Dynamic relocations for these jump slots go in
   * section .rela.plt.
   *
   * The easiest way to understand the PLT/GOT system is to draw it:
   *
   *     PLT                          GOT
   *   +----------------------+     +----------------------+
   *  0| push &<GOT[1]>            0| <reserved>
   *   | jmp *GOT[2]               1| <libcookie>
   *   |                           2| & <ld.so:resolve-a-sym>
   *  1| jmp *GOT[3]               3| & <'push 0' in PLT[1]>
   *   | push 0                    4| & <'push 1' in PLT[2]>
   *   | jmp *PLT[0]               5| & <'push 2' in PLT[3]>
   *   |
   *  2| jmp *GOT[4]
   *   | push 1
   *   | jmp *PLT[0]
   *   |
   *  2| jmp *GOT[5]
   *   | push 2
   *   | jmp *PLT[0]
   *
   *
   * In normal user code, we call PLT entries with a call to a
   * PC-relative address, the PLT entry, which itself does an indirect
   * jump through a slot in the GOT that it also addresses
   * PC-relative. This makes the whole scheme PIC.
   *
   * The linker fills in the GOT on startup. For the first 3, it uses
   * its own thinking. For the remainder it needs to be instructed to
   * fill them in with "jump slot relocs", type R_386_JUMP_SLOT, each
   * of which says in effect which PLT entry it's to point back to and
   * which symbol it's to be resolved to later. These relocs go in the
   * section .rela.plt.
   *)

    let plt0_fixup = new_fixup "PLT[0]" in
    let got1_fixup = new_fixup "GOT[1]" in
    let got2_fixup = new_fixup "GOT[2]" in
    let got_prefix = SEQ [| WORD (TY_u32, (IMM 0L));
                            DEF (got1_fixup, WORD (TY_u32, (IMM 0L)));
                            DEF (got2_fixup, WORD (TY_u32, (IMM 0L))); |]
    in
    let plt0_frag =
      let e = Il.new_emitter X86.prealloc_quad true in
        Il.emit e (Il.Push (X86.imm (M_POS got1_fixup)));
        Il.emit e (Il.jmp Il.JMP (Il.CodeMem (Il.AbsIn (M_POS got2_fixup, None))));
        Il.emit e Il.Nop;
        Il.emit e Il.Nop;
        Il.emit e Il.Nop;
        Il.emit e Il.Nop;
        DEF (plt0_fixup, (X86.frags_of_emitted_quads sess e))
    in


  (*
   * The existence of the GOT/PLT mish-mash causes, therefore, the
   * following new sections:
   *
   *   .plt       - the PLT itself, in the r/x text segment
   *   .got.plt   - the PLT-used portion of the GOT, in the r/w segment
   *   .rela.plt  - the dynamic relocs for the GOT-PLT, in the r/x segment
   *
   * In addition, because we're starting up a dynamically linked executable,
   * we have to have several more sections!
   *
   *   .interp    - the read-only section that names ld.so
   *   .dynsym    - symbols named by the PLT/GOT entries, r/x segment
   *   .dynstr    - string-names used in those symbols, r/x segment
   *   .dynamic   - the machine-readable description of the dynamic
   *                linkage requirements of this elf file, in the
   *                r/w _DYNAMIC segment
   *
   * The Dynamic section contains a sequence of 2-word records of type
   * d_tag.
   *
   *)

  (* There are 11 official section headers in the file we're making:  *)
  (*                                                                  *)
  (* section 0: <null section>                                        *)
  (*                                                                  *)
  (* section 1:  .interp            (segment 1: R+X, INTERP)          *)
  (*                                                                  *)
  (* section 2:  .text              (segment 2: R+X, LOAD)            *)
  (* section 3:  .rodata                   ...                        *)
  (* section 4:  .dynsym                   ...                        *)
  (* section 5:  .dynstr                   ...                        *)
  (* section 6:  .rela.plt                 ...                        *)
  (*                                                                  *)
  (* section 7:  .data              (segment 3: R+W, LOAD)            *)
  (* section 8:  .bss                      ...                        *)
  (*                                                                  *)
  (* section 9:  .dynamic           (segment 4: R+W, DYNAMIC)         *)
  (*                                                                  *)
  (* section 10: .shstrtab          (not in a segment)                *)

  let null_section_name_fixup = new_fixup "string name of <null> section" in
  let interp_section_name_fixup = new_fixup "string name of '.interp' section" in
  let text_section_name_fixup = new_fixup "string name of '.text' section" in
  let rodata_section_name_fixup = new_fixup "string name of '.rodata' section" in
  let dynsym_section_name_fixup = new_fixup "string name of '.dynsym' section" in
  let dynstr_section_name_fixup = new_fixup "string name of '.dynstr' section" in
  let plt_section_name_fixup = new_fixup "string name of '.plt' section" in
  let got_plt_section_name_fixup = new_fixup "string name of '.got.plt' section" in
  let rela_plt_section_name_fixup = new_fixup "string name of '.rela.plt' section" in
  let data_section_name_fixup = new_fixup "string name of '.data' section" in
  let bss_section_name_fixup = new_fixup "string name of '.bss' section" in
  let dynamic_section_name_fixup = new_fixup "string name of '.dynamic' section" in
  let shstrtab_section_name_fixup = new_fixup "string name of '.shstrtab' section" in
  let debug_aranges_section_name_fixup = new_fixup "string name of '.debug_aranges' section" in
  let debug_pubnames_section_name_fixup = new_fixup "string name of '.debug_pubnames' section" in
  let debug_info_section_name_fixup = new_fixup "string name of '.debug_info' section" in
  let debug_abbrev_section_name_fixup = new_fixup "string name of '.debug_abbrev' section" in
  let debug_line_section_name_fixup = new_fixup "string name of '.debug_line' section" in
  let debug_frame_section_name_fixup = new_fixup "string name of '.debug_frame' section" in

  (* let interpndx      = 1L in *)  (* Section index of .interp *)
  let textndx        = 2L in  (* Section index of .text *)
  let rodatandx      = 3L in  (* Section index of .rodata *)
  let dynsymndx      = 4L in  (* Section index of .dynsym *)
  let dynstrndx      = 5L in  (* Section index of .dynstr *)
  (* let pltndx         = 6L in *)  (* Section index of .plt *)
  (* let gotpltndx      = 7L in *)  (* Section index of .got.plt *)
  (* let relapltndx     = 8L in *)  (* Section index of .rela.plt *)
  let datandx        = 9L in  (* Section index of .data *)
  (* let bssndx         = 10L in *) (* Section index of .bss *)
  (* let dynamicndx     = 11L in *) (* Section index of .dynamic *)
  let shstrtabndx    = 12L in (* Section index of .shstrtab *)

  let section_header_table_fixup = new_fixup ".section header table" in
  let interp_section_fixup = new_fixup ".interp section" in
  let text_section_fixup = new_fixup ".text section" in
  let rodata_section_fixup = new_fixup ".rodata section" in
  let dynsym_section_fixup = new_fixup ".dynsym section" in
  let dynstr_section_fixup = new_fixup ".dynstr section" in
  let plt_section_fixup = new_fixup ".plt section" in
  let got_plt_section_fixup = new_fixup ".got.plt section" in
  let rela_plt_section_fixup = new_fixup ".rela.plt section" in
  let data_section_fixup = new_fixup ".data section" in
  let bss_section_fixup = new_fixup ".bss section" in
  let dynamic_section_fixup = new_fixup ".dynamic section" in
  let shstrtab_section_fixup = new_fixup ".shstrtab section" in

  let shstrtab_section =
    SEQ
      [|
        DEF (null_section_name_fixup, ZSTRING "");
        DEF (interp_section_name_fixup, ZSTRING ".interp");
        DEF (text_section_name_fixup, ZSTRING ".text");
        DEF (rodata_section_name_fixup, ZSTRING ".rodata");
        DEF (dynsym_section_name_fixup, ZSTRING ".dynsym");
        DEF (dynstr_section_name_fixup, ZSTRING ".dynstr");
        DEF (plt_section_name_fixup, ZSTRING ".plt");
        DEF (got_plt_section_name_fixup, ZSTRING ".got.plt");
        DEF (rela_plt_section_name_fixup, ZSTRING ".rela.plt");
        DEF (data_section_name_fixup, ZSTRING ".data");
        DEF (bss_section_name_fixup, ZSTRING ".bss");
        DEF (dynamic_section_name_fixup, ZSTRING ".dynamic");
        DEF (shstrtab_section_name_fixup, ZSTRING ".shstrtab");
        DEF (debug_aranges_section_name_fixup, ZSTRING ".debug_aranges");
        DEF (debug_pubnames_section_name_fixup, ZSTRING ".debug_pubnames");
        DEF (debug_info_section_name_fixup, ZSTRING ".debug_info");
        DEF (debug_abbrev_section_name_fixup, ZSTRING ".debug_abbrev");
        DEF (debug_line_section_name_fixup, ZSTRING ".debug_line");
        DEF (debug_frame_section_name_fixup, ZSTRING ".debug_frame");
      |]
  in

  let section_headers =
    [|
        (* <null> *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: null_section_name_fixup
           ~sh_type: SHT_NULL
           ~sh_flags: []
           ~section_fixup: None
           ~sh_addralign: 0L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .interp *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: interp_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: [ SHF_ALLOC ]
           ~section_fixup: (Some interp_section_fixup)
           ~sh_addralign: 1L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .text *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: text_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: [ SHF_ALLOC; SHF_EXECINSTR ]
           ~section_fixup: (Some text_section_fixup)
           ~sh_addralign: 32L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .rodata *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: rodata_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: [ SHF_ALLOC ]
           ~section_fixup: (Some rodata_section_fixup)
           ~sh_addralign: 32L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .dynsym *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: dynsym_section_name_fixup
           ~sh_type: SHT_DYNSYM
           ~sh_flags: [ SHF_ALLOC ]
           ~section_fixup: (Some dynsym_section_fixup)
           ~sh_addralign: 8L
           ~sh_entsize: elf32_symsize
           ~sh_link: (Some dynstrndx) );

        (* .dynstr *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: dynstr_section_name_fixup
           ~sh_type: SHT_STRTAB
           ~sh_flags: [ SHF_ALLOC ]
           ~section_fixup: (Some dynstr_section_fixup)
           ~sh_addralign: 1L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .plt *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: plt_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: [ SHF_ALLOC; SHF_EXECINSTR ]
           ~section_fixup: (Some plt_section_fixup)
           ~sh_addralign: 4L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .got.plt *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: got_plt_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: [ SHF_ALLOC; SHF_WRITE ]
           ~section_fixup: (Some got_plt_section_fixup)
           ~sh_addralign: 4L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .rela.plt *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: rela_plt_section_name_fixup
           ~sh_type: SHT_RELA
           ~sh_flags: [ SHF_ALLOC ]
           ~section_fixup: (Some rela_plt_section_fixup)
           ~sh_addralign: 4L
           ~sh_entsize: elf32_rela_entsz
           ~sh_link: (Some dynsymndx));

        (* .data *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: data_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: [ SHF_ALLOC; SHF_WRITE ]
           ~section_fixup: (Some data_section_fixup)
           ~sh_addralign: 32L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .bss *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: bss_section_name_fixup
           ~sh_type: SHT_NOBITS
           ~sh_flags: [ SHF_ALLOC; SHF_WRITE ]
           ~section_fixup: (Some bss_section_fixup)
           ~sh_addralign: 32L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .dynamic *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: dynamic_section_name_fixup
           ~sh_type: SHT_DYNAMIC
           ~sh_flags: [ SHF_ALLOC; SHF_WRITE ]
           ~section_fixup: (Some dynamic_section_fixup)
           ~sh_addralign: 8L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .shstrtab *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: shstrtab_section_name_fixup
           ~sh_type: SHT_STRTAB
           ~sh_flags: []
           ~section_fixup: (Some shstrtab_section_fixup)
           ~sh_addralign: 1L
           ~sh_entsize: 0L
           ~sh_link: None);

(* 
   FIXME: uncomment the dwarf section headers as you make use of them;
   recent gdb versions have got fussier about parsing dwarf and don't
   like seeing junk there. 
*)

        (* .debug_aranges *)
(*

        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: debug_aranges_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: []
           ~section_fixup: (Some dwarf.Dwarf.debug_aranges_fixup)
           ~sh_addralign: 8L
           ~sh_entsize: 0L
           ~sh_link: None);
*)
        (* .debug_pubnames *)
(*
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: debug_pubnames_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: []
           ~section_fixup: (Some dwarf.Dwarf.debug_pubnames_fixup)
           ~sh_addralign: 1L
           ~sh_entsize: 0L
           ~sh_link: None);
*)

        (* .debug_info *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: debug_info_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: []
           ~section_fixup: (Some dwarf.Dwarf.debug_info_fixup)
           ~sh_addralign: 1L
           ~sh_entsize: 0L
           ~sh_link: None);

        (* .debug_abbrev *)
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: debug_abbrev_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: []
           ~section_fixup: (Some dwarf.Dwarf.debug_abbrev_fixup)
           ~sh_addralign: 1L
           ~sh_entsize: 0L
           ~sh_link: None);
        (* .debug_line *)
(*
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: debug_line_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: []
           ~section_fixup: (Some dwarf.Dwarf.debug_line_fixup)
           ~sh_addralign: 1L
           ~sh_entsize: 0L
           ~sh_link: None);
*)

        (* .debug_frame *)
(*
        (section_header
           ~shstring_table_fixup: shstrtab_section_fixup
           ~shname_string_fixup: debug_frame_section_name_fixup
           ~sh_type: SHT_PROGBITS
           ~sh_flags: []
           ~section_fixup: (Some dwarf.Dwarf.debug_frame_fixup)
           ~sh_addralign: 4L
           ~sh_entsize: 0L
           ~sh_link: None);
*)

      |]
  in
  let section_header_table = SEQ section_headers in


  (* There are 3 official program headers in the file we're making:   *)
  (* segment 0: RX / PHDR                                             *)
  (* segment 1: R  / INTERP                                           *)
  (* segment 2: RX / LOAD                                             *)
  (* segment 3: RW / LOAD                                             *)
  (* segment 4: RW / DYNAMIC                                          *)

  let program_header_table_fixup = new_fixup "program header table" in
  let segment_0_fixup = new_fixup "segment 0" in
  let segment_1_fixup = new_fixup "segment 1" in
  let segment_2_fixup = new_fixup "segment 2" in
  let segment_3_fixup = new_fixup "segment 3" in
  let segment_4_fixup = new_fixup "segment 4" in

  let segment_0_align = 4 in
  let segment_1_align = 1 in
  let segment_2_align = 0x1000 in
  let segment_3_align = 0x1000 in
  let segment_4_align = 0x1000 in

  let program_headers = [|
        (program_header
           ~p_type: PT_PHDR
           ~segment_fixup: segment_0_fixup
           ~p_flags: [ PF_R; PF_X ]
           ~p_align: (Int64.of_int segment_0_align));
        (program_header
           ~p_type: PT_INTERP
           ~segment_fixup: segment_1_fixup
           ~p_flags: [ PF_R ]
           ~p_align: (Int64.of_int segment_1_align));
        (program_header
           ~p_type: PT_LOAD
           ~segment_fixup: segment_2_fixup
           ~p_flags: [ PF_R; PF_X ]
           ~p_align: (Int64.of_int segment_2_align));
        (program_header
           ~p_type: PT_LOAD
           ~segment_fixup: segment_3_fixup
           ~p_flags: [ PF_R; PF_W ]
           ~p_align: (Int64.of_int segment_3_align));
        (program_header
           ~p_type: PT_DYNAMIC
           ~segment_fixup: segment_4_fixup
           ~p_flags: [ PF_R; PF_W ]
           ~p_align: (Int64.of_int segment_4_align));
      |]
  in
  let program_header_table = SEQ program_headers in

  let e_entry_fixup = new_fixup "entry symbol" in

  let elf_header =
    elf32_header
      ~ei_data: ELFDATA2LSB
      ~e_type: ET_EXEC
      ~e_machine: EM_386
      ~e_version: EV_CURRENT

      ~e_entry_fixup: e_entry_fixup
      ~e_phoff_fixup: program_header_table_fixup
      ~e_shoff_fixup: section_header_table_fixup
      ~e_phnum: (Int64.of_int (Array.length program_headers))
      ~e_shnum: (Int64.of_int (Array.length section_headers))
      ~e_shstrndx: shstrtabndx
  in

  let data_sym name st_bind fixup =
    let name_fixup = new_fixup ("data symbol name fixup: '" ^ name ^ "'") in
    let strtab_entry = DEF (name_fixup, ZSTRING name) in
    let symtab_entry =
      symbol
        ~string_table_fixup: dynstr_section_fixup
        ~name_string_fixup: name_fixup
        ~sym_target_fixup: (Some fixup)
        ~st_bind
        ~st_type: STT_OBJECT
        ~st_shndx: datandx
    in
      (strtab_entry, symtab_entry)
  in

  let rodata_sym name st_bind fixup =
    let name_fixup = new_fixup ("rodata symbol name fixup: '" ^ name ^ "'") in
    let strtab_entry = DEF (name_fixup, ZSTRING name) in
    let symtab_entry =
      symbol
        ~string_table_fixup: dynstr_section_fixup
        ~name_string_fixup: name_fixup
        ~sym_target_fixup: (Some fixup)
        ~st_bind
        ~st_type: STT_OBJECT
        ~st_shndx: rodatandx
    in
      (strtab_entry, symtab_entry)
  in

  let text_sym name st_bind fixup =
    let name_fixup = new_fixup ("text symbol name fixup: '" ^ name ^ "'") in
    let strtab_frag = DEF (name_fixup, ZSTRING name) in
    let symtab_frag =
      symbol
        ~string_table_fixup: dynstr_section_fixup
        ~name_string_fixup: name_fixup
        ~sym_target_fixup: (Some fixup)
        ~st_bind: st_bind
        ~st_type: STT_FUNC
        ~st_shndx: textndx
    in
      (strtab_frag, symtab_frag)
  in

  let import_sym name st_bind _(*fixup*) =
    let name_fixup = new_fixup ("import symbol name fixup: '" ^ name ^ "'") in
    let strtab_frag = DEF (name_fixup, ZSTRING name) in
    let symtab_frag =
      symbol
        ~string_table_fixup: dynstr_section_fixup
        ~name_string_fixup: name_fixup
        ~sym_target_fixup: None
        ~st_bind
        ~st_type: STT_FUNC
        ~st_shndx: shn_UNDEF
    in
      (strtab_frag, symtab_frag)
  in

  let frags_of_symbol sym_emitter st_bind symname_opt symbody x =
    let (strtab_frags, symtab_frags, body_frags) = x in
    let (strtab_frag, symtab_frag, body_frag) =
      match symname_opt with
          None -> (MARK, MARK, symbody)
        | Some symname ->
            let body_fixup = new_fixup ("symbol body fixup: '" ^ symname ^ "'") in
            let body =
              if symname = entry_name
              then DEF (e_entry_fixup, DEF (body_fixup, symbody))
              else DEF (body_fixup, symbody)
            in
            let (str, sym) = sym_emitter symname st_bind body_fixup in
              (str, sym, body)
    in
      ((strtab_frag :: strtab_frags),
       (symtab_frag :: symtab_frags),
       (body_frag :: body_frags))
  in

  let frags_of_import_symbol sym_emitter st_bind symname plt_entry_fixup x =
    let (i, strtab_frags, symtab_frags,
         plt_frags, got_plt_frags, rela_plt_frags) = x in
    let (strtab_frag, symtab_frag) = sym_emitter symname st_bind None in
    let e = Il.new_emitter X86.prealloc_quad true in
    let jump_slot_fixup = new_fixup ("jump slot #" ^ string_of_int i) in
    let jump_slot_initial_target_fixup =
      new_fixup ("jump slot #" ^ string_of_int i ^ " initial target") in
    let plt_frag =
      Il.emit_full e (Some plt_entry_fixup)
        (Il.jmp Il.JMP (Il.CodeMem (Il.AbsIn (M_POS jump_slot_fixup, None))));
      Il.emit_full e (Some jump_slot_initial_target_fixup)
        (Il.Push (X86.immi (Int64.of_int i)));
      Il.emit e (Il.jmp Il.JMP (Il.CodeMem (Il.Abs (M_POS plt0_fixup))));
      X86.frags_of_emitted_quads sess e
    in
    let got_plt_frag = DEF (jump_slot_fixup,
                            WORD (TY_u32, (M_POS jump_slot_initial_target_fixup))) in
    let rela_plt =
      { elf32_386_rela_type = R_386_JMP_SLOT;
        elf32_386_rela_offset = (M_POS jump_slot_fixup);
        elf32_386_rela_sym = (IMM (Int64.of_int i));
        elf32_386_rela_addend = (IMM 0L) }
    in
    let rela_plt_frag = elf32_386_rela_frag rela_plt in
      (i+1,
       (strtab_frag :: strtab_frags),
       (symtab_frag :: symtab_frags),
       (plt_frag :: plt_frags),
       (got_plt_frag :: got_plt_frags),
       (rela_plt_frag :: rela_plt_frags))
  in

  let (global_text_strtab_frags,
       global_text_symtab_frags,
       text_body_frags) =
    Hashtbl.fold (frags_of_symbol text_sym STB_GLOBAL) text_frags ([],[],[])
  in

  let (local_text_strtab_frags,
       local_text_symtab_frags) =

    let symbol_frags_of_code _ code accum =
      let (strtab_frags, symtab_frags) = accum in
      let fix = code.Semant.code_fixup in
      let (strtab_frag, symtab_frag) = text_sym fix.fixup_name STB_LOCAL fix in
      (strtab_frag :: strtab_frags,
       symtab_frag :: symtab_frags)
    in

    let item_str_frags, item_sym_frags =
      Hashtbl.fold symbol_frags_of_code sem.Semant.ctxt_all_item_code ([], [])
    in
    let glue_str_frags, glue_sym_frags =
      Hashtbl.fold symbol_frags_of_code sem.Semant.ctxt_glue_code ([], [])
    in
      (item_str_frags @ glue_str_frags,
       item_sym_frags @ glue_sym_frags)
  in

  let (rodata_strtab_frags,
       rodata_symtab_frags,
       rodata_body_frags) =
    Hashtbl.fold (frags_of_symbol rodata_sym STB_GLOBAL) rodata_frags ([],[],[])
  in

  let (data_strtab_frags,
       data_symtab_frags,
       data_body_frags) =
    Hashtbl.fold (frags_of_symbol data_sym STB_GLOBAL) data_frags ([],[],[])
  in

  let (_,
       import_strtab_frags,
       import_symtab_frags,
       plt_frags,
       got_plt_frags,
       rela_plt_frags) =
    Hashtbl.fold (frags_of_import_symbol import_sym STB_GLOBAL) import_fixups
      (1,[],[],[plt0_frag],[got_prefix],[])
  in
  let import_symtab_frags = List.rev import_symtab_frags in
  let plt_frags = List.rev plt_frags in
  let got_plt_frags = List.rev got_plt_frags in
  let rela_plt_frags = List.rev rela_plt_frags in

  let dynamic_needed_strtab_frags = Array.make (Array.length needed_libs) MARK in

  let dynamic_frags =
    let dynamic_needed_frags = Array.make (Array.length needed_libs) MARK in
      for i = 0 to (Array.length needed_libs) - 1 do
        let fixup = new_fixup ("needed library name fixup: " ^ needed_libs.(i)) in
          dynamic_needed_frags.(i) <- elf32_dyn_frag (DT_NEEDED, SUB (M_POS fixup,
                                                                      M_POS dynstr_section_fixup));
          dynamic_needed_strtab_frags.(i) <- DEF (fixup, ZSTRING needed_libs.(i))
      done;
      (SEQ [|
         SEQ dynamic_needed_frags;
         elf32_dyn_frag (DT_STRTAB, M_POS dynstr_section_fixup);
         elf32_dyn_frag (DT_STRSZ, M_SZ dynstr_section_fixup);

         elf32_dyn_frag (DT_SYMTAB, M_POS dynsym_section_fixup);
         elf32_dyn_frag (DT_SYMENT, IMM elf32_symsize);

         elf32_dyn_frag (DT_PLTGOT, M_POS got_plt_section_fixup);

         elf32_dyn_frag (DT_PLTREL, IMM (elf32_num_of_dyn_tag DT_RELA));
         elf32_dyn_frag (DT_PLTRELSZ, M_SZ rela_plt_section_fixup);
         elf32_dyn_frag (DT_JMPREL, M_POS rela_plt_section_fixup);

         elf32_dyn_frag (DT_NULL, IMM 0L)
       |])
  in

  let null_strtab_fixup = new_fixup "null dynstrtab entry" in
  let null_strtab_frag = DEF (null_strtab_fixup, ZSTRING "") in
  let null_symtab_frag = (symbol
                            ~string_table_fixup: dynstr_section_fixup
                            ~name_string_fixup: null_strtab_fixup
                            ~sym_target_fixup: None
                            ~st_bind: STB_LOCAL
                            ~st_type: STT_NOTYPE
                            ~st_shndx: 0L) in

  let dynsym_frags = (null_symtab_frag ::
                        (import_symtab_frags @
                           global_text_symtab_frags @
                           local_text_symtab_frags @
                           rodata_symtab_frags @
                           data_symtab_frags))
  in

  let dynstr_frags = (null_strtab_frag ::
                        (import_strtab_frags @
                           global_text_strtab_frags @
                           local_text_strtab_frags @
                           rodata_strtab_frags @
                           data_strtab_frags @
                           (Array.to_list dynamic_needed_strtab_frags)))
  in

  let interp_section =
    DEF (interp_section_fixup, ZSTRING "/lib/ld-linux.so.2")
  in

  let text_section =
    DEF (text_section_fixup,
         SEQ (Array.of_list text_body_frags))
  in
  let rodata_section =
    DEF (rodata_section_fixup,
         SEQ (Array.of_list rodata_body_frags))
  in
  let data_section =
    DEF (data_section_fixup,
         SEQ (Array.of_list data_body_frags))
  in
  let bss_section =
    DEF (bss_section_fixup,
         SEQ [| |])
  in
  let dynsym_section =
    DEF (dynsym_section_fixup,
         SEQ (Array.of_list dynsym_frags))
  in
  let dynstr_section =
    DEF (dynstr_section_fixup,
         SEQ (Array.of_list dynstr_frags))
  in

  let plt_section =
    DEF (plt_section_fixup,
         SEQ (Array.of_list plt_frags))
  in

  let got_plt_section =
    DEF (got_plt_section_fixup,
         SEQ (Array.of_list got_plt_frags))
  in

  let rela_plt_section =
    DEF (rela_plt_section_fixup,
         SEQ (Array.of_list rela_plt_frags))
  in

  let dynamic_section =
    DEF (dynamic_section_fixup, dynamic_frags)
  in


  let page_alignment = 0x1000 in

  let align_both i =
    ALIGN_FILE (page_alignment,
                (ALIGN_MEM (page_alignment, i)))
  in

  let def_aligned f i =
    align_both
      (SEQ [| DEF(f,i);
              (align_both MARK)|])
  in

  let debug_aranges_section =
    def_aligned dwarf.Dwarf.debug_aranges_fixup dwarf.Dwarf.debug_aranges
  in
  let debug_pubnames_section =
    def_aligned dwarf.Dwarf.debug_pubnames_fixup dwarf.Dwarf.debug_pubnames
  in
  let debug_info_section =
    def_aligned dwarf.Dwarf.debug_info_fixup dwarf.Dwarf.debug_info
  in
  let debug_abbrev_section =
    def_aligned dwarf.Dwarf.debug_abbrev_fixup dwarf.Dwarf.debug_abbrev
  in
  let debug_line_section =
    def_aligned dwarf.Dwarf.debug_line_fixup dwarf.Dwarf.debug_line
  in
  let debug_frame_section =
    def_aligned dwarf.Dwarf.debug_frame_fixup dwarf.Dwarf.debug_frame
  in

  let load_address = 0x0804_8000L in

    SEQ
      [|
        MEMPOS load_address;
        ALIGN_FILE
          (segment_2_align,
           DEF
             (segment_2_fixup,
              SEQ
                [|
                  elf_header;
                  ALIGN_FILE
                    (segment_0_align,
                     DEF
                       (segment_0_fixup,
                        SEQ
                          [|
                            DEF (program_header_table_fixup,
                                 program_header_table);
                          |]));
                  ALIGN_FILE
                    (segment_1_align,
                     DEF (segment_1_fixup, interp_section));
                  text_section;
                  rodata_section;
                  dynsym_section;
                  dynstr_section;
                  plt_section;
                  rela_plt_section;
                  debug_aranges_section;
                  debug_pubnames_section;
                  debug_info_section;
                  debug_abbrev_section;
                  debug_line_section;
                  debug_frame_section;
                |]));
        ALIGN_FILE
          (segment_3_align,
           DEF
             (segment_3_fixup,
              SEQ
                [|
                  data_section;
                  got_plt_section;
                  bss_section;
                  ALIGN_FILE
                    (segment_4_align,
                     DEF (segment_4_fixup,
                          dynamic_section));
                |]));
        DEF (shstrtab_section_fixup,
             shstrtab_section);
        DEF (section_header_table_fixup,
             section_header_table);
      |]
;;

let emit_file
    (sess:Session.sess)
    (code:Asm.frag)
    (data:Asm.frag)
    (sem:Semant.ctxt)
    (dwarf:Dwarf.debug_records)
    : unit =

  let text_frags = Hashtbl.create 4 in
  let rodata_frags = Hashtbl.create 4 in
  let data_frags = Hashtbl.create 4 in
  let import_fixups = Hashtbl.create 4 in

  (*
   * Startup on elf-linux is more complex than in win32. It's
   * thankfully documented in some detail around the net.
   *
   *   - The elf entry address is for _start.
   *
   *   - _start pushes:
   *
   *       eax   (should be zero)
   *       esp   (holding the kernel-provided stack end)
   *       edx   (address of _rtld_fini)
   *       address of _fini
   *       address of _init
   *       ecx   (argv)
   *       esi   (argc)
   *       address of main
   *
   *     and then calls __libc_start_main@plt.
   *
   *   - This means any sensible binary has a PLT. Fun. So
   *     We call into the PLT, which itself is just a bunch
   *     of indirect jumps through slots in the GOT, and wind
   *     up in __libc_start_main. Which calls _init, then
   *     essentially exit(main(argc,argv)).
   *)


  let init_fixup = new_fixup "_init function entry" in
  let fini_fixup = new_fixup "_fini function entry" in
  let start_fixup = new_fixup "start function entry" in
  let rust_start_fixup = (Semant.import sem LIB_rustrt "rust_start") in
  let libc_start_main_fixup = new_fixup "__libc_start_main@plt stub" in

  let start_fn =
    let e = X86.new_emitter () in
    let push_r32 r = Il.emit e
      (Il.Push (Il.Cell (Il.Reg (Il.Hreg r, Il.ValTy Il.Bits32))))
    in
    let push_pos32 fix = Il.emit e
      (Il.Push (X86.imm (M_POS fix)))
    in
      push_r32 X86.eax;
      push_r32 X86.esp;
      push_r32 X86.edx;
      push_pos32 fini_fixup;
      push_pos32 init_fixup;
      push_r32 X86.ecx;
      push_r32 X86.esi;
      push_pos32 start_fixup;
      Il.emit e (Il.call
                   (Il.Reg (Il.Hreg X86.eax, Il.ValTy Il.Bits32))
                   (Il.CodeMem (Il.Abs (M_POS libc_start_main_fixup))));
      X86.frags_of_emitted_quads sess e
  in

  let do_nothing_fn =
    let e = Il.new_emitter X86.prealloc_quad true in
      Il.emit e Il.Ret;
      X86.frags_of_emitted_quads sess e
  in

  let main_fn =
    let e = X86.new_emitter() in
      X86.objfile_start e
        ~start_fixup ~rust_start_fixup
        ~main_fn_fixup: sem.Semant.ctxt_main_fn_fixup
        ~global_glue: sem.Semant.ctxt_global_glue_fixup
        ~indirect_start: false;
      X86.frags_of_emitted_quads sess e
  in

  let needed_libs =
    [|
      "libc.so.6";
      "librustrt.so"
    |]
  in

  let _ =
    htab_put text_frags (Some "_start") start_fn;
    htab_put text_frags (Some "_init") (DEF (init_fixup, do_nothing_fn));
    htab_put text_frags (Some "_fini") (DEF (fini_fixup, do_nothing_fn));
    htab_put text_frags (Some "main") main_fn;
    htab_put text_frags None code;
    htab_put rodata_frags None data;
    htab_put import_fixups "__libc_start_main" libc_start_main_fixup;

    Hashtbl.iter
      begin
        fun _ tab ->
          Hashtbl.iter
            begin
              fun name fixup ->
                htab_put import_fixups name fixup
            end
            tab
      end
      sem.Semant.ctxt_imports
  in
  let all_frags =
    elf32_linux_x86_file
      ~sess
      ~entry_name: "_start"
      ~text_frags
      ~data_frags
      ~dwarf
      ~sem
      ~rodata_frags
      ~import_fixups
      ~needed_libs
  in
  let buf = Buffer.create 16 in
  let out = open_out_bin sess.Session.sess_out in
    resolve_frag sess all_frags;
    lower_frag ~sess ~lsb0: true ~buf: buf ~it: all_frags;
    Buffer.output_buffer out buf;
    flush out;
    close_out out


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
