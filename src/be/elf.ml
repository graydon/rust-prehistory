(*    
   Module for writing System V ELF files.
*)

open Asm
;;


(* Fixed sizes of structs involved in elf32 spec. *)
let elf32_ehsize = 52L;;
let elf32_phentsize = 32L;;
let elf32_shentsize = 40L;;
let elf32_symsize = 16L;;
let elf32_rela_entsz = 8L;;

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
	: item = 
  let elf_header_fixup = new_fixup "elf header" in
	DEF 
	  (elf_header_fixup, 
	   SEQ [| elf_identification ELFCLASS32 ei_data;
			  WORD16 (IMM (match e_type with 
							   ET_NONE -> 0L
							 | ET_REL -> 1L
							 | ET_EXEC -> 2L
							 | ET_DYN -> 3L
							 | ET_CORE -> 4L));
			  WORD16 (IMM (match e_machine with 
							   EM_NONE -> 0L
							 | EM_386 -> 3L
							 | EM_X86_64 -> 62L));
			  WORD32 (IMM (match e_version with 
							   EV_NONE -> 0L
							 | EV_CURRENT -> 1L));
			  WORD32 (M_POS e_entry_fixup);
			  WORD32 (F_POS e_phoff_fixup);
			  WORD32 (F_POS e_shoff_fixup);
			  WORD32 (IMM 0L); (* e_flags *)
			  WORD16 (IMM elf32_ehsize);
			  WORD16 (IMM elf32_phentsize);
			  WORD16 (IMM e_phnum);
			  WORD16 (IMM elf32_shentsize);
			  WORD16 (IMM e_shnum);
			  WORD16 (IMM e_shstrndx); 
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
	: item = 
  SEQ 
	[|
	  WORD32 (SUB 
				((F_POS shname_string_fixup),
				 (F_POS shstring_table_fixup)));
	  WORD32 (IMM (match sh_type with 
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
					 | SHT_DYNSYM -> 11L));
	  WORD32 (IMM (fold_flags 
					 (fun f -> match f with 
						  SHF_WRITE -> 0x1L
						| SHF_ALLOC -> 0x2L
						| SHF_EXECINSTR -> 0x3L) sh_flags));
	  WORD32 (match section_fixup with 
				  None -> (IMM 0L)
				| Some s -> (M_POS s));
	  WORD32 (match section_fixup with 
				  None -> (IMM 0L)
				| Some s -> (F_POS s));
	  WORD32 (match section_fixup with 
				  None -> (IMM 0L)
				| Some s -> (F_SZ s));
	  WORD32 (IMM (match sh_link with 
					   None -> 0L
					 | Some i -> i)); 
	  WORD32 (IMM 0L); (* sh_info *)
	  WORD32 (IMM sh_addralign);
	  WORD32 (IMM sh_entsize);
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
	: item = 
  SEQ 
	[|
	  WORD32 (IMM (match p_type with 
					   PT_NULL -> 0L
					 | PT_LOAD -> 1L 
					 | PT_DYNAMIC -> 2L
					 | PT_INTERP -> 3L
					 | PT_NOTE -> 4L
					 | PT_SHLIB -> 5L
				   | PT_PHDR -> 6L));
	  WORD32 (F_POS segment_fixup);
	  WORD32 (M_POS segment_fixup);
	  WORD32 (IMM 0L); (* p_paddr, 0 on most archs *)
	  WORD32 (F_SZ segment_fixup);
	  WORD32 (M_SZ segment_fixup);
	  WORD32 (IMM (fold_flags
					 (fun f -> 
						match f with 
							PF_X -> 0x1L
						| PF_W -> 0x2L
						| PF_R -> 0x4L)
					 p_flags));
	  WORD32 (IMM p_align);
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
let shn_UNDEF   = 0;;
let shn_ABS     = 0xfff1;;
let shn_ABS     = 0xfff2;;

	
let symbol 
	~(string_table_fixup:fixup)
	~(name_string_fixup:fixup)
	~(sym_target_fixup:fixup)
	~(st_bind:st_bind)
	~(st_type:st_type)
	~(st_shndx:int64)
	: item = 
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
		WORD32 (SUB 
				  ((F_POS name_string_fixup),
				   (F_POS string_table_fixup)));
		WORD32 (M_POS sym_target_fixup);
		WORD32 (M_SZ sym_target_fixup);
		WORD8           (* st_info *)
		  (OR 
			 ((SLL ((IMM st_bind_num), 4)),
			  (AND ((IMM st_type_num), (IMM 0xfL)))));
		WORD8 (IMM 0L); (* st_other *)
		WORD16 (IMM st_shndx);
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

let elf32_dyn_item d = 
  match d with 
	  (tag, expr) -> 
		let tagval = 
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
		in
		  SEQ [| WORD32 (IMM tagval);
				 WORD32 expr |]
;;

let elf32_linux_x86_file 
	~(entry_name:string)
	~(text_items:(string, item) Hashtbl.t)
	~(data_items:(string, item) Hashtbl.t)
	~(rodata_items:(string, item) Hashtbl.t)
	: item = 


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

  let x86_items_of_emitted_triples e = 
	(SEQ (Array.map X86.select_insn e.Il.emit_triples))
  in

  let synthetic_start_fn = 
	let init_fixup = new_fixup "_init function entry" in
	let fini_fixup = new_fixup "_fini function entry" in
	let main_fixup = new_fixup "main function entry" in
	let libc_start_main_plt_fixup = new_fixup "__libc_start_main@plt stub" in
	let e = Il.new_emitter X86.n_hardregs in
	  Il.emit e (Il.CPUSH Il.DATA32) (Il.HWreg X86.eax) Il.Nil;
	  Il.emit e (Il.CPUSH Il.DATA32) (Il.HWreg X86.esp) Il.Nil;
	  Il.emit e (Il.CPUSH Il.DATA32) (Il.HWreg X86.edx) Il.Nil;
	  Il.emit e (Il.CPUSH Il.DATA32) (Il.Imm (M_POS fini_fixup)) Il.Nil;
	  Il.emit e (Il.CPUSH Il.DATA32) (Il.Imm (M_POS init_fixup)) Il.Nil;
	  Il.emit e (Il.CPUSH Il.DATA32) (Il.HWreg X86.ecx) Il.Nil;
	  Il.emit e (Il.CPUSH Il.DATA32) (Il.HWreg X86.esi) Il.Nil;
	  Il.emit e (Il.CPUSH Il.DATA32) (Il.Imm (M_POS main_fixup)) Il.Nil;
	  Il.emit e Il.CCALL (Il.Imm (M_POS libc_start_main_plt_fixup)) Il.Nil;
	  x86_items_of_emitted_triples e
  in

  (* And now, Procedure Linkage Tables (PLTs) and Global Offset Tables
   * (GOTs). The PLT goes in a section called .plt and GOT in a section
   * called .got. The portion of the GOT that holds PLT jump slots goes
   * in a section called .got.plt. Dynamic relocations for these jump slots
   * go in section .rela.plt.
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

  let (plt_items, got_items, got_reloc_items) = 
	let plt0_fixup = new_fixup "PLT[0]" in
	let got1_fixup = new_fixup "GOT[1]" in
	let got2_fixup = new_fixup "GOT[2]" in	  
	let got_prefix = SEQ [| WORD32 (IMM 0L); 
							DEF (got1_fixup, WORD32 (IMM 0L)); 
							DEF (got2_fixup, WORD32 (IMM 0L)); |] 
	in
	let plt0_item = 
	  let e = Il.new_emitter X86.n_hardregs in
		Il.emit e (Il.CPUSH Il.DATA32) (Il.Imm (M_POS got1_fixup)) Il.Nil;
		Il.emit e Il.JMP (Il.Pcrel got2_fixup) Il.Nil;
		Il.emit e Il.NOP Il.Nil Il.Nil;
		Il.emit e Il.NOP Il.Nil Il.Nil;
		Il.emit e Il.NOP Il.Nil Il.Nil;
		Il.emit e Il.NOP Il.Nil Il.Nil;
		x86_items_of_emitted_triples e
	in
	let form_item_triple i = 
	  let e = Il.new_emitter X86.n_hardregs in 
	  let jump_slot_fixup = new_fixup ("jump slot #" ^ string_of_int i) in
	  let jump_slot_initial_target_fixup = 
		new_fixup ("jump slot #" ^ string_of_int i ^ " initial target") in
 	  let plt_item = 
		Il.emit e Il.JMP (Il.Pcrel jump_slot_fixup) Il.Nil;
		Il.emit_full e (Some jump_slot_initial_target_fixup)
		  (Il.CPUSH Il.DATA32) (Il.Imm (IMM (Int64.of_int i))) Il.Nil;
		Il.emit e Il.JMP (Il.Pcrel plt0_fixup) Il.Nil;
		x86_items_of_emitted_triples e
	  in
	  let got_item = WORD32 (M_POS jump_slot_fixup) in 
		0
	in
	  (0,0,0)
  in
(*	  in
*)

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
  let rela_plt_section_name_fixup = new_fixup "string name of '.rela.plt' section" in
  let data_section_name_fixup = new_fixup "string name of '.data' section" in
  let bss_section_name_fixup = new_fixup "string name of '.bss' section" in
  let dynamic_section_name_fixup = new_fixup "string name of '.dynamic' section" in
  let shstrtab_section_name_fixup = new_fixup "string name of '.shstrtab' section" in

  let n_shdrs        = 11L in
  let interpndx      = 1L in  (* Section index of .interp *)
  let textndx        = 2L in  (* Section index of .text *)
  let rodatandx      = 3L in  (* Section index of .rodata *)
  let dynsymndx      = 4L in  (* Section index of .dynsym *)
  let dynstrndx      = 5L in  (* Section index of .dynstr *)
  let relapltndx     = 6L in  (* Section index of .rela.dyn *)
  let datandx        = 7L in  (* Section index of .data *)
  let bssndx         = 8L in  (* Section index of .bss *)
  let dynamicndx     = 9L in  (* Section index of .dynamic *)
  let shstrtabndx    = 10L in (* Section index of .shstrtab *)

  let section_header_table_fixup = new_fixup ".section header table" in
  let interp_section_fixup = new_fixup ".interp section" in
  let text_section_fixup = new_fixup ".text section" in
  let rodata_section_fixup = new_fixup ".rodata section" in
  let dynsym_section_fixup = new_fixup ".dynsym section" in
  let dynstr_section_fixup = new_fixup ".dynstr section" in
  let rela_plt_section_fixup = new_fixup ".rela.plt section" in
  let data_section_fixup = new_fixup ".data section" in
  let bss_section_fixup = new_fixup ".bss section" in
  let dynamic_section_fixup = new_fixup ".dynamic section" in
  let shstrtab_section_fixup = new_fixup ".shstrtab section" in

  let shstrtab_section = 
	SEQ
	  [|
		DEF(null_section_name_fixup, ZSTRING "");
		DEF(interp_section_name_fixup, ZSTRING ".interp");
		DEF(text_section_name_fixup, ZSTRING ".text");
		DEF(rodata_section_name_fixup, ZSTRING ".rodata");
		DEF(dynsym_section_name_fixup, ZSTRING ".dynsym");
		DEF(dynstr_section_name_fixup, ZSTRING ".dynstr");
		DEF(rela_plt_section_name_fixup, ZSTRING ".rela.plt");
		DEF(data_section_name_fixup, ZSTRING ".data");
		DEF(bss_section_name_fixup, ZSTRING ".bss");
		DEF(dynamic_section_name_fixup, ZSTRING ".dynamic");
		DEF(shstrtab_section_name_fixup, ZSTRING ".shstrtab");
	  |]
  in

  let section_header_table = 
	SEQ
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
		   ~sh_link: None);

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
	  |]
  in

  (* There are 3 official program headers in the file we're making:   *)
  (* segment 0: PHDR                                                  *)
  (* segment 1: RE / LOAD                                             *)
  (* segment 2: RW / LOAD                                             *)

  let n_phdrs = 3L in
  let program_header_table_fixup = new_fixup "program header table" in
  let segment_0_fixup = new_fixup "segment 0" in
  let segment_1_fixup = new_fixup "segment 1" in
  let segment_2_fixup = new_fixup "segment 2" in

  let program_header_table = 
	SEQ
	  [| 
		(program_header 
		   ~p_type: PT_PHDR
		   ~segment_fixup: segment_0_fixup
		   ~p_flags: [ PF_R; PF_X ]
		   ~p_align: 4L);
		(program_header 
		   ~p_type: PT_LOAD
		   ~segment_fixup: segment_1_fixup
		   ~p_flags: [ PF_R; PF_X ]
		   ~p_align: 0x1000L);
		(program_header 
		   ~p_type: PT_LOAD
		   ~segment_fixup: segment_2_fixup
		   ~p_flags: [ PF_R; PF_W ]
		   ~p_align: 0x1000L);		
	  |]
  in

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
	  ~e_phnum: n_phdrs
	  ~e_shnum: n_shdrs
	  ~e_shstrndx: shstrtabndx
  in	

  let data_sym name st_bind fixup =
	let name_fixup = new_fixup "data symbol name fixup" in
	let strtab_entry = DEF (name_fixup, ZSTRING name) in
	let symtab_entry =
	  symbol 
		~string_table_fixup: dynstr_section_fixup
		~name_string_fixup: name_fixup
		~sym_target_fixup: fixup
		~st_bind: STB_GLOBAL
		~st_type: STT_OBJECT
		~st_shndx: datandx
	in
	  (strtab_entry, symtab_entry)
  in
    
  let rodata_sym name st_bind fixup =
	let name_fixup = new_fixup "rodata symbol name fixup" in
	let strtab_entry = DEF (name_fixup, ZSTRING name) in
	let symtab_entry =
	  symbol 
		~string_table_fixup: dynstr_section_fixup
		~name_string_fixup: name_fixup
		~sym_target_fixup: fixup
		~st_bind: st_bind
		~st_type: STT_OBJECT
		~st_shndx: rodatandx
	in
	  (strtab_entry, symtab_entry)
  in

  let text_sym name st_bind fixup =
	let name_fixup = new_fixup "text symbol name fixup" in
	let strtab_item = DEF (name_fixup, ZSTRING name) in
	let symtab_item =
	  symbol 
		~string_table_fixup: dynstr_section_fixup
		~name_string_fixup: name_fixup
		~sym_target_fixup: fixup
		~st_bind: st_bind
		~st_type: STT_FUNC
		~st_shndx: textndx
	in
	  (strtab_item, symtab_item)
  in

  let items_of_symbol sym_emitter st_bind symname symbody x = 
	let (strtab_items, symtab_items, body_items) = x in
	let body_fixup = new_fixup "symbol body fixup" in
	let body_item = 
	  if symname = entry_name
	  then DEF (e_entry_fixup, DEF (body_fixup, symbody))
	  else DEF (body_fixup, symbody) 
	in
	let (strtab_item, symtab_item) = sym_emitter symname st_bind body_fixup in
	  ((strtab_item :: strtab_items),
	   (symtab_item :: symtab_items),
	   (body_item :: body_items))
  in

  let (text_strtab_items, 
	   text_symtab_items,
	   text_body_items) = 
	Hashtbl.fold (items_of_symbol text_sym STB_GLOBAL) text_items ([],[],[])
  in

  let (rodata_strtab_items, 
	   rodata_symtab_items,
	   rodata_body_items) = 
	Hashtbl.fold (items_of_symbol rodata_sym STB_GLOBAL) rodata_items ([],[],[])
  in

  let (data_strtab_items, 
	   data_symtab_items,
	   data_body_items) = 
	Hashtbl.fold (items_of_symbol data_sym STB_GLOBAL) data_items ([],[],[])
  in

  let dynsym_items = (text_symtab_items @ 
						rodata_symtab_items @
						data_symtab_items) 
  in

  let dynstr_items = (text_strtab_items @ 
						rodata_strtab_items @
						data_strtab_items) 
  in

  (* Juicy bits from the caller. *)
  let text_section = 
	DEF (text_section_fixup, 
		 SEQ (Array.of_list text_body_items))
  in
  let rodata_section = 
	DEF (rodata_section_fixup,
		 SEQ (Array.of_list rodata_body_items))
  in
  let data_section = 
	DEF (data_section_fixup,
		 SEQ (Array.of_list data_body_items))
  in
  let bss_section = 
	DEF (bss_section_fixup,
		 SEQ [| |])
  in
  let dynsym_section = 
	DEF (dynsym_section_fixup,
		 SEQ (Array.of_list dynsym_items))
  in
  let dynstr_section = 
	DEF (dynstr_section_fixup,
		 SEQ (Array.of_list dynstr_items))
  in

  let load_address = 0x0804_8000L in
	
	SEQ 
	  [|
		MEMPOS load_address;
		DEF 
		  (segment_0_fixup, 
		   SEQ 
			 [| 
			   elf_header;
			   DEF (program_header_table_fixup,
					program_header_table);
			 |]);
		DEF 
		  (segment_1_fixup, 
		   SEQ 
			 [| 
			   text_section;
			   rodata_section;		   
			 |]);
		DEF 
		  (segment_2_fixup, 
		   SEQ 
			 [| 
			   data_section;
			   bss_section;
			 |]);
		DEF (shstrtab_section_fixup,
			 shstrtab_section);
		DEF (dynsym_section_fixup,
			 dynsym_section);
		DEF (dynstr_section_fixup,
			 dynstr_section);
		DEF (section_header_table_fixup,
			 section_header_table);
	  |]
;;

let emit_testfile outfile = 
  let text_items = Hashtbl.create 4 in
  let rodata_items = Hashtbl.create 4 in
  let data_items = Hashtbl.create 4 in

  let rodata_fixup = new_fixup "rodata item" in
  let str = "hello, world\n" in
  let rodata_item = DEF (rodata_fixup, (STRING str)) in
  let emitter = Il.new_emitter X86.n_hardregs in
  let text_item = 
	Il.emit emitter (Il.MOV Il.DATA32) (Il.HWreg X86.eax) (Il.Imm (F_SZ rodata_fixup));
	(SEQ (Array.map X86.select_insn emitter.Il.emit_triples))
  in	
  let _ = 
	Hashtbl.add text_items "entryfn" text_item;
	Hashtbl.add rodata_items "str" rodata_item
  in
  let all_items = 
	elf32_linux_x86_file 
	  ~entry_name: "entryfn"
	  ~text_items: text_items
	  ~data_items: data_items
	  ~rodata_items: rodata_items
  in
  let buf = Buffer.create 16 in
  let out = open_out_bin outfile in
	resolve_fixups all_items;
	lower_item ~lsb0: true ~buf: buf ~it: all_items;
	Buffer.output_buffer out buf;
	flush out;
	close_out out
