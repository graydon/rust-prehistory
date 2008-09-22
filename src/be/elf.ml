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
			  WORD32 (IMM (match e_type with 
							   ET_NONE -> 0L
							 | ET_REL -> 1L
							 | ET_EXEC -> 2L
							 | ET_DYN -> 3L
							 | ET_CORE -> 4L));
			  WORD32 (IMM (match e_machine with 
							   EM_NONE -> 0L
							 | EM_386 -> 3L
							 | EM_X86_64 -> 62L));
			  WORD32 (IMM (match e_version with 
							   EV_NONE -> 0L
							 | EV_CURRENT -> 1L));
			  WORD32 (M_POS e_entry_fixup);
			  WORD32 (F_POS e_phoff_fixup);
			  WORD32 (F_POS e_shoff_fixup);
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
	~(shname_string_fixup:fixup option)
	~(sh_type:sh_type)
	~(sh_flags:sh_flags list)
	~(section_fixup:fixup option)
	~(sh_addralign:int64)
	~(sh_entsize:int64)
	: item = 
  SEQ 
	[|
	  (* sh_name *)	  
	  WORD32 (match shname_string_fixup with 
				  None -> (IMM 0L)
				| Some nf -> 
					(SUB 
					   ((F_POS nf),
						(F_POS shstring_table_fixup))));
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
	  WORD32 (IMM 0L); (* sh_link, possibly use later. *)
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

let elf32_x86_file 
	~(e_entry_fixup:fixup)
	: item = 

  (* There are 8 official section headers in the file we're making:   *)
  (* section 0: <null section>                                        *)
  (* section 1: .text                                                 *)
  (* section 2: .rodata                                               *)
  (* section 3: .data                                                 *)
  (* section 4: .bss                                                  *)
  (* section 5: .shstrtab                                             *)
  (* section 6: .symtab                                               *)
  (* section 7: .strtab                                               *)

  let null_section_name_fixup = new_fixup "string name of <null> section" in
  let text_section_name_fixup = new_fixup "string name of '.text' section" in
  let rodata_section_name_fixup = new_fixup "string name of '.rodata section" in
  let data_section_name_fixup = new_fixup "string name of '.data' section" in
  let bss_section_name_fixup = new_fixup "string name of '.bss' section" in
  let shstrtab_section_name_fixup = new_fixup "string name of '.shstrtab section" in
  let symtab_section_name_fixup = new_fixup "string name of '.symtab' section" in
  let strtab_section_name_fixup = new_fixup "string name of '.strtab' section" in

  let n_shdrs        = 8L in
  let textndx        = 1L in  (* Section index of .text *)
  let rodatandx      = 2L in  (* Section index of .rodata *)
  let datandx        = 3L in  (* Section index of .data *)
  let shstrndx       = 5L in  (* Section index of .shstrtab *)
  let strndx         = 7L in  (* Section index of .strtab *)

  let section_header_table_fixup = new_fixup "section header table" in	
  let null_section_fixup = new_fixup "null section" in	
  let text_section_fixup = new_fixup "text section" in	
  let rodata_section_fixup = new_fixup "rodata section" in	
  let data_section_fixup = new_fixup "data section" in	
  let bss_section_fixup = new_fixup "bss section" in	
  let shstrtab_section_fixup = new_fixup "shstrtab section" in	
  let symtab_section_fixup = new_fixup "symbtab section" in	
  let strtab_section_fixup = new_fixup "strtab section" in	

  let section_header_table = 
	SEQ
	  [|
		(* <null> *)
		(section_header 
		   ~shstring_table_fixup: shstrtab_section_fixup
		   ~shname_string_fixup: None
		   ~sh_type: SHT_NULL
		   ~sh_flags: []
		   ~section_fixup: None
		   ~sh_addralign: 0L
		   ~sh_entsize: 0L);

		(* .text *)
		(section_header 
		   ~shstring_table_fixup: shstrtab_section_fixup
		   ~shname_string_fixup: (Some text_section_name_fixup)
		   ~sh_type: SHT_PROGBITS
		   ~sh_flags: [ SHF_ALLOC; SHF_EXECINSTR ]
		   ~section_fixup: (Some text_section_fixup)
		   ~sh_addralign: 32L
		   ~sh_entsize: 0L);

		(* .rodata *)
		(section_header 
		   ~shstring_table_fixup: strtab_section_fixup
		   ~shname_string_fixup: (Some rodata_section_name_fixup)
		   ~sh_type: SHT_PROGBITS
		   ~sh_flags: [ SHF_ALLOC ]
		   ~section_fixup: (Some rodata_section_fixup)
		   ~sh_addralign: 32L
		   ~sh_entsize: 0L);

		(* .data *)
		(section_header 
		   ~shstring_table_fixup: shstrtab_section_fixup
		   ~shname_string_fixup: (Some data_section_name_fixup)
		   ~sh_type: SHT_PROGBITS
		   ~sh_flags: [ SHF_ALLOC; SHF_WRITE ]
		   ~section_fixup: (Some data_section_fixup)
		   ~sh_addralign: 32L
		   ~sh_entsize: 0L);

		(* .bss *)
		(section_header 
		   ~shstring_table_fixup: shstrtab_section_fixup
		   ~shname_string_fixup: (Some bss_section_name_fixup)
		   ~sh_type: SHT_NOBITS
		   ~sh_flags: [ SHF_ALLOC; SHF_WRITE ]
		   ~section_fixup: (Some bss_section_fixup)
		   ~sh_addralign: 32L
		   ~sh_entsize: 0L);

		(* .shstrtab *)
		(section_header 
		   ~shstring_table_fixup: shstrtab_section_fixup
		   ~shname_string_fixup: (Some shstrtab_section_name_fixup)
		   ~sh_type: SHT_STRTAB
		   ~sh_flags: []
		   ~section_fixup: (Some shstrtab_section_fixup)
		   ~sh_addralign: 1L
		   ~sh_entsize: 0L);

		(* .symtab *)
		(section_header 
		   ~shstring_table_fixup: shstrtab_section_fixup
		   ~shname_string_fixup: (Some symtab_section_name_fixup)
		   ~sh_type: SHT_SYMTAB
		   ~sh_flags: []
		   ~section_fixup: (Some symtab_section_fixup)
		   ~sh_addralign: 4L
		   ~sh_entsize: 0L);

		(* .strtab *)
		(section_header 
		   ~shstring_table_fixup: shstrtab_section_fixup
		   ~shname_string_fixup: (Some strtab_section_name_fixup)
		   ~sh_type: SHT_STRTAB
		   ~sh_flags: []
		   ~section_fixup: (Some strtab_section_fixup)
		   ~sh_addralign: 1L
		   ~sh_entsize: 0L);
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

	SEQ 
	  [| 
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
		  ~e_shstrndx: shstrndx;
		program_header_table;
		(* ... *)
		section_header_table;
	|]
;;

(* 

let write_elf_header_at (b:buf) (pos:int) (lim:int) (eh:elf_header) =
  let p = ref pos in 
  let wbs bs = (p := write_bytes b !p lim bs) in
  let wh h = wbs (int_to_u16_lsb0 h) in
  let ww w = wbs (int32_lsb0 w) in
  wbs eh.e_ident;
  wh eh.e_type;
  wh eh.e_machine;
  ww eh.e_version;
  ww eh.e_entry;
  ww eh.e_phoff;
  ww eh.e_shoff;
  ww eh.e_flags;
  wh eh.e_ehsize;
  wh eh.e_phentsize;
  wh eh.e_phnum;
  wh eh.e_shentsize;
  wh eh.e_shnum;
  wh eh.e_shstrndx;
  !p
;;

let write_section_header_at (b:buf) (pos:int) (lim:int) (sh:section_header) =
  let p = ref pos in 
  let wbs bs = (p := write_bytes b !p lim bs) in
  let ww w = wbs (int32_lsb0 w) in
  List.iter ww 
    [ sh.sh_name; sh.sh_type; sh.sh_flags; sh.sh_addr; 
      sh.sh_offset; sh.sh_size; sh.sh_link; sh.sh_info; 
      sh.sh_addralign; sh.sh_entsize; ];
  !p
;;

let write_program_header_at (b:buf) (pos:int) (lim:int) (ph:program_header) =
  let p = ref pos in 
  let wbs bs = (p := write_bytes b !p lim bs) in
  let ww w = wbs (int32_lsb0 w) in
  List.iter ww 
    [ ph.p_type; ph.p_offset; ph.p_vaddr; ph.p_paddr; 
      ph.p_filesz; ph.p_memsz; ph.p_flags; ph.p_align ];
  !p
;;

let write_sym_at (b:buf) (pos:int) (lim:int) (s:symbol) =
  let p = ref pos in 
  let wbs bs = (p := write_bytes b !p lim bs) in
  let wh h = wbs (int_to_u16_lsb0 h) in
  let wb b = wbs (int_to_u8 b) in
  let ww w = wbs (int32_lsb0 w) in
    ww s.st_name;
    ww s.st_value;
    ww s.st_size;
    wb s.st_info;
    wb s.st_other;
    wh s.st_shndx;
    !p
;;
 




let mk_basic_sym ~name ~value ~size ~ty ~bind ~shndx =
    {
      st_name = name;
      st_value = value;
      st_size = size;
      st_info = ((bind lsl 4) lor (ty land 0xf));
      st_other = 0;
      st_shndx = shndx
    }


type fixup = FIXUP_VMA | FIXUP_SIZE;;

type basic_elf_file = 
    { 
      file_buf: buf;
      file_ehdr: elf_header;
      
      file_shdr_null: section_header;
      file_shdr_text: section_header;
      file_shdr_rodata: section_header;
      file_shdr_data: section_header;
      file_shdr_bss: section_header;
      file_shdr_shstrtab: section_header;
      file_shdr_symtab: section_header;
      file_shdr_strtab: section_header;
      
      file_phdr_phdr: program_header;
      file_phdr_re_load: program_header;
      file_phdr_rw_load: program_header;

      (* Symbol table management *)
      file_syms: (string, (int * int * int * int)) Hashtbl.t;
      file_fixups: (int * fixup * string) Stack.t;
      add_data_sym: (string -> int -> int -> unit);
      add_rodata_sym: (string -> int -> int -> unit);
      add_func_sym: (string -> int -> int -> unit);
      add_vma_fixup: (int -> string -> unit);
      add_size_fixup: (int -> string -> unit);
    }
;;


let mk_basic_x86_elf_file fn = 
  (* As this is just a boostrap system, we adopt an incredibly cheesy *)
  (* fixed-sizes-for-everything layout, and fault if they're overrun. *)
  (* The file is < 4mb long:                                          *)
  (*      4kb for eh and phdrs, before the main sections              *)
  (*      1mb for the .text section                                   *)
  (*      1mb for the .data section                                   *)
  (*      1mb for the .rodata section                                 *)
  (*      4kb for the .shstrtab sectin                                *)
  (*      4kb for the .symtab section                                 *)
  (*    512kb for the .strtab section                                 *)
  (*      4kb for shdrs at end of file                                *)
  (*                                                                  *)
  (* Note that since all these are big round numbers, we don't do     *)
  (* any explicit alignment padding on the section boundaries. If     *)
  (* you were to improve this to be dense you would have to do so.    *)
  
  let load_base      = 0x0804_8000l in
  let eh_phdr_size   = 0x0000_1000l in
  let text_size      = 0x0010_0000l in
  let rodata_size    = 0x0010_0000l in
  let data_size      = 0x0010_0000l in
  let bss_size       = 0x0010_0000l in (* NOBITS, not "in" the file *)
  let shstrtab_size  = 0x0000_1000l in
  let symtab_size    = 0x0000_1000l in
  let strtab_size    = 0x0008_0000l in
  let shdr_size      = 0x0000_1000l in
  
  (* There are 8 official section headers in the file we're making:   *)
  (* section 0: <null section>                                        *)
  (* section 1: .text                                                 *)
  (* section 2: .rodata                                               *)
  (* section 3: .data                                                 *)
  (* section 4: .bss                                                  *)
  (* section 5: .shstrtab                                             *)
  (* section 6: .symtab                                               *)
  (* section 7: .strtab                                               *)

  let n_shdrs        = 8 in
  let textndx        = 1 in  (* Section index of .text *)
  let rodatandx      = 2 in  (* Section index of .rodata *)
  let datandx        = 3 in  (* Section index of .data *)
  let shstrndx       = 5 in  (* Section index of .shstrtab *)
  let strndx         = 7 in  (* Section index of .strtab *)

  let text_off       = eh_phdr_size in
  let rodata_off     = Int32.add text_off text_size in
  let data_off       = Int32.add rodata_off rodata_size in
  (* NB: bss is NOBITS so it's normal to "overlap" with shstrtab *)
  let bss_off        = Int32.add data_off data_size in
  let shstrtab_off   = Int32.add data_off data_size in
  let symtab_off     = Int32.add shstrtab_off shstrtab_size in
  let strtab_off     = Int32.add symtab_off symtab_size in
  let shdr_off       = Int32.add strtab_off strtab_size in
  
  let text_addr      = Int32.add load_base text_off in
  let rodata_addr    = Int32.add load_base rodata_off in
  let data_addr      = Int32.add load_base data_off in
  let bss_addr       = Int32.add load_base bss_off in
  
  let file_size      = Int32.add shdr_off shdr_size in

  (* There are 3 official program headers in the file we're making:   *)
  (* segment 0: PHDR                                                  *)
  (* segment 1: RE / LOAD                                             *)
  (* segment 2: RW / LOAD                                             *)

  let n_phdrs        = 3 in
  let phdr_off       = Int32.of_int ehsize in 
  let phdr_size      = (Int32.mul 
			  (Int32.of_int n_phdrs) 
			  (Int32.of_int phentsize))
  in
  let phdr_addr      = Int32.add load_base phdr_off in
  let re_off         = 0l in
  let re_size        = data_off in
  let re_addr        = load_base in
  let rw_off         = data_off in
  let rw_file_size   = data_size in
  let rw_mem_size    = Int32.add data_size bss_size in
  let rw_addr        = Int32.add load_base re_size in

  let symtab = Hashtbl.create 100 in
  let fixups = Stack.create () in

    { 
      file_buf = openbuf fn (Int32.to_int file_size);
      file_ehdr = (mk_basic_x86_ehdr 
		     n_phdrs phdr_off 
		     n_shdrs shdr_off
		     shstrndx);
      
      file_shdr_null = (mk_basic_shdr 
			  ~typ: sht_NULL ~flags: [] 
			  ~align: 0l ~addr: 0l
			  ~off: 0l ~sz: 0l);
      
      file_shdr_text = (mk_basic_shdr 
			  ~typ: sht_PROGBITS 
			  ~flags: [shf_ALLOC; shf_EXECINSTR]
			  ~align: 32l ~addr: text_addr 
			  ~off: text_off ~sz: text_size);
      
      file_shdr_rodata = (mk_basic_shdr 
			    ~typ: sht_PROGBITS
			    ~flags: [shf_ALLOC] 
			    ~align: 32l ~addr: rodata_addr
			    ~off: rodata_off ~sz: rodata_size);

      file_shdr_data = (mk_basic_shdr 
			  ~typ: sht_PROGBITS
			  ~flags: [shf_ALLOC; shf_WRITE] 
			  ~align: 32l ~addr: data_addr
			  ~off: data_off ~sz: data_size);
      
      file_shdr_bss = (mk_basic_shdr 
			 ~typ: sht_NOBITS
			 ~flags: [shf_ALLOC; shf_WRITE] 
			 ~align: 32l ~addr: bss_addr
			 ~off: bss_off ~sz: bss_size);

      file_shdr_shstrtab = (mk_basic_shdr 
			      ~typ: sht_STRTAB
			      ~flags: [] 
			      ~align: 1l ~addr: 0l
			      ~off: shstrtab_off ~sz: shstrtab_size);

      file_shdr_symtab = (let 
			      sh = (mk_basic_shdr 
				      ~typ: sht_SYMTAB
				      ~flags: [] 
				      ~align: 4l ~addr: 0l
				      ~off: symtab_off ~sz: symtab_size)
			  in
			    sh.sh_entsize <- Int32.of_int symsize;
			    sh.sh_link <- Int32.of_int strndx;
			    sh
			 );
      
      file_shdr_strtab = (mk_basic_shdr 
			    ~typ: sht_STRTAB
			    ~flags: [] 
			    ~align: 1l ~addr: 0l
			    ~off: strtab_off ~sz: strtab_size);
      
      file_phdr_phdr = (mk_basic_phdr 
			  ~typ: pt_PHDR ~flags: [pf_R; pf_X] ~align: 4l
			  ~off: phdr_off ~addr: phdr_addr 
			  ~filesz: phdr_size ~memsz: phdr_size);

      file_phdr_re_load = (mk_basic_phdr 
			     ~typ: pt_LOAD ~flags: [pf_R; pf_X] ~align: 0x1000l
			     ~off: re_off ~addr: re_addr 
			     ~filesz: re_size ~memsz: re_size);

      file_phdr_rw_load = (mk_basic_phdr 
			     ~typ: pt_LOAD ~flags: [pf_R; pf_W] ~align: 0x1000l
			     ~off: rw_off ~addr: rw_addr 
			     ~filesz: rw_file_size ~memsz: rw_mem_size);

      file_syms = symtab;
      file_fixups = fixups;

      add_data_sym = (fun name vma size ->
			Hashtbl.add symtab name (stt_OBJECT, datandx, vma, size));
      
      add_rodata_sym = (fun name vma size ->
			  Hashtbl.add symtab name (stt_OBJECT, rodatandx, vma, size));
      add_func_sym  = (fun name vma size ->
			 Hashtbl.add symtab name (stt_FUNC, textndx, vma, size));
      
      add_vma_fixup = (fun off name -> Stack.push (off, FIXUP_VMA, name) fixups);
      add_size_fixup = (fun off name -> Stack.push (off, FIXUP_SIZE, name) fixups);
    }
;;

let write_basic_x86_elf_file f =
  let write_section_names _ =
    let pos0 = Int32.to_int f.file_shdr_shstrtab.sh_offset in
    let setname pos (sh, name) = 
      (sh.sh_name <- Int32.of_int (pos - pos0);
       write_zstring f.file_buf pos (pos + shentsize) name)
    in

    List.fold_left 
      setname pos0
      [
       (f.file_shdr_null,     "");
       (f.file_shdr_text,     ".text");
       (f.file_shdr_rodata,   ".rodata");
       (f.file_shdr_data,     ".data");
       (f.file_shdr_bss,      ".bss");
       (f.file_shdr_shstrtab, ".shstrtab");
       (f.file_shdr_symtab,   ".symtab");
       (f.file_shdr_strtab,   ".strtab")
     ]
  in 
  let ws pos sh = write_section_header_at f.file_buf pos (pos + shentsize) sh in
  let wp pos ph = write_program_header_at f.file_buf pos (pos + phentsize) ph in

  (* Write the section names and elf header. *)
  let _ = write_section_names () in 
  let _ = write_elf_header_at f.file_buf 0 f.file_ehdr.e_ehsize f.file_ehdr in  

  (* Write the section headers. *)
  let _ = List.fold_left ws (Int32.to_int f.file_ehdr.e_shoff)
      [
       f.file_shdr_null;
       f.file_shdr_text;
       f.file_shdr_rodata;
       f.file_shdr_data;
       f.file_shdr_bss;
       f.file_shdr_shstrtab;
       f.file_shdr_symtab;
       f.file_shdr_strtab
     ] 
  in

  (* Write the program headers. *)
  let _ = List.fold_left wp (Int32.to_int f.file_ehdr.e_phoff)
    [
      f.file_phdr_phdr; 
      f.file_phdr_re_load;
      f.file_phdr_rw_load
    ]
  in

  (* Write the symbol table. *)
  let _ = 
    let sym_pos = ref (Int32.to_int f.file_shdr_symtab.sh_offset) in
    let str_pos0 = Int32.to_int f.file_shdr_strtab.sh_offset in 
    let str_pos = ref str_pos0 in 
    let sym_lim = (!sym_pos) + (Int32.to_int f.file_shdr_symtab.sh_size) in
    let str_lim = (!str_pos) + (Int32.to_int f.file_shdr_strtab.sh_size) in

    let wsym s = (sym_pos := write_sym_at f.file_buf (!sym_pos) sym_lim s) in
    let wstr s = (str_pos := write_zstring f.file_buf (!str_pos) str_lim s) in
    let 
	null_sym = (mk_basic_sym 
		      ~name: (Int32.of_int 0) 
		      ~value: (Int32.of_int 0) 
		      ~size: (Int32.of_int 0)
		      ~ty: 0 ~bind: 0 ~shndx: shn_UNDEF)
    in
    let wsyms = 
      Hashtbl.iter 
	(fun name (ty, section, vma, size) -> 
	   let 
	       sym = (mk_basic_sym 
			~name: (Int32.of_int ((!str_pos) - str_pos0))
			~value: (Int32.of_int vma) 
			~size: (Int32.of_int size)
			~ty: ty
			~bind: stb_GLOBAL 
			~shndx: section)
	   in
	     wsym sym;
	     wstr name)
    in
      wstr "";
      wsym null_sym;
      wsyms f.file_syms;
      while (!sym_pos) + symsize < sym_lim do
	wsym null_sym;
      done
  in

  (* Apply the fixups. *)
  let _ = 
    let apply_fixup (off, ty, name) = 
      let (_,_,vma,size) = Hashtbl.find f.file_syms name in
      let 
	  v = (match ty with 
		   FIXUP_VMA -> vma
		 | FIXUP_SIZE -> size)
      in
      let _ = write_bytes f.file_buf off (off + 4) (int_to_u32_lsb0 v) in ()
    in	  
    Stack.iter apply_fixup f.file_fixups
  in
    ()
;;

(* Basic test *)

let test_asm _ = 
  let f = mk_basic_x86_elf_file "test.elf32" in
  let buf = f.file_buf in

  let text_shdr = f.file_shdr_text in
  let rodata_shdr = f.file_shdr_rodata in

  let text_off = (Int32.to_int text_shdr.sh_offset) in
  let text_lim = text_off + (Int32.to_int text_shdr.sh_size) in
  let text_pos = ref text_off in
  let text_size _ = (!text_pos - text_off) in
  let text_vma _ = (Int32.to_int text_shdr.sh_addr) + (text_size()) in

  let rodata_off = (Int32.to_int rodata_shdr.sh_offset) in
  let rodata_lim = rodata_off + (Int32.to_int rodata_shdr.sh_size) in
  let rodata_pos = ref rodata_off in
  let rodata_size _ = (!rodata_pos - rodata_off) in
  let rodata_vma _ = (Int32.to_int rodata_shdr.sh_addr) + (rodata_size()) in

  let append_ro_rawstring str = 
    rodata_pos := write_rawstring buf !rodata_pos rodata_lim str 
  in

  let append_rodata_sym f data sym = 
    let size = String.length data in
      f.add_rodata_sym sym (rodata_vma()) size;
      append_ro_rawstring data
  in

  let append_insns iss = (text_pos := write_bytes buf !text_pos text_lim iss) in
  let append_push_sizeof sym = 
    append_insns (push_imm32 0);
    f.add_size_fixup (!text_pos - 4) sym
  in
  let append_push_vmaof sym = 
    append_insns (push_imm32 0);
    f.add_vma_fixup (!text_pos - 4) sym
  in
    
  let literal_data = "Hello, world\n" in
  let literal_symbol = "lit1" in
  let main_vma = text_vma() in

    f.file_ehdr.e_entry <- f.file_shdr_text.sh_addr;
    
    append_push_sizeof literal_symbol;
    append_push_vmaof literal_symbol;
    append_insns (push_imm32 1); (* fd 1 *)
    append_insns op_SYS_WRITE;
    append_insns op_SYS_EXIT;

    f.add_func_sym "main" main_vma (text_size());
    append_rodata_sym f literal_data literal_symbol;

    write_basic_x86_elf_file f;
    Unix.close f.file_buf.buf_fd
;;
*)
