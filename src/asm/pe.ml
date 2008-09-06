(* 
   
   Module for writing Microsoft PE files

   Notes 
   -----

   Every image has a base address it's to be loaded at.

   "file pointer" = offset in file

   "VA" = address at runtime

   "RVA" = VA - base address

   If you write a non-RVA absolute address at any point you must put it
   in a rebasing list so the loader can adjust it when/if it has to load
   you at a different address.

   Almost all addresses in the file are RVAs. Worry about the VAs.

*)

open Asm
;;

(* 

   The default image base (VA) for an executable on Win32 is 0x400000.

   We use this too. RVAs are relative to this. RVA 0 = VA 0x400000.

   Alignments are also relatively standard and fixed for Win32/PE32:
   4k memory pages, 512 byte disk sectors.
   
   Since this is a stupid emitter, and we're not generating an awful
   lot of sections, we are not going to differentiate between these
   two kinds of alignment: we just align our sections to memory pages
   and sometimes waste most of them. Shucks.
   
*)

let pe_image_base = 0x400000L;;
let pe_file_alignment = 0x200;;
let pe_mem_alignment = 0x1000;;

let def_file_aligned f i = 
  ALIGN_FILE 
	(pe_file_alignment, 
	 SEQ [| 
	   DEF(f, 
		   SEQ [| i; 
				  ALIGN_FILE 
					(pe_file_alignment, MARK) |]) |] )
;;

let def_mem_aligned f i = 
  ALIGN_MEM 
	(pe_mem_alignment, 
	 SEQ [| 
	   DEF(f, 
		   SEQ [| i; 
				  ALIGN_MEM 
					(pe_mem_alignment, MARK) |]) |] )
;;

let def_aligned f i = 
  ALIGN_FILE 
	(pe_file_alignment, 
	 ALIGN_MEM 
	   (pe_mem_alignment, 
		SEQ [| 
		  DEF(f, 
			  SEQ [| i; 
					 ALIGN_FILE
					   ( pe_file_alignment, 
						 ALIGN_MEM 
						   (pe_mem_alignment, MARK)) |] )|]))
;;


(*
  
  At the beginning of a PE file there is an MS-DOS stub, 0x00 - 0x7F,
  that we just insert literally. It prints "This program must be run
  under Win32" and exits. Woo!
  
  Within it, at offset 0x3C, there is an encoded offset of the PE
  header we actually care about. So 0x3C - 0x3F are 0x00000100 (LE)
  which say "the PE header is actually at 0x100", a nice sensible spot
  for it. We pad the next 128 bytes out to 0x100 and start there for
  real.
  
  From then on in it's a sensible object file. Here's the MS-DOS bit.
*)

let pe_msdos_header_and_padding 
    : item = 
  SEQ [|
    BYTES 
      [|
		(* 00000000 *) 
		0x4d; 0x5a; 0x50; 0x00; 0x02; 0x00; 0x00; 0x00; 
		0x04; 0x00; 0x0f; 0x00; 0xff; 0xff; 0x00; 0x00;
		
		(* 00000010 *)
		0xb8; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 
		0x40; 0x00; 0x1a; 0x00; 0x00; 0x00; 0x00; 0x00;
		
		(* 00000020 *)
		0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 
		0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
		
		(* 00000030 *) 
		0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 
		0x00; 0x00; 0x00; 0x00; 0x00; 0x01; 0x00; 0x00;
        (* ^PE HDR offset^  *)
		
		(* 00000040 *)
		0xba; 0x10; 0x00; 0x0e; 0x1f; 0xb4; 0x09; 0xcd; 
		0x21; 0xb8; 0x01; 0x4c; 0xcd; 0x21; 0x90; 0x90;
		
		(* 00000050 *)
		0x54; 0x68; 0x69; 0x73; 0x20; 0x70; 0x72; 0x6f;  (* "This pro" *)
		0x67; 0x72; 0x61; 0x6d; 0x20; 0x6d; 0x75; 0x73;  (* "gram mus" *)
		
		(* 00000060 *)
		0x74; 0x20; 0x62; 0x65; 0x20; 0x72; 0x75; 0x6e;  (* "t be run" *)
		0x20; 0x75; 0x6e; 0x64; 0x65; 0x72; 0x20; 0x57;  (* " under W" *)
		
		(* 00000070 *)
		0x69; 0x6e; 0x33; 0x32; 0x0d; 0x0a; 0x24; 0x37;  (* "in32\r\n" *)
		0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
      |];
    PAD 0x80
  |]
;;
   
(*
  A work of art, is it not? Take a moment to appreciate the madness.

  All done? Ok, now on to the PE header proper.

  PE headers are just COFF headers with a little preamble.
*)

type pe_machine = 
    (* Maybe support more later. *)
    IMAGE_FILE_MACHINE_AMD64
  | IMAGE_FILE_MACHINE_I386
;;


let pe_timestamp _ = 
  Int64.of_float (Unix.gettimeofday()) 
;;


type pe_characteristics = 
    (* Maybe support more later. *)
	IMAGE_FILE_RELOCS_STRIPPED
  | IMAGE_FILE_EXECUTABLE_IMAGE
  | IMAGE_FILE_LINE_NUMS_STRIPPED
  | IMAGE_FILE_LOCAL_SYMS_STRIPPED
  | IMAGE_FILE_32BIT_MACHINE
  | IMAGE_FILE_DEBUG_STRIPPED
  | IMAGE_FILE_DLL
;;


let fold_flags (f:'a -> int64) (flags:'a list) : int64 = 
  List.fold_left (Int64.logor) 0x0L (List.map f flags)
;;


let pe_header 
    ~(machine:pe_machine)
    ~(pointer_to_symbol_table:int64)
    ~(number_of_sections:int64)
    ~(number_of_symbols:int64)
    ~(loader_hdr_fixup:fixup) 
    ~(characteristics:pe_characteristics list)
    : item =
  ALIGN_FILE 
	(8, 
	 SEQ [|
	   STRING "PE\x00\x00";
	   WORD16 (IMM (match machine with 
						IMAGE_FILE_MACHINE_AMD64 -> 0x8664L
					  | IMAGE_FILE_MACHINE_I386 -> 0x014cL));
	   WORD16 (IMM number_of_sections);
	   WORD32 (IMM (pe_timestamp()));
	   WORD32 (IMM pointer_to_symbol_table);
	   WORD32 (IMM number_of_symbols);
	   WORD16 (F_SZ loader_hdr_fixup);
	   WORD16 (IMM (fold_flags 
					  (fun c -> match c with 
						   IMAGE_FILE_RELOCS_STRIPPED -> 0x1L
						 | IMAGE_FILE_EXECUTABLE_IMAGE -> 0x2L
						 | IMAGE_FILE_LINE_NUMS_STRIPPED -> 0x4L
						 | IMAGE_FILE_LOCAL_SYMS_STRIPPED -> 0x8L
						 | IMAGE_FILE_32BIT_MACHINE -> 0x100L
						 | IMAGE_FILE_DEBUG_STRIPPED -> 0x200L
						 | IMAGE_FILE_DLL -> 0x2000L)
					  characteristics))
	 |])
;;

(* 

   After the PE header comes an "optional" header for the loader. In
   our case this is hardly optional since we are producing a file for
   the loader.

*)

type pe_subsystem = 
    (* Maybe support more later. *)
    IMAGE_SUBSYSTEM_WINDOWS_GUI
  | IMAGE_SUBSYSTEM_WINDOWS_CUI
;;

let pe_loader_header 
    ~(text_fixup:fixup)
    ~(init_data_fixup:fixup)
    ~(size_of_uninit_data:int64)
    ~(entry_point_fixup:fixup)
    ~(base_of_code:int64)
    ~(base_of_data:int64)
    ~(image_fixup:fixup)
    ~(all_hdrs_fixup:fixup)
    ~(subsys:pe_subsystem)
	~(loader_hdr_fixup:fixup)
	~(import_dir_fixup:fixup)
    : item =
  DEF 
	(loader_hdr_fixup, 
	 SEQ [|
       WORD16 (IMM 0x10bL);                 (* COFF magic tag for PE32.  *)
	   (* Snagged *)
       WORD8 (IMM 0x2L);                    (* Linker major version.     *)
       WORD8 (IMM 0x38L);                   (* Linker minor version.     *)
	   
       WORD32 (F_SZ text_fixup);            (* "size of code"            *)
	   WORD32 (F_SZ init_data_fixup);       (* "size of all init data"   *)
       WORD32 (IMM size_of_uninit_data);
       WORD32 (M_POS entry_point_fixup);    (* "address of entry point"  *)
       WORD32 (M_POS text_fixup);           (* "base of code"            *)
       WORD32 (M_POS init_data_fixup);      (* "base of data"            *)
       WORD32 (IMM pe_image_base);
       WORD32 (IMM (Int64.of_int 
					  pe_mem_alignment));
       WORD32 (IMM (Int64.of_int 
					  pe_file_alignment));

       WORD16 (IMM 4L);                     (* Major OS version: NT4.     *)
       WORD16 (IMM 0L);                     (* Minor OS version.          *)
       WORD16 (IMM 1L);                     (* Major image version.       *)
       WORD16 (IMM 0L);                     (* Minor image version.       *)
       WORD16 (IMM 4L);                     (* Major subsystem version.   *)
       WORD16 (IMM 0L);                     (* Minor subsystem version.   *)
	   
       WORD32 (IMM 0L);                     (* Reserved.                  *)

       WORD32 (M_SZ image_fixup);
       WORD32 (M_SZ all_hdrs_fixup);

       WORD32 (IMM 0L);                     (* Checksum, but OK if zero.  *)
       WORD16 (IMM (match subsys with
						IMAGE_SUBSYSTEM_WINDOWS_GUI -> 2L
					  | IMAGE_SUBSYSTEM_WINDOWS_CUI -> 3L));

       WORD16 (IMM 0L);                     (* DLL characteristics.       *)

       WORD32 (IMM 0x100000L);              (* Size of stack reserve.     *)
       WORD32 (IMM 0x4000L);                (* Size of stack commit.      *)

       WORD32 (IMM 0x100000L);              (* Size of heap reserve.      *)
       WORD32 (IMM 0x1000L);                (* Size of heap commit.       *)

       WORD32 (IMM 0L);                     (* Reserved.                  *)    
       WORD32 (IMM 16L);                    (* Number of dir references.  *)

       (* Begin directories, variable part of hdr.        *)

       (* 

		  Standard PE files have ~10 directories referenced from
		  here. We only fill in one of them -- the import directory --
		  because we don't care about the others. We leave the rest as
		  zero in case someone is looking for them. This may be
		  superfluous or wrong.

       *)
       
       
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Export dir.        *) 

       WORD32 (M_POS import_dir_fixup);
       WORD32 (M_SZ import_dir_fixup);

       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Resource dir.      *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Exception dir.     *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Security dir.      *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Base reloc dir.    *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Debug dir.         *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Image desc dir.    *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Mach spec dir.     *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* TLS dir.           *) 

       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Load config.       *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Bound import.      *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* IAT                *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* Delay import.      *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* COM descriptor     *) 
       WORD32 (IMM 0L); WORD32 (IMM 0L);    (* ????????           *) 
	 |])
    
;;


type pe_section_id = 
    (* Maybe support more later. *)
    SECTION_ID_TEXT
  | SECTION_ID_DATA
  | SECTION_ID_RDATA
  | SECTION_ID_BSS
  | SECTION_ID_IMPORTS
;;

type pe_section_characteristics = 
    (* Maybe support more later. *)
	IMAGE_SCN_CNT_CODE
  | IMAGE_SCN_CNT_INITIALIZED_DATA
  | IMAGE_SCN_CNT_UNINITIALIZED_DATA
  | IMAGE_SCN_MEM_SHARED
  | IMAGE_SCN_MEM_EXECUTE
  | IMAGE_SCN_MEM_READ
  | IMAGE_SCN_MEM_WRITE

let pe_section_header
    ~(id:pe_section_id)
    ~(hdr_fixup:fixup)
    : item =   
  let
      characteristics = 
    match id with 
		SECTION_ID_TEXT -> [ IMAGE_SCN_CNT_CODE;
							 IMAGE_SCN_MEM_READ;
							 IMAGE_SCN_MEM_EXECUTE ]
      | SECTION_ID_DATA -> [ IMAGE_SCN_CNT_INITIALIZED_DATA;
							 IMAGE_SCN_MEM_READ;
							 IMAGE_SCN_MEM_WRITE ]
      | SECTION_ID_RDATA -> [ IMAGE_SCN_CNT_INITIALIZED_DATA;
							  IMAGE_SCN_MEM_READ ]
      | SECTION_ID_BSS -> [ IMAGE_SCN_CNT_UNINITIALIZED_DATA;
							IMAGE_SCN_MEM_READ;
							IMAGE_SCN_MEM_WRITE ]
      | SECTION_ID_IMPORTS -> [ IMAGE_SCN_CNT_INITIALIZED_DATA;
								IMAGE_SCN_MEM_READ;
								IMAGE_SCN_MEM_WRITE ]
  in
    SEQ [|      
      STRING (match id with 
				  SECTION_ID_TEXT -> ".text\x00\x00\x00"
				| SECTION_ID_DATA -> ".data\x00\x00\x00"
				| SECTION_ID_RDATA -> ".rdata\x00\x00"
				| SECTION_ID_BSS -> ".bss\x00\x00\x00\x00"
				| SECTION_ID_IMPORTS -> ".idata\x00\x00");

	  (* The next two pairs are only supposed to be different if the 
		 file and section alignments differ. This is a stupid emitter
		 so they're not, no problem. *)

	  WORD32 (M_SZ hdr_fixup);  (* "Virtual size"    *)
	  WORD32 (M_POS hdr_fixup); (* "Virtual address" *)

	  WORD32 (F_SZ hdr_fixup);  (* "Size of raw data"    *)
	  WORD32 (F_POS hdr_fixup); (* "Pointer to raw data" *)
      
      WORD32 (IMM 0L);      (* Reserved. *)
      WORD32 (IMM 0L);      (* Reserved. *)
      WORD32 (IMM 0L);      (* Reserved. *)
      
      WORD32 (IMM (fold_flags 
					 (fun c -> match c with 
						  IMAGE_SCN_CNT_CODE -> 0x20L
						| IMAGE_SCN_CNT_INITIALIZED_DATA -> 0x40L
						| IMAGE_SCN_CNT_UNINITIALIZED_DATA -> 0x80L
						| IMAGE_SCN_MEM_SHARED -> 0x10000000L
						| IMAGE_SCN_MEM_EXECUTE -> 0x20000000L
						| IMAGE_SCN_MEM_READ -> 0x40000000L
						| IMAGE_SCN_MEM_WRITE -> 0x80000000L)
					 characteristics))
    |]
;;


(* 

   "Thunk" is a misnomer here; the thunk RVA is the address of a word
   that the loader will store an address into. The stored address is
   the address of the imported object.
   
   So if the imported object is X, and the thunk slot is Y, the loader
   is doing "Y = &X" and returning &Y as the thunk RVA. To load datum X
   after the imports are resolved, given the thunk RVA R, you load
   **R.
   
*)

type pe_import = 
    { 
      pe_import_name_fixup: fixup;
      pe_import_name: string;
	  pe_import_address_fixup: fixup;
    }

type pe_import_dll_entry = 
    { 
      pe_import_dll_name_fixup: fixup;
      pe_import_dll_name: string;
      pe_import_dll_ILT_fixup: fixup; 
      pe_import_dll_IAT_fixup: fixup; 
      pe_import_dll_imports: pe_import array;
    }

  (* 

     The import section .idata has a mostly self-contained table
     structure. You feed it a list of DLL entries, each of which names
     a DLL and lists symbols in the DLL to import.

     For each named symbol, a 4-byte slot will be reserved in an
     "import lookup table" (ILT, also in this section). The slot is
     a pointer to a string in this section giving the name.

	 Immediately *after* the ILT, there is an "import address table"
	 (IAT), which is initially identical to the ILT. The loader
	 replaces the entries in the IAT slots with the imported pointers
	 at runtime.

     A central directory at the start of the section lists all the the
     import thunk tables. Each entry in the import directory is 20 bytes
     (5 words) but only the last 2 are used: the second last is a pointer
     to the string name of the DLL in question (string also in this
     section) and the last is a pointer to the import thunk table itself
     (also in this section).

     Curiously, of the 5 documents I've consulted on the nature of the
     first 3 fields, I find a variety of interpretations.

  *)

let pe_import_section
	~(section_fixup:fixup)
    ~(dlls:pe_import_dll_entry array) 
    : item =
  
  let form_dir_entry
      (entry:pe_import_dll_entry)
      : item =
	SEQ [| 	  
	  (* Note: documented opinions vary greatly about whether the
	     first, last, or both of the slots in one of these rows points
	     to the RVA of the name/hint used to look the import up. This
	     table format is a mess!  *)
	  WORD32 (M_POS entry.pe_import_dll_ILT_fixup);    (* Import lookup table. *)
	  WORD32 (IMM 0L);                                 (* Timestamp, unused.   *)
	  WORD32 (IMM 0x0L);                               (* Forwarder chain, unused. *)
	  WORD32 (M_POS entry.pe_import_dll_name_fixup);
	  WORD32 (M_POS entry.pe_import_dll_IAT_fixup);    (* Import address table.*)
	|]
  in

  let form_ILT_slot
      (import:pe_import)
      : item = 
	(WORD32 (M_POS import.pe_import_name_fixup))
  in

  let form_IAT_slot
      (import:pe_import)
      : item = 
	(DEF (import.pe_import_address_fixup, (WORD32 (M_POS import.pe_import_name_fixup))))
  in
    
  let form_tables_for_dll
	  (dll:pe_import_dll_entry)
      : item = 
	let terminator = WORD32 (IMM 0L) in
    let ilt = 
	  SEQ [| 
		SEQ (Array.map form_ILT_slot dll.pe_import_dll_imports);
		terminator 
	  |]
	in
    let iat = 
	  SEQ [| 
		SEQ (Array.map form_IAT_slot dll.pe_import_dll_imports);
		terminator 
	  |]
	in
      if Array.length dll.pe_import_dll_imports < 1
      then failwith "empty imports"
      else 
		SEQ [|
		  DEF (dll.pe_import_dll_ILT_fixup, ilt);
		  DEF (dll.pe_import_dll_IAT_fixup, iat) 
		|]
			
  in
	
  let form_import_string
      (import:pe_import) 
	  : item = 
	DEF 
	  (import.pe_import_name_fixup,
	   SEQ [| 
		 (* import string entries begin with a 2-byte "hint", but we just
			set it to zero.  *)
		 (WORD16 (IMM 0L));
		 ZSTRING import.pe_import_name;
		 (if String.length import.pe_import_name mod 2 == 0
		  then PAD 1
		  else PAD 0)
	   |])
  in
	
  let form_dir_entry_string
      (dll:pe_import_dll_entry)
      : item =
	DEF 
	  (dll.pe_import_dll_name_fixup,
	   SEQ [| ZSTRING dll.pe_import_dll_name;
			  (if String.length dll.pe_import_dll_name mod 2 == 0
			   then PAD 1
			   else PAD 0);
			  SEQ (Array.map form_import_string dll.pe_import_dll_imports) |])
  in
	
  let dir = SEQ (Array.map form_dir_entry dlls) in    
  let dir_terminator = PAD 20 in
  let tables = SEQ (Array.map form_tables_for_dll dlls) in
  let strings = SEQ (Array.map form_dir_entry_string dlls)
  in
	def_aligned 
	  section_fixup
	  (SEQ 
		 [| 
		   dir; 
		   dir_terminator;
		   tables; 
		   strings 
		 |])
      
;;

let pe_text_section
	~(exit_fn_fixup:fixup)
	~(text_fixup:fixup)
    : item =
  let
	  emit = Il.new_emitter 5 	
  in
  let 
	  exit_fn_imm = Il.Imm (ADD ((IMM pe_image_base),
								 (M_POS exit_fn_fixup)))
  in
	Il.emit_triple emit None Il.MOV (Il.HWreg 2) (Il.Deref (exit_fn_imm, 0L));
	Il.emit_triple emit None Il.CCALL (Il.HWreg 2) (Il.Imm (IMM 7L));
	def_aligned
	  text_fixup
	  (SEQ (Array.map X86.select_insn emit.Il.emit_triples))
		
(*********************************************************************************)

let test_imports = 
  { 
    pe_import_dll_name_fixup = new_fixup "dll name";
    pe_import_dll_name = "KERNEL32.dll";
    pe_import_dll_ILT_fixup = new_fixup "dll ILT";
    pe_import_dll_IAT_fixup = new_fixup "dll IAT";
    pe_import_dll_imports = 
      [| 
		{ 
		  pe_import_name_fixup = new_fixup "import name";
		  pe_import_name = "ExitProcess";
		  pe_import_address_fixup = new_fixup "import address";
		} 
      |];
  }
;;

let testfile = 

  let all_hdrs_fixup = new_fixup "all headers" in
  let all_init_data_fixup = new_fixup "all initialized data" in
  let loader_hdr_fixup = new_fixup "loader header" in
  let import_dir_fixup = new_fixup "import directory" in
  let text_fixup = new_fixup "text section" in
  let bss_fixup = new_fixup "bss section" in
  let image_fixup = new_fixup "image fixup" in

  let header = (pe_header 
				  ~machine: IMAGE_FILE_MACHINE_I386
				  ~pointer_to_symbol_table: 0L
				  ~number_of_sections: 3L
				  ~number_of_symbols: 0L
				  ~loader_hdr_fixup: loader_hdr_fixup
				  ~characteristics:[IMAGE_FILE_EXECUTABLE_IMAGE;
									IMAGE_FILE_RELOCS_STRIPPED;
									IMAGE_FILE_LINE_NUMS_STRIPPED;
									IMAGE_FILE_LOCAL_SYMS_STRIPPED;
									IMAGE_FILE_32BIT_MACHINE;
									IMAGE_FILE_DEBUG_STRIPPED])
  in
  let loader_header = (pe_loader_header
						 ~text_fixup: text_fixup
						 ~init_data_fixup: all_init_data_fixup
						 ~size_of_uninit_data: 0L
						 ~entry_point_fixup: text_fixup
						 ~base_of_code: 0L
						 ~base_of_data: 0L
						 ~image_fixup: image_fixup
						 ~subsys: IMAGE_SUBSYSTEM_WINDOWS_CUI
						 ~all_hdrs_fixup: all_hdrs_fixup
						 ~loader_hdr_fixup: loader_hdr_fixup
						 ~import_dir_fixup: import_dir_fixup)
  in

  let text_section = (pe_text_section 
						~exit_fn_fixup: test_imports.pe_import_dll_imports.(0).pe_import_address_fixup
						~text_fixup: text_fixup)
  in
  let bss_section = def_aligned bss_fixup (BSS 0x10L)
  in
  let text_header = (pe_section_header 
					   ~id: SECTION_ID_TEXT
					   ~hdr_fixup: text_fixup)

  in
  let bss_header = (pe_section_header 
					   ~id: SECTION_ID_BSS
					   ~hdr_fixup: bss_fixup)
  in
  let import_section = (pe_import_section 
						  ~section_fixup: import_dir_fixup
						  ~dlls: [| test_imports |])
  in
  let import_header = (pe_section_header 
						 ~id: SECTION_ID_IMPORTS
						 ~hdr_fixup: import_dir_fixup)
  in
  let all_init_data = (def_aligned 
						 all_init_data_fixup 
						 (SEQ 
							[| 
							  import_section; 
							|]))
  in
  let all_headers = (def_file_aligned 
					   all_hdrs_fixup
					   (SEQ 
						  [| 
							pe_msdos_header_and_padding; 
							header;
							loader_header; 
							text_header;
							bss_header;
							import_header
						  |]))
  in 
  let all_items = (def_file_aligned image_fixup
					 (SEQ [| all_headers;
							 text_section;
							 bss_section;
							 all_init_data;
							 ALIGN_MEM (pe_mem_alignment, MARK) |]))
  in
  let buf = Buffer.create 16 in
  let out = open_out_bin "rust_out.exe" in
	resolve_fixups all_items;
	lower_item ~lsb0: true ~buf: buf ~it: all_items;
	Buffer.output_buffer out buf;
	flush out;
	close_out out
;;
