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

(* 

   The default image base (VA) for an executable on Win32 is 0x400000.

   We use this too. RVAs are relative to this. RVA 0 = VA 0x400000.

   Alignments are also relatively standard and fixed for Win32/PE32:
   4k memory pages, 512 byte disk sectors.

*)
let pe_image_base = 0x400000l;;
let pe_section_alignment = 0x1000l;;
let pe_file_alignment = 0x200l;;

(* 

   A fixup is an RVA we're going to learn about a little while after
   we want to place it. In such cases, we create fixup ref cell to 
   hold the eventual value and we run the item list we're creating
   through a simulated write pass that records RVAs along the way. 

   Technically this is more primitive than patching back into the
   buffer when we discover a location. But in ocaml we don't have
   mutable growable byte buffers. We have appendable byte buffers, and
   mutable integer arrays that don't grow and can't be shrunk down to
   bytes. For the same of convenience, we do the 2-pass write trick
   instead.

*)

type fixup32 = 
    int32 option ref
;;

type item = 
    PAD of int
  | BSS of int32      
  | BYTE of int
  | BYTES of int array
  | CHAR of char
  | STRING of string
  | ZSTRING of string
  | WORD16 of int
  | WORD32 of int32
  | WORD64 of int64
  | FIXUP32_USE of fixup32
  | FIXUP32_DEF of (fixup32 * item)
;;

let resolve_fixups 
    ~(rva0:int32)
    ~(items:item array)
    : unit =   
  let rva = ref rva0 in
  let rec resolve_item it = 
    match it with 	
      | PAD i -> rva := Int32.add (!rva) (Int32.of_int i)
      | BSS i -> rva := Int32.add (!rva) i
      | BYTE _ -> rva := Int32.add (!rva) 1l
      | BYTES ia -> rva := Int32.add (!rva) (Int32.of_int (Array.length ia))
      | CHAR _ -> rva := Int32.add (!rva) 1l
      | STRING s -> rva := Int32.add (!rva) (Int32.of_int (String.length s))
      | ZSTRING s -> rva := Int32.add (!rva) (Int32.of_int ((String.length s) + 1))
      | WORD16 _ -> rva := Int32.add (!rva) 2l
      | WORD32 _ -> rva := Int32.add (!rva) 4l
      | WORD64 _ -> rva := Int32.add (!rva) 8l
      | FIXUP32_USE f -> rva := Int32.add (!rva) 4l
      | FIXUP32_DEF (f,i) -> 
	  f := Some (!rva); 
	  resolve_item i
  in
    Array.iter resolve_item items 
;;  


let rec lower_item
    ~(lsb0:bool)
    ~(buf:Buffer.t)
    ~(it:item)
    : unit =
  let 
      byte (i:int) = Buffer.add_char buf (Char.chr (0xff land i)) 
  in
    match it with
	PAD c -> 
	  for i = 1 to c do
	    Buffer.add_char buf '\x00'
	  done

      | BSS _ -> ()	    
      | BYTE b -> byte b

      | BYTES bs -> 
	  Array.iter byte bs
	    
      | CHAR c -> 
	  Buffer.add_char buf c
	    
      | STRING s -> 
	  Buffer.add_string buf s
	    
      | ZSTRING s -> 
	  Buffer.add_string buf s;
	  byte 0
	    
      | WORD16 i ->
	  if lsb0 
	  then (byte i; byte (i lsr 8))
	  else (byte (i lsr 8); byte i)
	    
      | WORD32 i -> 
	  let mask = Int32.logand 0xffl in
	  let shift = Int32.shift_right_logical in  
	  let conv = Int32.to_int in
	    if lsb0
	    then 
	      for n = 0 to 3 do
		byte (conv (mask (shift i (8*n))))
	      done
	    else 
	      for n = 3 downto 0 do
		byte (conv (mask (shift i (8*n))))
	      done
	      
      | WORD64 i -> 
	  let mask = Int64.logand 0xffL in
	  let shift = Int64.shift_right_logical in  
	  let conv = Int64.to_int in
	    if lsb0
	    then 
	      for n = 0 to 7 do
		byte (conv (mask (shift i (8*n))))
	      done
	    else 
	      for n = 7 downto 0 do
		byte (conv (mask (shift i (8*n))))
	      done

      | FIXUP32_USE f ->
	  (match !f with 
	       None -> failwith "unresolved fixup" 
	     | Some i32 -> lower_item_2 lsb0 buf (WORD32 i32))
	    
      | FIXUP32_DEF (f, i) ->  
	  lower_item_2 lsb0 buf i
and
    (* Some odd recursion bug? Unifier gets sad without this indirection. *)
    lower_item_2 lsb0 buf i = lower_item lsb0 buf i
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
    : item array = 
  [|
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
  Int32.of_float (Unix.gettimeofday()) 
;;


type pe_characteristics = 
    (* Maybe support more later. *)
    IMAGE_FILE_EXECUTABLE_IMAGE
  | IMAGE_FILE_DLL
;;


let fold_flags16 (f:'a -> int) (flags:'a list) : int = 
  List.fold_left (fun a b -> a lor b) 0x0 (List.map f flags)
;;

let fold_flags32 (f:'a -> int32) (flags:'a list) : int32 = 
  List.fold_left (Int32.logor) 0x0l (List.map f flags)
;;


let pe_header 
    ~(machine:pe_machine)
    ~(pointer_to_symbol_table:int32)
    ~(number_of_sections:int)
    ~(number_of_symbols:int32)
    ~(size_of_optional_header:int) 
    ~(characteristics:pe_characteristics list)
    : item array =
  [|
    STRING "PE\x00\x00";
    WORD16 (match machine with 
		IMAGE_FILE_MACHINE_AMD64 -> 0x8664
	      | IMAGE_FILE_MACHINE_I386 -> 0x014c);
    WORD16 number_of_sections;
    WORD32 (pe_timestamp());
    WORD32 pointer_to_symbol_table;
    WORD32 number_of_symbols;
    WORD16 size_of_optional_header;
    WORD16 (fold_flags16 
	      (fun c -> match c with 
		   IMAGE_FILE_EXECUTABLE_IMAGE -> 0x2
		 | IMAGE_FILE_DLL -> 0x2000)
	      characteristics)
  |]
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

let pe_optional_loader_header 
    ~(size_of_code:int32)
    ~(size_of_initialized_data:int32)
    ~(size_of_uninitialized_data:int32)
    ~(address_of_entry_point:int32)
    ~(base_of_code:int32)
    ~(base_of_data:int32)
    ~(size_of_image:int32)
    ~(size_of_headers:int32)
    ~(subsys:pe_subsystem)
    ~(import_directory_rva:int32)
    ~(import_directory_size:int32)
    : item array =
  [|
    WORD16 0x10b; (* COFF "magic" tag for PE32. *)
    BYTE 1;       (* Linker major version.      *)
    BYTE 1;       (* Linker minor version.      *)

    WORD32 size_of_code;
    WORD32 size_of_initialized_data;
    WORD32 size_of_uninitialized_data;
    WORD32 address_of_entry_point;
    WORD32 base_of_code;
    WORD32 base_of_data;
    WORD32 pe_image_base;
    WORD32 pe_section_alignment;
    WORD32 pe_file_alignment;

    WORD16 4;     (* Major OS version: NT4.     *)
    WORD16 0;     (* Minor OS version.          *)
    WORD16 4;     (* Major subsystem version.   *)
    WORD16 0;     (* Minor subsystem version.   *)

    WORD32 0l;    (* Reserved.                  *)

    WORD32 size_of_image;
    WORD32 size_of_headers;

    WORD32 0l;    (* Checksum, but OK if zero.  *)
    WORD32 (match subsys with
		IMAGE_SUBSYSTEM_WINDOWS_GUI -> 2l
	      | IMAGE_SUBSYSTEM_WINDOWS_CUI -> 3l);

    WORD16 0;     (* DLL characteristics.       *)

    WORD32 0x100000l; (* Size of stack reserve. *)
    WORD32 0x4000l;   (* Size of stack commit.  *)

    WORD32 0x100000l; (* Size of heap reserve.  *)
    WORD32 0x1000l;   (* Size of heap commit.   *)

    WORD32 0l;    (* Reserved.                  *)    
    WORD32 10l;   (* Number of dir references.  *)

    (* Begin directories, variable part of hdr. *)

    (* 

       Standard PE files have ~10 directories referenced from here. We
       only fill in one of them -- the import directory -- because we
       don't care about the others. We leave the rest as zero in case
       someone is looking for them. This may be superfluous or wrong.

    *)
    
    
    WORD32 0l; WORD32 0l; (* Export dir.        *) 

    WORD32 import_directory_rva;
    WORD32 import_directory_size;    

    WORD32 0l; WORD32 0l; (* Resource dir.      *) 
    WORD32 0l; WORD32 0l; (* Exception dir.     *) 
    WORD32 0l; WORD32 0l; (* Security dir.      *) 
    WORD32 0l; WORD32 0l; (* Base reloc dir.    *) 
    WORD32 0l; WORD32 0l; (* Debug dir.         *) 
    WORD32 0l; WORD32 0l; (* Image desc dir.    *) 
    WORD32 0l; WORD32 0l; (* Mach spec dir.     *) 
    WORD32 0l; WORD32 0l; (* TLS dir.           *) 
  |]
    
;;


type pe_section_id = 
    (* Maybe support more later. *)
    SECTION_ID_CODE
  | SECTION_ID_DATA
  | SECTION_ID_BSS
  | SECTION_ID_IMPORTS
;;

type pe_section_characteristics = 
    (* Maybe support more later. *)
    SECTION_IS_TEXT
  | SECTION_IS_DATA
  | SECTION_IS_BSS
  | SECTION_PERMIT_READ
  | SECTION_PERMIT_WRITE
  | SECTION_PERMIT_EXEC

let pe_section_header
    ~(id:pe_section_id)
    ~(virtual_size:int32)
    ~(virtual_address:int32)    
    ~(size_of_raw_data:int32)    (* File size. *)
    ~(pointer_to_raw_data:int32) (* File ptr.  *)
    : item array =   
  let
      characteristics = 
    match id with 
	SECTION_ID_CODE -> [ SECTION_IS_TEXT; 
			     SECTION_PERMIT_READ;
			     SECTION_PERMIT_EXEC ]
      | SECTION_ID_DATA -> [ SECTION_IS_DATA;
			     SECTION_PERMIT_READ;
			     SECTION_PERMIT_WRITE ]
      | SECTION_ID_BSS -> [ SECTION_IS_BSS;
			    SECTION_PERMIT_READ;
			    SECTION_PERMIT_WRITE ]
      | SECTION_ID_IMPORTS -> [ SECTION_IS_DATA;
				SECTION_PERMIT_READ;
				SECTION_PERMIT_WRITE ]
  in
    
    [|      
      STRING (match id with 
		  SECTION_ID_CODE -> "CODE\x00\x00\x00\x00"
		| SECTION_ID_DATA -> "DATA\x00\x00\x00\x00"
		| SECTION_ID_BSS -> "BSS\x00\x00\x00\x00\x00"
		| SECTION_ID_IMPORTS -> ".idata\x00\x00");
      WORD32 virtual_size;
      WORD32 virtual_address;
      WORD32 size_of_raw_data;
      WORD32 pointer_to_raw_data;
      
      WORD32 0l;      (* Reserved. *)
      WORD32 0l;      (* Reserved. *)
      WORD32 0l;      (* Reserved. *)
      
      WORD32 (fold_flags32 
		(fun c -> match c with 
		     SECTION_IS_TEXT -> 0x20l
		   | SECTION_IS_DATA -> 0x40l
		   | SECTION_IS_BSS -> 0x80l
		   | SECTION_PERMIT_READ -> 0x40000000l
		   | SECTION_PERMIT_WRITE -> 0x80000000l
		   | SECTION_PERMIT_EXEC -> 0x20000000l)
		characteristics)
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

type pe_import_thunk = 
    { 
      pe_import_thunk_name_rva: fixup32;
      pe_import_thunk_name: string;
    }

type pe_import_dll_entry = 
    { 
      pe_import_dll_name_rva: fixup32;
      pe_import_dll_name: string;
      pe_import_dll_thunks_rva: fixup32; 
      pe_import_dll_thunks: pe_import_thunk array;
    }

  (* 

     The import section .idata has a mostly self-contained table
     structure. You feed it a list of DLL entries, each of which names
     a DLL and lists symbols in the DLL to import.

     For each named symbol, a 4-byte slot will be reserved in an "import
     thunk table" (also in this section). The slot is initially a pointer
     to a string in this section giving the name. Later, the loader will
     overwrite the name pointer with the importeded pointer.

     A central directory at the start of the section lists all the the
     import thunk tables. Each entry in the import directory is 20 bytes
     (5 words) but only the last 2 are used: the second last is a pointer
     to the string name of the DLL in question (string also in this
     section) and the last is a pointer to the import thunk table itself
     (also in this section).

     Curiously, of the 5 documents I've consulted on the nature of the
     first 3 fields, I find a variety of interpretations, none of which
     seem to matter in practice: in real programs, they are always set to
     zero and nothing bad happens. So we do that here too.

  *)

let pe_import_section
    (imports:pe_import_dll_entry array) 
    : item array =
  
  let fuse x = Array.concat (Array.to_list x) in
  
  let form_dir_entry
      (entry:pe_import_dll_entry)
      : item array =
    [| 
      WORD32 0l; (* Opinions vary! Unused anyway.   *)
      WORD32 0l; (* Timestamp, unused.              *)
      WORD32 0l; (* Forwarder chain, unused.        *)
      FIXUP32_USE entry.pe_import_dll_name_rva;
      FIXUP32_USE entry.pe_import_dll_thunks_rva;
    |]
  in

  let form_thunk
      (thunk:pe_import_thunk)
      : item = 
    (FIXUP32_USE thunk.pe_import_thunk_name_rva)
  in
      
  let form_thunks
      ~(table_rva:fixup32)
      ~(thunks:pe_import_thunk array)
      : item array = 
    let table = Array.map form_thunk thunks in
      if Array.length table < 1
      then failwith "import address table is empty!"
      else (table.(0) <- FIXUP32_DEF (table_rva, table.(0)); table)
  in

  let form_thunk_string
      (thunk:pe_import_thunk) = 
    [| 
      (* 
	 Thunk string entries begin with a 2-byte "hint", 
	 but we just set it to zero. 
       *)
      FIXUP32_DEF (thunk.pe_import_thunk_name_rva, (WORD16 0));
      ZSTRING thunk.pe_import_thunk_name;
      (if String.length thunk.pe_import_thunk_name mod 2 == 0
       then PAD 1
       else PAD 0)
    |]
  in

  let form_dir_entry_string
      (entry:pe_import_dll_entry)
      : item array =
    Array.concat 
      [ 
	[| 
	  FIXUP32_DEF (entry.pe_import_dll_name_rva, 
		       ZSTRING (entry.pe_import_dll_name));
	  (if String.length entry.pe_import_dll_name mod 2 == 0
	   then PAD 1
	   else PAD 0) 
	|];
	fuse (Array.map form_thunk_string entry.pe_import_dll_thunks)
      ]
  in

  let dir = fuse (Array.map form_dir_entry imports) in    
  let thunks = 
    fuse (Array.map 
	    (fun x -> (form_thunks 
			 x.pe_import_dll_thunks_rva 
			 x.pe_import_dll_thunks))
	    imports)
  in
  let strings = fuse (Array.map form_dir_entry_string imports)
  in
    Array.concat [ dir; thunks; strings ]
    
;;
 


(*********************************************************************************)

let test_imports = 
  { 
    pe_import_dll_name_rva = ref None;
    pe_import_dll_name = "kernel32.dll";
    pe_import_dll_thunks_rva = ref None;
    pe_import_dll_thunks = 
      [| 
	{ 
	  pe_import_thunk_name_rva = ref None;
	  pe_import_thunk_name = "ExitProcess";
	} 
      |];
  }
;;

let testfile = 
  let import_section = pe_import_section test_imports in
  let optional_loader_header = (pe_optional_loader_header
				  ~size_of_code: 0l
				  ~size_of_initialized_data:int32
				  ~size_of_uninitialized_data:int32
				  ~address_of_entry_point:int32
				  ~base_of_code: 0l
				  ~base_of_data: 0l
				  ~size_of_image: 0l
				  ~size_of_headers: 0l
				  ~subsys: IMAGE_SUBSYSTEM_WINDOWS_CUI
				  ~import_directory_rva:int32
				  ~import_directory_size:int32)

    
  Array.concat 
    [

      pe_import_section test_imports;
      
    ]
  
