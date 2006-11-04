(* This is a very simple bootstrap assembler/linker for x86 elf32    *)
(* linux ABI binaries, that should be enough to get us going.        *)

type arr = (int, 
	    Bigarray.int8_unsigned_elt, 
	    Bigarray.c_layout) 
      Bigarray.Array1.t
;;

type buf = { buf_fd: Unix.file_descr;
	     buf_arr: arr;
	     mutable buf_pos: int }
;;

let openbuf (fn:string) (sz:int) = 
  let fd = Unix.openfile fn [Unix.O_RDWR; Unix.O_CREAT] 0o700 in
  let alayout = Bigarray.c_layout in
  let akind = Bigarray.int8_unsigned in
  let shared = true in
  let a = Bigarray.Array1.map_file fd akind alayout shared sz in
  { buf_arr = a;
    buf_pos = 0;
    buf_fd = fd }
;;

let write_bytes (b:buf) (pos0:int) (bytes:int array) =
  let 
      write1 pos byte = (b.buf_arr.{pos} <- (byte land 0xff); 
			 pos + 1)
  in    
  Array.fold_left write1 pos0 bytes
;;

let write_rawstring (b:buf) (pos0:int) (str:string) =
  let len = (String.length str) in
  for i = 0 to len - 1 do
    b.buf_arr.{pos0 + i} <- ((Char.code str.[i]) land 0xff)
  done;
  pos0 + len
;;

let write_zstring (b:buf) (pos0:int) (str:string) =
  let len = (String.length str) in
  for i = 0 to len - 1 do
    b.buf_arr.{pos0 + i} <- ((Char.code str.[i]) land 0xff)
  done;
  b.buf_arr.{pos0 + len} <- 0;
  pos0 + len + 1
;;


let append_bytes (b:buf) (bytes:int array) =
  b.buf_pos <- (write_bytes b b.buf_pos bytes)

let append_rawstring (b:buf) (str:string) = 
  b.buf_pos <- (write_rawstring b b.buf_pos str)

let append_zstring (b:buf) (str:string) = 
  b.buf_pos <- (write_zstring b b.buf_pos str)

let ub i n = (i lsr (n * 8)) land 0xff ;;
let sb i n = (i asr (n * 8)) land 0xff ;;
let ubl i n = Int32.to_int (Int32.logand (Int32.shift_right_logical i (n * 8)) 0xffl) ;;
let ubL i n = Int64.to_int (Int64.logand (Int64.shift_right_logical i (n * 8)) 0xffL) ;;

let int_to_u8 i = [| ub i 0 |] ;;
let int_to_s8 i = [| sb i 0 |] ;;

let int_to_u16_lsb0 i = [| ub i 0; ub i 1 |] ;;
let int_to_u16_msb0 i = [| ub i 1; ub i 0 |] ;;
let int_to_s16_lsb0 i = [| sb i 0; sb i 1 |] ;;
let int_to_s16_msb0 i = [| sb i 1; sb i 0 |] ;;

let int_to_u32_lsb0 i = [| ub i 0; ub i 1; ub i 2; ub i 3 |] ;;
let int_to_u32_msb0 i = [| ub i 3; ub i 2; ub i 1; ub i 0 |] ;;
let int_to_s32_lsb0 i = [| sb i 0; sb i 1; sb i 2; sb i 3 |] ;;
let int_to_s32_msb0 i = [| sb i 3; sb i 2; sb i 1; sb i 0 |] ;;

let int32_lsb0 i = [| ubl i 0; ubl i 1; ubl i 2; ubl i 3 |] ;;
let int32_msb0 i = [| ubl i 3; ubl i 2; ubl i 1; ubl i 0 |] ;;

let int_to_u64_lsb0 i = [| ub i 0; ub i 1; ub i 2; ub i 3;
			   ub i 4; ub i 5; ub i 6; ub i 7 |] ;;
let int_to_u64_msb0 i = [| ub i 7; ub i 6; ub i 5; ub i 4;
			   ub i 3; ub i 2; ub i 1; ub i 0 |] ;;
let int_to_s64_lsb0 i = [| sb i 0; sb i 1; sb i 2; sb i 3;
			   sb i 4; sb i 5; sb i 6; sb i 7 |] ;;
let int_to_s64_msb0 i = [| sb i 7; sb i 6; sb i 5; sb i 4; 
			   sb i 3; sb i 2; sb i 1; sb i 0 |] ;;

let int64_lsb0 i = [| ubL i 0; ubL i 1; ubL i 2; ubL i 3;
		      ubL i 4; ubL i 5; ubL i 6; ubL i 7 |] ;;
let int64_msb0 i = [| ubL i 7; ubL i 6; ubL i 5; ubL i 4;
		      ubL i 3; ubL i 2; ubL i 1; ubL i 0 |] ;;


type int8 = int;;
type int16 = int;;

(* ELF stuff *)

type elf_header = 
    { 
      e_ident: int8 array;
      e_type: int16;
      e_machine: int16; 
      e_version: int32;
      mutable e_entry: int32;
      mutable e_phoff: int32;
      mutable e_shoff: int32;
      e_flags: int32;
      e_ehsize: int16;
      e_phentsize: int16;
      mutable e_phnum: int16; 
      e_shentsize: int16;
      mutable e_shnum: int16; 
      mutable e_shstrndx: int16;
    }


type section_header = 
    {
     mutable sh_name: int32;
     sh_type: int32;
     sh_flags: int32;
     mutable sh_addr: int32;
     mutable sh_offset: int32;
     mutable sh_size: int32;
     mutable sh_link: int32;
     mutable sh_info: int32;
     sh_addralign: int32;
     mutable sh_entsize: int32;
   }
;;

type program_header = 
    {
     p_type: int32;
     p_offset: int32;
     p_vaddr: int32;
     p_paddr: int32;
     p_filesz: int32;
     p_memsz: int32;
     p_flags: int32;
     p_align: int32;
   }

let write_elf_header_at (b:buf) (pos:int) (eh:elf_header) =
  let p = ref pos in 
  let wbs bs = (p := write_bytes b !p bs) in
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

let write_section_header_at (b:buf) (pos:int) (sh:section_header) =
  let p = ref pos in 
  let wbs bs = (p := write_bytes b !p bs) in
  let ww w = wbs (int32_lsb0 w) in
  List.iter ww 
    [ sh.sh_name; sh.sh_type; sh.sh_flags; sh.sh_addr; 
      sh.sh_offset; sh.sh_size; sh.sh_link; sh.sh_info; 
      sh.sh_addralign; sh.sh_entsize; ];
  !p
;;

let write_program_header_at (b:buf) (pos:int) (ph:program_header) =
  let p = ref pos in 
  let wbs bs = (p := write_bytes b !p bs) in
  let ww w = wbs (int32_lsb0 w) in
  List.iter ww 
    [ ph.p_type; ph.p_offset; ph.p_vaddr; ph.p_paddr; 
      ph.p_filesz; ph.p_memsz; ph.p_flags; ph.p_align ];
  !p
;;

let ehsize = 52;;
let phentsize = 32;;
let shentsize = 40;;

let mk_basic_x86_ehdr n_phdrs phdr_off n_shdrs shdr_off = 
  { 
    e_ident = [| 
    0x7f;             (* EI_MAG0 *)
   (Char.code 'E');   (* EI_MAG1 *)
   (Char.code 'L');   (* EI_MAG2 *)
   (Char.code 'F');   (* EI_MAG3 *)
   1;                 (* EI_CLASS = ELFCLASS32 *)
   1;                 (* EI_DATA = ELFDATA2LSB *)
   1;                 (* EI_VERSION = EV_CURRENT *)
   0;                 (* EI_PAD #7 *)
   0;                 (* EI_PAD #8 *)
   0;                 (* EI_PAD #9 *)
   0;                 (* EI_PAD #A *)
   0;                 (* EI_PAD #B *)
   0;                 (* EI_PAD #C *)
   0;                 (* EI_PAD #D *)
   0;                 (* EI_PAD #E *)
   0;                 (* EI_PAD #F *)
   |];
    e_type = 2;               (* ET_EXEC : Elf32_Half *)
    e_machine = 3;            (* EM_386  : Elf32_Half *)
    e_version = 1l;           (* EV_CURRENT : Elf32_Word *)
    e_entry = 0l;             (* : Elf32_Addr *)
    e_phoff = phdr_off;       (* : Elf32_Off *)
    e_shoff = shdr_off;       (* : Elf32_Off *)
    e_flags = 0l;             (* : Elf32_Word *)
    e_ehsize = ehsize;        (* : Elf32_Half *)
    e_phentsize = phentsize;  (* : Elf32_Half *)
    e_phnum = n_phdrs;        (* : Elf32_Half *)
    e_shentsize = shentsize;  (* : Elf32_Half *)
    e_shnum = n_shdrs;        (* : Elf32_Half *)
    e_shstrndx = n_shdrs - 1; (* : Elf32_Half *)
  }
;;

let sht_NULL = 0l;;
let sht_PROGBITS = 1l;;
let sht_SYMTAB = 2l;;
let sht_STRTAB = 3l;;
let sht_RELA = 4l;;
let sht_NOBITS = 8l;;

let shf_WRITE = 1l;;
let shf_ALLOC = 2l;;
let shf_EXECINSTR = 4l;;

let pt_NULL = 0l;;
let pt_LOAD = 1l;;
let pt_PHDR = 6l;;

let pf_X = 1l;;
let pf_W = 2l;;
let pf_R = 4l;;

let mk_basic_shdr ~typ ~align ~off ~addr ~sz ~flags = 
{
 sh_name = 0l;
 sh_type = typ;
 sh_flags = List.fold_left Int32.logor 0l flags;
 sh_addr = addr;
 sh_offset = off;
 sh_size = sz;
 sh_link = 0l;
 sh_info = 0l;
 sh_addralign = align;
 sh_entsize = 0l
}

let mk_basic_phdr ~typ ~align ~off ~addr ~sz ~flags =
  {
   p_type = typ;
   p_offset = off;
   p_vaddr = addr;
   p_paddr = addr;
   p_filesz = sz;
   p_memsz = sz;
   p_flags = List.fold_left Int32.logor 0l flags;
   p_align = align
 }

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
      
      file_phdr_phdr: program_header;
      file_phdr_re_load: program_header;
      file_phdr_rw_load: program_header
    }
;;

let mk_basic_x86_elf_file fn = 
  (* As this is just a boostrap system, we adopt an incredibly cheesy *)
  (* fixed-sizes-for-everything layout, and fault if they're overrun. *)
  (* The file is < 4mb long:                                          *)
  (*      16kb for eh and phdrs, before the main sections             *)
  (*      1mb for the .text section                                   *)
  (*      1mb for the .data section                                   *)
  (*      1mb for the .rodata section                                 *)
  (*      16kb for .shstrtab                                          *)
  (*      16kb for shdrs at end of file                               *)
  (*                                                                  *)
  (* Note that since all these are big round numbers, we don't do     *)
  (* any explicit alignment padding on the section boundaries. If     *)
  (* you were to improve this to be dense you would have to do so.    *)
  
  let load_base      = 0x0804_8000l in
  let eh_phdr_size   = 0x0000_4000l in
  let text_size      = 0x0010_0000l in
  let rodata_size    = 0x0010_0000l in
  let data_size      = 0x0010_0000l in
  let bss_size       = 0x0010_0000l in (* NOBITS, not "in" the file *)
  let shstrtab_size  = 0x0000_4000l in
  let shdr_size      = 0x0000_4000l in
  
  (* There are 6 official section headers in the file we're making:   *)
  (* section 0: <null section>                                        *)
  (* section 1: .text                                                 *)
  (* section 2: .rodata                                               *)
  (* section 3: .data                                                 *)
  (* section 4: .bss                                                  *)
  (* section 5: .shstrtab                                             *)

  let n_shdrs        = 6 in
  let text_off       = eh_phdr_size in
  let rodata_off     = Int32.add text_off text_size in
  let data_off       = Int32.add rodata_off rodata_size in
  let shstrtab_off   = Int32.add data_off data_size in
  (* NB: bss is NOBITS so it's normal to "overlap" with shstrtab *)
  let bss_off        = Int32.add data_off data_size in
  let shdr_off       = Int32.add shstrtab_off shstrtab_size in
  
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
  let rw_size        = Int32.add data_size bss_size in
  let rw_addr        = Int32.add load_base re_size in

  let write_section_names f =
    let pos0 = Int32.to_int f.file_shdr_shstrtab.sh_offset in
    let setname pos (sh, name) = 
      (sh.sh_name <- Int32.of_int (pos - pos0);
       write_zstring f.file_buf pos name)
    in

    List.fold_left 
      setname pos0
      [
       (f.file_shdr_null,     "");
       (f.file_shdr_text,     ".text");
       (f.file_shdr_rodata,   ".rodata");
       (f.file_shdr_data,     ".data");
       (f.file_shdr_bss,      ".bss");
       (f.file_shdr_shstrtab, ".shstrtab")
     ]
  in 
  let f = 
    { 
      file_buf = openbuf fn (Int32.to_int file_size);
      file_ehdr = (mk_basic_x86_ehdr 
		     n_phdrs phdr_off 
		     n_shdrs shdr_off);
      
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
      
      file_phdr_phdr = (mk_basic_phdr 
			  ~typ: pt_PHDR ~flags: [pf_R; pf_X] ~align: 4l
			  ~off: phdr_off ~addr: phdr_addr ~sz: phdr_size);

      file_phdr_re_load = (mk_basic_phdr 
			     ~typ: pt_LOAD ~flags: [pf_R; pf_X] ~align: 0x1000l
			     ~off: re_off ~addr: re_addr ~sz: re_size);

      file_phdr_rw_load = (mk_basic_phdr 
			     ~typ: pt_LOAD ~flags: [pf_R; pf_W] ~align: 0x1000l
			     ~off: rw_off ~addr: rw_addr ~sz: rw_size);
    }
  in
  let ws = write_section_header_at f.file_buf in
  let wp = write_program_header_at f.file_buf in
  let _ = write_section_names f in 
  let _ = write_elf_header_at f.file_buf 0 f.file_ehdr in  
  let _ = List.fold_left ws (Int32.to_int shdr_off)
      [
       f.file_shdr_null;
       f.file_shdr_text;
       f.file_shdr_rodata;
       f.file_shdr_data;
       f.file_shdr_bss;
       f.file_shdr_shstrtab
     ] 
  in
  let _ = List.fold_left wp (Int32.to_int phdr_off)
      [
       f.file_phdr_phdr; 
       f.file_phdr_re_load;
       f.file_phdr_rw_load
     ] 
  in
  f
;;

(*
let f = mk_basic_x86_elf_file "test.elf32" in
Unix.close f.file_buf.buf_fd
;;
*)


(* x86 instruction emitting... *)

let modrm m rm reg_or_subopcode = 
  (((m land 0b11) lsl 6) 
     lor 
   (rm land 0b111)) 
    lor 
  ((reg_or_subopcode land 0b111) lsl 3)
;;

let modrm_deref_EAX = modrm 0b00 0b000;;
let modrm_deref_ECX = modrm 0b00 0b001;;
let modrm_deref_EDX = modrm 0b00 0b010;;
let modrm_deref_EBX = modrm 0b00 0b011;;
let modrm_disp32    = modrm 0b00 0b101;;
let modrm_deref_ESI = modrm 0b00 0b110;;
let modrm_deref_EDI = modrm 0b00 0b111;;

let modrm_deref_EAX_plus_disp8 = modrm 0b01 0b000;;
let modrm_deref_ECX_plus_disp8 = modrm 0b01 0b001;;
let modrm_deref_EDX_plus_disp8 = modrm 0b01 0b010;;
let modrm_deref_EBX_plus_disp8 = modrm 0b01 0b011;;
let modrm_deref_EBP_plus_disp8 = modrm 0b01 0b101;;
let modrm_deref_ESI_plus_disp8 = modrm 0b01 0b110;;
let modrm_deref_EDI_plus_disp8 = modrm 0b01 0b111;;

let modrm_deref_EAX_plus_disp32 = modrm 0b01 0b000;;
let modrm_deref_ECX_plus_disp32 = modrm 0b01 0b001;;
let modrm_deref_EDX_plus_disp32 = modrm 0b01 0b010;;
let modrm_deref_EBX_plus_disp32 = modrm 0b01 0b011;;
let modrm_deref_EBP_plus_disp32 = modrm 0b01 0b101;;
let modrm_deref_ESI_plus_disp32 = modrm 0b01 0b110;;
let modrm_deref_EDI_plus_disp32 = modrm 0b01 0b111;;

let modrm_EAX = modrm 0b01 0b000;;
let modrm_ECX = modrm 0b01 0b001;;
let modrm_EDX = modrm 0b01 0b010;;
let modrm_EBX = modrm 0b01 0b011;;
let modrm_ESP = modrm 0b01 0b011;;
let modrm_EBP = modrm 0b01 0b101;;
let modrm_ESI = modrm 0b01 0b110;;
let modrm_EDI = modrm 0b01 0b111;;

let reg2_EAX = 0b000;;
let reg2_ECX = 0b001;;
let reg2_EDX = 0b010;;
let reg2_EBX = 0b011;;
let reg2_ESP = 0b100;;
let reg2_EBP = 0b101;;
let reg2_ESI = 0b110;;
let reg2_EDI = 0b111;;

let slash0 = 0;;
let slash1 = 1;;
let slash2 = 2;;
let slash3 = 3;;
let slash4 = 4;;
let slash5 = 5;;
let slash6 = 6;;
let slash7 = 7;;

let plusrd_EAX opc = opc + 0;;
let plusrd_ECX opc = opc + 1;;
let plusrd_EDX opc = opc + 2;;
let plusrd_EBX opc = opc + 3;;
let plusrd_ESP opc = opc + 4;;
let plusrd_EBP opc = opc + 5;;
let plusrd_ESI opc = opc + 6;;
let plusrd_EDI opc = opc + 7;;


(* We use a fixed, extremely suboptimal assignment of       *)
(* registers. Every binary operation has either this form:  *)
(*                                                          *)
(*  POP EBX                                                 *)
(*  POP EAX                                                 *)
(*  EAX <- EAX op EBX                                       *)
(*  PUSH EAX                                                *)
(*                                                          *)
(*  or this form:                                           *)
(*                                                          *)
(*  POP EBX                                                 *)
(*  POP EAX                                                 *)
(*  (EDX,EAX) <- EAX op EBX                                 *)
(*  PUSH EDX                                                *)
(*  PUSH EAX                                                *)
(*                                                          *)
(* some other registers are permanently assigned:           *)
(*                                                          *)
(*  EBP    frame base pointer                               *)
(*  ESP    stack pointer                                    *)
(*                                                          *)
(*  ESI    crate static data pointer                        *)
(*  EDI    process control bloc pointer                     *)

let disp32 = int_to_u32_lsb0;;

let push_local n = 
  if (n * 4) < 256
  then [| 0xff; modrm_deref_EBP_plus_disp8 slash6; (n * 4) |]
  else Array.append 
      [| 0xff; modrm_deref_EBP_plus_disp32 slash6 |] 
      (disp32 (n * 4))
;;

let pop_local n = 
  if (n * 4) < 256
  then [| 0x8f; modrm_deref_EBP_plus_disp8 slash0; (n * 4) |]
  else Array.append 
      [| 0x8f; modrm_deref_EBP_plus_disp32 slash0 |] 
      (disp32 (n * 4))
;;


let pop_EAX = plusrd_EAX 0x58;;
let pop_EBX = plusrd_EBX 0x58;;

let push_EAX = plusrd_EAX 0x50;;
let push_EDX = plusrd_EDX 0x50;;

(* For ops listed as "NN /r  OP rm/32 r32"  *)
let simple_binary_op op = 
  [| pop_EBX; 
     pop_EAX; 
     op; modrm_EAX reg2_EBX; 
     push_EAX |]
;;

let op_ADD = simple_binary_op 0x01;;
let op_SUB = simple_binary_op 0x29;;

let op_AND = simple_binary_op 0x21;;
let op_OR = simple_binary_op 0x21;;
let op_XOR = simple_binary_op 0x29;;

let op_NOT = [| pop_EAX; 
		0xf7; modrm_EAX slash2; 
		push_EAX |];;

let op_NEG = [| pop_EAX; 
		0xf7; modrm_EAX slash3; 
		push_EAX |];;

let op_MUL = [| pop_EBX;
		pop_EAX;
		0xf7; modrm_EBX slash4;
		push_EDX;
		push_EAX |];;

let op_DIV = [| pop_EBX;
		pop_EAX;
		0xf7; modrm_EBX slash6;
		push_EDX;
		push_EAX |];;
		
let op_POP = [| pop_EAX; |];;
