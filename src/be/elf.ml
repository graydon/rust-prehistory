(* This is a very simple bootstrap assembler/linker for x86 elf32    *)
(* linux ABI binaries, that should be enough to get us going.        *)

type arr = (int, 
	    Bigarray.int8_unsigned_elt, 
	    Bigarray.c_layout) 
    Bigarray.Array1.t
;;

type buf = { buf_fd: Unix.file_descr;
	     buf_arr: arr }
;;

let openbuf (fn:string) (sz:int) = 
  let fd = Unix.openfile fn [Unix.O_RDWR; Unix.O_CREAT] 0o700 in
  let alayout = Bigarray.c_layout in
  let akind = Bigarray.int8_unsigned in
  let shared = true in
  let a = Bigarray.Array1.map_file fd akind alayout shared sz in
  { buf_arr = a;
    buf_fd = fd }
;;

(* 
 * "lim" here is the offset that is one-past-the-last-byte we want to accept,
 * a la C++ iterators 
*)
let check_write (pos:int) (lim:int) (proposed_len:int) =
  (if (pos < 0) then failwith "check_write: pos0 < 0");
  (if (lim < pos) then failwith "check_write: lim < pos0");
  (if (proposed_len > (lim - pos)) then failwith "write_bytes: write exceeds limit")
;;  

let write_bytes (b:buf) (pos0:int) (lim:int) (bytes:int array) =
  let
      write1 pos byte = (b.buf_arr.{pos} <- (byte land 0xff); 
			 pos + 1)
  in
    check_write pos0 lim (Array.length bytes);
    Array.fold_left write1 pos0 bytes
;;

let write_rawstring (b:buf) (pos0:int) (lim:int) (str:string) =
  let len = (String.length str) in
    check_write pos0 lim len;
    for i = 0 to len - 1 do
      b.buf_arr.{pos0 + i} <- ((Char.code str.[i]) land 0xff)
    done;
    pos0 + len
;;

let write_zstring (b:buf) (pos0:int) (lim:int) (str:string) =
  let len = (String.length str) in
    check_write pos0 lim (len+1);
    for i = 0 to len - 1 do
      b.buf_arr.{pos0 + i} <- ((Char.code str.[i]) land 0xff)
    done;
    b.buf_arr.{pos0 + len} <- 0;
    pos0 + len + 1
;;

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

type symbol = 
    {
      st_name: int32;   (* : Elf32_Word *)
      st_value: int32;  (* : Elf32_Addr *)
      st_size: int32;   (* : Elf32_Word *)
      st_info: int;     (* u8 *)
      st_other: int;    (* u8 *)
      st_shndx: int;    (* : Elf32_Half *)
    }


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

(* Fixed sizes of structs involved in elf32 spec. *)
let ehsize = 52;;
let phentsize = 32;;
let shentsize = 40;;
let symsize = 16;;

let mk_basic_x86_ehdr n_phdrs phdr_off n_shdrs shdr_off shstrndx = 
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
    e_shstrndx = shstrndx;    (* : Elf32_Half *)
  }
;;

(* Section types *)
let sht_NULL = 0l;;
let sht_PROGBITS = 1l;;
let sht_SYMTAB = 2l;;
let sht_STRTAB = 3l;;
let sht_RELA = 4l;;
let sht_NOBITS = 8l;;

(* Section flags *)
let shf_WRITE = 1l;;
let shf_ALLOC = 2l;;
let shf_EXECINSTR = 4l;;

(* Program (segment) types *)
let pt_NULL = 0l;;
let pt_LOAD = 1l;;
let pt_PHDR = 6l;;

(* Program (segment) flags *)
let pf_X = 1l;;
let pf_W = 2l;;
let pf_R = 4l;;

(* Symbol types (ELF32_ST_TYPE) *)
let stt_NOTYPE  = 0;;
let stt_OBJECT  = 1;;
let stt_FUNC    = 2;;
let stt_SECTION = 3;;
let stt_FILE    = 4;;

(* Symbol binding (ELF32_ST_BIND) *)
let stb_LOCAL   = 0;;
let stb_GLOBAL  = 1;;
let stb_WEAK    = 2;;

(* Symbol section indices *)
let shn_UNDEF   = 0;;
let shn_ABS     = 0xfff1;;


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

let mk_basic_phdr ~typ ~align ~off ~addr ~filesz ~memsz ~flags =
  {
   p_type = typ;
   p_offset = off;
   p_vaddr = addr;
   p_paddr = addr;
   p_filesz = filesz;
   p_memsz = memsz;
   p_flags = List.fold_left Int32.logor 0l flags;
   p_align = align
 }

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
