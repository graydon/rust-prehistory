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

      file_syms: (string, (int * int * int * int)) Hashtbl.t;
      mutable file_fixups: (int * string) list;
    }
;;

let textndx        = 1;;  (* Section index of .text *)
let rodatandx      = 2;;  (* Section index of .rodata *)
let datandx        = 3;;  (* Section index of .data *)
let shstrndx       = 5;;  (* Section index of .shstrtab *)
let strndx         = 7;;  (* Section index of .strtab *)

let add_data_sym f name vma size = 
  Hashtbl.add f.file_syms name (stt_OBJECT, datandx, vma, size)
;;

let add_rodata_sym f name vma size = 
  Hashtbl.add f.file_syms name (stt_OBJECT, rodatandx, vma, size)
;;

let add_func_sym f name vma size = 
  Hashtbl.add f.file_syms name (stt_FUNC, textndx, vma, size)
;;

let add_fixup f off name = 
  f.file_fixups <- ((off, name) :: f.file_fixups)
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

      file_syms = Hashtbl.create 100;
      file_fixups = [];
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
  let _ = write_section_names () in 
  let _ = write_elf_header_at f.file_buf 0 f.file_ehdr.e_ehsize f.file_ehdr in  
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
  let _ = List.fold_left wp (Int32.to_int f.file_ehdr.e_phoff)
    [
      f.file_phdr_phdr; 
      f.file_phdr_re_load;
      f.file_phdr_rw_load
    ]
  in
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
    ()
;;

(* x86 instruction emitting *)

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
let pop_ECX = plusrd_ECX 0x58;;
let pop_EDX = plusrd_EDX 0x58;;
let pop_ESI = plusrd_ESI 0x58;;
let pop_EDI = plusrd_EDI 0x58;;

let push_EAX = plusrd_EAX 0x50;;
let push_EBX = plusrd_EBX 0x50;;
let push_ECX = plusrd_ECX 0x50;;
let push_EDX = plusrd_EDX 0x50;;
let push_ESI = plusrd_ESI 0x50;;
let push_EDI = plusrd_EDI 0x50;;

let mov_EAX_imm8 = plusrd_EAX 0xB0;;

let push_imm8 i  = [| 0x6A; ub i 0 |]
let push_imm32 i = Array.append [| 0x68 |] (int_to_u32_lsb0 i)

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


(* 
 * Note: our calling convention here is like everywhere else: "cdecl"
 * 
 * that is to say, the caller of f(a,b,c,d,e) emits:
 * 
 * push e
 * push d
 * push c
 * push b
 * push a
 * call f
 * push EAX  -- return val was in EAX
 * 
 * With "SYSCALL5 i" the only difference is that "call f" takes the form of
 * popping args off the stack and into registers, loading an immediate syscall 
 * number into EAX, and invoking int 0x80.
 * 
 * pop EBX
 * pop ECX
 * pop EDX
 * pop ESI
 * pop EDI
 * mov EAX i
 * int 0x80
 *
 *)

let op_SYSCALL0 i = [| mov_EAX_imm8; ub i 0; 0xCD; 0x80; push_EAX |];;
let op_SYSCALL1 i = [| mov_EAX_imm8; ub i 0; pop_EBX; 0xCD; 0x80; push_EAX |];;
let op_SYSCALL2 i = [| mov_EAX_imm8; ub i 0; pop_EBX; pop_ECX; 0xCD; 0x80; push_EAX |];;
let op_SYSCALL3 i = [| mov_EAX_imm8; ub i 0; pop_EBX; pop_ECX; pop_EDX; 0xCD; 0x80; push_EAX |];;
let op_SYSCALL4 i = [| mov_EAX_imm8; ub i 0; pop_EBX; pop_ECX; pop_EDX; pop_ESI; 0xCD; 0x80; push_EAX |];;
let op_SYSCALL5 i = [| mov_EAX_imm8; ub i 0; pop_EBX; pop_ECX; pop_EDX; pop_ESI; pop_EDI; 0xCD; 0x80; push_EAX |];;

let op_SYS_EXIT  = op_SYSCALL1 1;;    (* sys_exit(int status)                         *)
let op_SYS_FORK  = op_SYSCALL0 2;;    (* sys_fork()                                   *)
let op_SYS_READ  = op_SYSCALL3 3;;    (* sys_read(int fd, char* buf, size_t count)    *)
let op_SYS_WRITE = op_SYSCALL3 4;;    (* sys_write(int fd, char* buf, size_t count)   *)
let op_SYS_OPEN  = op_SYSCALL3 4;;    (* sys_open(const char *f, int flags, int mode) *)
let op_SYS_CLOSE = op_SYSCALL1 4;;    (* sys_close(unsigned int fd)                   *)


(* Basic test *)

let test_asm _ = 
  let f = mk_basic_x86_elf_file "test.elf32" in
  let buf = f.file_buf in

  let text_shdr = f.file_shdr_text in
  let rodata_shdr = f.file_shdr_rodata in

  let text_off = (Int32.to_int text_shdr.sh_offset) in
  let text_vma = (Int32.to_int text_shdr.sh_addr) in
  let text_pos = ref text_off in
  let text_lim = text_off + (Int32.to_int text_shdr.sh_size) in

  let rodata_off = (Int32.to_int rodata_shdr.sh_offset) in
  let rodata_vma = (Int32.to_int f.file_shdr_rodata.sh_addr) in
  let rodata_pos = ref rodata_off in
  let rodata_lim = rodata_off + (Int32.to_int rodata_shdr.sh_size) in

  let append_ro_rawstring str = (rodata_pos := write_rawstring buf (!rodata_pos) rodata_lim str) in
  let append_insns iss = (text_pos := write_bytes buf (!text_pos) text_lim iss) in
  let push_rodata_addr dpos = push_imm32 ((dpos - rodata_off) + rodata_vma) in
  let 
      call_write str = (let dp = !rodata_pos in
			  append_ro_rawstring str;
			  append_insns (Array.concat 
					  [
					    push_imm32 (String.length str);
					    push_rodata_addr dp;
					    push_imm32 1; (* fd1 *)
					    op_SYS_WRITE
					  ]))
  in
  let 
      main = (Array.concat 
		[
		  push_imm32 23;
		  op_SYS_EXIT
		])
  in
    f.file_ehdr.e_entry <- f.file_shdr_text.sh_addr;
    call_write "hello, world!\n";
    add_func_sym f "main" text_vma (Array.length main);
    append_insns main;
    write_basic_x86_elf_file f;
    Unix.close f.file_buf.buf_fd
;;

test_asm ();;

