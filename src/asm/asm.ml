
(* 
 * x86/ia32 instructions have 6 parts: 
 * 
 *    [pre][op][modrm][sib][disp][imm]
 * 
 * [pre] = 0..4 bytes of prefix
 * [op] = 1..3 byte opcode
 * [modrm] = 0 or 1 byte:  [mod:2][reg/op:3][r/m:3]
 * [sib] = 0 or 1 byte: [scale:2][index:3][base:3]
 * [disp] = 1, 2 or 4 byte displacement
 * [imm] = 1, 2 or 4 byte immediate
 * 
 * So between 1 and 17 bytes total.
 * 
 * We're not going to use sib, but modrm is worth discussing. 
 * 
 * The high two bits of modrm denote an operand "mode". The modes are:
 * 
 *   00 - "mostly" *(reg)
 *   01 - "mostly" *(reg) + disp8
 *   10 - "mostly" *(reg) + disp32
 *   11 - reg
 * 
 * The next-lowest 3 bits denote a specific register, or a subopcode if 
 * there is a fixed register or only one operand. The instruction format
 * reference will say "/<n>" for some number n, if a fixed subopcode is used.
 * It'll say "/r" if the instruction uses this field to specify a register.
 * 
 * The registers specified in this field are:
 * 
 *   000 - EAX or XMM0
 *   001 - ECX or XMM1
 *   010 - EDX or XMM2
 *   011 - EBX or XMM3
 *   100 - ESP or XMM4
 *   101 - EBP or XMM5
 *   110 - ESI or XMM6
 *   111 - EDI or XMM7
 * 
 * The next-lowest 3 bits denote sub-modes of the primary mode selected
 * with the top 2 bits. In particular, they "mostly" select the reg that is 
 * to be used for effective address calculation.
 * 
 * For the most part, these follow the same numbering order: EAX, ECX, EDX,
 * EBX, ESP, EBP, ESI, EDI. There are two unusual deviations from the rule
 * though:
 * 
 *  - In primary modes 00, 01 and 10, r/m=100 means "use SIB byte". 
 *    There is no way to use ESP as the register in these modes. Weird.
 * 
 *  - In primary mode 00, r/m=101 means "just disp32", no register is involved.
 *    There is no way to use EBP in primary mode 00.
 *)

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

test_asm ();;

