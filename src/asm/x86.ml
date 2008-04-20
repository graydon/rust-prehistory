(* 
 * x86/ia32 instructions have 6 parts: 
 * 
 *    [pre][op][modrm][sib][disp][imm]
 * 
 * [pre] = 0..4 bytes of prefix
 * [op] = 1..3 byte opcode
 * [modrm] = 0 or 1 byte: [mod:2][reg/op:3][r/m:3]
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
 * The final low 3 bits denote sub-modes of the primary mode selected
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
 * 
 * Some opcodes are written 0xNN +rd. This means "we decided to chew up a whole
 * pile of opcodes here, with each opcode including a hard-wired reference to a
 * register". For example, POP is "0x58 +rd", which means that the 1-byte insns
 * 0x58..0x5f are chewed up for "POP EAX" ... "POP EDI" (again, the canonical
 * order of register numberings)
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

(* Following is no longer true: we have a register allocator. *)

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




(* 
 * Frames look like this, as in C (stack grows down):
 * 
 *    [arg0     ]
 *    ...        
 *    [argN     ]
 *    [env ptr  ]
 *    [desc ptr ]
 *    [yield sp ] 
 *    [yield pc ]
 *    [return sp]
 *    [return pc]  <-- sp for this frame.
 *    [local 0  ] 
 *    ...
 *    [local N  ]
 *    [spill 0  ]
 *    ...
 *    [spill N  ]
 * 
 * All you have to work with is sp. At sp there is a return
 * pc, at sp+4 there is a saved sp of the frame under us,
 * which we reload before jumping back to pc=*sp. Note that 
 * the values of sps do not need to be anything remotely
 * like linear. Stack segments may go all over the heap.
 * 
 * At sp+8 there is a descriptor that tells you what 
 * sort of frame you're in. You should not look at anything
 * aside from sp, sp+4 and sp+8 "generically"; you have
 * to use the descriptor to do anything else.
 * 
 * If the descriptor says you're in a function that can yield,
 * you will then have a yield pc and yield sp above it. If the
 * descriptor says you're in a closure, you will have an 
 * environment pointer above that. Above these optional parts
 * you'll have the args.
 * 
 * The caller must know at least the following when it makes
 * a call:
 * 
 *   - if it's calling into a yielding function
 *   - if it's calling into a closure
 *   - if it's tail-calling
 *   - if it's tail-yielding
 * 
 * It needs to know these things for the following reasons:
 * 
 *   - When entering a yielding function, two extra words need
 *     to be reserved. Nothing needs to be put in them unless
 *     it's a tail-yield; the prologue of the callee will set 
 *     it up normally.
 * 
 *   - When entering a closure, the environment needs to be
 *     set.
 * 
 *   - When tail-calling, the current frame is taken apart
 *     and a new frame built in its place before jumping to
 *     the target.
 * 
 *   - When tail-yielding, the current frame remains but the
 *     caller copies its yield sp and pc to the callee. It does
 *     this by calling to an address a few words inside the callee,
 *     past the callee prologue that would *normally* set up the
 *     default yield sp and yield pc from the incoming return sp
 *     and return pc.
 * 
 * 
 *)
