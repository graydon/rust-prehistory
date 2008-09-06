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

(* 
 * Notes on register availability of x86:
 * 
 * There are 8 GPRs but we use 3 of them for specific purposes:
 * 
 *   - ESP always points to the current stack frame.
 *   - EBP always points to the current process.
 *   - EDX is reserved for reload/spill operations on locals that did not make
 *     it into hardregs. It is chosen for this because it's also occasionally
 *     clobbered by other ops (mul, udiv and idiv, f.e.) and this saves us
 *     having to bother with fancy support for clobbered regs.
 * 
 * We tell IL that we have 5 GPRs then, and permit most register-register ops
 * on any of these 5, mostly-unconstrained. Still need to support pre-allocating
 * EAX for the destination of mul/imul
 * 
 *)

(* x86 instruction emitting *)

let modrm m rm reg_or_subopcode = 
  if (((m land 0b11) != m) or
		((rm land 0b111) != rm) or
		((reg_or_subopcode land 0b111) != reg_or_subopcode))
  then raise (Invalid_argument "X86.modrm_deref")
  else 
	((((m land 0b11) lsl 6) 
      lor 
	  (rm land 0b111)) 
     lor 
	  ((reg_or_subopcode land 0b111) lsl 3))
;;

let modrm_deref_reg = modrm 0b00 ;;
let modrm_deref_disp32 = modrm 0b00 0b101 ;;
let modrm_deref_reg_plus_disp8 = modrm 0b01 ;;
let modrm_deref_reg_plus_disp32 = modrm 0b10 ;;
let modrm_reg = modrm 0b11 ;;

let slash0 = 0;;
let slash1 = 1;;
let slash2 = 2;;
let slash3 = 3;;
let slash4 = 4;;
let slash5 = 5;;
let slash6 = 6;;
let slash7 = 7;;


(* Translate an IL-level hwreg number from 0..nregs into the 3-bit code number
 * used through the mod r/m byte and /r sub-register specifiers of the x86 ISA.
 * 
 * See "Table 2-2: 32-Bit Addressing Forms with the ModR/M Byte", in the IA32
 * Architecture Software Developer's Manual, volume 2a.  *)

let eax = 0b000;;
let ecx = 0b001;;
let edx = 0b010;;
let ebx = 0b011;;
let ebp = 0b101;;
let esi = 0b110;;
let edi = 0b111;;

let reg r = 
  match r with 
	  0 -> eax
	| 1 -> ecx
	| 2 -> ebx
	| 3 -> esi
	| 4 -> edi
	| _ -> raise (Invalid_argument "X86.reg")
;;

(* FIXME: factor the instruction selector often. There's lots of
 * semi-redundancy in the ISA.
 *)

let select_insn t = 
  match (t.Il.triple_op, t.Il.triple_dst, t.Il.triple_src) with
	  (Il.ADD, Il.HWreg dst, Il.HWreg src) -> 
		Asm.BYTES [| 0x03; modrm_reg (reg dst) (reg src) |]
	| (Il.SUB, Il.HWreg dst, Il.HWreg src) -> 
		Asm.BYTES [| 0x29; modrm_reg (reg dst) (reg src) |]
	| (Il.NEG, Il.HWreg dst, Il.Nil) -> 
		Asm.BYTES [| 0xF7; modrm_reg (reg dst) slash3 |]

	(* The mess that is the DIV/IDIV/MUL/IMUL instructions:
	 * 
	 * They can only ever write to EAX:EDX. So we only recognize EAX as a
	 * destination at all. Needs pre-allocation. If you're trying to take a MOD
	 * rather than a DIV, we move the EDX part (remainder) to EAX (quotient),
	 * overwriting it. In this way DIV and IDIV get used for dual-duty.  
	 *)

	| (Il.UMUL, Il.HWreg 0, Il.HWreg src) -> 
		Asm.BYTES [| 0xF7; modrm_reg (reg src) slash4 |]
	| (Il.IMUL, Il.HWreg 0, Il.HWreg src) -> 
		Asm.BYTES [| 0xF7; modrm_reg (reg src) slash5 |]
	| (Il.UDIV, Il.HWreg 0, Il.HWreg src) -> 
		Asm.BYTES [| 0xF7; modrm_reg (reg src) slash6 |]
	| (Il.UMOD, Il.HWreg 0, Il.HWreg src) -> 
		Asm.BYTES [| 0xF7; modrm_reg (reg src) slash6;
					 0x89; modrm_reg eax edx; |]
	| (Il.IDIV, Il.HWreg 0, Il.HWreg src) -> 
		Asm.BYTES [| 0xF7; modrm_reg (reg src) slash7 |]
	| (Il.IMOD, Il.HWreg 0, Il.HWreg src) -> 
		Asm.BYTES [| 0xF7; modrm_reg (reg src) slash7;
					 0x89; modrm_reg eax edx; |]
		  
	| (Il.MOV, Il.HWreg dst, Il.Imm imm) -> 
		Asm.SEQ [| Asm.BYTES [| 0xB8 + (reg dst) |];
				   Asm.WORD32 imm |]

	| (Il.MOV, Il.HWreg dst, Il.Deref ((Il.Imm imm), disp)) -> 
		Asm.SEQ [| Asm.BYTES [| 0x8B; modrm_deref_disp32 (reg dst) |];
				   Asm.WORD32 (Asm.ADD ((Asm.IMM disp),imm)) |]

	| (Il.MOV, Il.HWreg dst, Il.HWreg src) -> 
		Asm.BYTES [| 0x89; modrm_reg (reg dst) (reg src); |]

	| (Il.MOV, Il.HWreg dst, (Il.Deref ((Il.HWreg src), disp))) -> 
		if disp = 0L
		then Asm.BYTES [| 0x8B; modrm_deref_reg (reg dst) (reg src); |]
		else 
		  if (Int64.logand disp 0xffL) = disp
		  then Asm.BYTES [| 0x8B; 
							modrm_deref_reg_plus_disp8 (reg dst) (reg src);
							Int64.to_int disp |]			
		  else 
			if (Int64.logand disp 0xffffffffL) = disp
			then 
			  Asm.SEQ [| 
				Asm.BYTES [| 0x8B; modrm_deref_reg_plus_disp32 (reg dst) (reg src); |];
				Asm.WORD32 (Asm.IMM disp) |]
		
			else
			  raise (Invalid_argument "X86.select_insn: displacement overflow")

	| (Il.CCALL, Il.HWreg r, Il.Imm arg) -> 
		Asm.SEQ [|
		  Asm.BYTES [| 0x68; |]; Asm.WORD32 arg; (* push arg *)
		  Asm.BYTES [| 0xff; modrm_reg (reg r) slash2 |] (* call *r *)
		|]

	| (Il.JMP, Il.HWreg dst, Il.Nil) -> 
		Asm.BYTES [| 0xff; modrm_reg (reg dst) slash4 |]

	| (Il.BNOT, Il.HWreg dst, Il.Nil) -> 
		Asm.BYTES [| 0xF7; modrm_reg (reg dst) slash2 |]
	| (Il.BXOR, Il.HWreg dst, Il.HWreg src) -> 
		Asm.BYTES [| 0x33; modrm_reg (reg dst) (reg src) |]
	| (Il.BOR, Il.HWreg dst, Il.HWreg src) -> 
		Asm.BYTES [| 0x09; modrm_reg (reg dst) (reg src) |]

	| (Il.END, _, _) -> Asm.BYTES [| 0x90 |]
	| (Il.NOP, _, _) -> Asm.BYTES [| 0x90 |]

	| _ -> raise (Invalid_argument "X86.select_insn: no matching insn")


(* Somewhat obsolete stuff follows to EOF ... salvaging *)

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
(* 
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
*)



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
