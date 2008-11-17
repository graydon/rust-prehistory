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
 * There are 8 GPRs but we use 4 of them for specific purposes:
 * 
 *   - ESP always points to the current stack frame.
 *   - EBP always points to the current process.
 *   - EDX is reserved for reload/spill operations on locals that did not make
 *     it into hardregs. It is chosen for this because it's also occasionally
 *     clobbered by other ops (mul, udiv and idiv, f.e.) and this saves us
 *     having to bother with fancy support for clobbered regs.
 *   - EDI we also use for reload/spill, because we're using complex addressing 
 *     modes and, sadly, this means we can have 2 spill slots used as memory
 *     base operands in use in a single instruction. On the upside, complex
 *     addressing modes are possibly still cheaper than lots of manual address
 *     arithmetic!
 * 
 * We tell IL that we have 4 GPRs then, and permit most register-register ops
 * on any of these 4, mostly-unconstrained. Still need to support pre-allocating
 * EAX for the destination of mul/imul.
 * 
 *)

open Common;;
open Il;;

exception Unrecognized
;;

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


(* 
 * Translate an IL-level hwreg number from 0..nregs into the 3-bit code number
 * used through the mod r/m byte and /r sub-register specifiers of the x86 ISA.
 * 
 * See "Table 2-2: 32-Bit Addressing Forms with the ModR/M Byte", in the IA32
 * Architecture Software Developer's Manual, volume 2a.  
 *)

let eax = 0
let ecx = 1
let ebx = 2
let esi = 3
let edi = 4
let edx = 5
let ebp = 6
let esp = 7

let code_eax = 0b000;;
let code_ecx = 0b001;;
let code_edx = 0b010;;
let code_ebx = 0b011;;
let code_esp = 0b100;;
let code_ebp = 0b101;;
let code_esi = 0b110;;
let code_edi = 0b111;;

let reg r = 
  match r with 
	  0 -> code_eax
	| 1 -> code_ecx
	| 2 -> code_ebx
	| 3 -> code_esi
		(* Never assigned by the register allocator, but synthetic code uses them *)
	| 4 -> code_edi
	| 5 -> code_edx
	| 6 -> code_ebp
	| 7 -> code_esp
	| _ -> raise (Invalid_argument "X86.reg")
;;

let reg_str r = 
  match r with 
	  0 -> "eax"
	| 1 -> "ecx"
	| 2 -> "ebx"
	| 3 -> "esi"
	| 4 -> "edi"
	| 5 -> "edx"
	| 6 -> "ebp"
	| 7 -> "esp"
	| _ -> raise (Invalid_argument "X86.reg_str")
;;


(* This is a basic ABI. You might need to customize it by platform. *)
let n_hardregs = 4;;
let (abi:Abi.abi) = 
  {
    Abi.abi_ptrsz = 4;
    Abi.abi_ptr_mem = Il.M32;

    Abi.abi_is_2addr_machine = true;
    Abi.abi_n_hardregs = n_hardregs;
    Abi.abi_str_of_hardreg = reg_str;
    
    Abi.abi_fp_operand = Il.Reg (Il.HWreg ebp);
    Abi.abi_pp_operand = Il.Mem (Il.M32, Some (Il.HWreg ebp), Asm.IMM 0L);
    Abi.abi_cp_operand = Il.Mem (Il.M32, Some (Il.HWreg ebp), Asm.IMM 4L);
    Abi.abi_rp_operand = Il.Mem (Il.M32, Some (Il.HWreg ebp), Asm.IMM 8L);
  }


(* 
 * NB: factor the instruction selector often. There's lots of
 * semi-redundancy in the ISA.  
 *)


let imm_is_byte n = (n = (Int64.logand 0xffL n))
;;


let rm_r (oper:operand) (r:int) = 
  let reg_ebp = 6 in
  let reg_esp = 7 in 
    match oper with 
        Reg (HWreg rm) -> 
          Asm.BYTE (modrm_reg (reg rm) r)
      | Mem (_, None, disp) -> 
          Asm.SEQ [| Asm.BYTE (modrm_deref_disp32 r);
                     Asm.WORD32 disp |]
      | Mem (_, Some (HWreg rm), disp) when rm != reg_esp -> 
          (match disp with 
               Asm.IMM 0L when rm != reg_ebp -> 
                 Asm.BYTE (modrm_deref_reg (reg rm) r)
             | Asm.IMM n when imm_is_byte n -> 
                 Asm.SEQ [| Asm.BYTE (modrm_deref_reg_plus_disp8 (reg rm) r);
                            Asm.WORD8 disp|]
             | Asm.IMM _ -> 
                 Asm.SEQ [| Asm.BYTE (modrm_deref_reg_plus_disp32 (reg rm) r);
                            Asm.WORD32 disp |]
             | _ -> 
                 Asm.new_relaxation
                   [|
                     Asm.SEQ [| Asm.BYTE (modrm_deref_reg_plus_disp32 (reg rm) r);
                                Asm.WORD32 disp |];
                     Asm.SEQ [| Asm.BYTE (modrm_deref_reg_plus_disp8 (reg rm) r);
                                Asm.WORD8 disp|];
                   |])
      | _ -> raise Unrecognized
;;


let insn_rm_r (op:int) (oper:operand) (r:int) : Asm.item = 
  Asm.SEQ [| Asm.BYTE op; rm_r oper r |]
;;


let insn_rm_r_imm8 (op:int) (oper:operand) (r:int) (i:Asm.expr64) : Asm.item = 
  Asm.SEQ [| Asm.BYTE op; rm_r oper r; Asm.WORD8 i |]
;;


let insn_rm_r_imm32 (op:int) (oper:operand) (r:int) (i:Asm.expr64) : Asm.item = 
  Asm.SEQ [| Asm.BYTE op; rm_r oper r; Asm.WORD32 i |]
;;


let insn_rm_r_imm (op8:int) (op32:int) (oper:operand) (r:int) (i:Asm.expr64) : Asm.item = 
  match i with 
      Asm.IMM n when imm_is_byte n -> 
        insn_rm_r_imm8 op8 oper r i
    | Asm.IMM k -> 
        insn_rm_r_imm32 op32 oper r i
    | _ -> 
        Asm.new_relaxation
          [| 
            insn_rm_r_imm32 op32 oper r i;
            insn_rm_r_imm8 op8 oper r i 
          |]
;;


let insn_pcrel_relax
    (op8_item:Asm.item) 
    (op32_item:Asm.item) 
    (fix:fixup) 
    : Asm.item = 
  let pcrel_mark_fixup = new_fixup "ccall-pcrel mark fixup" in 
  let def = Asm.DEF (pcrel_mark_fixup, Asm.MARK) in
  let pcrel_expr = (Asm.SUB (Asm.M_POS fix, 
						     Asm.M_POS pcrel_mark_fixup))
  in
    Asm.new_relaxation 
      [|
        Asm.SEQ [| op32_item; Asm.WORD32 pcrel_expr; def |];
        Asm.SEQ [| op8_item; Asm.WORD8 pcrel_expr; def |];
      |]
;;

let insn_pcrel_simple (op32:int) (fix:fixup) : Asm.item = 
  let pcrel_mark_fixup = new_fixup "ccall-pcrel mark fixup" in 
  let def = Asm.DEF (pcrel_mark_fixup, Asm.MARK) in
  let pcrel_expr = (Asm.SUB (Asm.M_POS fix, 
						     Asm.M_POS pcrel_mark_fixup))
  in
    Asm.SEQ [| Asm.BYTE op32; Asm.WORD32 pcrel_expr; def |]
;;

let insn_pcrel (op8:int) (op32:int) (fix:fixup) : Asm.item = 
  insn_pcrel_relax (Asm.BYTE op8) (Asm.BYTE op32) fix
;;

let insn_pcrel_prefix32 (op8:int) (prefix32:int) (op32:int) (fix:fixup) : Asm.item = 
  insn_pcrel_relax (Asm.BYTE op8) (Asm.BYTES [| prefix32; op32 |]) fix
;;


let is_rm32 (oper:operand) : bool = 
  match oper with 
      Mem (M32, _, _) -> true
    | Reg (HWreg _) -> true
    | _ -> false
;;


let is_rm8 (oper:operand) : bool = 
  match oper with 
      Mem (M8, _, _) -> true
    | Reg (HWreg _) -> true
    | _ -> false
;;


let cmp (a:operand) (b:operand) : Asm.item = 
  match (a,b) with 
      (_, Imm i) when is_rm32 a -> insn_rm_r_imm 0x83 0x81 a slash7 i
    | (_, Reg (HWreg r)) -> insn_rm_r 0x39 a r
    | (Reg (HWreg r), _) -> insn_rm_r 0x3b b r
    | _ -> raise Unrecognized
;;


let mov (dst:operand) (src:operand) : Asm.item = 
  match (dst,src) with 
      (Mem (M8, _, _), Reg (HWreg r)) -> 
        insn_rm_r 0x88 dst (reg r)
    | (_, Reg (HWreg r)) when is_rm32 dst -> 
        insn_rm_r 0x89 dst (reg r)
    | (Reg (HWreg r), Mem (M8, _, _)) -> 
        insn_rm_r 0x8a src (reg r)
    | (Reg (HWreg r), Mem (M32, _, _)) -> 
        insn_rm_r 0x8b src (reg r);
        
    | (_, Imm (Asm.IMM n)) when is_rm8 dst && imm_is_byte n -> 
        insn_rm_r_imm8 0xc6 dst slash0 (Asm.IMM n)
          
    | (_, Imm i) when is_rm32 dst -> 
        insn_rm_r_imm32 0xc7 dst slash0 i
          
    | _ -> raise Unrecognized
;;


let select_item_misc t =
  match (t.quad_op, t.quad_dst, t.quad_lhs, t.quad_rhs) with
      
	  (CCALL, r, _, _) when is_rm32 r -> insn_rm_r 0xff r slash2          
	| (CCALL, Pcrel f, _, _) -> insn_pcrel_simple 0xe8 f

	| (CPUSH M32, Reg (HWreg r), _, _) -> Asm.BYTE (0x50 + (reg r))
	| (CPUSH M32, r, _, _) when is_rm32 r -> insn_rm_r 0xff r slash6
	| (CPUSH M32, Imm i, _, _) -> Asm.SEQ [| Asm.BYTE 0x68; Asm.WORD32 i |]          
	| (CPUSH M8, Imm i, _, _) -> Asm.SEQ [| Asm.BYTE 0x6a; Asm.WORD8 i |]
        
	| (CRET, _, _, _) -> Asm.BYTE 0xc3

	| (JC,  Pcrel f, _, _) -> insn_pcrel_prefix32 0x72 0x0f 0x82 f
	| (JNC, Pcrel f, _, _) -> insn_pcrel_prefix32 0x73 0x0f 0x83 f
	| (JO,  Pcrel f, _, _) -> insn_pcrel_prefix32 0x70 0x0f 0x80 f
	| (JNO, Pcrel f, _, _) -> insn_pcrel_prefix32 0x71 0x0f 0x81 f
	| (JE,  Pcrel f, _, _) -> insn_pcrel_prefix32 0x74 0x0f 0x84 f
	| (JNE, Pcrel f, _, _) -> insn_pcrel_prefix32 0x75 0x0f 0x85 f
	| (JL,  Pcrel f, _, _) -> insn_pcrel_prefix32 0x7c 0x0f 0x8c f
	| (JLE, Pcrel f, _, _) -> insn_pcrel_prefix32 0x7e 0x0f 0x8e f
	| (JG,  Pcrel f, _, _) -> insn_pcrel_prefix32 0x7f 0x0f 0x8f f
	| (JGE, Pcrel f, _, _) -> insn_pcrel_prefix32 0x7d 0x0f 0x8d f

	| (JMP, r, _, _) when is_rm32 r -> insn_rm_r 0xff r slash4
	| (JMP, Pcrel f, _, _) -> insn_pcrel 0xeb 0xe9 f

	| (DEAD, _, _, _) -> Asm.MARK
	| (END, _, _, _) -> Asm.BYTES [| 0x90 |]
	| (NOP, _, _, _) -> Asm.BYTES [| 0x90 |]
        
	| _ -> 
		raise Unrecognized
;;


let alu_binop dst src immslash rm_dst_op rm_src_op = 
  match (dst, src) with 
      (Reg (HWreg r), _) when is_rm32 src -> insn_rm_r rm_src_op src (reg r)
    | (_, Reg (HWreg r)) when is_rm32 dst -> insn_rm_r rm_dst_op dst (reg r)
    | (Reg (HWreg _), Imm i) -> insn_rm_r_imm 0x83 0x81 dst immslash i 
    | _ -> raise Unrecognized
;;


let mul_like src slash = 
  if is_rm32 src
  then insn_rm_r 0x7f src slash
  else raise Unrecognized
;;


let mod_like src slash = 
  Asm.SEQ [| mul_like src slash; 
             mov (Reg (HWreg eax)) (Reg (HWreg edx)) |]
;;


let select_insn q =  
  let item = 
    match q.quad_op with 
        MOV -> mov q.quad_dst q.quad_lhs 
      | CMP -> cmp q.quad_lhs q.quad_rhs
      | _ -> 
          begin
            if q.quad_dst = q.quad_lhs 
            then 
              let binop = alu_binop q.quad_lhs q.quad_rhs in
              let unop = insn_rm_r 0xf7 q.quad_lhs in
              let mulop = mul_like q.quad_rhs in
              let modop = mod_like q.quad_rhs in
                match (q.quad_dst, q.quad_op) with 
                    (_, ADD) -> binop slash0 0x1 0x3 
                  | (_, SUB) -> binop slash5 0x29 0x2b
                  | (_, AND) -> binop slash4 0x21 0x23
                  | (_, OR) -> binop slash1 0x09 0x0b
                      
                  | (Reg (HWreg 0), UMUL) -> mulop slash4
                  | (Reg (HWreg 0), IMUL) -> mulop slash5
                  | (Reg (HWreg 0), UDIV) -> mulop slash6
                  | (Reg (HWreg 0), IDIV) -> mulop slash7
                      
                  | (Reg (HWreg 0), UMOD) -> modop slash6
                  | (Reg (HWreg 0), IMOD) -> modop slash7
                      
                  | (_, NEG) -> unop slash3 
                  | (_, NOT) -> unop slash2
                      
                  | _ -> select_item_misc q
            else 
              select_item_misc q
          end
  in
	match q.quad_fixup with 
		None -> item
	  | Some f -> Asm.DEF (f, item)
;;

let select_insns (sess:Session.sess) (q:Il.quads) : Asm.item = 
  let sel q = 
    try 
      select_insn q 
    with 
        Unrecognized -> 
          Session.fail sess
            "E:Assembly error: unrecognized quad: %s\n%!" 
            (Il.string_of_quad q);
          Asm.MARK
  in
    Asm.SEQ (Array.map sel q)
;;


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


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
