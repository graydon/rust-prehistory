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
 *    You can use (unscaled) ESP as the base register in these modes by appending
 *    the SIB byte 0x24. We do that in our rm_r operand-encoder function.
 *
 *  - In primary mode 00, r/m=101 means "just disp32", no register is involved.
 *    There is no way to use EBP in primary mode 00. If you try, we just
 *    decay into a mode 01 with an appended 8-bit immediate displacement.
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
 * There are 8 GPRs but we use 2 of them for specific purposes:
 *
 *   - ESP always points to the current stack frame.
 *   - EBP always points to the current frame base.
 *
 * We tell IL that we have 6 GPRs then, and permit most register-register ops
 * on any of these 6, mostly-unconstrained.
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
let (n_hardregs:int) = 6;;

let prealloc_quad (quad:Il.quad) : Il.quad =
  match quad.Il.quad_op with
      Il.IMUL | Il.UMUL -> { quad with Il.quad_dst = Il.Reg (Il.Hreg eax) }
    | Il.IDIV | Il.UDIV -> { quad with Il.quad_dst = Il.Reg (Il.Hreg eax) }
    | Il.IMOD | Il.UMOD -> { quad with Il.quad_dst = Il.Reg (Il.Hreg edx) }
    | Il.CCALL -> { quad with Il.quad_dst = Il.Reg (Il.Hreg eax) }
    | _ -> quad
;;

let clobbers (quad:Il.quad) : Il.hreg list =
  match quad.Il.quad_op with
      Il.IMUL | Il.UMUL -> [ edx ]
    | Il.IDIV | Il.UDIV -> [ edx ]
    | Il.IMOD | Il.UMOD -> [ eax ]
    | Il.CCALL -> [ eax; ecx; edx; ]
    | _ -> []
;;


let word_sz = 4L
;;

let word_mem = Il.M32
;;

let word_ty = TY_u32
;;

let spill_slot (framesz:int64) (i:int) : Il.operand =
  Il.Mem
    (word_mem, Some (Il.Hreg ebp),
     (Asm.IMM
        (Int64.neg (Int64.add framesz (Int64.mul word_sz (Int64.of_int (i+1)))))))
;;


let save_callee_saves (e:Il.emitter) : unit =
  let r x = Il.Reg (Il.Hreg x) in
    Il.emit e (Il.CPUSH word_mem) Il.Nil (r ebp) Il.Nil;
    Il.emit e (Il.CPUSH word_mem) Il.Nil (r edi) Il.Nil;
    Il.emit e (Il.CPUSH word_mem) Il.Nil (r esi) Il.Nil;
    Il.emit e (Il.CPUSH word_mem) Il.Nil (r ebx) Il.Nil;
;;


let restore_callee_saves (e:Il.emitter) : unit =
  let r x = Il.Reg (Il.Hreg x) in
    Il.emit e (Il.CPOP word_mem) (r ebx) Il.Nil Il.Nil;
    Il.emit e (Il.CPOP word_mem) (r esi) Il.Nil Il.Nil;
    Il.emit e (Il.CPOP word_mem) (r edi) Il.Nil Il.Nil;
    Il.emit e (Il.CPOP word_mem) (r ebp) Il.Nil Il.Nil;
;;

let fn_prologue
    (e:Il.emitter)
    (framesz:int64)
    (spill_fixup:fixup)
    (callsz:int64)
    : unit =
  let r x = Il.Reg (Il.Hreg x) in
  let ssz = Int64.add framesz callsz in
    save_callee_saves e;
    Il.emit e Il.UMOV (r ebp) (r esp) Il.Nil;
    Il.emit e Il.SUB (r esp) (r esp)
      (Il.Imm (Asm.ADD ((Asm.IMM ssz), Asm.M_SZ spill_fixup)))
;;

let fn_epilogue (e:Il.emitter) : unit =
  let r x = Il.Reg (Il.Hreg x) in
    Il.emit e Il.UMOV (r esp) (r ebp) Il.Nil;
    restore_callee_saves e;
    Il.emit e Il.CRET Il.Nil Il.Nil Il.Nil;
;;

let main_prologue
    (e:Il.emitter)
    (block:Ast.block)
    (framesz:int64)
    (spill_fixup:fixup)
    (callsz:int64)
    : unit =
  let r x = Il.Reg (Il.Hreg x) in
  let ssz = Int64.add framesz callsz in
    save_callee_saves e;
    Il.emit e Il.UMOV (r ebp) (r esp) Il.Nil;
    Il.emit e Il.SUB (r esp) (r esp)
      (Il.Imm (Asm.ADD ((Asm.IMM ssz), Asm.M_SZ spill_fixup)))
;;

let word_n reg i =
  Il.Mem (word_mem, Some reg,
          Asm.IMM (Int64.mul (Int64.of_int i) word_sz))
;;

(*
 * Our arrangement on x86 is this:
 *
 *   *ebp+20+(4*N) = [argN   ]
 *   ...
 *   *ebp+24       = [arg1   ] = proc ptr
 *   *ebp+20       = [arg0   ] = out ptr
 *   *ebp+16       = [retpc  ]
 *   *ebp+12       = [old_ebp]
 *   *ebp+8        = [old_edi]
 *   *ebp+4        = [old_esi]
 *   *ebp          = [old_ebx]
 *
 * For x86-cdecl:
 *
 *  %eax, %ecx, %edx are "caller save" registers
 *  %ebp, %ebx, %esi, %edi are "callee save" registers
 *
 *)

let proc_ptr = word_n (Il.Hreg ebp) 6;;
let out_ptr = word_n (Il.Hreg ebp) 5;;

let load_proc_word (e:Il.emitter) (i:int) : Il.reg =
  let vr = Il.next_vreg e in
    Il.emit e Il.UMOV (Il.Reg vr) proc_ptr Il.Nil;
    Il.emit e Il.UMOV (Il.Reg vr) (word_n vr i) Il.Nil;
    vr
;;

let store_proc_word (e:Il.emitter) (i:int) (oper:Il.operand) : unit =
  let vr = Il.next_vreg e in
    Il.emit e Il.UMOV (Il.Reg vr) proc_ptr Il.Nil;
    Il.emit e Il.UMOV (word_n vr i) oper Il.Nil
;;

let load_rt_word (e:Il.emitter) (i:int) : Il.reg =
  let rt = load_proc_word e 0 in
  let vr = Il.next_vreg e in
    Il.emit e Il.UMOV (Il.Reg vr) (word_n rt i) Il.Nil;
    vr
;;

let store_rt_word (e:Il.emitter) (i:int) (oper:Il.operand) : unit =
  let rt = load_proc_word e 0 in
    Il.emit e Il.UMOV (word_n rt i) oper Il.Nil;
;;

let emit_proc_state_change (e:Il.emitter) (state:Abi.proc_state) : unit =
  let code = Abi.proc_state_to_code state in
  let r x = Il.Reg x in
  let vr = Il.next_vreg e in
  let vr_n = word_n vr in
  let emit = Il.emit e in
  let mov dst src = emit Il.UMOV dst src Il.Nil in
  let imm i = Il.Imm (Asm.IMM i) in

    mov (r vr) proc_ptr;
    mov (vr_n Abi.proc_field_state) (imm code);
;;

let emit_upcall
    (e:Il.emitter)
    (u:Abi.upcall)
    (args:Il.operand array)
    (proc_to_c_fixup:fixup)
    : unit =
  let upcall_code = Abi.upcall_to_code u in
  let state_code = Abi.upcall_to_code u in

  let r x = Il.Reg x in
  let vr = Il.next_vreg e in
  let dst = Il.next_vreg e in
  let vr_n = word_n vr in
  let emit = Il.emit e in
  let mov dst src = emit Il.UMOV dst src Il.Nil in
  let imm i = Il.Imm (Asm.IMM i) in

    assert ((Array.length args) <= Abi.max_upcall_args);

    mov (r vr) proc_ptr;
    mov (vr_n Abi.proc_field_state) (imm state_code);
    mov (vr_n Abi.proc_field_upcall_code) (imm upcall_code);

    Array.iteri
      begin
        fun i arg ->
          mov (vr_n (Abi.proc_field_upcall_args + i)) arg
      end
      args;
    emit Il.CCALL (r dst) (Il.Pcrel proc_to_c_fixup) Il.Nil
;;

let c_to_proc (e:Il.emitter) (fix:fixup) : unit =
  (*
   * This is a bit of glue-code. It should be emitted once per
   * compilation unit.
   *
   *   - save regs on C stack
   *   - save sp to rt.sp
   *   - load saved proc sp (switch stack)
   *   - restore saved proc regs
   *   - return to saved proc pc
   *
   * Our incoming stack looks like this:
   *
   *   *esp+4        = [arg1   ] = proc ptr
   *   *esp          = [retpc  ]
   *)

  let r x = Il.Reg (Il.Hreg x) in
  let sp_n = word_n (Il.Hreg esp) in
  let edx_n = word_n (Il.Hreg edx) in
  let ecx_n = word_n (Il.Hreg ecx) in
  let emit = Il.emit e in
  let mov dst src = emit Il.UMOV dst src Il.Nil in

    Il.emit_full e (Some fix) Il.DEAD Il.Nil Il.Nil Il.Nil;

    mov (r edx) (sp_n 1);                     (* edx <- proc          *)
    mov (r ecx) (edx_n Abi.proc_field_rt);    (* ecx <- proc->rt      *)
    save_callee_saves e;
    mov (ecx_n Abi.rt_field_sp) (r esp);      (* rt->regs.sp <- esp   *)
    mov (r esp) (edx_n Abi.proc_field_sp);    (* esp <- proc->regs.sp *)

    (**** IN PROC STACK ****)
    restore_callee_saves e;
    emit Il.CRET Il.Nil Il.Nil Il.Nil;
    (***********************)
  ()
;;


let proc_to_c (e:Il.emitter) (fix:fixup) : unit =

  (*
   * More glue code. Here we've been called from a proc and
   * we want to return to the saved C stack/pc. So:
   *
   *   - save regs on proc stack
   *   - save sp to proc.sp
   *   - load saved C sp (switch stack)
   *   - restore saved C regs
   *   - return to saved C pc
   *
   *   *esp          = [retpc  ]
   *)
  let r x = Il.Reg (Il.Hreg x) in
  let edx_n = word_n (Il.Hreg edx) in
  let ecx_n = word_n (Il.Hreg ecx) in
  let emit = Il.emit e in
  let mov dst src = emit Il.UMOV dst src Il.Nil in

    Il.emit_full e (Some fix) Il.DEAD Il.Nil Il.Nil Il.Nil;

    mov (r edx) proc_ptr;                     (* edx <- proc            *)
    mov (r ecx) (edx_n Abi.proc_field_rt);    (* ecx <- proc->rt        *)
    save_callee_saves e;
    mov (edx_n Abi.proc_field_sp) (r esp);    (* proc->regs.sp <- esp   *)
    mov (r esp) (ecx_n Abi.rt_field_sp);      (* esp <- rt->regs.sp     *)

    (**** IN C STACK ****)
    restore_callee_saves e;
    emit Il.CRET Il.Nil Il.Nil Il.Nil;
    (***********************)
  ()
;;


let (abi:Abi.abi) =
  {
    Abi.abi_word_sz = word_sz;
    Abi.abi_word_mem = word_mem;
    Abi.abi_word_ty = word_ty;

    Abi.abi_is_2addr_machine = true;
    Abi.abi_has_pcrel_loads = false;
    Abi.abi_has_pcrel_jumps = true;
    Abi.abi_has_imm_loads = false;
    Abi.abi_has_imm_jumps = false;

    Abi.abi_n_hardregs = n_hardregs;
    Abi.abi_str_of_hardreg = reg_str;
    Abi.abi_prealloc_quad = prealloc_quad;

    Abi.abi_emit_fn_prologue = fn_prologue;
    Abi.abi_emit_fn_epilogue = fn_epilogue;
    Abi.abi_emit_main_prologue = main_prologue;
    Abi.abi_clobbers = clobbers;

    Abi.abi_emit_proc_state_change = emit_proc_state_change;
    Abi.abi_emit_upcall = emit_upcall;
    Abi.abi_c_to_proc = c_to_proc;
    Abi.abi_proc_to_c = proc_to_c;

    Abi.abi_sp_reg = (Il.Hreg esp);
    Abi.abi_fp_reg = (Il.Hreg ebp);
    Abi.abi_pp_operand = proc_ptr;
    Abi.abi_frame_base_sz = (* eip,ebp,edi,esi,ebx *) Int64.mul 5L word_sz;
    Abi.abi_implicit_args_sz = (* proc ptr,out ptr *) Int64.mul 2L word_sz;
    Abi.abi_spill_slot = spill_slot;
  }


(*
 * NB: factor the instruction selector often. There's lots of
 * semi-redundancy in the ISA.
 *)


let imm_is_byte (n:int64) : bool =
  (i64_le (-128L) n) && (i64_le n 127L)
;;


let rm_r (oper:operand) (r:int) : Asm.item =
  let reg_ebp = 6 in
  let reg_esp = 7 in
    match oper with
        Reg (Hreg rm) ->
          Asm.BYTE (modrm_reg (reg rm) r)
      | Mem (_, None, disp) ->
          Asm.SEQ [| Asm.BYTE (modrm_deref_disp32 r);
                     Asm.WORD (TY_s32, disp) |]
      | Mem (_, Some (Hreg rm), disp) ->
          (*
           * We do a little contortion here to accommodate the special
           * case of being asked to form esp-relative addresses; these
           * require SIB bytes on x86. Of course!
           *)
          begin
            let sib_esp_base = Asm.BYTE 0x24 in
            let seq1 modrm =
              if rm = reg_esp
              then Asm.SEQ [| modrm; sib_esp_base |]
              else modrm
            in
            let seq2 modrm disp =
              if rm = reg_esp
              then Asm.SEQ [| modrm; sib_esp_base; disp |]
              else Asm.SEQ [| modrm; disp |]
            in
            match disp with
                Asm.IMM 0L when rm != reg_ebp ->
                  seq1 (Asm.BYTE (modrm_deref_reg (reg rm) r))

              (* The next two are just to save the relaxation system some churn. *)
              | Asm.IMM n when imm_is_byte n ->
                  seq2
                    (Asm.BYTE (modrm_deref_reg_plus_disp8 (reg rm) r))
                    (Asm.WORD (TY_s8, disp))
              | Asm.IMM n ->
                  seq2
                    (Asm.BYTE (modrm_deref_reg_plus_disp32 (reg rm) r))
                    (Asm.WORD (TY_s32, disp))

              | _ ->
                  Asm.new_relaxation
                    [|
                      seq2
                        (Asm.BYTE (modrm_deref_reg_plus_disp32 (reg rm) r))
                        (Asm.WORD (TY_s32, disp));
                      seq2
                        (Asm.BYTE (modrm_deref_reg_plus_disp8 (reg rm) r))
                        (Asm.WORD (TY_s8, disp))
                    |]
          end
      | _ -> raise Unrecognized
;;


let insn_rm_r (op:int) (oper:operand) (r:int) : Asm.item =
  Asm.SEQ [| Asm.BYTE op; rm_r oper r |]
;;


let insn_rm_r_imm (op:int) (oper:operand) (r:int) (ty:ty_mach) (i:Asm.expr64) : Asm.item =
  Asm.SEQ [| Asm.BYTE op; rm_r oper r; Asm.WORD (ty, i) |]
;;

let insn_rm_r_imm_s8_s32 (op8:int) (op32:int) (oper:operand) (r:int) (i:Asm.expr64) : Asm.item =
  match i with
      Asm.IMM n when imm_is_byte n ->
        insn_rm_r_imm op8 oper r TY_s8 i
    | _ ->
        Asm.new_relaxation
          [|
            insn_rm_r_imm op32 oper r TY_s32 i;
            insn_rm_r_imm op8 oper r TY_s8 i
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
        Asm.SEQ [| op32_item; Asm.WORD (TY_s32, pcrel_expr); def |];
        Asm.SEQ [| op8_item; Asm.WORD (TY_s8, pcrel_expr); def |];
      |]
;;

let insn_pcrel_simple (op32:int) (fix:fixup) : Asm.item =
  let pcrel_mark_fixup = new_fixup "ccall-pcrel mark fixup" in
  let def = Asm.DEF (pcrel_mark_fixup, Asm.MARK) in
  let pcrel_expr = (Asm.SUB (Asm.M_POS fix,
                             Asm.M_POS pcrel_mark_fixup))
  in
    Asm.SEQ [| Asm.BYTE op32; Asm.WORD (TY_s32, pcrel_expr); def |]
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
    | Reg (Hreg _) -> true
    | _ -> false
;;


let is_rm8 (oper:operand) : bool =
  (*
   * NB: you can't refer to the Hregs in question as rm8 values
   * because if you do, MOV only writes to the low 8 bit subreg.
   *)
  match oper with
      Mem (M8, _, _) -> true
    | _ -> false
;;


let cmp (a:operand) (b:operand) : Asm.item =
  match (a,b) with
      (_, Imm i) when is_rm32 a -> insn_rm_r_imm_s8_s32 0x83 0x81 a slash7 i
    | (_, Reg (Hreg r)) -> insn_rm_r 0x39 a (reg r)
    | (Reg (Hreg r), _) -> insn_rm_r 0x3b b (reg r)
    | _ -> raise Unrecognized
;;


let mov (signed:bool) (dst:operand) (src:operand) : Asm.item =
  match (signed, dst, src) with
      (_, _, Reg (Hreg r)) when is_rm8 dst ->
        insn_rm_r 0x88 dst (reg r)
    | (_, _, Reg (Hreg r)) when is_rm32 dst ->
        insn_rm_r 0x89 dst (reg r)

    (* MOVZX *)
    | (false, Reg (Hreg r), Mem (M8, _, _)) ->
        Asm.SEQ [| Asm.BYTE 0x0f; insn_rm_r 0xb6 src (reg r) |]

    (* MOVSX *)
    | (true, Reg (Hreg r), Mem (M8, _, _)) ->
        Asm.SEQ [| Asm.BYTE 0x0f; insn_rm_r 0xbe src (reg r) |]

    (* MOV *)
    | (_, Reg (Hreg r), _) when is_rm32 src ->
        insn_rm_r 0x8b src (reg r);

    | (_, _, Imm (Asm.IMM n)) when is_rm8 dst && imm_is_byte n ->
        insn_rm_r_imm 0xc6 dst slash0 TY_u8 (Asm.IMM n)

    | (_, _, Imm i) when is_rm32 dst ->
        insn_rm_r_imm 0xc7 dst slash0 TY_u32 i

    | _ -> raise Unrecognized
;;


let lea (dst:operand) (src:operand) : Asm.item =
  match (dst,src) with
      (Reg (Hreg r), Mem _) -> insn_rm_r 0x8d src (reg r)
    | _ -> raise Unrecognized
;;


let select_item_misc (q:quad) : Asm.item =
  match (q.quad_op, q.quad_dst, q.quad_lhs, q.quad_rhs) with

      (CCALL, Reg (Hreg 0), r, _) when is_rm32 r -> insn_rm_r 0xff r slash2
    | (CCALL, Reg (Hreg 0), Pcrel f, _) -> insn_pcrel_simple 0xe8 f

    | (CPUSH M32, _, Reg (Hreg r), _) -> Asm.BYTE (0x50 + (reg r))
    | (CPUSH M32, _, r, _) when is_rm32 r -> insn_rm_r 0xff r slash6
    | (CPUSH M32, _, Imm i, _) -> Asm.SEQ [| Asm.BYTE 0x68; Asm.WORD (TY_u32, i) |]
    | (CPUSH M8, _, Imm i, _) -> Asm.SEQ [| Asm.BYTE 0x6a; Asm.WORD (TY_u8, i) |]

    | (CPOP M32, r, _, _) when is_rm32 r -> insn_rm_r 0x8f r slash0

    | (CRET, _, _, _) -> Asm.BYTE 0xc3

    | (JC,  _, Pcrel f, _) -> insn_pcrel_prefix32 0x72 0x0f 0x82 f
    | (JNC, _, Pcrel f, _) -> insn_pcrel_prefix32 0x73 0x0f 0x83 f
    | (JO,  _, Pcrel f, _) -> insn_pcrel_prefix32 0x70 0x0f 0x80 f
    | (JNO, _, Pcrel f, _) -> insn_pcrel_prefix32 0x71 0x0f 0x81 f
    | (JE,  _, Pcrel f, _) -> insn_pcrel_prefix32 0x74 0x0f 0x84 f
    | (JNE, _, Pcrel f, _) -> insn_pcrel_prefix32 0x75 0x0f 0x85 f
    | (JL,  _, Pcrel f, _) -> insn_pcrel_prefix32 0x7c 0x0f 0x8c f
    | (JLE, _, Pcrel f, _) -> insn_pcrel_prefix32 0x7e 0x0f 0x8e f
    | (JG,  _, Pcrel f, _) -> insn_pcrel_prefix32 0x7f 0x0f 0x8f f
    | (JGE, _, Pcrel f, _) -> insn_pcrel_prefix32 0x7d 0x0f 0x8d f

    | (JMP, _, r, _) when is_rm32 r -> insn_rm_r 0xff r slash4
    | (JMP, _, Pcrel f, _) -> insn_pcrel 0xeb 0xe9 f

    | (DEAD, _, _, _) -> Asm.MARK
    | (END, _, _, _) -> Asm.BYTES [| 0x90 |]
    | (NOP, _, _, _) -> Asm.BYTES [| 0x90 |]

    | _ ->
        raise Unrecognized
;;


let alu_binop
    (dst:operand) (src:operand) (immslash:int)
    (rm_dst_op:int) (rm_src_op:int)
    : Asm.item =
  match (dst, src) with
      (Reg (Hreg r), _) when is_rm32 src -> insn_rm_r rm_src_op src (reg r)
    | (_, Reg (Hreg r)) when is_rm32 dst -> insn_rm_r rm_dst_op dst (reg r)
    | (_, Imm i) when is_rm32 dst -> insn_rm_r_imm_s8_s32 0x83 0x81 dst immslash i
    | _ -> raise Unrecognized
;;


let mul_like (src:operand) (signed:bool) (slash:int)
    : Asm.item =
  if is_rm32 src
  then insn_rm_r 0xf7 src slash
  else
    match src with
        Imm i ->
          Asm.SEQ [| mov signed (Reg (Hreg edx)) src;
                     insn_rm_r 0xf7 (Reg (Hreg edx)) slash |]
      | _ -> raise Unrecognized
;;


let select_insn (q:quad) : Asm.item =
  let item =
    match q.quad_op with
        UMOV -> mov false q.quad_dst q.quad_lhs
      | IMOV -> mov true q.quad_dst q.quad_lhs
      | LEA -> lea q.quad_dst q.quad_lhs
      | CMP -> cmp q.quad_lhs q.quad_rhs
      | _ ->
          begin
            if q.quad_dst = q.quad_lhs
            then
              let binop = alu_binop q.quad_lhs q.quad_rhs in
              let unop = insn_rm_r 0xf7 q.quad_lhs in
              let mulop = mul_like q.quad_rhs in
                match (q.quad_dst, q.quad_op) with
                    (_, ADD) -> binop slash0 0x1 0x3
                  | (_, SUB) -> binop slash5 0x29 0x2b
                  | (_, AND) -> binop slash4 0x21 0x23
                  | (_, OR) -> binop slash1 0x09 0x0b

                  | (Reg (Hreg 0), UMUL) -> mulop false slash4
                  | (Reg (Hreg 0), IMUL) -> mulop true slash5
                  | (Reg (Hreg 0), UDIV) -> mulop false slash6
                  | (Reg (Hreg 0), IDIV) -> mulop true slash7

                  | (Reg (Hreg 0), UMOD) -> mulop false slash6
                  | (Reg (Hreg 0), IMOD) -> mulop true slash7

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
            (Il.string_of_quad reg_str q);
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
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
