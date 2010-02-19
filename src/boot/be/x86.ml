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
    | 4 -> code_edi
    | 5 -> code_edx
        (* Never assigned by the register allocator, but synthetic code uses them *)
    | 6 -> code_ebp
    | 7 -> code_esp
    | _ -> raise (Invalid_argument "X86.reg")
;;


let dwarf_eax = 0;;
let dwarf_ecx = 1;;
let dwarf_edx = 2;;
let dwarf_ebx = 3;;
let dwarf_esp = 4;;
let dwarf_ebp = 5;;
let dwarf_esi = 6;;
let dwarf_edi = 7;;

let dwarf_reg r =
  match r with
      0 -> dwarf_eax
    | 1 -> dwarf_ecx
    | 2 -> dwarf_ebx
    | 3 -> dwarf_esi
    | 4 -> dwarf_edi
    | 5 -> dwarf_edx
    | 6 -> dwarf_ebp
    | 7 -> dwarf_esp
    | _ -> raise (Invalid_argument "X86.dwarf_reg")

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

let prealloc_quad (quad':Il.quad') : Il.quad' =
  let target_bin_to_hreg bin hreg =
    let ty = Il.cell_scalar_ty bin.Il.binary_dst in
      { bin with
          Il.binary_lhs = Il.Cell (Il.Reg ((Il.Hreg hreg), ty));
          Il.binary_dst = Il.Reg ((Il.Hreg hreg), ty) }
  in
    match quad' with
        Il.Binary bin ->
          begin
            Il.Binary
              begin
                match bin.Il.binary_op with
                    Il.IMUL | Il.UMUL
                  | Il.IDIV | Il.UDIV -> target_bin_to_hreg bin eax
                  | Il.IMOD | Il.UMOD -> target_bin_to_hreg bin edx
                  | _ -> bin
              end
          end
      | Il.Call c ->
          let ty = Il.cell_scalar_ty c.Il.call_dst in
            Il.Call { c with
                        Il.call_dst = Il.Reg ((Il.Hreg eax), ty) }
      | x -> x
;;

let clobbers (quad:Il.quad) : Il.hreg list =
  match quad.Il.quad_body with
      Il.Binary bin ->
        begin
          match bin.Il.binary_op with
              Il.IMUL | Il.UMUL
            | Il.IDIV | Il.UDIV -> [ edx ]
            | Il.IMOD | Il.UMOD -> [ eax ]
            | _ -> []
        end
    | Il.Unary un ->
        begin
          match un.Il.unary_op with
              Il.ZERO -> [ eax; edi; ecx ]
            | _ -> [ ]
        end
    | Il.Call _ -> [ eax; ecx; edx; ]
    | _ -> []
;;


let word_sz = 4L
;;

let word_bits = Il.Bits32
;;

let word_ty = TY_u32
;;

let spill_slot (framesz:int64) (i:Il.spill) : Il.mem =
  let imm = (Asm.IMM
               (Int64.neg
                  (Int64.add framesz
                     (Int64.mul word_sz
                        (Int64.of_int (i+1))))))
  in
    Il.RegIn ((Il.Hreg ebp), Some imm)
;;

let c (c:Il.cell) : Il.operand = Il.Cell c ;;
let r (r:Il.reg) : Il.cell = Il.Reg ( r, (Il.ValTy word_bits) ) ;;
let h (x:Il.hreg) : Il.reg = Il.Hreg x ;;
let rc (x:Il.hreg) : Il.cell = r (h x) ;;
let ro (x:Il.hreg) : Il.operand = c (rc x) ;;
let vreg (e:Il.emitter) : (Il.reg * Il.cell) =
  let vr = Il.next_vreg e in
    (vr, (Il.Reg (vr, (Il.ValTy word_bits))))
;;
let imm (x:Asm.expr64) : Il.operand =
  Il.Imm (x, word_ty)
;;
let immi (x:int64) : Il.operand =
  imm (Asm.IMM x)
;;

let imm_byte (x:Asm.expr64) : Il.operand =
  Il.Imm (x, TY_u8)
;;
let immi_byte (x:int64) : Il.operand =
  imm_byte (Asm.IMM x)
;;


let save_callee_saves (e:Il.emitter) : unit =
    Il.emit e (Il.Push (ro ebp));
    Il.emit e (Il.Push (ro edi));
    Il.emit e (Il.Push (ro esi));
    Il.emit e (Il.Push (ro ebx));
;;


let restore_callee_saves (e:Il.emitter) : unit =
    Il.emit e (Il.Pop (rc ebx));
    Il.emit e (Il.Pop (rc esi));
    Il.emit e (Il.Pop (rc edi));
    Il.emit e (Il.Pop (rc ebp));
;;

let word_off_n (i:int) : Asm.expr64 =
  Asm.IMM (Int64.mul (Int64.of_int i) word_sz)
;;

let word_at (reg:Il.reg) : Il.cell =
  let mem = Il.RegIn (reg, None) in
    Il.Mem (mem, Il.ScalarTy (Il.ValTy word_bits))
;;

let word_at_abs (abs:Asm.expr64) : Il.cell =
  let mem = Il.Abs abs in
    Il.Mem (mem, Il.ScalarTy (Il.ValTy word_bits))
;;

let word_n (reg:Il.reg) (i:int) : Il.cell =
  let imm = word_off_n i in
  let mem = Il.RegIn (reg, Some imm) in
    Il.Mem (mem, Il.ScalarTy (Il.ValTy word_bits))
;;

let reg_codeptr (reg:Il.reg) : Il.code =
  Il.CodePtr (Il.Cell (Il.Reg (reg, Il.AddrTy Il.CodeTy)))
;;

let word_n_low_byte (reg:Il.reg) (i:int) : Il.cell =
  let imm = word_off_n i in
  let mem = Il.RegIn (reg, Some imm) in
    Il.Mem (mem, Il.ScalarTy (Il.ValTy Il.Bits8))
;;

let wordptr_n (reg:Il.reg) (i:int) : Il.cell =
  let imm = word_off_n i in
  let mem = Il.RegIn (reg, Some imm) in
    Il.Mem (mem, Il.ScalarTy (Il.AddrTy (Il.ScalarTy (Il.ValTy word_bits))))
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

let proc_ptr = wordptr_n (Il.Hreg ebp) 6;;
let out_ptr = wordptr_n (Il.Hreg ebp) 5;;
let frame_base_sz = (* eip,ebp,edi,esi,ebx *) Int64.mul 5L word_sz;;
let implicit_args_sz = (* proc ptr,out ptr *) Int64.mul 2L word_sz;;

let emit_c_call
    (e:Il.emitter)
    (ret:Il.cell)
    (tmp1:Il.reg)
    (tmp2:Il.reg)
    (nabi:Abi.nabi)
    (in_prologue:bool)
    (fn:fixup)
    (args:Il.operand array)
    : unit =

  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in
  let binary op dst imm = emit (Il.binary op dst (c dst) (immi imm)) in

  let args =                                                      (* rust calls get proc as arg0  *)
    if nabi.Abi.nabi_convention = Abi.CONV_rust
    then Array.append [| c proc_ptr |] args
    else args
  in
  let nargs = Array.length args in
  let arg_sz = Int64.mul (Int64.of_int nargs) word_sz
  in

    mov (r tmp1) (c proc_ptr);                                    (* tmp1 = proc from argv[-1]    *)
    mov (r tmp2) (ro esp);                                        (* tmp2 = esp                   *)
    mov (word_n tmp1 Abi.proc_field_rust_sp) (c (r tmp2));        (* proc->rust_sp = tmp2         *)
    mov (rc esp) (c (word_n tmp1 Abi.proc_field_runtime_sp));     (* esp = proc->runtime_sp       *)

    binary Il.SUB (rc esp) arg_sz;                                (* make room on the stack and   *)
    binary Il.AND (rc esp) 0xfffffffffffffff0L;                   (* and 16-byte align sp         *)

    Array.iteri (fun i (arg:Il.operand) ->                        (* write arguments onto C stack *)
                     match arg with
                         Il.Cell (Il.Mem (a, ty)) ->
                           begin
                             match a with
                                 Il.RegIn (Il.Hreg base, off) when base == esp ->
                                   mov (r tmp1) (c (Il.Mem (Il.RegIn (tmp2, off), ty)));
                                   mov (word_n (h esp) i) (c (r tmp1));
                               | _ ->
                                   mov (r tmp1) arg;
                                   mov (word_n (h esp) i) (c (r tmp1));
                           end
                       | _ ->
                           mov (word_n (h esp) i) arg)
                  args;

      let fptr =
        Il.CodePtr
          (if nabi.Abi.nabi_indirect
           then Il.Cell (Il.Mem (Il.Abs (Asm.M_POS fn), Il.ScalarTy (Il.AddrTy Il.CodeTy)))
           else Il.ImmPtr ((Asm.M_POS fn), Il.CodeTy))
      in
        begin
          match ret with
              Il.Mem (Il.RegIn (Il.Hreg base, _), _) when base == esp ->
                assert (not in_prologue);
                (* If ret is esp-relative, use a temporary register until we switched stacks. *)
                emit (Il.call (r tmp1) fptr);
                mov (r tmp2) (c proc_ptr);
                mov (rc esp) (c (word_n tmp2 Abi.proc_field_rust_sp));
                mov ret (c (r tmp1));

            | _ when in_prologue ->
                (* 
                 * We have to do something a little surprising here:
                 * we're doing a 'grow' call so ebp is going to point
                 * into a dead stack frame on call-return. So we
                 * temporarily store proc-ptr into ebp and then reload
                 * esp *and* ebp via ebp->rust_sp on the other side of
                 * the call. 
                 *)
                mov (rc ebp) (c proc_ptr);
                emit (Il.call ret fptr);
                mov (rc esp) (c (word_n (h ebp) Abi.proc_field_rust_sp));
                mov (rc ebp) (ro esp);

            | _ ->
                emit (Il.call ret fptr);
                mov (r tmp2) (c proc_ptr);
                mov (rc esp) (c (word_n tmp2 Abi.proc_field_rust_sp));
        end
;;

let emit_void_prologue_call
    (e:Il.emitter)
    (nabi:Abi.nabi)
    (fn:fixup)
    (args:Il.operand array)
    : unit =
    emit_c_call e (rc eax) (h edx) (h ecx) nabi true fn args
;;

let emit_native_call
    (e:Il.emitter)
    (ret:Il.cell)
    (nabi:Abi.nabi)
    (fn:fixup)
    (args:Il.operand array)
    : unit =

  let (tmp1, _) = vreg e in
  let (tmp2, _) = vreg e in
    emit_c_call e ret tmp1 tmp2 nabi false fn args
;;

let emit_native_void_call
    (e:Il.emitter)
    (nabi:Abi.nabi)
    (fn:fixup)
    (args:Il.operand array)
    : unit =

  let (ret, _) = vreg e in
    emit_native_call e (r ret) nabi fn args
;;

let emit_native_call_in_thunk
    (e:Il.emitter)
    (ret:Il.cell)
    (nabi:Abi.nabi)
    (fn:fixup)
    (args:Il.operand array)
    : unit =

  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in

    emit_c_call e (rc eax) (h edx) (h ecx) nabi false fn args;

    match ret with
        Il.Reg (r, _) -> mov (word_at r) (ro eax)
      | _ -> mov (rc edx) (c ret);
          mov (word_at (h edx)) (ro eax)
;;

let unwind_glue
    (e:Il.emitter)
    (nabi:Abi.nabi)
    (exit_proc_fixup:fixup)
    : unit =

  let fp_n = word_n (Il.Hreg ebp) in
  let edx_n = word_n (Il.Hreg edx) in
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in
  let push x = emit (Il.Push x) in
  let pop x = emit (Il.Pop x) in
  let codefix fix = Il.CodePtr (Il.ImmPtr ((Asm.M_POS fix), Il.CodeTy)) in
  let mark fix = Il.emit_full e (Some fix) Il.Dead in
  let glue_field = Abi.frame_glue_fns_field_drop in

  let repeat_jmp_fix = new_fixup "repeat jump" in
  let skip_jmp_fix = new_fixup "skip jump" in
  let exit_jmp_fix = new_fixup "exit jump" in

    mov (rc edx) (c proc_ptr);                      (* switch back to rust stack    *)
    mov (rc esp) (c (edx_n Abi.proc_field_rust_sp));

    mark repeat_jmp_fix;
    mov (rc edx) (c (fp_n (-1)));                   (* edx <- frame glue functions. *)
    emit (Il.cmp (ro edx) (immi 0L));

    emit (Il.jmp Il.JE (codefix skip_jmp_fix));     (* if struct* is nonzero        *)
    mov (rc ecx) (c (edx_n glue_field));            (* edx <- drop glue             *)
    emit (Il.cmp (ro ecx) (immi 0L));

    emit (Il.jmp Il.JE (codefix skip_jmp_fix));     (* if glue-fn is nonzero        *)
    push (ro ebp);                                  (* frame-to-drop                *)
    push (c proc_ptr);                              (* form usual call to glue      *)
    push (immi 0L);                                 (* outptr                       *)
    emit (Il.call (rc eax) (reg_codeptr (h ecx)));  (* call glue_fn, trashing eax.  *)
    pop (rc eax);
    pop (rc eax);
    pop (rc eax);

    mark skip_jmp_fix;
    mov (rc edx) (c (fp_n 3));                      (* load next fp (callee-saves[3]) *)
    emit (Il.cmp (ro edx) (immi 0L));
    emit (Il.jmp Il.JE (codefix exit_jmp_fix));     (* if nonzero                     *)
    mov (rc ebp) (ro edx);                          (* move to next frame             *)
    emit (Il.jmp Il.JMP (codefix repeat_jmp_fix));  (* loop                           *)

    (* exit path. *)
    mark exit_jmp_fix;

    emit_native_call_in_thunk e (rc eax) nabi exit_proc_fixup [| (c proc_ptr) |];
;;


let fn_prologue
    (e:Il.emitter)
    (framesz:int64)
    (spill_fixup:fixup)
    (callsz:int64)
    (nabi:Abi.nabi)
    (grow_proc_fixup:fixup)
    : unit =
  (*
   *  - save callee-saves
   *  - load esp into eax
   *  - subtract sz from eax
   *  - load bot = proc->stk->data (bottom of stack chunk)
   *  - compare bot to eax
   *  - fwd jump if bot <= eax
   *  - emit prologue-upcall grow_proc
   *  - fwd jump target
   *  - subtract frame size from esp
   *  - zero frame (for gc)
   *)
  let ecx_n = word_n (h ecx) in
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in
  let add = Int64.add in

  (*
   *  After we save callee-saves, We have a stack like this:
   *
   *  | ...           |
   *  | caller frame  |
   *  | + spill       |
   *  | caller arg K  |
   *  | ...           |
   *  | caller arg 0  | 
   *  | retpc         | <-- sp we received, top of callee frame
   *  | callee save 1 |
   *  | ...           |
   *  | callee save N | <-- ebp and esp after saving callee-saves
   *  | ...           |
   *  | callee frame  |
   *  | + spill       |
   *  | callee arg J  |
   *  | ...           |
   *  | callee arg 0  | <-- bottom of callee frame
   *  | next retpc    |
   *  | next save 1   |
   *  | ...           |
   *  | next save N   | <-- bottom of region we must reserve
   *  | ...           |
   * 
   * A "frame base" is the retpc and set of callee-saves.
   * 
   * We need to reserve room for our frame *and* the next frame-base,
   * because we're going to be blindly entering the next frame-base
   * (pushing eip and callee-saves) before we perform the next check.
   *)

  let callee_frame_sz = (Asm.ADD ((Asm.IMM (add framesz callsz)),
                                  Asm.M_SZ spill_fixup))
  in

  (* Amount of memory we need to *have* in this chunk, beneath sp
   * (after saving), to not-grow. Add in one more term of frame_base_sz
   * worth to permit a 'grow' upcall to occur!
   *)
  let reserve_current_sz =
    (Asm.ADD (callee_frame_sz, (Asm.IMM frame_base_sz)))
  in

    (* We must have room to save regs on entry. *)
    save_callee_saves e;

    mov (rc ebp) (ro esp);                        (* Establish frame base.   *)

    mov (rc eax) (ro esp);                        (* eax = esp               *)
    emit (Il.binary Il.SUB (rc eax)
            (ro eax) (imm reserve_current_sz));
    mov (rc ecx) (c proc_ptr);                    (* ecx = proc              *)
    mov (rc ecx) (c (ecx_n Abi.proc_field_stk));  (* ecx = proc->stk         *)
    emit (Il.binary Il.ADD (rc ecx) (ro ecx)      (* ecx = &stk->data[0]     *)
            (imm (word_off_n Abi.stk_field_data)));

    (* Compare and possibly upcall to 'grow'. *)
    (* The case we *want* to jump for is the 'not underflowing' case,
     * which is when ecx (the bottom) is less than or equal to
     * eax (the proposed new sp) 
     *)
    emit (Il.cmp (ro ecx) (ro eax));
    let jmp_pc = e.Il.emit_pc in

      emit (Il.jmp Il.JBE Il.CodeNone);

      emit_void_prologue_call e nabi grow_proc_fixup [| imm reserve_current_sz |];

      Il.patch_jump e jmp_pc e.Il.emit_pc;

      (* Now set up a frame, wherever we landed. *)
      emit (Il.binary Il.SUB (rc esp) (ro esp) (imm callee_frame_sz));

      (* Zero the frame
       * 
       * FIXME: this is awful, will go away when we have proper CFI.
       *)
      mov (rc edi) (ro esp);
      mov (rc ecx) (imm callee_frame_sz);
      emit (Il.unary Il.ZERO (word_at (h edi)) (ro ecx));
;;


let fn_epilogue (e:Il.emitter) : unit =

  (* Tear down existing frame. *)
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in
    mov (rc esp) (ro ebp);
    restore_callee_saves e;
    emit Il.Ret;
;;


let objfile_start
    (e:Il.emitter)
    ~(start_fixup:fixup)
    ~(rust_start_fixup:fixup)
    ~(main_fn_fixup:fixup)
    ~(crate_fixup:fixup)
    ~(indirect_start:bool)
    : unit =
  Il.emit_full e (Some start_fixup) Il.Dead;
  save_callee_saves e;
  Il.emit e (Il.umov (rc ebp) (ro esp));
  Il.emit e (Il.Push (immi 0L));
  Il.emit e (Il.Push (imm (Asm.M_POS crate_fixup)));
  Il.emit e (Il.Push (imm (Asm.M_POS main_fn_fixup)));
  let fptr =
    Il.CodePtr
      (if indirect_start
       then Il.Cell (Il.Mem (Il.Abs (Asm.M_POS rust_start_fixup),
                             Il.ScalarTy (Il.AddrTy Il.CodeTy)))
       else Il.ImmPtr ((Asm.M_POS rust_start_fixup), Il.CodeTy))
  in
    Il.emit e (Il.call (rc eax) fptr);
    Il.emit e (Il.Pop (rc ecx));
    Il.emit e (Il.Pop (rc ecx));
    Il.emit e (Il.umov (rc esp) (ro ebp));
    restore_callee_saves e;
    Il.emit e Il.Ret;
;;

let c_to_proc_glue (e:Il.emitter) : unit =
  (*
   * This is a bit of glue-code. It should be emitted once per
   * compilation unit.
   *
   *   - save regs on C stack
   *   - align sp on a 16-byte boundary
   *   - save sp to proc.runtime_sp (runtime_sp is thus always aligned)
   *   - load saved proc sp (switch stack)
   *   - restore saved proc regs
   *   - return to saved proc pc
   *
   * Our incoming stack looks like this:
   *
   *   *esp+4        = [arg1   ] = proc ptr
   *   *esp          = [retpc  ]
   *)

  let sp_n = word_n (Il.Hreg esp) in
  let edx_n = word_n (Il.Hreg edx) in
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in

    mov (rc edx) (c (sp_n 1));                       (* edx <- proc             *)
    save_callee_saves e;
    mov (edx_n Abi.proc_field_runtime_sp) (ro esp);  (* proc->runtime_sp <- esp *)
    mov (rc esp) (c (edx_n Abi.proc_field_rust_sp)); (* esp <- proc->rust_sp    *)

    (**** IN PROC STACK ****)
    restore_callee_saves e;
    emit Il.Ret;
    (***********************)
  ()
;;

let yield_glue (e:Il.emitter) : unit =

  (*
   * More glue code. Here we've been called from a proc and
   * we want to return to the saved C stack/pc. So:
   *
   *   - save regs on proc stack
   *   - save sp to proc.sp
   *   - load saved C sp (switch stack)
   *   - undo alignment (see c_to_proc)
   *   - restore saved C regs
   *   - return to saved C pc
   *
   *   *esp          = [retpc  ]
   *)
  let esp_n = word_n (Il.Hreg esp) in
  let edx_n = word_n (Il.Hreg edx) in
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in

    mov (rc edx) (c (esp_n 0));                         (* edx <- arg0 (proc)      *)
    mov (rc esp) (c (edx_n Abi.proc_field_rust_sp));    (* esp <- proc->rust_sp    *)
    save_callee_saves e;
    mov (edx_n Abi.proc_field_rust_sp) (ro esp);        (* proc->rust_sp <- esp    *)
    mov (rc esp) (c (edx_n Abi.proc_field_runtime_sp)); (* esp <- proc->runtime_sp *)

    (**** IN C STACK ****)
    restore_callee_saves e;
    emit Il.Ret;
    (***********************)
  ()
;;


let get_next_pc_thunk : (Il.reg * fixup * (Il.emitter -> unit)) =
  let get_next_pc_thunk_fixup = new_fixup "glue$get_next_pc" in
  let emit_get_next_pc_thunk (e:Il.emitter) : unit =
    let sty = Il.AddrTy Il.CodeTy in
    let rty = Il.ScalarTy sty in
    let deref_esp = Il.Mem (Il.RegIn (Il.Hreg esp, None), rty) in
    let eax = (Il.Reg (Il.Hreg eax, sty)) in
      Il.emit_full e (Some get_next_pc_thunk_fixup)
        (Il.umov eax (Il.Cell deref_esp));
      Il.emit e Il.Ret;
  in
    (Il.Hreg eax, get_next_pc_thunk_fixup, emit_get_next_pc_thunk)
;;

let (abi:Abi.abi) =
  {
    Abi.abi_word_sz = word_sz;
    Abi.abi_word_bits = word_bits;
    Abi.abi_word_ty = word_ty;

    Abi.abi_is_2addr_machine = true;
    Abi.abi_has_absin_data = false; (* abs-in data = N/A          *)
    Abi.abi_has_absin_code = true;  (* abs-in code = deref_disp32 *)
    Abi.abi_has_abs_data = true;    (* abs data == deref_disp32   *)
    Abi.abi_has_abs_code = true;    (* abs code == pcrel          *)

    Abi.abi_n_hardregs = n_hardregs;
    Abi.abi_str_of_hardreg = reg_str;
    Abi.abi_prealloc_quad = prealloc_quad;

    Abi.abi_emit_fn_prologue = fn_prologue;
    Abi.abi_emit_fn_epilogue = fn_epilogue;
    Abi.abi_clobbers = clobbers;

    Abi.abi_emit_native_call = emit_native_call;
    Abi.abi_emit_native_void_call = emit_native_void_call;
    Abi.abi_emit_native_call_in_thunk = emit_native_call_in_thunk;
    Abi.abi_c_to_proc = c_to_proc_glue;
    Abi.abi_yield = yield_glue;
    Abi.abi_unwind = unwind_glue;
    Abi.abi_get_next_pc_thunk = Some get_next_pc_thunk;

    Abi.abi_sp_reg = (Il.Hreg esp);
    Abi.abi_fp_reg = (Il.Hreg ebp);
    Abi.abi_dwarf_fp_reg = dwarf_ebp;
    Abi.abi_pp_cell = proc_ptr;
    Abi.abi_frame_base_sz = frame_base_sz;
    Abi.abi_implicit_args_sz = implicit_args_sz;
    Abi.abi_spill_slot = spill_slot;
  }


(*
 * NB: factor the instruction selector often. There's lots of
 * semi-redundancy in the ISA.
 *)


let imm_is_signed_byte (n:int64) : bool =
  (i64_le (-128L) n) && (i64_le n 127L)
;;

let imm_is_unsigned_byte (n:int64) : bool =
  (i64_le (0L) n) && (i64_le n 255L)
;;


let rm_r (c:Il.cell) (r:int) : Asm.frag =
  let reg_ebp = 6 in
  let reg_esp = 7 in

  (* 
   * We do a little contortion here to accommodate the special case of
   * being asked to form esp-relative addresses; these require SIB
   * bytes on x86. Of course!
   *)
  let sib_esp_base = Asm.BYTE 0x24 in
  let seq1 rm modrm =
    if rm = reg_esp
    then Asm.SEQ [| modrm; sib_esp_base |]
    else modrm
  in
  let seq2 rm modrm disp =
    if rm = reg_esp
    then Asm.SEQ [| modrm; sib_esp_base; disp |]
    else Asm.SEQ [| modrm; disp |]
  in

    match c with
        Il.Reg ((Il.Hreg rm), _) ->
          Asm.BYTE (modrm_reg (reg rm) r)
      | Il.Mem (a, _) ->
          begin
            match a with
                Il.Abs disp ->
                  Asm.SEQ [| Asm.BYTE (modrm_deref_disp32 r);
                             Asm.WORD (TY_s32, disp) |]

              | Il.RegIn ((Il.Hreg rm), None) when rm != reg_ebp ->
                  seq1 rm (Asm.BYTE (modrm_deref_reg (reg rm) r))

              | Il.RegIn ((Il.Hreg rm), Some (Asm.IMM 0L)) when rm != reg_ebp ->
                  seq1 rm (Asm.BYTE (modrm_deref_reg (reg rm) r))

              (* The next two are just to save the relaxation system some churn. *)
              | Il.RegIn ((Il.Hreg rm), Some (Asm.IMM n)) when imm_is_signed_byte n ->
                  seq2 rm
                    (Asm.BYTE (modrm_deref_reg_plus_disp8 (reg rm) r))
                    (Asm.WORD (TY_s8, Asm.IMM n))

              | Il.RegIn ((Il.Hreg rm), Some (Asm.IMM n)) ->
                  seq2 rm
                    (Asm.BYTE (modrm_deref_reg_plus_disp32 (reg rm) r))
                    (Asm.WORD (TY_s32, Asm.IMM n))

              | Il.RegIn ((Il.Hreg rm), Some disp) ->
                  Asm.new_relaxation
                    [|
                      seq2 rm
                        (Asm.BYTE (modrm_deref_reg_plus_disp32 (reg rm) r))
                        (Asm.WORD (TY_s32, disp));
                      seq2 rm
                        (Asm.BYTE (modrm_deref_reg_plus_disp8 (reg rm) r))
                        (Asm.WORD (TY_s8, disp))
                    |]
              | _ -> raise Unrecognized
          end
      | _ -> raise Unrecognized
;;


let insn_rm_r (op:int) (c:Il.cell) (r:int) : Asm.frag =
  Asm.SEQ [| Asm.BYTE op; rm_r c r |]
;;


let insn_rm_r_imm (op:int) (c:Il.cell) (r:int) (ty:ty_mach) (i:Asm.expr64) : Asm.frag =
  Asm.SEQ [| Asm.BYTE op; rm_r c r; Asm.WORD (ty, i) |]
;;

let insn_rm_r_imm_s8_s32 (op8:int) (op32:int) (c:Il.cell) (r:int) (i:Asm.expr64) : Asm.frag =
  match i with
      Asm.IMM n when imm_is_signed_byte n ->
        insn_rm_r_imm op8 c r TY_s8 i
    | _ ->
        Asm.new_relaxation
          [|
            insn_rm_r_imm op32 c r TY_s32 i;
            insn_rm_r_imm op8 c r TY_s8 i
          |]
;;

let insn_rm_r_imm_u8_u32 (op8:int) (op32:int) (c:Il.cell) (r:int) (i:Asm.expr64) : Asm.frag =
  match i with
      Asm.IMM n when imm_is_unsigned_byte n ->
        insn_rm_r_imm op8 c r TY_u8 i
    | _ ->
        Asm.new_relaxation
          [|
            insn_rm_r_imm op32 c r TY_u32 i;
            insn_rm_r_imm op8 c r TY_u8 i
          |]
;;


let insn_pcrel_relax
    (op8_frag:Asm.frag)
    (op32_frag:Asm.frag)
    (fix:fixup)
    : Asm.frag =
  let pcrel_mark_fixup = new_fixup "pcrel mark fixup" in
  let def = Asm.DEF (pcrel_mark_fixup, Asm.MARK) in
  let pcrel_expr = (Asm.SUB (Asm.M_POS fix,
                             Asm.M_POS pcrel_mark_fixup))
  in
    Asm.new_relaxation
      [|
        Asm.SEQ [| op32_frag; Asm.WORD (TY_s32, pcrel_expr); def |];
        Asm.SEQ [| op8_frag; Asm.WORD (TY_s8, pcrel_expr); def |];
      |]
;;

let insn_pcrel_simple (op32:int) (fix:fixup) : Asm.frag =
  let pcrel_mark_fixup = new_fixup "pcrel mark fixup" in
  let def = Asm.DEF (pcrel_mark_fixup, Asm.MARK) in
  let pcrel_expr = (Asm.SUB (Asm.M_POS fix,
                             Asm.M_POS pcrel_mark_fixup))
  in
    Asm.SEQ [| Asm.BYTE op32; Asm.WORD (TY_s32, pcrel_expr); def |]
;;

let insn_pcrel (op8:int) (op32:int) (fix:fixup) : Asm.frag =
  insn_pcrel_relax (Asm.BYTE op8) (Asm.BYTE op32) fix
;;

let insn_pcrel_prefix32 (op8:int) (prefix32:int) (op32:int) (fix:fixup) : Asm.frag =
  insn_pcrel_relax (Asm.BYTE op8) (Asm.BYTES [| prefix32; op32 |]) fix
;;

let is_ty32 (ty:Il.scalar_ty) : bool =
  match ty with
      Il.ValTy (Il.Bits32) -> true
    | Il.AddrTy _ -> true
    | _ -> false

let is_rm32 (c:Il.cell) : bool =
  match c with
      Il.Mem (_, Il.ScalarTy st) -> is_ty32 st
    | Il.Reg (_, st) -> is_ty32 st
    | _ -> false
;;

let is_ty8 (ty:Il.scalar_ty) : bool =
  match ty with
      Il.ValTy (Il.Bits8) -> true
    | _ -> false
;;

let is_m8 (c:Il.cell) : bool =
  match c with
      Il.Mem (_, Il.ScalarTy st) -> is_ty8 st
    | _ -> false
;;

let is_r8 (c:Il.cell) : bool =
  match c with
      Il.Reg (_, st) -> is_ty8 st
    | _ -> false
;;

let is_rm8 (c:Il.cell) : bool =
  match c with
      Il.Mem (_, Il.ScalarTy st) -> is_ty8 st
    | Il.Reg (_, st) -> is_ty8 st
    | _ -> false
;;

(* FIXME: tighten imm-based dispatch by imm type. *)
let cmp (a:Il.operand) (b:Il.operand) : Asm.frag =
  match (a,b) with
      (Il.Cell c, Il.Imm (i, TY_s8)) when is_rm8 c ->
        insn_rm_r_imm 0x80 c slash7 TY_s8 i
    | (Il.Cell c, Il.Imm (i, TY_u8)) when is_rm8 c ->
        insn_rm_r_imm 0x80 c slash7 TY_u8 i
    | (Il.Cell c, Il.Imm (i, _)) when is_rm32 c ->
        (* 
         * NB: We can't switch on signed-ness here, as 'cmp' is
         * defined to sign-extend its operand; i.e. we have to treat
         * it as though you're emitting a signed byte (in the sense of
         * immediate-size selection) even if the incoming value is
         * unsigned.
         *)
        insn_rm_r_imm_s8_s32 0x83 0x81 c slash7 i
    | (Il.Cell c, Il.Cell (Il.Reg (Il.Hreg r, _))) ->
        insn_rm_r 0x39 c (reg r)
    | (Il.Cell (Il.Reg (Il.Hreg r, _)), Il.Cell c) ->
        insn_rm_r 0x3b c (reg r)
    | _ -> raise Unrecognized
;;

let zero (dst:Il.cell) (count:Il.operand) : Asm.frag =
  match (dst, count) with

      ((Il.Mem (Il.RegIn ((Il.Hreg dst_ptr), None), _)),
       Il.Cell (Il.Reg ((Il.Hreg count), _)))
        when dst_ptr = edi && count = ecx ->
          Asm.BYTES [|
            0xb0; 0x0;  (* mov %eax, 0 : move a zero into al. *)
            0xf3; 0xaa; (* rep stos m8 : fill ecx bytes at [edi] with al *)
          |]

    | _ -> raise Unrecognized
;;

let mov (signed:bool) (dst:Il.cell) (src:Il.operand) : Asm.frag =

  match (signed, dst, src) with

      (* m8 <- r8 only. *)
      (_,  _, Il.Cell (Il.Reg ((Il.Hreg r), src_ty)))
        when is_m8 dst && is_ty8 src_ty ->
          insn_rm_r 0x88 dst (reg r)

    (* r8 <- r8: treat as r32 <- r32. *)
    | (_,  Il.Reg ((Il.Hreg r), _), Il.Cell src_cell)
        when is_r8 dst && is_r8 src_cell ->
        insn_rm_r 0x8b src_cell (reg r)

    (* rm32 <- r32 *)
    | (_,  _, Il.Cell (Il.Reg ((Il.Hreg r), src_ty)))
        when (is_r8 dst || is_rm32 dst) && is_ty32 src_ty ->
        insn_rm_r 0x89 dst (reg r)

    (* r32 <- rm32 *)
    | (_,  (Il.Reg ((Il.Hreg r), dst_ty)), Il.Cell src_cell)
        when is_ty32 dst_ty && is_rm32 src_cell ->
          insn_rm_r 0x8b src_cell (reg r)

    (* MOVZX: r8/r32 <- zx(rm8) *)
    | (false, Il.Reg ((Il.Hreg r, dst_ty)), Il.Cell src_cell)
        when (is_ty8 dst_ty || is_ty32 dst_ty) && is_rm8 src_cell ->
        Asm.SEQ [| Asm.BYTE 0x0f;
                   insn_rm_r 0xb6 src_cell (reg r) |]

    (* MOVSX: r8/r32 <- sx(rm8) *)
    | (true, Il.Reg ((Il.Hreg r), dst_ty), Il.Cell src_cell)
        when (is_ty8 dst_ty || is_ty32 dst_ty) && is_rm8 src_cell ->
        Asm.SEQ [| Asm.BYTE 0x0f;
                   insn_rm_r 0xbe src_cell (reg r) |]

    (* m8 <- imm8 (signed) *)
    | (_, _, Il.Imm ((Asm.IMM n), _))
        when is_m8 dst && imm_is_signed_byte n && signed ->
          insn_rm_r_imm 0xc6 dst slash0 TY_s8 (Asm.IMM n)

    (* m8 <- imm8 (unsigned) *)
    | (_, _, Il.Imm ((Asm.IMM n), _))
        when is_m8 dst && imm_is_unsigned_byte n && (not signed) ->
          insn_rm_r_imm 0xc6 dst slash0 TY_u8 (Asm.IMM n)

    (* rm32 <- imm32 *)
    | (_, _, Il.Imm (i, _)) when is_rm32 dst || is_r8 dst ->
        let t = if signed then TY_u32 else TY_s32 in
          insn_rm_r_imm 0xc7 dst slash0 t i

    (* rm32 <- immptr32 *)
    | (_, _, Il.ImmPtr (i, _)) when is_rm32 dst ->
        let t = if signed then TY_u32 else TY_s32 in
          insn_rm_r_imm 0xc7 dst slash0 t i

    | _ -> raise Unrecognized
;;


let lea (dst:Il.cell) (mem:Il.mem) : Asm.frag =
  match dst with
      Il.Reg ((Il.Hreg r), dst_ty) when is_ty32 dst_ty ->
        insn_rm_r 0x8d (Il.Mem (mem, Il.OpaqueTy)) (reg r)

    | _ -> raise Unrecognized
;;


let select_insn_misc (q:Il.quad') : Asm.frag =

  match q with
      Il.Call c ->
        begin
          match c.Il.call_dst with
              Il.Reg ((Il.Hreg dst), _) when dst = eax ->
                begin
                  match c.Il.call_targ with

                      Il.CodePtr (Il.Cell c)
                        when Il.cell_referent_ty c = Il.ScalarTy (Il.AddrTy Il.CodeTy) ->
                          insn_rm_r 0xff c slash2

                    | Il.CodePtr (Il.ImmPtr (Asm.M_POS f, Il.CodeTy)) ->
                        insn_pcrel_simple 0xe8 f

                    | _ -> raise Unrecognized
                end
            | _ -> raise Unrecognized
        end

    | Il.Push (Il.Cell (Il.Reg ((Il.Hreg r), t))) when is_ty32 t ->
        Asm.BYTE (0x50 + (reg r))

    | Il.Push (Il.Cell c) when is_rm32 c ->
        insn_rm_r 0xff c slash6

    | Il.Push (Il.Imm (Asm.IMM i, _)) when imm_is_unsigned_byte i ->
        Asm.SEQ [| Asm.BYTE 0x6a; Asm.WORD (TY_u8, (Asm.IMM i)) |]

    | Il.Push (Il.Imm (i, _)) ->
        Asm.SEQ [| Asm.BYTE 0x68; Asm.WORD (TY_u32, i) |]

    | Il.Pop (Il.Reg ((Il.Hreg r), t)) when is_ty32 t ->
        Asm.BYTE (0x58 + (reg r))

    | Il.Pop c when is_rm32 c ->
        insn_rm_r 0x8f c slash0

    | Il.Ret -> Asm.BYTE 0xc3

    | Il.Jmp j ->
        begin
          match (j.Il.jmp_op, j.Il.jmp_targ) with

              (Il.JMP, Il.CodePtr (Il.ImmPtr (Asm.M_POS f, Il.CodeTy))) ->
                insn_pcrel 0xeb 0xe9 f

            | (Il.JMP, Il.CodePtr (Il.Cell c))
                when Il.cell_referent_ty c = Il.ScalarTy (Il.AddrTy Il.CodeTy) ->
                insn_rm_r 0xff c slash4

            (* FIXME: refactor this to handle cell-based jumps
             * if we ever need them. So far not. *)
            | (_, Il.CodePtr (Il.ImmPtr (Asm.M_POS f, Il.CodeTy))) ->
                let (op8, op32) =
                  match j.Il.jmp_op with
                    | Il.JC  -> (0x72, 0x82)
                    | Il.JNC -> (0x73, 0x83)
                    | Il.JZ  -> (0x74, 0x84)
                    | Il.JNZ -> (0x75, 0x85)
                    | Il.JO  -> (0x70, 0x80)
                    | Il.JNO -> (0x71, 0x81)
                    | Il.JE  -> (0x74, 0x84)
                    | Il.JNE -> (0x75, 0x85)

                    | Il.JL  -> (0x7c, 0x8c)
                    | Il.JLE -> (0x7e, 0x8e)
                    | Il.JG  -> (0x7f, 0x8f)
                    | Il.JGE -> (0x7d, 0x8d)

                    | Il.JB  -> (0x72, 0x82)
                    | Il.JBE -> (0x76, 0x86)
                    | Il.JA  -> (0x77, 0x87)
                    | Il.JAE -> (0x73, 0x83)
                    | _ -> raise Unrecognized
                in
                let prefix32 = 0x0f in
                  insn_pcrel_prefix32 op8 prefix32 op32 f

            | _ -> raise Unrecognized
        end

    | Il.Dead -> Asm.MARK
    | Il.Debug -> Asm.BYTES [| 0xcc |] (* int 3 *)
    | Il.End -> Asm.BYTES [| 0x90 |]
    | Il.Nop -> Asm.BYTES [| 0x90 |]
    | _ -> raise Unrecognized
;;


type alu_binop_codes =
     {
       insn: string;
       immslash: int;    (* mod/rm "slash" code for imm-src variant *)
       rm_dst_op8: int;  (* opcode for 8-bit r/m dst variant *)
       rm_dst_op32: int; (* opcode for 32-bit r/m dst variant *)
       rm_src_op8: int;  (* opcode for 8-bit r/m src variant *)
       rm_src_op32: int; (* opcode for 32-bit r/m src variant *)
     }
;;

let alu_binop
    (dst:Il.cell) (src:Il.operand) (codes:alu_binop_codes)
    : Asm.frag =
  match (dst, src) with
      (Il.Reg ((Il.Hreg r), dst_ty), Il.Cell c)
        when (is_ty32 dst_ty && is_rm32 c) || (is_ty8 dst_ty && is_rm8 c)
          -> insn_rm_r codes.rm_src_op32 c (reg r)

    | (_, Il.Cell (Il.Reg ((Il.Hreg r), src_ty)))
        when (is_rm32 dst && is_ty32 src_ty) || (is_rm8 dst && is_ty8 src_ty)
          -> insn_rm_r codes.rm_dst_op32 dst (reg r)

    | (_, Il.Imm (i, _)) when is_rm32 dst || is_rm8 dst
        -> insn_rm_r_imm_s8_s32 0x83 0x81 dst codes.immslash i

    | _ -> raise Unrecognized
;;


let mul_like (src:Il.operand) (signed:bool) (slash:int)
    : Asm.frag =
  match src with
      Il.Cell src when is_rm32 src ->
        insn_rm_r 0xf7 src slash

    | Il.Imm _ ->
        let tmp = Il.Reg ((Il.Hreg edx), Il.ValTy Il.Bits32) in
        Asm.SEQ [| mov signed tmp src;
                   insn_rm_r 0xf7 tmp slash |]

    | _ -> raise Unrecognized
;;


let select_insn (q:Il.quad) : Asm.frag =
  match q.Il.quad_body with
      Il.Unary u ->
        let unop s =
          if u.Il.unary_src = Il.Cell u.Il.unary_dst
          then insn_rm_r 0xf7 u.Il.unary_dst s
          else raise Unrecognized
        in
          begin
            match u.Il.unary_op with
                Il.UMOV -> mov false u.Il.unary_dst u.Il.unary_src
              | Il.IMOV -> mov true u.Il.unary_dst u.Il.unary_src
              | Il.NEG -> unop slash3
              | Il.NOT -> unop slash2
              | Il.ZERO -> zero u.Il.unary_dst u.Il.unary_src
          end

    | Il.Lea le -> lea le.Il.lea_dst le.Il.lea_src

    | Il.Cmp c -> cmp c.Il.cmp_lhs c.Il.cmp_rhs

    | Il.Binary b ->
        begin
          if Il.Cell b.Il.binary_dst = b.Il.binary_lhs
          then
            let binop = alu_binop b.Il.binary_dst b.Il.binary_rhs in
            let mulop = mul_like b.Il.binary_rhs in
              match (b.Il.binary_dst, b.Il.binary_op) with
                  (_, Il.ADD) -> binop { insn="ADD";
                                         immslash=slash0;
                                         rm_dst_op8=0x0;
                                         rm_dst_op32=0x1;
                                         rm_src_op8=0x2;
                                         rm_src_op32=0x3; }
                | (_, Il.SUB) -> binop { insn="SUB";
                                         immslash=slash5;
                                         rm_dst_op8=0x28;
                                         rm_dst_op32=0x29;
                                         rm_src_op8=0x2a;
                                         rm_src_op32=0x2b; }
                | (_, Il.AND) -> binop { insn="AND";
                                         immslash=slash4;
                                         rm_dst_op8=0x20;
                                         rm_dst_op32=0x21;
                                         rm_src_op8=0x22;
                                         rm_src_op32=0x23; }
                | (_, Il.OR) -> binop { insn="OR";
                                        immslash=slash1;
                                        rm_dst_op8=0x08;
                                        rm_dst_op32=0x09;
                                        rm_src_op8=0x0a;
                                        rm_src_op32=0x0b; }

                | (Il.Reg (Il.Hreg r, t), Il.UMUL)
                    when is_ty32 t && r = eax -> mulop false slash4

                | (Il.Reg (Il.Hreg r, t), Il.IMUL)
                    when is_ty32 t && r = eax -> mulop true slash5

                | (Il.Reg (Il.Hreg r, t), Il.UDIV)
                    when is_ty32 t && r = eax -> mulop false slash6

                | (Il.Reg (Il.Hreg r, t), Il.IDIV)
                    when is_ty32 t && r = eax -> mulop true slash7

                | (Il.Reg (Il.Hreg r, t), Il.UMOD)
                    when is_ty32 t && r = edx -> mulop false slash6

                | (Il.Reg (Il.Hreg r, t), Il.IMOD)
                    when is_ty32 t && r = edx -> mulop true slash7

                | _ -> raise Unrecognized
          else raise Unrecognized
        end
    | _ -> select_insn_misc q.Il.quad_body
;;


let new_emitter _ : Il.emitter =
  Il.new_emitter
    abi.Abi.abi_prealloc_quad
    abi.Abi.abi_is_2addr_machine
;;

let select_insns (sess:Session.sess) (q:Il.quads) : Asm.frag =
  let scopes = Stack.create () in
  let fixups = Stack.create () in
  let pop_frags _ =
    Asm.SEQ (Array.of_list
               (List.rev
                  (!(Stack.pop scopes))))
  in
    ignore (Stack.push (ref []) scopes);
    for i = 0 to (Array.length q) - 1 do
      let append frag =
        let frags = Stack.top scopes in
          frags := frag :: (!frags)
      in
        begin
          match q.(i).Il.quad_fixup with
              None -> ()
            | Some f -> append (Asm.DEF (f, Asm.MARK))
        end;
        begin
          match q.(i).Il.quad_body with
              Il.Enter f ->
                Stack.push f fixups;
                Stack.push (ref []) scopes;
            | Il.Leave ->
                append (Asm.DEF (Stack.pop fixups, pop_frags ()))
            | _ ->
                try
                  append (select_insn q.(i))
                with
                    Unrecognized ->
                      Session.fail sess
                        "E:Assembly error: unrecognized quad: %s\n%!"
                        (Il.string_of_quad reg_str q.(i));
                      ()
        end
    done;
    pop_frags()
;;

let frags_of_emitted_quads (sess:Session.sess) (e:Il.emitter) : Asm.frag =
  let frag = select_insns sess e.Il.emit_quads in
    if sess.Session.sess_failed
    then raise Unrecognized
    else frag
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
