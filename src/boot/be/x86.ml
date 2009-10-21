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
    let ty = Il.cell_ty bin.Il.binary_dst in
      { bin with
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
          let ty = Il.cell_ty c.Il.call_dst in
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
    | Il.Call _ -> [ eax; ecx; edx; ]
    | _ -> []
;;


let word_sz = 4L
;;

let word_bits = Il.Bits32
;;

let word_ty = TY_u32
;;

let spill_slot (framesz:int64) (i:Il.spill) : Il.addr =
  let imm = (Asm.IMM
               (Int64.neg
                  (Int64.add framesz
                     (Int64.mul word_sz
                        (Int64.of_int (i+1))))))
  in
    Il.Based ((Il.Hreg ebp), Some imm)
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
  Il.Imm (x, Il.ValTy word_bits)
;;
let immi (x:int64) : Il.operand =
  imm (Asm.IMM x)
;;

let imm_byte (x:Asm.expr64) : Il.operand =
  Il.Imm (x, Il.ValTy Il.Bits8)
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

let word_n (reg:Il.reg) (i:int) : Il.cell =
  let imm = Asm.IMM (Int64.mul (Int64.of_int i) word_sz) in
  let addr = Il.Based (reg, Some imm) in
    Il.Addr (addr, Il.ScalarTy (Il.ValTy word_bits))
;;

let word_n_low_byte (reg:Il.reg) (i:int) : Il.cell =
  let imm = Asm.IMM (Int64.mul (Int64.of_int i) word_sz) in
  let addr = Il.Based (reg, Some imm) in
    Il.Addr (addr, Il.ScalarTy (Il.ValTy Il.Bits8))
;;

let wordptr_n (reg:Il.reg) (i:int) : Il.cell =
  let imm = Asm.IMM (Int64.mul (Int64.of_int i) word_sz) in
  let addr = Il.Based (reg, Some imm) in
    Il.Addr (addr, Il.ScalarTy (Il.AddrTy (Il.ScalarTy (Il.ValTy word_bits))))
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
let proc_to_c_glue_sz = frame_base_sz;;

let load_proc_word (e:Il.emitter) (i:int) : Il.reg =
  let (vr, vc) = vreg e in
    Il.emit e (Il.umov vc (c proc_ptr));
    Il.emit e (Il.umov vc (c (word_n vr i)));
    vr
;;

let store_proc_word (e:Il.emitter) (i:int) (oper:Il.operand) : unit =
  let (vr, vc) = vreg e in
    Il.emit e (Il.umov vc (c proc_ptr));
    Il.emit e (Il.umov (word_n vr i) oper)
;;

let load_rt_word (e:Il.emitter) (i:int) : Il.reg =
  let rt = load_proc_word e 0 in
  let (vr, vc) = vreg e in
    Il.emit e (Il.umov vc (c (word_n rt i)));
    vr
;;

let store_rt_word (e:Il.emitter) (i:int) (oper:Il.operand) : unit =
  let rt = load_proc_word e 0 in
    Il.emit e (Il.umov (word_n rt i) oper);
;;

let emit_proc_state_change (e:Il.emitter) (state:Abi.proc_state) : unit =
  let code = Abi.proc_state_to_code state in
  let (vr,vc) = vreg e in
  let vr_n = word_n vr in
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in
    mov (r vr)(c proc_ptr);
    mov (vr_n Abi.proc_field_state) (immi code);
;;

let emit_upcall
    (e:Il.emitter)
    (u:Abi.upcall)
    (args:Il.operand array)
    (proc_to_c_fixup:fixup)
    : unit =
  let upcall_code = Abi.upcall_to_code u in
  let state_code = Abi.proc_state_to_code Abi.STATE_calling_c in

  let (vr,vc) = vreg e in
  let vr_n = word_n vr in
  let vr_n_low_byte = word_n_low_byte vr in
  let (_, dst_c) = vreg e in
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in
  (* 
   * This is an x86-ism, but a significant savings: inclusive-OR rather
   * than MOV, and we get sign-extension on the immediate for free.
   * Strangely, the MOV-immediates don't have a r32 <- imm8 mode. 
   *)
  let pcrel f = Il.CodeAddr (Il.Pcrel (f, None)) in

    assert ((Array.length args) <= Abi.max_upcall_args);

    mov vc (c proc_ptr);
    mov (vr_n_low_byte Abi.proc_field_state) (immi_byte state_code);
    mov (vr_n_low_byte Abi.proc_field_upcall_code) (immi_byte upcall_code);

    Array.iteri
      begin
        fun i arg ->
          mov (vr_n (Abi.proc_field_upcall_args + i)) arg
      end
      args;
    emit (Il.call dst_c (pcrel proc_to_c_fixup))
;;

let emit_frame_setup
    (e:Il.emitter)
    (argsz:int64)
    (framesz:int64)
    (spill_fixup:fixup)
    (callsz:int64)
    (proc_to_c_fixup:fixup)
    : unit =
  (*
   *  - save esp to ebp
   *  - subtract sz from esp
   *  - load proc->stk->limit
   *  - compare esp to limit
   *  - fwd jump if esp < limit
   *  - emit upcall grow_proc
   *  - fwd jump target
   *)
  let ecx_n = word_n (h ecx) in
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in
  let add = Int64.add in

  let n_call_bytes = Asm.IMM (add argsz frame_base_sz) in
  let subtrahend = (Asm.ADD ((Asm.IMM (add framesz callsz)),
                             Asm.M_SZ spill_fixup))
  in
  let n_frame_bytes = (Asm.ADD
                         (subtrahend,
                          (Asm.IMM (add frame_base_sz proc_to_c_glue_sz))))
  in
    save_callee_saves e;
    mov (rc ebp) (ro esp);                        (* ebp <- esp              *)
    emit (Il.binary Il.SUB
            (rc esp) (ro esp) (imm subtrahend));  (* esp <- esp - subtrahend *)
    mov (rc ecx) (c proc_ptr);                    (* ecx <- proc             *)
    mov (rc ecx) (c (ecx_n Abi.proc_field_stk));  (* ecx <- proc->stk        *)
    mov (rc ecx) (c (ecx_n Abi.stk_field_limit)); (* ecx <- proc->stk->limit *)
    emit (Il.cmp (ro esp) (ro ecx));
    let jmp_pc = e.Il.emit_pc in
      emit (Il.jmp Il.JL Il.CodeNone);
      emit_upcall
        e Abi.UPCALL_grow_proc
        [| (imm n_call_bytes);
           (imm n_frame_bytes) |]
        proc_to_c_fixup;
      Il.patch_jump e e.Il.emit_pc jmp_pc;
      emit Il.Dead;
;;


let fn_prologue
    (e:Il.emitter)
    (argsz:int64)
    (framesz:int64)
    (spill_fixup:fixup)
    (callsz:int64)
    : unit =
  let ssz = Int64.add framesz callsz in
    save_callee_saves e;
    Il.emit e (Il.umov (rc ebp) (ro esp));
    Il.emit e (Il.binary Il.SUB (rc esp) (ro esp)
                 (imm (Asm.ADD ((Asm.IMM ssz), Asm.M_SZ spill_fixup))))
;;

let fn_epilogue (e:Il.emitter) : unit =
    Il.emit e (Il.umov (rc esp) (ro ebp));
    restore_callee_saves e;
    Il.emit e Il.Ret;
;;

let main_prologue
    (e:Il.emitter)
    (block:Ast.block)
    (framesz:int64)
    (spill_fixup:fixup)
    (callsz:int64)
    : unit =
  let ssz = Int64.add framesz callsz in
    save_callee_saves e;
    Il.emit e (Il.umov (rc ebp) (ro esp));
    Il.emit e (Il.binary Il.SUB (rc esp) (ro esp)
                 (imm (Asm.ADD ((Asm.IMM ssz), Asm.M_SZ spill_fixup))))
;;

let objfile_main
    (e:Il.emitter)
    ~(main_fixup:fixup)
    ~(rust_start_fixup:fixup)
    ~(root_prog_fixup:fixup)
    ~(c_to_proc_fixup:fixup)
    ~(indirect_start:bool)
    : unit =
  Il.emit_full e (Some main_fixup) Il.Dead;
  save_callee_saves e;
  Il.emit e (Il.umov (rc ebp) (ro esp));
  Il.emit e (Il.Push (imm (Asm.M_POS c_to_proc_fixup)));
  Il.emit e (Il.Push (imm (Asm.M_POS root_prog_fixup)));
  if indirect_start
  then
    begin
      let addr = Il.Abs (Asm.M_POS rust_start_fixup) in
        Il.emit e (Il.call (rc eax) (Il.CodeAddr addr));
(*
        Il.emit e (Il.umov (rc ecx) (c (Il.Addr (addr, Il.OpaqueTy))));
        Il.emit e (Il.call (rc eax) (Il.CodeAddr (Il.Based ((h ecx), None))));
*)
    end
  else
    Il.emit e (Il.call (rc eax) (Il.CodeAddr (Il.Pcrel (rust_start_fixup, None))));
  Il.emit e (Il.Pop (rc ecx));
  Il.emit e (Il.Pop (rc ecx));
  Il.emit e (Il.umov (rc esp) (ro ebp));
  restore_callee_saves e;
  Il.emit e Il.Ret;
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

  let sp_n = word_n (Il.Hreg esp) in
  let edx_n = word_n (Il.Hreg edx) in
  let ecx_n = word_n (Il.Hreg ecx) in
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in

    Il.emit_full e (Some fix) Il.Dead;

    mov (rc edx) (c (sp_n 1));                     (* edx <- proc          *)
    mov (rc ecx) (c (edx_n Abi.proc_field_rt));    (* ecx <- proc->rt      *)
    save_callee_saves e;
    mov (ecx_n Abi.rt_field_sp) (ro esp);          (* rt->regs.sp <- esp   *)
    mov (rc esp) (c (edx_n Abi.proc_field_sp));    (* esp <- proc->regs.sp *)

    (**** IN PROC STACK ****)
    restore_callee_saves e;
    emit Il.Ret;
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
  let edx_n = word_n (Il.Hreg edx) in
  let ecx_n = word_n (Il.Hreg ecx) in
  let emit = Il.emit e in
  let mov dst src = emit (Il.umov dst src) in

    Il.emit_full e (Some fix) Il.Dead;

    mov (rc edx) (c proc_ptr);                     (* edx <- proc            *)
    mov (rc ecx) (c (edx_n Abi.proc_field_rt));    (* ecx <- proc->rt        *)
    save_callee_saves e;
    mov (edx_n Abi.proc_field_sp) (ro esp);        (* proc->regs.sp <- esp   *)
    mov (rc esp) (c (ecx_n Abi.rt_field_sp));      (* esp <- rt->regs.sp     *)

    (**** IN C STACK ****)
    restore_callee_saves e;
    emit Il.Ret;
    (***********************)
  ()
;;


let (abi:Abi.abi) =
  {
    Abi.abi_word_sz = word_sz;
    Abi.abi_word_bits = word_bits;
    Abi.abi_word_ty = word_ty;

    Abi.abi_is_2addr_machine = true;
    Abi.abi_has_pcrel_data = false;
    Abi.abi_has_pcrel_code = true;
    Abi.abi_has_abs_data = false;
    Abi.abi_has_abs_code = false;

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


let imm_is_byte (n:int64) : bool =
  (i64_le (-128L) n) && (i64_le n 127L)
;;


let rm_r (c:Il.cell) (r:int) : Asm.item =
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
      | Il.Addr (a, _) ->
          begin
            match a with
                Il.Abs disp ->
                  Asm.SEQ [| Asm.BYTE (modrm_deref_disp32 r);
                             Asm.WORD (TY_s32, disp) |]

              | Il.Based ((Il.Hreg rm), None) when rm != reg_ebp ->
                  seq1 rm (Asm.BYTE (modrm_deref_reg (reg rm) r))

              | Il.Based ((Il.Hreg rm), Some (Asm.IMM 0L)) when rm != reg_ebp ->
                  seq1 rm (Asm.BYTE (modrm_deref_reg (reg rm) r))

              (* The next two are just to save the relaxation system some churn. *)
              | Il.Based ((Il.Hreg rm), Some (Asm.IMM n)) when imm_is_byte n ->
                  seq2 rm
                    (Asm.BYTE (modrm_deref_reg_plus_disp8 (reg rm) r))
                    (Asm.WORD (TY_s8, Asm.IMM n))

              | Il.Based ((Il.Hreg rm), Some (Asm.IMM n)) ->
                  seq2 rm
                    (Asm.BYTE (modrm_deref_reg_plus_disp32 (reg rm) r))
                    (Asm.WORD (TY_s32, Asm.IMM n))

              | Il.Based ((Il.Hreg rm), Some disp) ->
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


let insn_rm_r (op:int) (c:Il.cell) (r:int) : Asm.item =
  Asm.SEQ [| Asm.BYTE op; rm_r c r |]
;;


let insn_rm_r_imm (op:int) (c:Il.cell) (r:int) (ty:ty_mach) (i:Asm.expr64) : Asm.item =
  Asm.SEQ [| Asm.BYTE op; rm_r c r; Asm.WORD (ty, i) |]
;;

let insn_rm_r_imm_s8_s32 (op8:int) (op32:int) (c:Il.cell) (r:int) (i:Asm.expr64) : Asm.item =
  match i with
      Asm.IMM n when imm_is_byte n ->
        insn_rm_r_imm op8 c r TY_s8 i
    | _ ->
        Asm.new_relaxation
          [|
            insn_rm_r_imm op32 c r TY_s32 i;
            insn_rm_r_imm op8 c r TY_s8 i
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

let is_ty32 (ty:Il.scalar_ty) : bool =
  match ty with
      Il.ValTy (Il.Bits32) -> true
    | Il.AddrTy _ -> true
    | _ -> false

let is_rm32 (c:Il.cell) : bool =
  match c with
      (* FIXME: tighten this up. Currently it's willing to accept *any* address as rm32. *)
      Il.Addr (_, _) -> true
    | Il.Reg (_, st) -> is_ty32 st
;;

let is_ty8 (ty:Il.scalar_ty) : bool =
  match ty with
      Il.ValTy (Il.Bits8) -> true
    | _ -> false
;;

let is_rm8 (c:Il.cell) : bool =
  match c with
      Il.Addr (_, Il.ScalarTy st) -> is_ty8 st
    | Il.Reg (_, st) -> is_ty8 st
    | _ -> false
;;

(* FIXME: tighten imm-based dispatch by imm type. *)
let cmp (a:Il.operand) (b:Il.operand) : Asm.item =
  match (a,b) with
      (Il.Cell c, Il.Imm (i, _)) when is_rm32 c ->
        insn_rm_r_imm_s8_s32 0x83 0x81 c slash7 i
    | (Il.Cell c, Il.Cell (Il.Reg (Il.Hreg r, _))) ->
        insn_rm_r 0x39 c (reg r)
    | (Il.Cell (Il.Reg (Il.Hreg r, _)), Il.Cell c) ->
        insn_rm_r 0x3b c (reg r)
    | _ -> raise Unrecognized
;;


let mov (signed:bool) (dst:Il.cell) (src:Il.operand) : Asm.item =

  match (signed, dst, src) with

      (* rm8 <- r8 *)
      (_,  _, Il.Cell (Il.Reg ((Il.Hreg r), src_ty)))
        when is_rm8 dst && is_ty8 src_ty ->
          insn_rm_r 0x88 dst (reg r)

    (* rm32 <- r32 *)
    | (_,  _, Il.Cell (Il.Reg ((Il.Hreg r), src_ty)))
        when is_rm32 dst && is_ty32 src_ty ->
        insn_rm_r 0x89 dst (reg r)

    (* r8 <- rm8 *)
    | (_,  Il.Reg ((Il.Hreg r), dst_ty), Il.Cell src_cell)
        when is_ty8 dst_ty && is_rm8 src_cell ->
          insn_rm_r 0x8a src_cell (reg r)

    (* r32 <- rm32 *)
    | (_,  (Il.Reg ((Il.Hreg r), dst_ty)), Il.Cell src_cell)
        when is_ty32 dst_ty && is_rm32 src_cell ->
          insn_rm_r 0x8b src_cell (reg r)


    (* MOVZX: r32 <- zx(rm8) *)
    | (false, Il.Reg ((Il.Hreg r, dst_ty)), Il.Cell src_cell)
        when is_ty32 dst_ty && is_rm8 src_cell ->
        Asm.SEQ [| Asm.BYTE 0x0f;
                   insn_rm_r 0xb6 src_cell (reg r) |]

    (* MOVSX: r32 <- sx(rm8) *)
    | (true, Il.Reg ((Il.Hreg r), dst_ty), Il.Cell src_cell)
        when is_ty32 dst_ty && is_rm8 src_cell ->
        Asm.SEQ [| Asm.BYTE 0x0f;
                   insn_rm_r 0xbe src_cell (reg r) |]

    (* rm8 <- imm8 *)
    | (_, _, Il.Imm ((Asm.IMM n), _))
        when is_rm8 dst && imm_is_byte n ->
        insn_rm_r_imm 0xc6 dst slash0 TY_u8 (Asm.IMM n)

    (* rm32 <- imm32 *)
    | (_, _, Il.Imm (i, _)) when is_rm32 dst ->
        insn_rm_r_imm 0xc7 dst slash0 TY_u32 i

    | _ -> raise Unrecognized
;;


let lea (dst:Il.cell) (addr:Il.addr) : Asm.item =
  match dst with
      Il.Reg ((Il.Hreg r), dst_ty) when is_ty32 dst_ty ->
        insn_rm_r 0x8d (Il.Addr (addr, Il.OpaqueTy)) (reg r)

    | _ -> raise Unrecognized
;;


let select_insn_misc (q:Il.quad') : Asm.item =

  match q with
      Il.Call c ->
        begin
          match c.Il.call_dst with
              Il.Reg ((Il.Hreg dst), _) when dst = eax ->
                begin
                  match c.Il.call_targ with
                      Il.CodeAddr (Il.Based b) ->
                        insn_rm_r 0xff (Il.Addr (Il.Based b, Il.OpaqueTy)) slash2
                    | Il.CodeAddr (Il.Abs a) ->
                        insn_rm_r 0xff (Il.Addr (Il.Abs a, Il.OpaqueTy)) slash2
                    | Il.CodeAddr (Il.Pcrel (f, None)) ->
                        insn_pcrel_simple 0xe8 f
                    | _ -> raise Unrecognized
                end
            | _ -> raise Unrecognized
        end

    | Il.Push (Il.Cell (Il.Reg ((Il.Hreg r), t))) when is_ty32 t ->
        Asm.BYTE (0x50 + (reg r))

    | Il.Push (Il.Cell c) when is_rm32 c ->
        insn_rm_r 0xff c slash6

    | Il.Push (Il.Imm (Asm.IMM i, _)) when imm_is_byte i ->
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

              (Il.JMP, Il.CodeAddr (Il.Pcrel (f, None))) ->
                insn_pcrel 0xeb 0xe9 f

            | (Il.JMP, Il.CodeAddr r) ->
                insn_rm_r 0xff (Il.Addr (r, Il.OpaqueTy)) slash4


            | (_, Il.CodeAddr (Il.Pcrel (f, None))) ->
                let (op8, op32) =
                  match j.Il.jmp_op with
                    | Il.JC  -> (0x72, 0x82)
                    | Il.JNC -> (0x73, 0x83)
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
    | Il.End -> Asm.BYTES [| 0x90 |]
    | Il.Nop -> Asm.BYTES [| 0x90 |]
    | _ -> raise Unrecognized
;;


let alu_binop
    (dst:Il.cell) (src:Il.operand) (immslash:int)
    (rm_dst_op:int) (rm_src_op:int)
    : Asm.item =
  match (dst, src) with
      (Il.Reg ((Il.Hreg r), dst_ty), Il.Cell c)
        when is_ty32 dst_ty && is_rm32 c ->
          insn_rm_r rm_src_op c (reg r)

    | (_, Il.Cell (Il.Reg ((Il.Hreg r), src_ty)))
        when is_rm32 dst && is_ty32 src_ty ->
        insn_rm_r rm_dst_op dst (reg r)

    | (_, Il.Imm (i, _)) when is_rm32 dst
        -> insn_rm_r_imm_s8_s32 0x83 0x81 dst immslash i

    | _ -> raise Unrecognized
;;


let mul_like (src:Il.operand) (signed:bool) (slash:int)
    : Asm.item =
  match src with
      Il.Cell src when is_rm32 src ->
        insn_rm_r 0xf7 src slash

    | Il.Imm i ->
        let tmp = Il.Reg ((Il.Hreg edx), Il.ValTy Il.Bits32) in
        Asm.SEQ [| mov signed tmp src;
                   insn_rm_r 0xf7 tmp slash |]

    | _ -> raise Unrecognized
;;


let select_insn (q:Il.quad) : Asm.item =
  let item =
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
                    (_, Il.ADD) -> binop slash0 0x1 0x3
                  | (_, Il.SUB) -> binop slash5 0x29 0x2b
                  | (_, Il.AND) -> binop slash4 0x21 0x23
                  | (_, Il.OR) -> binop slash1 0x09 0x0b

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
  in
    match q.Il.quad_fixup with
        None -> item
      | Some f -> Asm.DEF (f, item)
;;


let new_emitter _ : Il.emitter =
  Il.new_emitter
    abi.Abi.abi_prealloc_quad
    abi.Abi.abi_is_2addr_machine
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

let items_of_emitted_quads (sess:Session.sess) (e:Il.emitter) : Asm.item =
  let item = select_insns sess e.Il.emit_quads in
    if sess.Session.sess_failed
    then raise Unrecognized
    else item
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
