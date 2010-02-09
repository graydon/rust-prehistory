open Common;;

(* FIXME (bug 541581): thread a session object through this eventually. *)
let log_iltypes = ref false;;

(* IL type system, very rudimentary. *)

type bits =
    Bits8
  | Bits16
  | Bits32
  | Bits64
;;

type scalar_ty =
    ValTy of bits
  | AddrTy of referent_ty

and referent_ty =
    ScalarTy of scalar_ty
  | StructTy of referent_ty array
  | UnionTy of referent_ty array
  | OpaqueTy (* Unknown memory-resident thing. *)
  | CodeTy   (* Executable machine code. *)
  | NilTy    (* 0 bits of space. *)
;;

let (voidptr_t:scalar_ty) = AddrTy OpaqueTy;;
let (codeptr_t:scalar_ty) = AddrTy CodeTy;;

(* Operands. *)

type vreg = int ;;
type hreg = int ;;
type label = int ;;
type spill = int ;;

type reg =
    Vreg of vreg
  | Hreg of hreg
;;

type mem =
    Abs of Asm.expr64
  | RegIn of (reg * (Asm.expr64 option))
  | AbsIn of (Asm.expr64 * (Asm.expr64 option))
  | Spill of spill
;;

type code =
    CodeLabel of label (* Index into current quad block. *)
  | CodeMem of mem
  | CodeNone
;;

type typed_reg = (reg * scalar_ty);;
type typed_mem = (mem * referent_ty);;
type typed_imm = (Asm.expr64 * ty_mach);;

type cell =
    Reg of typed_reg
  | Mem of typed_mem
;;

type operand =
    Cell of cell
  | Imm of typed_imm
;;

(* NB: for the most part, we let the register allocator assign spills
 * from vregs, and we permanently allocate aliased slots to stack
 * locations by static aliasing information early, in layout.
 * 
 * The one awkward case this doesn't handle is when someone tries to
 * pass a literal-atom to an alias-slot. This *requires* a memory slot
 * but we only realize it rather late, much later than we'd normally
 * have thougt to desugar the literal into a temporary.
 * 
 * So in these cases, we let the trans module explicitly demand a
 * "Spill n" operand, which the register allocator mops up before it
 * gets started on the vregs.
 * 
 * NOTE: if we were more clever we'd integrate vregs and spills like
 * this together along with the general notion of a temporary way back
 * at the desugaring stage, and use some kind of size-class
 * consolidation so that spills with non-overlapping lifetimes could
 * share memory. But we're not that clever yet.
 *)


(* Helpers. *)

let cell_is_nil c =
  match c with
      Mem (_, NilTy) -> true
    | Reg (_, AddrTy NilTy) -> true
    | _ -> false
;;

let operand_is_nil o =
  match o with
      Cell c -> cell_is_nil c
    | _ -> false
;;

let mem_off (mem:mem) (off:Asm.expr64) : mem =
  let addto e = Asm.ADD (off, e) in
    match mem with
        Abs e -> Abs (addto e)
      | RegIn (r, None) -> RegIn (r, Some off)
      | RegIn (r, Some e) -> RegIn (r, Some (addto e))
      | AbsIn (f, None) -> AbsIn (f, Some off)
      | AbsIn (f, Some e) -> AbsIn (f, Some (addto e))
      | Spill _ -> bug () "Adding offset to spill slot"
;;

let mem_off_imm (mem:mem) (imm:int64) : mem =
  mem_off mem (Asm.IMM imm)
;;


(* Quads. *)

type binop =
    ADD | SUB
  | IMUL | UMUL
  | IDIV | UDIV
  | IMOD | UMOD
  | AND | OR
  | LSL | LSR | ASR
;;

type unop =
    NEG | NOT
  | UMOV | IMOV
  | ZERO
;;

type jmpop =
    JE | JNE
  | JZ | JNZ (* FIXME: Synonyms with JE/JNE in x86, others? *)
  | JL | JLE | JG | JGE (* Signed.   *)
  | JB | JBE | JA | JAE (* Unsigned. *)
  | JC | JNC | JO | JNO
  | JMP
;;

type binary =
    {
      binary_op: binop;
      binary_dst: cell;
      binary_lhs: operand;
      binary_rhs: operand
    }
;;

type unary =
    {
      unary_op: unop;
      unary_dst: cell;
      unary_src: operand
    }
;;

type cmp =
    {
      cmp_lhs: operand;
      cmp_rhs: operand
    }
;;

type lea =
    {
      lea_dst: cell;
      lea_src: mem
    }
;;

type jmp =
    {
      jmp_op: jmpop;
      jmp_targ: code;
    }
;;

type call =
    {
      call_dst: cell;
      call_targ: code
    }

type quad' =
    Binary of binary
  | Unary of unary
  | Lea of lea
  | Cmp of cmp
  | Jmp of jmp
  | Push of operand
  | Pop of cell
  | Call of call
  | Debug (* Debug-break pseudo-instruction. *)
  | Enter of fixup (* Enter-fixup-block pseudo-instruction. *)
  | Leave          (* Leave-fixup-block pseudo-instruction. *)
  | Ret  (* Return to caller. *)
  | Nop  (* Keep this quad here, emit CPU nop, will patch / trampoline later. *)
  | Dead (* Keep this quad but emit nothing. *)
  | End  (* Space past the end of quads to emit. *)
;;

type quad =
    { quad_fixup: fixup option;
      quad_body: quad'; }

type quads = quad array ;;

(* Query functions. *)

let cell_scalar_ty (c:cell) : scalar_ty =
  match c with
      Reg (_, st) -> st
    | Mem (_, rt) -> AddrTy rt
;;

let bits_of_ty_mach (tm:ty_mach) : bits =
  match tm with
    | TY_u8 -> Bits8
    | TY_s8 -> Bits8
    | TY_u16 -> Bits16
    | TY_s16 -> Bits16
    | TY_u32 -> Bits32
    | TY_s32 -> Bits32
    | TY_u64 -> Bits64
    | TY_s64 -> Bits64
    | TY_f32 -> Bits32
    | TY_f64 -> Bits64
;;


let scalar_ty_bits (word_bits:bits) (st:scalar_ty) : bits =
  match st with
      ValTy bits -> bits
    | AddrTy _ -> word_bits
;;

let cell_bits (word_bits:bits) (c:cell) : bits =
  match c with
      Reg (_, st) -> scalar_ty_bits word_bits st
    | Mem (_, ScalarTy st) -> scalar_ty_bits word_bits st
    | Mem _ -> failwith "mem of non-scalar in Il.cell_bits"
;;

let operand_bits (word_bits:bits) (op:operand) : bits =
  match op with
      Cell cell -> cell_bits word_bits cell
    | Imm (_, tm) -> bits_of_ty_mach tm
;;

let bits_size (bits:bits) : int64 =
  match bits with
      Bits8 -> 1L
    | Bits16 -> 2L
    | Bits32 -> 4L
    | Bits64 -> 8L
;;

let bits_align (bits:bits) : int64 =
  match bits with
      Bits8 -> 1L
    | Bits16 -> 2L
    | Bits32 -> 4L
    | Bits64 -> 8L
;;

let scalar_ty_size (word_bits:bits) (st:scalar_ty) : int64 =
  bits_size (scalar_ty_bits word_bits st)
;;

let scalar_ty_align (word_bits:bits) (st:scalar_ty) : int64 =
  bits_align (scalar_ty_bits word_bits st)
;;

let align_to (align:int64) (v:int64) : int64 =
  if align = 0L || align = 1L
  then v
  else
    let rem = Int64.rem v align in
      if rem = 0L
      then v
      else
        let padding = Int64.sub align rem in
          Int64.add v padding
;;

let rec referent_ty_layout (word_bits:bits) (rt:referent_ty) : (int64 * int64) =
  match rt with
      ScalarTy st -> (scalar_ty_size word_bits st, scalar_ty_align word_bits st)
    | StructTy rts ->
        begin
          let accum (off,align) rt : (int64 * int64) =
            let (elt_size, elt_align) = referent_ty_layout word_bits rt in
            let elt_off = align_to elt_align off in
              (Int64.add elt_off elt_size, i64_max elt_align align)
          in
            Array.fold_left accum (0L,0L) rts
        end
   | UnionTy rts ->
        begin
          let accum (sz,align) rt : (int64 * int64) =
            let (elt_size, elt_align) = referent_ty_layout word_bits rt in
              (i64_max sz elt_size, i64_max elt_align align)
          in
            Array.fold_left accum (0L,0L) rts
        end
    | OpaqueTy _ -> bug () "opaque ty in referent_ty_layout"
    | CodeTy _ -> bug () "code ty in referent_ty_layout"
    | NilTy -> (0L, 0L)

and referent_ty_size (word_bits:bits) (rt:referent_ty) : int64 =
  fst (referent_ty_layout word_bits rt)

and referent_ty_align (word_bits:bits) (rt:referent_ty) : int64 =
  snd (referent_ty_layout word_bits rt)
;;


(* Processor. *)

type quad_processor =
    { qp_reg:  (quad_processor -> reg -> reg);
      qp_mem:  (quad_processor -> mem -> mem);
      qp_cell_read: (quad_processor -> cell -> cell);
      qp_cell_write: (quad_processor -> cell -> cell);
      qp_code: (quad_processor -> code -> code);
      qp_op: (quad_processor -> operand -> operand); }
;;

let identity_processor =
  let qp_cell = (fun qp c -> match c with
                     Reg (r, b) -> Reg (qp.qp_reg qp r, b)
                   | Mem (a, b) -> Mem (qp.qp_mem qp a, b))
  in
    { qp_reg = (fun _ r -> r);
      qp_mem = (fun qp a -> match a with
                     RegIn (r, o) -> RegIn (qp.qp_reg qp r, o)
                   | Abs _
                   | AbsIn _
                   | Spill _ -> a);
      qp_cell_read = qp_cell;
      qp_cell_write = qp_cell;
      qp_code = (fun qp c -> match c with
                     CodeMem a -> CodeMem (qp.qp_mem qp a)
                   | CodeLabel _
                   | CodeNone -> c);
      qp_op = (fun qp op -> match op with
                   Cell c -> Cell (qp.qp_cell_read qp c)
                 | Imm _ -> op) }
;;

let process_quad (qp:quad_processor) (q:quad) : quad =
  { q with
      quad_body = match q.quad_body with
          Binary b ->
            Binary { b with
                       binary_dst = qp.qp_cell_write qp b.binary_dst;
                       binary_lhs = qp.qp_op qp b.binary_lhs;
                       binary_rhs = qp.qp_op qp b.binary_rhs }
        | Unary u ->
            Unary { u with
                      unary_dst = qp.qp_cell_write qp u.unary_dst;
                      unary_src = qp.qp_op qp u.unary_src }

        | Lea le ->
            Lea { lea_dst = qp.qp_cell_write qp le.lea_dst;
                  lea_src = qp.qp_mem qp le.lea_src }

        | Cmp c ->
            Cmp { cmp_lhs = qp.qp_op qp c.cmp_lhs;
                  cmp_rhs = qp.qp_op qp c.cmp_rhs }

        | Jmp j ->
            Jmp { j with
                    jmp_targ = qp.qp_code qp j.jmp_targ }

        | Push op ->
            Push (qp.qp_op qp op)

        | Pop c ->
            Pop (qp.qp_cell_write qp c)

        | Call c ->
            Call { call_dst = qp.qp_cell_write qp c.call_dst;
                   call_targ = qp.qp_code qp c.call_targ }

        | Ret -> Ret
        | Nop -> Nop
        | Debug -> Debug
        | Enter f -> Enter f
        | Leave -> Leave
        | Dead -> Dead
        | End -> End }
;;

let visit_quads (qp:quad_processor) (qs:quads) : unit =
  Array.iter (fun x ->ignore ( process_quad qp x); ()) qs
;;

let process_quads (qp:quad_processor) (qs:quads) : quads =
  Array.map (process_quad qp) qs
;;

let rewrite_quads (qp:quad_processor) (qs:quads) : unit =
  for i = 0 to ((Array.length qs) - 1) do
    qs.(i) <- process_quad qp qs.(i)
  done
;;

(* Formatters. *)




let string_of_bits (b:bits) : string =
  match b with
      Bits8 -> "b8"
    | Bits16 -> "b16"
    | Bits32 -> "b32"
    | Bits64 -> "b64"
;;

let rec string_of_scalar_ty (s:scalar_ty) : string =
  match s with
      ValTy b -> (string_of_bits b)
    | AddrTy r -> (string_of_referent_ty r) ^ "*"

and string_of_referent_ty (r:referent_ty) : string =
  match r with
      ScalarTy s ->  (string_of_scalar_ty s)
    | StructTy rs ->
        Printf.sprintf "[%s]"
          (String.concat ","
             (Array.to_list (Array.map string_of_referent_ty rs)))
    | UnionTy rs ->
        Printf.sprintf "(%s)"
          (String.concat "|"
             (Array.to_list (Array.map string_of_referent_ty rs)))
    | OpaqueTy -> "?"
    | CodeTy -> "!"
    | NilTy -> "()"
;;


type hreg_formatter = hreg -> string;;

let string_of_reg (f:hreg_formatter) (r:reg) : string =
  match r with
      Vreg i -> Printf.sprintf "<v%d>" i
    | Hreg i -> f i
;;

let rec string_of_expr64 (e64:Asm.expr64) : string =
  let bin op a b =
    Printf.sprintf "(%s %s %s)" (string_of_expr64 a) op (string_of_expr64 b)
  in
  let bini op a b =
    Printf.sprintf "(%s %s %d)" (string_of_expr64 a) op b
  in
    match e64 with
        Asm.IMM i when (i64_lt i 0L) -> Printf.sprintf "-0x%Lx" (Int64.neg i)
      | Asm.IMM i -> Printf.sprintf "0x%Lx" i
      | Asm.ADD (a,b) -> bin "+" a b
      | Asm.SUB (a,b) -> bin "-" a b
      | Asm.MUL (a,b) -> bin "*" a b
      | Asm.DIV (a,b) -> bin "/" a b
      | Asm.REM (a,b) -> bin "%" a b
      | Asm.SLL (a,b) -> bini "<<" a b
      | Asm.SLR (a,b) -> bini ">>" a b
      | Asm.SAR (a,b) -> bini ">>>" a b
      | Asm.AND (a,b) -> bin "&" a b
      | Asm.XOR (a,b) -> bin "xor" a b
      | Asm.OR (a,b) -> bin "|" a b
      | Asm.NOT a -> Printf.sprintf "(not %s)" (string_of_expr64 a)
      | Asm.F_POS f -> Printf.sprintf "<%s>.fpos" f.fixup_name
      | Asm.F_SZ f -> Printf.sprintf "<%s>.fsz" f.fixup_name
      | Asm.M_POS f -> Printf.sprintf "<%s>.mpos" f.fixup_name
      | Asm.M_SZ f -> Printf.sprintf "<%s>.msz" f.fixup_name
      | Asm.EXT _ -> "??ext??"
;;

let string_of_off (e:Asm.expr64 option) : string =
  match e with
      None -> ""
    | Some (Asm.IMM i) when (i64_lt i 0L) -> Printf.sprintf " - 0x%Lx" (Int64.neg i)
    | Some e' -> " + " ^ (string_of_expr64 e')
;;

let string_of_mem (f:hreg_formatter) (a:mem) : string =
  match a with
      Abs e ->
        Printf.sprintf "[%s]" (string_of_expr64 e)
    | RegIn (r, off) ->
        Printf.sprintf "[%s%s]" (string_of_reg f r) (string_of_off off)
    | AbsIn (e, off) ->
        Printf.sprintf "[[%s]%s]" (string_of_expr64 e) (string_of_off off)
    | Spill i ->
        Printf.sprintf "[<spill %d>]" i
;;

let string_of_code (f:hreg_formatter) (c:code) : string =
  match c with
      CodeLabel lab -> Printf.sprintf "<label %d>" lab
    | CodeMem a -> string_of_mem f a
    | CodeNone -> "<none>"
;;

let string_of_cell (f:hreg_formatter) (c:cell) : string =
  match c with
      Reg (r,ty) ->
        if !log_iltypes
        then
          Printf.sprintf "%s:%s" (string_of_reg f r) (string_of_scalar_ty ty)
        else
          Printf.sprintf "%s" (string_of_reg f r)
    | Mem (a,ty) ->
        if !log_iltypes
        then
          Printf.sprintf "%s:%s" (string_of_mem f a) (string_of_referent_ty ty)
        else
          Printf.sprintf "%s" (string_of_mem f a)
;;

let string_of_operand (f:hreg_formatter) (op:operand) : string =
  match op with
      Cell c -> string_of_cell f c
    | Imm (i, ty) ->
        if !log_iltypes
        then
          Printf.sprintf "%s:%s" (string_of_expr64 i) (string_of_ty_mach ty)
        else
          Printf.sprintf "%s" (string_of_expr64 i)
;;

let string_of_binop (op:binop) : string =
  match op with
      ADD -> "add"
    | SUB -> "sub"
    | IMUL -> "imul"
    | UMUL -> "umul"
    | IDIV -> "idiv"
    | UDIV -> "udiv"
    | IMOD -> "imod"
    | UMOD -> "umod"
    | AND -> "and"
    | OR -> "or"
    | LSL -> "lsl"
    | LSR -> "lsr"
    | ASR -> "asr"
;;

let string_of_unop (op:unop) : string =
  match op with
      NEG -> "neg"
    | NOT -> "not"
    | UMOV -> "umov"
    | IMOV -> "imov"
    | ZERO -> "zero"
;;

let string_of_jmpop (op:jmpop) : string =
  match op with
      JE -> "je"
    | JNE -> "jne"
    | JL -> "jl"
    | JLE -> "jle"
    | JG -> "jg"
    | JGE -> "jge"
    | JB -> "jb"
    | JBE -> "jbe"
    | JA -> "ja"
    | JAE -> "jae"
    | JC -> "jc"
    | JNC ->"jnc"
    | JO -> "jo"
    | JNO -> "jno"
    | JZ -> "jz"
    | JNZ ->"jnz"
    | JMP -> "jmp"
;;

let string_of_quad (f:hreg_formatter) (q:quad) : string =
  match q.quad_body with
      Binary b ->
        Printf.sprintf "%s = %s %s %s"
          (string_of_cell f b.binary_dst)
          (string_of_operand f b.binary_lhs)
          (string_of_binop b.binary_op)
          (string_of_operand f b.binary_rhs)

    | Unary u ->
        Printf.sprintf "%s = %s %s"
          (string_of_cell f u.unary_dst)
          (string_of_unop u.unary_op)
          (string_of_operand f u.unary_src)

    | Cmp c ->
        Printf.sprintf "cmp %s %s"
          (string_of_operand f c.cmp_lhs)
          (string_of_operand f c.cmp_rhs)

    | Lea le ->
        Printf.sprintf "lea %s %s"
          (string_of_cell f le.lea_dst)
          (string_of_mem f le.lea_src)

    | Jmp j ->
        Printf.sprintf "%s %s"
          (string_of_jmpop j.jmp_op)
          (string_of_code f j.jmp_targ)

    | Push op ->
        Printf.sprintf "push %s"
          (string_of_operand f op)

    | Pop c ->
        Printf.sprintf "%s = pop"
          (string_of_cell f c)

    | Call c ->
        Printf.sprintf "%s = call %s"
          (string_of_cell f c.call_dst)
          (string_of_code f c.call_targ)

    | Ret -> "ret"
    | Nop -> "nop"
    | Dead -> "dead"
    | Debug -> "debug"
    | Enter f -> "enter " ^ f.fixup_name
    | Leave -> "leave"
    | End -> "---"
;;



(* Emitters. *)


type emitter = { mutable emit_pc: int;
                 mutable emit_next_vreg: int;
                 mutable emit_next_spill: int;
                 emit_preallocator: (quad' -> quad');
                 emit_is_2addr: bool;
                 mutable emit_quads: quads;
                 emit_annotations: (int,string) Hashtbl.t }


let badq = { quad_fixup = None;
             quad_body = End }
;;


let deadq = { quad_fixup = None;
              quad_body = Dead }
;;


let new_emitter (preallocator:quad' -> quad') (is_2addr:bool) =
  {
    emit_pc = 0;
    emit_next_vreg = 0;
    emit_next_spill = 0;
    emit_preallocator = preallocator;
    emit_is_2addr = is_2addr;
    emit_quads = Array.create 4 badq;
    emit_annotations = Hashtbl.create 0;
  }
;;


let next_vreg_num (e:emitter) : vreg =
  let i = e.emit_next_vreg in
    e.emit_next_vreg <- i + 1;
    i
;;

let next_vreg (e:emitter) : reg =
  Vreg (next_vreg_num e)
;;

let next_vreg_cell (e:emitter) (s:scalar_ty) : cell =
  Reg ((next_vreg e), s)
;;

let next_spill (e:emitter) : spill =
  let i = e.emit_next_spill in
    e.emit_next_spill <- i + 1;
    i
;;

let next_spill_slot (e:emitter) (r:referent_ty) : typed_mem =
  (Spill (next_spill e), r);
;;


let grow_if_necessary e =
  let len = Array.length e.emit_quads in
    if e.emit_pc >= len - 1
    then
      let n = Array.create (2 * len) badq in
        Array.blit e.emit_quads 0 n 0 len;
        e.emit_quads <- n
;;


let binary (op:binop) (dst:cell) (lhs:operand) (rhs:operand) : quad' =
  Binary { binary_op = op;
           binary_dst = dst;
           binary_lhs = lhs;
           binary_rhs = rhs }
;;

let unary (op:unop) (dst:cell) (src:operand) : quad' =
  Unary { unary_op = op;
          unary_dst = dst;
          unary_src = src }

let jmp (op:jmpop) (targ:code) : quad' =
  Jmp { jmp_op = op;
        jmp_targ = targ; }
;;

let lea (dst:cell) (src:mem) : quad' =
  Lea { lea_dst = dst;
        lea_src = src; }
;;

let cmp (lhs:operand) (rhs:operand) : quad' =
  Cmp { cmp_lhs = lhs;
        cmp_rhs = rhs; }
;;

let call (dst:cell) (targ:code) : quad' =
  Call { call_dst = dst;
         call_targ = targ; }
;;

let umov (dst:cell) (src:operand) : quad' =
    if (cell_is_nil dst || operand_is_nil src)
    then Dead
    else unary UMOV dst src
;;

let zero (dst:cell) (count:operand) : quad' =
  unary ZERO dst count
;;

let is_mov uop =
  match uop with
      UMOV | IMOV -> true
    | _ -> false
;;

let mk_quad (q':quad') : quad =
  { quad_body = q';
    quad_fixup = None }
;;

let emit_full (e:emitter) (fix:fixup option) (q':quad') =
  let fixup = ref fix in
  let emit_quad_bottom q' =
    grow_if_necessary e;
    e.emit_quads.(e.emit_pc) <- { quad_body = q';
                                  quad_fixup = (!fixup) };
    fixup := None;
    e.emit_pc <- e.emit_pc + 1
  in


  let emit_quad (q':quad') : unit =
    (* decay mem-mem movs *)
    match q' with
        Unary { unary_dst = Mem (dst_mem, dst_ty);
                unary_src = Cell (Mem (src_mem, src_ty));
                unary_op = op }
          when is_mov op ->
            begin
              let v = next_vreg_cell e (AddrTy dst_ty) in
                emit_quad_bottom (unary op v (Cell (Mem (src_mem, src_ty))));
                emit_quad_bottom (unary op (Mem (dst_mem, dst_ty)) (Cell v))
            end
      | _ -> emit_quad_bottom q'
  in

  let default_mov =
    match q' with
        Binary b ->
          begin
            match b.binary_op with
                IDIV | IMUL | IMOD -> IMOV
              | _ -> UMOV
          end
      | Unary u ->
          begin
            match u.unary_op with
                IMOV -> IMOV
              | _ -> UMOV
          end
      | _ -> UMOV
  in

  let emit_mov (dst:cell) (src:operand) : unit =
    emit_quad (unary default_mov dst src)
  in

  let mov_if_operands_differ (old_op:operand) (new_op:operand) : unit =
    if new_op != old_op
    then
      match new_op with
          Cell new_cell -> emit_mov new_cell old_op
        | _ -> ()
  in

  let mov_if_cells_differ (old_cell:cell) (new_cell:cell) : unit =
    if new_cell != old_cell
    then
      emit_mov old_cell (Cell new_cell)
  in

    match (q', e.emit_preallocator q') with
        (Binary b, Binary b') ->
          begin
            mov_if_operands_differ b.binary_lhs b'.binary_lhs;
            mov_if_operands_differ b.binary_rhs b'.binary_rhs;
            if e.emit_is_2addr &&
              (b'.binary_lhs != (Cell b'.binary_dst))
            then
              begin
                emit_mov b'.binary_dst b'.binary_lhs;
                emit_quad (Binary { b' with binary_lhs = (Cell b'.binary_dst) })
              end
            else
              emit_quad q';
            mov_if_cells_differ b.binary_dst b'.binary_dst
          end

      | (Unary u, Unary u') ->
          emit_quad (Unary u');
          mov_if_cells_differ u.unary_dst u'.unary_dst

      | (Cmp c, Cmp c') ->
          mov_if_operands_differ c.cmp_lhs c'.cmp_lhs;
          mov_if_operands_differ c.cmp_rhs c'.cmp_rhs;
          emit_quad (Cmp c');

      | (Push op, Push op') ->
          mov_if_operands_differ op op';
          emit_quad (Push op');

      | (Pop c, Pop c') ->
          emit_quad (Pop c');
          mov_if_cells_differ c c'

      | (Call c, Call c') ->
          emit_quad (Call c');
          mov_if_cells_differ c.call_dst c'.call_dst

      | (x, y) ->
          assert (x = y);
          emit_quad x
;;

let emit (e:emitter) (q':quad') : unit =
  emit_full e None q'
;;

let patch_jump (e:emitter) (jmp:int) (targ:int) : unit =
  let q = e.emit_quads.(jmp) in
    match q.quad_body with
        Jmp j ->
          assert (j.jmp_targ = CodeNone);
          e.emit_quads.(jmp) <-
            { q with quad_body =
                Jmp { j with jmp_targ = CodeLabel targ } }
      | _ -> ()
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
