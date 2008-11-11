
open Common;;

type mem = M8 | M16 | M32 | M64 
;;


type reg = Vreg of int
		   | HWreg of int
           | Preg of abi_pseudo_reg
;;


type operand =  Spill of int
			    | Label of int
			    | Imm of Asm.expr64
			    | Pcrel of fixup
                | Reg of reg
                | Mem of (mem * (reg option) * Asm.expr64)
			    | Nil
;;


type op = 
    ADD | SUB | NEG 
  | IMUL | UMUL
  | IDIV | UDIV
  | IMOD | UMOD
  | MOV 
  | AND | OR | NOT 
  | LSL | LSR | ASR
  | JNZ | JZ | JC | JNC | JO | JNO | JMP 
  | CALL | RET | YIELD | RESUME 
  | CCALL | CPUSH of mem | CPOP of mem | CRET
  | NOP | DEAD | END
;;


type quad = { quad_op: op;
			  quad_dst: operand;
			  quad_lhs: operand;
			  quad_rhs: operand;
			  quad_fixup: fixup option;
			}
;;


type quads = quad array
;;


let is_primitive_reg r = 
  match r with 
      HWreg _ -> true
    | _ -> false
;;


let is_primitive_operand op = 
  match op with 
      Spill _ -> false
    | Label _ -> false
    | Reg r -> is_primitive_reg r
    | Mem (_, Some r, _) -> is_primitive_reg r
    | _ -> true
;;


let is_primitive_quad q = 
  (is_primitive_operand q.quad_dst)
  && (is_primitive_operand q.quad_lhs)
  && (is_primitive_operand q.quad_rhs)
;;


let string_of_reg r = 
  match r with 
      Vreg i -> "vreg:" ^ (string_of_int i)
    | HWreg i -> "hreg:" ^ (string_of_int i)
    | Preg PP -> "pp"
    | Preg FP -> "fp"
    | Preg CP -> "cp"
    | Preg RP -> "rp"
;;


let string_of_operand operand = 
  match operand with
      Reg r -> string_of_reg r
    | Spill i -> "spill:" ^ (string_of_int i)
    | Imm (Asm.IMM i) -> Printf.sprintf "imm:0x%Lx" i
    | Imm _ -> "imm:??" 
	| Pcrel f -> "pcrel:" ^ f.fixup_name 
    | Mem (_, (Some r),(Asm.IMM 0L)) -> Printf.sprintf "*(%s)" (string_of_reg r)
    | Mem (_, (Some r),(Asm.IMM n)) -> Printf.sprintf "%s[%Ld]" (string_of_reg r) n
    | Mem (_, (Some r),_) -> Printf.sprintf "%s[??]" (string_of_reg r)
    | Mem (_, None,(Asm.IMM n)) -> Printf.sprintf "*(%Ld)" n
    | Mem (_, None,_) -> "*(??)" 
    | Label i -> "label:" ^ (string_of_int i)
    | Nil -> "nil"
;;


let string_of_mem m = 
  match m with 
	  M8 -> "M8"
	| M16 -> "M16"
	| M32 -> "M32"
	| M64 -> "M64"
;;


let string_of_op op = 
  match op with 
	  ADD -> "ADD"
    | SUB -> "SUB"
    | NEG -> "NEG"
    | UMUL -> "UMUL"
    | IMUL -> "IMUL"
    | UDIV -> "UDIV"
    | IDIV -> "IDIV"
    | UMOD -> "UMOD"
    | IMOD -> "IMOD"
    | MOV -> "MOV"
    | AND -> "AND"
    | OR -> "OR"
    | NOT -> "NOT"
    | LSL -> "LSL"
    | LSR -> "LSR"
    | ASR -> "ASR"
    | JNZ -> "JNZ"
    | JZ -> "JZ"
    | JC -> "JC"
    | JNC ->"JNC"
    | JO -> "JO"
    | JNO -> "JNO"
    | JMP -> "JMP"
    | CALL -> "CALL"
    | RET -> "RET"
    | NOP -> "NOP"
    | DEAD -> "DEAD"
	| CCALL -> "CCALL"
	| CPUSH m -> "CPUSH:" ^ string_of_mem m
	| CPOP m -> "CPOP:" ^ string_of_mem m
	| CRET -> "CRET"
	| RESUME -> "RESUME"
	| YIELD -> "YIELD"
    | END -> "---"
;;


let string_of_quad t = 
  match t.quad_op with 
      ADD | SUB | IMUL | UMUL | IDIV | UDIV | IMOD | UMOD
	| AND | OR | LSL | LSR | ASR -> 
		Printf.sprintf "%s = %s %s %s"
		  (string_of_operand t.quad_dst)
		  (string_of_operand t.quad_lhs)
		  (string_of_op t.quad_op)
		  (string_of_operand t.quad_rhs)

	| NOT | NEG -> 
		Printf.sprintf "%s = %s %s"
		  (string_of_operand t.quad_dst)
		  (string_of_op t.quad_op)
		  (string_of_operand t.quad_lhs)
		  
	| MOV -> 
		Printf.sprintf "%s = %s"
		  (string_of_operand t.quad_dst)
		  (string_of_operand t.quad_lhs)
	      
	| JNZ | JZ | JC | JNC | JO | JNO | JMP 
	| CALL | RESUME | CCALL | CPUSH _ | CPOP _ ->
		Printf.sprintf "%s %s"
		  (string_of_op t.quad_op)
		  (string_of_operand t.quad_lhs)
          
	| RET | YIELD | CRET | NOP | DEAD | END -> 
		(string_of_op t.quad_op)
;;

let print_quads qs = 
  Array.iteri (fun i q -> Printf.printf "[%6d]\t%s\n" i (string_of_quad q)) qs
;;

type emitter = { emit_n_hardregs: int;
				 mutable emit_pc: int;
				 mutable emit_next_vreg: int; 
				 mutable emit_next_spill: int;
				 mutable emit_quads: quads; }


let badq = { quad_op = END;
			 quad_dst = Nil;
			 quad_lhs = Nil;
			 quad_rhs = Nil;
			 quad_fixup = None }
;;

let deadq = { quad_op = DEAD;
			  quad_dst = Nil;
			  quad_lhs = Nil;
			  quad_rhs = Nil;
			  quad_fixup = None }
;;

let new_emitter n_hardregs = 
  { 
    emit_n_hardregs = n_hardregs;
    emit_pc = 0;
    emit_next_vreg = 0;
    emit_next_spill = 0;
    emit_quads = Array.create 4 badq;
  }
;;

let next_vreg e = 
  let i = e.emit_next_vreg in
    e.emit_next_vreg <- i + 1;
    (Vreg i)
;;


let next_spill e = 
  let i = e.emit_next_spill in
    e.emit_next_spill <- i + 1;
    i
;;


let grow_if_necessary e =
  let len = Array.length e.emit_quads in
    if e.emit_pc >= len - 1
    then
      let n = Array.create (2 * len) badq in
        Array.blit e.emit_quads 0 n 0 len;
        e.emit_quads <- n 
;;



let emit_full e fix op dst lhs rhs =
  grow_if_necessary e;
  e.emit_quads.(e.emit_pc) <- 
	{ quad_op = op;
	  quad_dst = dst;
	  quad_lhs = lhs;
	  quad_rhs = rhs;
	  quad_fixup = fix };
  e.emit_pc <- e.emit_pc + 1
;;

let emit e op dst lhs rhs = 
  emit_full e None op dst lhs rhs
;;


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
