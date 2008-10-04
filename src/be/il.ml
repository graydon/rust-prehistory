
type slot = Vreg of int
			| HWreg of int
			| SP (* Stack pointer *)
			| PP (* Process pointer *)
			| Local of int
			| Imm of Asm.expr64
			| Pcrel of Asm.fixup
			| Deref of (slot * int64)
			| Nil
			| Label of int
;;

type datasz = DATA8 | DATA16 | DATA32 | DATA64 
;;

type op = 
    ADD | SUB | NEG 
  | IMUL | UMUL
  | IDIV | UDIV
  | IMOD | UMOD
  | MOV of datasz 
  | LAND | LOR | LNOT 
  | BAND | BOR | BXOR | BNOT 
  | LSL | LSR | ASR
  | JNZ | JZ | JC | JNC | JO | JNO | JMP 
  | CALL | RET | YIELD | RESUME 
  | NOP 
  | CCALL | CPUSH of datasz | CPOP of datasz | CRET
  | END
;;

type triple = { triple_op: op;
				triple_dst: slot;
				triple_src: slot;
				triple_fixup: Asm.fixup option;
			  }
;;

type triples = triple array
;;

let rec fmt_slot out slot = 
  match slot with
      Vreg i -> Printf.fprintf out "vreg:%d" i
    | HWreg i -> Printf.fprintf out "hreg:%d" i
	| SP -> Printf.fprintf out "SP"
	| PP -> Printf.fprintf out "PP"
    | Local i -> Printf.fprintf out "local:%d" i
    | Imm i -> Printf.fprintf out "imm:??" 
	| Pcrel f -> Printf.fprintf out "pcrel:(%s)" f.Asm.fixup_name 
    | Deref (s,i) -> Printf.fprintf out "*(%a + %s)" fmt_slot s (Int64.to_string i)
    | Label i -> Printf.fprintf out "label:%d" i
    | Nil -> ()
;;


let fmt_datasz d = 
  match d with 
	  DATA8 -> "DATA8"
	| DATA16 -> "DATA16"
	| DATA32 -> "DATA32"
	| DATA64 -> "DATA64"
;;

let fmt_op out op = 
  output_string out
    (match op with 
		 ADD -> "ADD"
       | SUB -> "SUB"
       | NEG -> "NEG"
       | UMUL -> "UMUL"
       | IMUL -> "IMUL"
       | UDIV -> "UDIV"
       | IDIV -> "IDIV"
       | UMOD -> "UMOD"
       | IMOD -> "IMOD"
       | MOV d -> "MOV" ^ (fmt_datasz d)
       | LAND -> "LAND"
       | LOR -> "LOR"
       | LNOT -> "LNOT"
       | BAND -> "BAND"
       | BOR -> "BOR"
       | BXOR -> "BXOR"
       | BNOT -> "BNOT"
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
	   | CCALL -> "CCALL"
	   | CPUSH d -> "CPUSH" ^ (fmt_datasz d)
	   | CPOP d -> "CPOP" ^ (fmt_datasz d)
	   | CRET -> "CRET"
	   | RESUME -> "RESUME"
	   | YIELD -> "YIELD"
       | END -> "---")
;;


let fmt_triple out t =   
  Printf.fprintf out "%a %a %a"
    fmt_op t.triple_op
    fmt_slot t.triple_dst
    fmt_slot t.triple_src
    
;;

let print_triples qs = 
  Array.iteri (fun i q -> Printf.printf "[%6d]\t%a\n" i fmt_triple q) qs
;;

type emitter = { emit_n_hardregs: int;
				 mutable emit_pc: int;
				 mutable emit_next_vreg: int; 
				 mutable emit_next_spill: int;
				 mutable emit_triples: triples; }


let badt = { triple_op = END;
			 triple_dst = Nil;
			 triple_src = Nil;
			 triple_fixup = None }
;;

let new_emitter n_hardregs = 
  { 
    emit_n_hardregs = n_hardregs;
    emit_pc = 0;
    emit_next_vreg = 0;
    emit_next_spill = 0;
    emit_triples = Array.create 4 badt;
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
  let len = Array.length e.emit_triples in
  if e.emit_pc >= len - 1
  then
    let n = Array.create (2 * len) badt in
    Array.blit e.emit_triples 0 n 0 len;
    e.emit_triples <- n 
;;



let emit_full e fix op dst src =
  grow_if_necessary e;
  e.emit_triples.(e.emit_pc) <- 
	{ triple_op = op;
	  triple_dst = dst;
	  triple_src = src;
	  triple_fixup = fix };
  e.emit_pc <- e.emit_pc + 1
;;

let emit e op dst src = 
  emit_full e None op dst src
;;

