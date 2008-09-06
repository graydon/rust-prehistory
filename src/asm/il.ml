
(* 
 * Every triple has a label, which is used as a potential target for a
 * local jump. The label is initially the same as the triple's index in the
 * block. As some triples may be deleted as redundant, it's possible that
 * the indices will shift down a bit. The triple with label N is always at
 * some index M <= N in the block. We never insert new triples.
 *)

type label = int

type slot = Vreg of int
			| HWreg of int
			| SP (* Stack pointer *)
			| PP (* Process pointer *)
			| Local of int
			| Imm of Asm.expr64
			| Deref of (slot * int64)
			| Nil
			| Label of label
;;

type op = 
    ADD | SUB | NEG 
  | IMUL | UMUL
  | IDIV | UDIV
  | IMOD | UMOD
  | MOV 
  | LAND | LOR | LNOT 
  | BAND | BOR | BXOR | BNOT 
  | LSL | LSR | ASR
  | JNZ | JZ | JC | JNC | JO | JNO | JMP 
  | CALL | RET | YIELD | RESUME | CCALL 
  | NOP 
  | END
;;

type triple = { triple_lab: label option;
				triple_op: op;
				triple_dst: slot;
				triple_src: slot;
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
    | Deref (s,i) -> Printf.fprintf out "*(%a + %s)" fmt_slot s (Int64.to_string i)
    | Label i -> Printf.fprintf out "label:%d" i
    | Nil -> ()
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
       | MOV -> "MOV"
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
	   | RESUME -> "RESUME"
	   | YIELD -> "YIELD"
       | END -> "---")
  

let fmt_lab out l = 
  match l with 
      None -> ()
    | Some i -> Printf.fprintf out "\t[label=%d]" i
  
let fmt_triple out t = 
  Printf.fprintf out "%a <- %a %a %a"
    fmt_slot t.triple_dst
    fmt_op t.triple_op
    fmt_slot t.triple_src
    fmt_lab t.triple_lab
;;

let print_triples qs = 
  Array.iteri (fun i q -> Printf.printf "[%6d]\t%a\n" i fmt_triple q) qs
;;

type emitter = { emit_n_hardregs: int;
				 mutable emit_pc: int;
				 mutable emit_next_label: int; 
				 mutable emit_next_vreg: int; 
				 mutable emit_next_spill: int;
				 mutable emit_triples: triples; }


let badt = { triple_lab = None;
			 triple_op = END;
			 triple_dst = Nil;
			 triple_src = Nil }
;;

let new_emitter n_hardregs = 
  { 
    emit_n_hardregs = n_hardregs;
    emit_pc = 0;
    emit_next_label = 0;
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

let next_label e = 
  let i = e.emit_next_label in
    e.emit_next_label <- i + 1;
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

let emit_triple e lab op dst src =
  grow_if_necessary e;
  e.emit_triples.(e.emit_pc) <- 
	{ triple_lab = lab;
	  triple_op = op;
	  triple_dst = dst;
	  triple_src = src;};
  e.emit_pc <- e.emit_pc + 1
;;

