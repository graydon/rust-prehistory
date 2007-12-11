
(* 
 * Every quad has a label, which is used as a potential target for a
 * local jump. The label is initially the same as the quad's index in the
 * block. As some quads may be deleted as redundant, it's possible that
 * the indices will shift down a bit. The quad with label N is always at
 * some index M <= N in the block. We never insert new quads.
 *)

type label = int

type slot = Vreg of int
	    | HWreg of int
	    | Spill of int
	    | Local of int
	    | Lit of int32
	    | Deref of (slot * int32)
	    | Nil
	    | Label of label
;;

type op = 
    ADD | SUB | NEG 
  | MUL | DIV 
  | MOD 
  | MOV 
  | LAND | LOR | LNOT 
  | BAND | BOR | BXOR | BNOT 
  | LSL | LSR | ASR
  | JNZ | JZ | JC | JNC | JO | JNO | JMP | CALL | RET
  | NOP 
  | END
;;

type quad = { quad_lab: label option;
	      quad_op: op;
	      quad_dst: slot;
	      quad_lhs: slot;
	      quad_rhs: slot;
	    }
;;

type quads = quad array
;;

let rec fmt_slot out slot = 
  match slot with
      Vreg i -> Printf.fprintf out "vr:%d" i
    | HWreg i -> Printf.fprintf out "hwreg:%d" i
    | Spill i -> Printf.fprintf out "spill:%d" i
    | Local i -> Printf.fprintf out "local:%d" i
    | Lit i -> Printf.fprintf out "lit:%s" (Int32.to_string i)
    | Deref (s,i) -> Printf.fprintf out "*(%a + %s)" fmt_slot s (Int32.to_string i)
    | Label i -> Printf.fprintf out "label:%d" i
    | Nil -> ()
;;

let fmt_op out op = 
  output_string out
    (match op with 
	 ADD -> "ADD"
       | SUB -> "SUB"
       | NEG -> "NEG"
       | MUL -> "MUL"
       | DIV -> "DIV"
       | MOD -> "MOD"
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
       | END -> "---")
  

let fmt_lab out l = 
  match l with 
      None -> ()
    | Some i -> Printf.fprintf out "\t[label=%d]" i
  
let fmt_quad out q = 
  Printf.fprintf out "%a <- %a %a %a %a"
    fmt_slot q.quad_dst
    fmt_op q.quad_op
    fmt_slot q.quad_lhs
    fmt_slot q.quad_rhs
    fmt_lab q.quad_lab
;;

let print_quads qs = 
  Array.iteri (fun i q -> Printf.printf "[%6d]\t%a\n" i fmt_quad q) qs
;;

type emitter = { emit_n_hardregs: int;
		 mutable emit_pc: int;
		 mutable emit_next_label: int; 
		 mutable emit_next_vreg: int; 
		 mutable emit_next_spill: int;
		 mutable emit_quads: quads; }


let badq = { quad_lab = None;
	     quad_op = END;
	     quad_dst = Nil;
	     quad_lhs = Nil;
	     quad_rhs = Nil }
;;

let new_emitter n_hardregs = 
  { 
    emit_n_hardregs = n_hardregs;
    emit_pc = 0;
    emit_next_label = 0;
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

let next_label e = 
  let i = e.emit_next_label in
    e.emit_next_label <- i + 1;
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

let emit_quad e lab op dst lhs rhs =
  grow_if_necessary e;
  e.emit_quads.(e.emit_pc) <- { quad_lab = lab;
				quad_op = op;
				quad_dst = dst;
				quad_lhs = lhs;
				quad_rhs = rhs; };
  e.emit_pc <- e.emit_pc + 1
;;

