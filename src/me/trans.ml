(* Translation *)

open Semant;;
open Common;;

(* +++ At some point abstract this out per-machine-arch. *)
let is_2addr_machine = true;;
let ptr_mem = Il.M32;;
let fp_abi_operand = Il.Reg (Il.HWreg X86.esp);;
let pp_abi_operand = Il.Reg (Il.HWreg X86.ebp);;
let cp_abi_operand = Il.Mem (Il.M32, Some (Il.HWreg X86.ebp), Asm.IMM 0L);;
let rp_abi_operand = Il.Mem (Il.M32, Some (Il.HWreg X86.ebp), Asm.IMM 4L);;
(* --- At some point abstract this out per-machine-arch. *)

let marker = Il.Imm (Asm.IMM 0xdeadbeefL);;

let rec trans_lval_path emit lvp = 
  match lvp with 
      Ast.RES_pr FP -> fp_abi_operand
    | Ast.RES_pr PP -> pp_abi_operand
    | Ast.RES_pr CP -> cp_abi_operand
    | Ast.RES_pr RP -> rp_abi_operand
    | Ast.RES_idx (a, b) -> 
        let av = trans_lval_path emit a in
        let bv = trans_lval_path emit b in          
		let tmp = Il.Reg (Il.next_vreg emit) in 
		  Il.emit emit Il.ADD tmp av bv;
          tmp
    | Ast.RES_off (off, lv) -> 
        (match trans_lval_path emit lv with             
             Il.Mem (m, v, Asm.IMM off') -> 
               Il.Mem (m, v, Asm.IMM (Int64.add off off'))
           | v -> 
               let tmp = Il.Reg (Il.next_vreg emit) in
                 Il.emit emit Il.ADD tmp v (Il.Imm (Asm.IMM off));
                 tmp)
    | Ast.RES_deref lv -> 
        (match trans_lval_path emit lv with 
             Il.Reg r -> 
               Il.Mem (ptr_mem, Some r, Asm.IMM 0L)
           | v -> 
		       let tmp = (Il.next_vreg emit) in 
		         Il.emit emit Il.MOV (Il.Reg tmp) v Il.Nil;
                 Il.Mem (ptr_mem, Some tmp, Asm.IMM 0L))
;;

let trans_lval emit lv = 
  match !(lv.Ast.lval_res) with 
      None -> raise (Semant_err (None, "unresolved lval in trans_lval"))
    | Some res -> trans_lval_path emit res.Ast.res_path
;;

let trans_expr e expr = 
  let emit = Il.emit e in
	match expr with 
		Ast.EXPR_literal (Ast.LIT_nil) -> 
		  Il.Nil

	  | Ast.EXPR_literal (Ast.LIT_bool false) -> 
		  Il.Imm (Asm.IMM 0L)
            
	  | Ast.EXPR_literal (Ast.LIT_bool true) -> 
		  Il.Imm (Asm.IMM 1L)

	  | Ast.EXPR_literal (Ast.LIT_char c) -> 
		  Il.Imm (Asm.IMM (Int64.of_int (Char.code c)))

	  | Ast.EXPR_binary (binop, a, b) -> 
		  let lhs = trans_lval e a in
		  let rhs = trans_lval e b in
		  let dst = Il.Reg (Il.next_vreg e) in 
		  let op = match binop with
              Ast.BINOP_or -> Il.OR
            | Ast.BINOP_and -> Il.AND
                
            | Ast.BINOP_lsl -> Il.LSL
            | Ast.BINOP_lsr -> Il.LSR
            | Ast.BINOP_asr -> Il.ASR
                
            | Ast.BINOP_add -> Il.ADD
            | Ast.BINOP_sub -> Il.SUB

            (* FIXME: switch on type of operands. *)
            (* FIXME: wire to reg X86.eax, sigh.  *)
                (* 
            | Ast.BINOP_mul -> Il.UMUL
            | Ast.BINOP_div -> Il.UDIV
            | Ast.BINOP_mod -> Il.UMOD
                *)
            (*    
            | Ast.BINOP_eq ->
            | Ast.BINOP_ne -> 
                
            | Ast.BINOP_lt
            | Ast.BINOP_le
            | Ast.BINOP_ge
            | Ast.BINOP_gt
                *)                
                
			| _ -> Il.OR (* raise (Invalid_argument "Semant.trans_expr: unimplemented binop") *)
		  in
            if is_2addr_machine
            then 
			  (emit Il.MOV dst lhs Il.Nil;
               emit op dst dst rhs)
            else
			  emit op dst lhs rhs;
			dst
              
	  | Ast.EXPR_unary (unop, a) -> 
		  let src = trans_lval e a in
		  let dst = Il.Reg (Il.next_vreg e) in 
		  let op = match unop with
			  Ast.UNOP_not -> Il.NOT
			| Ast.UNOP_neg -> Il.NEG
		  in
            if is_2addr_machine
            then 
			  (emit Il.MOV dst src Il.Nil;
               emit op dst dst Il.Nil)
            else               
			  emit op dst src Il.Nil;
			dst
	  | _ -> marker (* raise (Invalid_argument "Semant.trans_expr: unimplemented translation") *)
;;


let rec trans_stmt e stmt =
  let emit = Il.emit e in
  let mark _ = e.Il.emit_pc in 
  let badlab = Il.Label (-1) in
  let imm_true = Il.Imm (Asm.IMM 1L) in
  let imm_false = Il.Imm (Asm.IMM 0L) in
  let patch i = 
    e.Il.emit_quads.(i) <- { e.Il.emit_quads.(i)
                             with Il.quad_dst = Il.Label (mark()) }
  in
    match stmt.node with 
	    Ast.STMT_copy (lv_dst, e_src) -> 
		  let dst = trans_lval e lv_dst in
		  let src = trans_expr e e_src in
		    emit Il.MOV dst src Il.Nil
              
	  | Ast.STMT_block stmts -> 
		  Array.iter (trans_stmt e) stmts.Ast.block_stmts
          
      | Ast.STMT_while sw -> 
          let back_jmp_target = mark() in 
          let (head_stmts, head_lval) = sw.Ast.while_lval in
		    Array.iter (trans_stmt e) head_stmts;
            let v = trans_lval e head_lval in
              emit Il.CMP v imm_false Il.Nil;
              let fwd_jmp_quad = mark() in
                emit Il.JE badlab Il.Nil Il.Nil;
                trans_stmt e sw.Ast.while_body;
                emit Il.JMP (Il.Label back_jmp_target) Il.Nil Il.Nil;
                patch fwd_jmp_quad
      | _ -> ()
                
(* 

    | Ast.STMT_do_while sw ->
  | STMT_foreach of stmt_foreach
  | STMT_for of stmt_for
  | STMT_if of stmt_if
  | STMT_try of stmt_try
  | STMT_put of (proto option * lval option)
  | STMT_ret of (proto option * lval option)
  | STMT_be of (proto option * lval * (lval array))
  | STMT_alt_tag of stmt_alt_tag
  | STMT_alt_type of stmt_alt_type
  | STMT_alt_port of stmt_alt_port
  | STMT_prove of (constrs)
  | STMT_check of (constrs)
  | STMT_checkif of (constrs * stmt)
  | STMT_call of (lval * lval * (lval array))
  | STMT_send of (lval * lval)
  | STMT_recv of (lval * lval)
  | STMT_decl of stmt_decl 
  | STMT_use of (ty * ident * lval)
	| _ -> raise (Invalid_argument "Semant.trans_stmt: unimplemented translation")
*)

and trans_fn emit fn = 
  trans_stmt emit fn.Ast.fn_body

and trans_prog emit p = 
  trans_mod_items emit p.Ast.prog_mod

and trans_mod_item emit name item = 
  match item.node with 
	  Ast.MOD_ITEM_fn f -> trans_fn emit f.Ast.decl_item
	| Ast.MOD_ITEM_mod m -> trans_mod_items emit m.Ast.decl_item
	| Ast.MOD_ITEM_prog p -> trans_prog emit p.Ast.decl_item
	| _ -> ()
 

and trans_mod_items emit items = 
  Hashtbl.iter (trans_mod_item emit) items

and trans_crate crate = 
  let emit = Il.new_emitter X86.n_hardregs in
	trans_mod_items emit crate;
    emit

(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
