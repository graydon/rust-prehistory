(* Translation *)

open Semant;;
open Common;;

let rec trans_lval_path emit lvp = 
  match lvp with 
      Ast.RES_pr pr -> Il.Reg (Il.preg pr)
    | Ast.RES_idx (a, b) -> 
        let av = trans_lval_path emit a in
        let bv = trans_lval_path emit b in          
		let tmp = Il.Reg (Il.next_vreg emit) in 
		  Il.emit emit Il.ADD tmp av bv;
          tmp
    | Ast.RES_deref (Ast.RES_off (off, Ast.RES_pr pr)) -> 
        Il.Deref (Il.Preg pr, Asm.IMM off)
    | Ast.RES_deref lv -> 
        let addr = trans_lval_path emit lv in
		let tmp = Il.Reg (Il.next_vreg emit) in 
		  Il.emit emit Il.MOV tmp addr Il.Nil;
          Il.Deref (tmp, Asm.IMM 0L)
;;

let trans_lval emit lv = 
  match !lv.Ast.lval_res with 
      None -> raise Semant_err (None, "unresolved lval in trans_lval")
    | Some res -> trans_lval_path emit res.Ast.resolved_path
;;

let trans_expr emit expr = 
	match expr.node with 
		Ast.EXPR_literal (Ast.LIT_nil) -> 
		  Il.Nil

	  | Ast.EXPR_literal (Ast.LIT_bool false) -> 
		  Il.Imm (Asm.IMM 0L)

	  | Ast.EXPR_literal (Ast.LIT_bool true) -> 
		  Il.Imm (Asm.IMM 1L)

	  | Ast.EXPR_literal (Ast.LIT_char c) -> 
		  Il.Imm (Asm.IMM (Int64.of_int (Char.code c)))

	  | Ast.EXPR_binary (binop, a, b) -> 
		  let lhs = trans_lval emit a in
		  let rhs = trans_lval emit b in
		  let dst = Il.next_vreg emit in 
		  let op = match binop with
			  Ast.BINOP_and -> Il.LAND
			| _ -> Il.ADD
		  in
			Il.emit emit op dst lhs rhs;
			dst

	  | Ast.EXPR_unary (unop, a) -> 
		  let src = trans_lval emit a in
		  let dst = Il.next_vreg emit in 
		  let op = match unop with
			  Ast.UNOP_not -> Il.LNOT
			| Ast.UNOP_neg -> Il.NEG
		  in
			Il.emit emit op dst src Il.Nil;
			dst
	  | _ -> raise (Invalid_argument "Semant.trans_expr: unimplemented translation")
;;

let rec trans_stmt emit stmt = 
  match stmt.node with 
	  Ast.STMT_copy (lv_dst, lv_src) -> 
		let dst = Il.Nil in
		let src = trans_lval emit lv_src in
		  Il.emit emit (Il.MOV Il.DATA32) dst src Il.Nil

	| Ast.STMT_block stmts -> 
		Array.iter (trans_stmt emit) stmts.Ast.block_stmts
(* 
    | Ast.STMT_while sw -> 
        let fwd_jmp_pc = emit.emit_pc in 
          Il.emit emit 

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
  | STMT_block of stmt_block
  | STMT_copy of (lval * expr)
  | STMT_call of (lval * lval * (lval array))
  | STMT_send of (lval * lval)
  | STMT_recv of (lval * lval)
  | STMT_decl of stmt_decl 
  | STMT_use of (ty * ident * lval)
*)
	| _ -> raise (Invalid_argument "Semant.trans_stmt: unimplemented translation")

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
