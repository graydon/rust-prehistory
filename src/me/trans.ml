(* Translation *)

open Semant;;
open Common;;

let trans_lval emit _ = Il.Nil

let rec trans_expr emit expr = 
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
			Il.emit emit (Il.MOV Il.DATA32) dst lhs;
			Il.emit emit op dst rhs;
			dst

	  | Ast.EXPR_unary (unop, a) -> 
		  let src = trans_lval emit a in
		  let dst = Il.next_vreg emit in 
		  let op = match unop with
			  Ast.UNOP_not -> Il.LNOT
			| Ast.UNOP_neg -> Il.NEG
		  in
			Il.emit emit op dst src;
			dst
	  | _ -> raise (Invalid_argument "Semant.trans_expr: unimplemented translation")

let rec trans_stmt emit stmt = 
  match stmt.node with 
	  Ast.STMT_copy (lv_dst, lv_src) -> 
		let dst = Il.Nil in
		let src = trans_lval emit lv_src in
		  Il.emit emit (Il.MOV Il.DATA32) dst src;
		  dst

	| _ -> raise (Invalid_argument "Semant.trans_stmt: unimplemented translation")

and trans_fn emit fn = ()

and trans_prog emit p = ()

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
	emit.Il.emit_triples

(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
