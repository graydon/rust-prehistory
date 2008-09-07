(* 
 * This module performs most of the semantic lowering:
 *
 *   - building frames full of typed slots
 *   - calculating the size of every type
 *   - resolving every name to a slot
 *   - checking type compatibility of every slot
 *   - running the type-state dataflow algorithm
 *   - checking type-states
 *   - inferring points of allocation and deallocation
 *)

let rec trans_expr emit expr = 
	match expr.Ast.node with 
		Ast.EXPR_literal (Ast.LIT_nil) -> 
		  Il.Nil

	  | Ast.EXPR_literal (Ast.LIT_bool false) -> 
		  Il.Imm (Asm.IMM 0L)

	  | Ast.EXPR_literal (Ast.LIT_bool true) -> 
		  Il.Imm (Asm.IMM 1L)

	  | Ast.EXPR_literal (Ast.LIT_char c) -> 
		  Il.Imm (Asm.IMM (Int64.of_int (Char.code c)))

	  (* ...literals *)
	  | Ast.EXPR_binary (binop, a, b) -> 
		  let lhs = trans_expr emit a in
		  let rhs = trans_expr emit b in
		  let dst = Il.next_vreg emit in 
		  let op = match binop with
			  Ast.BINOP_and -> Il.LAND
			| _ -> Il.ADD
		  in
			Il.emit_triple emit None Il.MOV dst lhs;
			Il.emit_triple emit None op dst rhs;
			dst

	  | Ast.EXPR_unary (unop, a) -> 
		  let src = trans_expr emit a in
		  let dst = Il.next_vreg emit in 
		  let op = match unop with
			  Ast.UNOP_not -> Il.LNOT
			| Ast.UNOP_neg -> Il.NEG
		  in
			Il.emit_triple emit None op dst src;
			dst
	  | _ -> raise (Invalid_argument "Semant.trans_expr: unimplemented translation")

let rec trans_stmt emit stmt = 
  match stmt.Ast.node with 
	  Ast.STMT_copy (Ast.COPY_to_lval (lval, expr)) -> 
		let dst = Il.Nil in
		let src = trans_expr emit expr in
		  Il.emit_triple emit None Il.MOV dst src;
		  dst

	| _ -> raise (Invalid_argument "Semant.trans_stmt: unimplemented translation")
