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
(* 
let trans_expr emit expr = 
	match expr with 
		Ast.EXPR_literal (Ast.LIT_nil) -> Il.Nil
	  | Ast.EXPR_literal (Ast.LIT_bool false) -> Il.lit 0l
	  | Ast.EXPR_literal (Ast.LIT_bool true) -> Il.lit 1l
	  | Ast.EXPR_literal (Ast.LIT_char c) -> Il.lit (Char.code c)
	  (* ...literals *)
	  | Ast.EXPR_binary (binop, a, b) -> 
		  let lhs = trans_expr emit a in
		  let rhs = trans_expr emit b in
		  let dst = Il.next_vreg emit in 
		  let op = match binop with
			  Ast.BINOP_and -> Il.LAND
			| _ -> Il.ADD
		  in
			emit_triple emit None Il.MOV dst lhs;
			emit_triple emit None op dst rhs;
			dst
	  | Ast.EXPR_unary (unop, a) -> 
		  let src = trans_expr emit a in
		  let dst = Il.next_vreg emit in 
		  let op = match binop with
			  Ast.UNOP_not -> Il.LNOT
			| Ast.UNOP_neg -> Il.NEG
		  in
			emit_triple emit None op dst src;
			dst
*)
