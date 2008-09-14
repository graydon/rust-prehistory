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
let rec resolve_types_block blocks block = 
  let blocks = block :: blocks in
	for i = Array.length - 1 downto 0 
	do
	  resolve_types_in_stmt blocks block.(i)
	done
	  
and resolve_types_stmt blocks stmt = 
  match stmt.node with 
	  Ast.STMT_while w -> 
		resolve_types_expr w.Ast.while_expr;
		resolve_types_stmt w.Ast.while_body

	| Ast.STMT_do_while w -> 
		resolve_types_expr w.Ast.while_expr;
		resolve_types_stmt w.Ast.while_body

	| Ast.STMT_foreach f -> 
		
	| Ast.STMT_for of stmt_for
	| Ast.STMT_if of stmt_if
	| Ast.STMT_try of stmt_try
	| Ast.STMT_put of (proto option * expr option)
	| Ast.STMT_ret of (proto option * expr option)
	| Ast.STMT_alt_tag of stmt_alt_tag
	| Ast.STMT_alt_type of stmt_alt_type
	| Ast.STMT_alt_port of stmt_alt_port
	| Ast.STMT_prove of (constrs)
	| Ast.STMT_check of (constrs)
	| Ast.STMT_checkif of (constrs * stmt)
	| Ast.STMT_block of stmt_block
	| Ast.STMT_copy of stmt_copy
	| Ast.STMT_call _ -> () (lval * lval * (expr array))
	| Ast.STMT_send _ -> ()
	| Ast.STMT_recv _ -> ()
	| Ast.STMT_decl of stmt_decl 
	| Ast.STMT_use (ty, ident, lval)

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
