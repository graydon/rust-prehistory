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

exception Semant_err of ((Ast.span option) * string)
;;

let ty_nonce = ref 0 
;;

let next_ty_nonce _ = (ty_nonce := (!ty_nonce) + 1; !ty_nonce)
;;

type ctxt = 
	{ ctxt_scopes: Ast.scope list;
	  ctxt_span: Ast.span option; }
;;

let	root_ctxt = { ctxt_scopes = []; 
				  ctxt_span = None }
;;

let rec lookup span ident scopes = 
  match scopes with 
	  [] -> raise (Semant_err (span, "unknown identifier: '" ^ ident ^ "'"))
	| (x::xs) -> 
		let tab = x.Ast.scope_items in 
		  if Hashtbl.mem tab ident
		  then Hashtbl.find tab ident
		  else lookup span ident xs
;;


let extend_ctxt_scopes cx items =  
  let scopes = { Ast.scope_temps = Hashtbl.create 0;
				 Ast.scope_items = items } :: cx.ctxt_scopes in
	{ cx with ctxt_scopes = scopes }
  

let rec resolve_mod_items cx items = 
  let cx = extend_ctxt_scopes cx items in
	Hashtbl.iter (resolve_mod_item cx) items

	  
and param_ctxt cx params span =
  let nparams = Array.length params in
	if nparams = 0
	then cx
	else 
	  let htab = Hashtbl.create nparams in
	  let addty (lim, ident) = 
		let nonce = next_ty_nonce () in
		let item' = (Ast.MOD_ITEM_public_type
					   { Ast.decl_params = [| |];
						 Ast.decl_item = (match lim with 
											  Ast.LIMITED -> (Ast.TY_lim (Ast.TY_opaque nonce))
											| Ast.UNLIMITED -> (Ast.TY_opaque nonce))})
		in
		  Hashtbl.add htab ident { Ast.node = item'; Ast.span = span }
	  in
		Array.iter addty params;
		extend_ctxt_scopes cx htab
		  

and resolve_mod_item cx id item =
  Printf.printf "resolving mod item %s\n" id;
  let span = item.Ast.span in
	match item.Ast.node with 
		Ast.MOD_ITEM_mod md ->
		  let cx = param_ctxt cx md.Ast.decl_params span in
			resolve_mod_items cx md.Ast.decl_item
			  
	  | Ast.MOD_ITEM_prog pd -> 
		  let cx = param_ctxt cx pd.Ast.decl_params span in
			resolve_prog cx pd.Ast.decl_item

	  | Ast.MOD_ITEM_fn fn -> 
		  let cx = param_ctxt cx fn.Ast.decl_params span in
			resolve_fn span cx fn.Ast.decl_item

	  | Ast.MOD_ITEM_slot (s, eo) -> 
		  (match eo with
			   None -> ()
			 | Some e -> resolve_expr cx e)

	  | _ -> ()


and resolve_fn span cx fn = 
  let bind = fn.Ast.fn_bind in
  let nbind = Array.length bind in
  let htab = Hashtbl.create nbind in
  let slots = 
	match fn.Ast.fn_ty.Ast.fn_sig.Ast.sig_input_slot with
		Ast.SLOT_interior (Ast.TY_tup slots) -> 
		  if nbind = 1
		  then [| Ast.SLOT_interior (Ast.TY_tup slots) |]
		  else slots
	  | slot -> [| slot |]
  in
  let addslot i s = 
	let item' = (Ast.MOD_ITEM_slot (s, None)) in
	let item = { Ast.node = item'; Ast.span = span } in
	  Hashtbl.add htab bind.(i) item
  in
	Array.iteri addslot slots;	  
	let cx = extend_ctxt_scopes cx htab in
	  resolve_stmt cx fn.Ast.fn_body

		
and resolve_prog cx prog = 
  let items = prog.Ast.prog_mod in
  let cx = extend_ctxt_scopes cx items in
	Hashtbl.iter (resolve_mod_item cx) items;
	resolve_init cx prog.Ast.prog_init;
  	resolve_stmt_option cx prog.Ast.prog_main;
  	resolve_stmt_option cx prog.Ast.prog_fini;


and resolve_init cx init = 
  ()


and resolve_block cx block = 
  let cx = 
	{ cx with ctxt_scopes = 
		block.Ast.block_scope :: cx.ctxt_scopes } 
  in
	Array.iter (resolve_stmt cx) block.Ast.block_stmts

	  
and resolve_expr cx expr = 
  let cx = { cx with ctxt_span = Some expr.Ast.span } in
	match expr.Ast.node with 
		Ast.EXPR_binary (_, a, b) -> 
		  resolve_expr cx a;
		  resolve_expr cx b
	  | Ast.EXPR_unary (_, e) -> 
		  resolve_expr cx e
	  | Ast.EXPR_lval lval -> 
		  resolve_lval cx lval
	  | Ast.EXPR_fn fn -> 
		  resolve_fn expr.Ast.span cx fn
	  | Ast.EXPR_prog p -> 
		  resolve_prog cx p
	  | Ast.EXPR_mod (ty, items) -> 
		  resolve_mod_items cx items
	  | Ast.EXPR_rec htab -> 
		  Hashtbl.iter (fun _ e -> resolve_expr cx e) htab
	  | Ast.EXPR_vec v -> 
		  Array.iter (resolve_expr cx) v
	  | Ast.EXPR_literal _ -> ()

		  
and resolve_lval cx lval = 
  match lval.Ast.node with 
	  Ast.LVAL_base (Ast.BASE_ident id) -> 
		(try
		   let _ = lookup cx.ctxt_span id cx.ctxt_scopes in			
			 Printf.printf "lval: %s (resolved)\n" id
		 with 
			 Semant_err (_, str) -> Printf.printf "semantic error: %s\n" str)		  
	| _ -> ()


and resolve_stmt_option cx stmtopt = 
  match stmtopt with 
	  None -> ()
	| Some s -> resolve_stmt cx s


and resolve_expr_option cx expropt = 
  match expropt with 
	  None -> ()
	| Some e -> resolve_expr cx e

		
and resolve_stmt cx stmt = 
  match stmt.Ast.node with 
	  Ast.STMT_while w -> 
		resolve_expr cx w.Ast.while_expr;
		resolve_stmt cx w.Ast.while_body

	| Ast.STMT_do_while w -> 
		resolve_expr cx w.Ast.while_expr;
		resolve_stmt cx w.Ast.while_body

	| Ast.STMT_foreach f -> 
		let (fn, args) = f.Ast.foreach_call in
		  resolve_lval cx fn;
		  let cx = 
			{ cx with ctxt_scopes = 
				f.Ast.foreach_scope :: cx.ctxt_scopes } 
		  in
			Array.iter (resolve_expr cx) args;
			resolve_stmt cx f.Ast.foreach_body

	| Ast.STMT_for f -> 
		resolve_stmt cx f.Ast.for_init;
		let cx = 
		  { cx with ctxt_scopes =
			  f.Ast.for_scope :: cx.ctxt_scopes }
		in
		  resolve_expr cx f.Ast.for_test;
		  resolve_stmt cx f.Ast.for_step;
		  resolve_stmt cx f.Ast.for_body;

	| Ast.STMT_if i -> 
		resolve_expr cx i.Ast.if_test;
		resolve_stmt cx i.Ast.if_then;
		resolve_stmt_option cx i.Ast.if_else

	| Ast.STMT_try t -> 
		resolve_stmt cx t.Ast.try_body;
		resolve_stmt_option cx t.Ast.try_fail;
		resolve_stmt_option cx t.Ast.try_fini
		
	| Ast.STMT_put (_, eo) -> 
		resolve_expr_option cx eo

	| Ast.STMT_ret (_, eo) -> 
		resolve_expr_option cx eo

	| Ast.STMT_block b -> 
		resolve_block cx b

	| Ast.STMT_decl d -> 
		let scope = List.hd cx.ctxt_scopes in
		  (match d with 
			   Ast.DECL_mod_item (id, item) -> 
				 Hashtbl.add scope.Ast.scope_items id item;
				 resolve_mod_item cx id item
				   
			 | Ast.DECL_temp (ty, nonce) -> 
				 Hashtbl.add scope.Ast.scope_temps nonce ty)

	| Ast.STMT_copy cp -> 
		(match cp with 
			 Ast.COPY_to_lval (lval, expr) -> 
			   resolve_lval cx lval;
			   resolve_expr cx expr
		   | _ -> ())

	| Ast.STMT_call (dst, fn, args) -> 
		resolve_lval cx dst;
		resolve_lval cx fn;
		Array.iter (resolve_expr cx) args

(* 
	| Ast.STMT_alt_tag of stmt_alt_tag
	| Ast.STMT_alt_type of stmt_alt_type
	| Ast.STMT_alt_port of stmt_alt_port
	| Ast.STMT_prove of (constrs)
	| Ast.STMT_check of (constrs)
	| Ast.STMT_checkif of (constrs * stmt)
	| Ast.STMT_send _ -> ()
	| Ast.STMT_recv _ -> ()
	| Ast.STMT_use (ty, ident, lval) 
*)
		  | _ -> ()


(* Translation *)

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
