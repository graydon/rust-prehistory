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




let ty_nonce = ref 0 
;;

let next_ty_nonce _ = (ty_nonce := (!ty_nonce) + 1; !ty_nonce)
;;

type ctxt = 
	{ ctxt_scopes: Ast.scope list;
	  ctxt_span: Ast.span option;
	  ctxt_sess: Session.sess;
	  ctxt_log: out_channel option;
	  ctxt_ptrsz: int64 }
;;

exception Semant_err of ((Ast.span option) * string)
;;

let err cx str = 
  (Semant_err (cx.ctxt_span, (str)))
;;

let	root_ctxt sess = { ctxt_scopes = []; 
					   ctxt_span = None;
					   ctxt_sess = sess;
					   ctxt_log = (if sess.Session.sess_log_env
								   then Some sess.Session.sess_log_out
								   else None);
					   ctxt_ptrsz = 4L }
;;

let rec expr_type cx expr = 
  (* FIXME: local type inference is more ugly than this *)
  let cx = { cx with ctxt_span = Some expr.Ast.span } in
	match expr.Ast.node with 
		Ast.EXPR_literal lit -> 
		  (match lit with 
			   Ast.LIT_nil -> Ast.TY_nil
			 | Ast.LIT_bool _ -> Ast.TY_bool
			 | Ast.LIT_unsigned (n, _) -> Ast.TY_mach (Ast.TY_unsigned, n)
			 | Ast.LIT_signed (n, _) -> Ast.TY_mach (Ast.TY_signed, n)
			 | Ast.LIT_ieee_bfp _ -> Ast.TY_mach (Ast.TY_ieee_bfp, 64)
			 | Ast.LIT_ieee_dfp _ -> Ast.TY_mach (Ast.TY_ieee_dfp, 128)
			 | Ast.LIT_int _ -> Ast.TY_int
			 | Ast.LIT_char _ -> Ast.TY_char
			 | Ast.LIT_str _ -> Ast.TY_str
			 | _ -> Ast.TY_any)		  
	  | Ast.EXPR_binary (_, a, b) -> 
		  let aty = expr_type cx a in
			(* let bty = expr_type cx b in *)
			aty
	  | Ast.EXPR_unary (_, e) -> 
		  expr_type cx e
	  | Ast.EXPR_lval lv -> 
		  (* FIXME: resolve lval, get type *)
		  Ast.TY_any
	  | _ -> raise (err cx "unhandled expression type in expr_type")
;;


let lval_type cx lval = 
  (* FIXME: this will be painful *)
  Ast.TY_any
;;

let infer_slot_from_init cx eo = 
  match eo with 
	  None -> Ast.SLOT_auto
	| Some e -> Ast.SLOT_interior (expr_type cx e)
;;

let update_inferred_type cx lval ty = 
  (* FIXME: this will be painful *)
  ()
;;

let rec size_of_ty cx t = 
  match t with 
	  Ast.TY_nil -> 0L
	| Ast.TY_bool -> 1L
	| Ast.TY_mach (_, n) -> Int64.of_int (n / 8)
	| Ast.TY_int -> cx.ctxt_ptrsz
	| Ast.TY_char -> 4L
	| Ast.TY_str -> cx.ctxt_ptrsz
	| Ast.TY_tup tys -> (Array.fold_left (fun n ty -> Int64.add n (size_of_slot cx ty)) 0L tys)
	| _ -> raise (err cx "unhandled type in size_of_ty")

and size_of_slot cx s = 
  match s with 
	  Ast.SLOT_exterior _ -> cx.ctxt_ptrsz
	| Ast.SLOT_read_alias _ -> cx.ctxt_ptrsz
	| Ast.SLOT_write_alias _ -> cx.ctxt_ptrsz
	| Ast.SLOT_interior t -> 
		size_of_ty cx t
	| Ast.SLOT_auto -> raise (err cx "SLOT_auto in size_of_slot")
;;

let layout_scope cx scope = 
  match scope with 
	  Ast.SCOPE_type ti -> ()
	| Ast.SCOPE_frame (len, sf) -> 
		let get_temp_slot nonce (i, s, eo) sz = ((nonce, i, s, eo) :: sz) in
		let get_named_slot name (i, item) sz = ((name, i, item) :: sz) in
		let temp_slots = Hashtbl.fold get_temp_slot sf.Ast.scope_temps [] in 
		let named_items = Hashtbl.fold get_named_slot sf.Ast.scope_items [] in
		let temp_slots = 
		  Array.of_list 
			(Sort.list 
			   (fun (a, _, _, _) (b, _, _, _) -> a < b) temp_slots) 
		in
		let named_items = 
		  Array.of_list 
			(Sort.list 
			   (fun (a, _, _) (b, _, _) -> a < b) named_items) 
		in
		  len := 0L;
		  Hashtbl.clear sf.Ast.scope_temps;
		  Hashtbl.clear sf.Ast.scope_items;
		  for i = 0 to (Array.length temp_slots) - 1 do
			let (nonce, _, s, eo) = temp_slots.(i) in
			let sz = size_of_slot cx s in
			  (match cx.ctxt_log with 
				   None -> ()
				 | Some out -> Printf.printf "laying out temp item %d: %Ld bytes @ %Ld\n" i sz (!len));
			  Hashtbl.add sf.Ast.scope_temps nonce ((!len), s, eo);
			  len := Int64.add (!len) sz
		  done;
		  for i = 0 to (Array.length named_items) - 1 do
			let (name, _, item) = named_items.(i) in
			let sz = match item.Ast.node with 
				Ast.MOD_ITEM_slot (slot, _) -> size_of_slot cx slot 
			  | _ -> 0L
			in
			  (match cx.ctxt_log with 
				   None -> ()
				 | Some out -> Printf.printf "laying out item %d (%s): %Ld bytes @ %Ld\n" i name sz (!len));
			  Hashtbl.add sf.Ast.scope_items name ((!len), item);
			  len := Int64.add (!len) sz
		  done
;; 

let extend_ctxt_scopes cx items =  
  let items' = Hashtbl.create (Hashtbl.length items) in 
  let insert_item name item = 
	Hashtbl.add items' name (-1L, item)
  in
	Hashtbl.iter insert_item items;	
  let scope = Ast.SCOPE_frame 
	((ref 0L), { Ast.scope_temps = Hashtbl.create 0;
				 Ast.scope_items = items' })
  in  
	layout_scope cx scope;
	let scopes = (scope :: cx.ctxt_scopes) 
	in
	  { cx with ctxt_scopes = scopes }
;;

let extend_ctxt_scopes_ty cx tyitems =  
  let scopes = (Ast.SCOPE_type tyitems) :: cx.ctxt_scopes in
	{ cx with ctxt_scopes = scopes }
;;

  
(* 
 * Extend a context with bindings for the type parameters of a
 * module item, mapping each to a new anonymous type.
 *)
let param_ctxt cx params span =
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
;;

(* 
 * Extend a context with bindings for the type parameters of a
 * module item or module-type item, given actual arguments. 
 *)
let apply_ctxt_generic cx extend ctor params args span = 
  let nparams = Array.length params in 
  let nargs = Array.length args in 
	if nargs != nparams 
	then raise (err cx "mismatched number of type parameters and arguments")
	else 
	  if nparams = 0
	  then cx
	  else
		let htab = Hashtbl.create nparams in 
		let addty i (lim, ident) = 
		  let ty = 
			match (args.(i), lim) with 
				(Ast.TY_lim _, Ast.UNLIMITED) -> 
				  raise (err cx "passing limited type where unlimited required")
			  | (Ast.TY_lim t, Ast.LIMITED) -> Ast.TY_lim t
			  | (t, Ast.LIMITED) -> Ast.TY_lim t
			  | (t, Ast.UNLIMITED) -> t
		  in
		  let item' = (ctor { Ast.decl_params = [| |];
							  Ast.decl_item = ty })
		  in
			Hashtbl.add htab ident { Ast.node = item'; 
									 Ast.span = span }
		in
		  Array.iteri addty params;
		  extend cx htab
;;

let apply_ctxt cx params args span = 
  apply_ctxt_generic cx 
	extend_ctxt_scopes 
	(fun x -> Ast.MOD_ITEM_public_type x)
	params args span
;;

let apply_ctxt_ty cx params args span = 
  apply_ctxt_generic cx 
	extend_ctxt_scopes_ty
	(fun x -> Ast.MOD_TYPE_ITEM_public_type x)
	params args span
;;


(* 
 * 'binding' is mostly just to permit returning a single ocaml type 
 * from a scope-based lookup; scopes (and the things found in them) 
 * have two separate flavours, those formed from modules and those 
 * formed from module *types*. The latter can nest in the former,
 * but not vice-versa.
 *)

type binding = 
	BINDING_item of (int64 * Ast.mod_item)
  | BINDING_type_item of Ast.mod_type_item
  | BINDING_temp of (int64 * Ast.slot * (Ast.expr option))

let rec lookup_ident cx ident = 
  match cx.ctxt_scopes with 
	  [] -> raise (err cx ("unknown identifier: '" ^ ident ^ "'"))
	| (x::xs) -> 
		match x with 
			Ast.SCOPE_frame (_, sf) -> 
			  let tab = sf.Ast.scope_items in
				if Hashtbl.mem tab ident
				then 
				  let (i, item) = Hashtbl.find tab ident in 
					({ cx with ctxt_span = Some item.Ast.span }, 
					 BINDING_item (i, item))
				else lookup_ident { cx with ctxt_scopes = xs } ident
		  | Ast.SCOPE_type st -> 
			  if Hashtbl.mem st ident
			  then 
				let tyitem = Hashtbl.find st ident in 
				  ({ cx with ctxt_span = Some tyitem.Ast.span }, 
				   BINDING_type_item tyitem)
			  else lookup_ident { cx with ctxt_scopes = xs } ident
;;


let rec lookup_temp cx temp = 
  match cx.ctxt_scopes with 
	  [] -> raise (err cx ("unknown temporary: '" ^ (string_of_int temp) ^ "'"))
	| (x::xs) -> 
		match x with 
			Ast.SCOPE_frame (_, sf) -> 
			  let tab = sf.Ast.scope_temps in
				if Hashtbl.mem tab temp
				then 
				  let se = Hashtbl.find tab temp in 
					({ cx with ctxt_span = None }, 
					 BINDING_temp se)
				else lookup_temp { cx with ctxt_scopes = xs } temp
		  | _ -> lookup_temp { cx with ctxt_scopes = xs } temp
;;

let rec mod_type_of_mod m = 
  let ty_items = Hashtbl.create 4 in 
  let add n i = Hashtbl.add ty_items n (mod_type_item_of_mod_item i) in
	Hashtbl.iter add m;
	ty_items
	  
and mod_type_item_of_mod_item item = 
  let decl params item = 
	{ Ast.decl_params = params;
	  Ast.decl_item = item }
  in
  let ty = 
	match item.Ast.node with 
		Ast.MOD_ITEM_opaque_type td -> 
		  (match (td.Ast.decl_params, td.Ast.decl_item) with 
			   (params, Ast.TY_lim _) -> 
				 Ast.MOD_TYPE_ITEM_opaque_type 
				   (decl params Ast.LIMITED)
			 | (params, _) -> 
				 Ast.MOD_TYPE_ITEM_opaque_type 
				   (decl params Ast.UNLIMITED))
	  | Ast.MOD_ITEM_public_type td ->
		  Ast.MOD_TYPE_ITEM_public_type td
	  | Ast.MOD_ITEM_pred pd -> 
		  Ast.MOD_TYPE_ITEM_pred 
			(decl pd.Ast.decl_params pd.Ast.decl_item.Ast.pred_ty)
	  | Ast.MOD_ITEM_mod md ->
			Ast.MOD_TYPE_ITEM_mod 
			  (decl md.Ast.decl_params (mod_type_of_mod md.Ast.decl_item))
	  | Ast.MOD_ITEM_fn fd -> 
		  Ast.MOD_TYPE_ITEM_fn
			(decl fd.Ast.decl_params fd.Ast.decl_item.Ast.fn_ty)
	  | Ast.MOD_ITEM_prog pd -> 
		  let prog = pd.Ast.decl_item in
		  let init_ty = (match prog.Ast.prog_init with 
							 None -> None
						   | Some init -> Some init.Ast.init_sig)
		  in
		  let prog_ty = 
			{ Ast.prog_mod_ty = mod_type_of_mod prog.Ast.prog_mod;
			  Ast.prog_init_ty = init_ty; }
		  in
			Ast.MOD_TYPE_ITEM_prog (decl pd.Ast.decl_params prog_ty)
	  | Ast.MOD_ITEM_slot (slot, _) -> 
		  Ast.MOD_TYPE_ITEM_slot slot
  in
	{ Ast.span = item.Ast.span;
	  Ast.node = ty }
;;


let mod_scope items = 
  { Ast.scope_temps = Hashtbl.create 4;
	Ast.scope_items = items; }
;;


let type_component_of_type_item cx tyitem comp = 
  match comp with 
	  Ast.COMP_ident id -> 
		(match tyitem.Ast.node with 
			 Ast.MOD_TYPE_ITEM_mod md -> 
			   let params = md.Ast.decl_params in 
			   let tyitems = md.Ast.decl_item in 				 
				 if Hashtbl.mem tyitems id
				 then 
				   let cx = param_ctxt cx params tyitem.Ast.span in
				   let cx = extend_ctxt_scopes_ty cx tyitems in
				   let ty_item = (Hashtbl.find tyitems id) in
					 (cx, ty_item)
				 else raise (err cx ("unknown component of module type: '" ^ id ^ "'"))
		   | _ -> raise (err cx ("looking up type in non-module type item: '" ^ id ^ "'")))
	| Ast.COMP_app (id, tys) -> 
		raise (Invalid_argument 
				 ("Semant.type_component_of_type_item lookup_type_in_item_by_component: " ^ 
					"unimplemented parametric types when looking up '" ^ id ^ "'"))
	| Ast.COMP_idx i -> 
		raise (err cx ("illegal index component in type name: .{" ^ (string_of_int i) ^ "}"))
;;


let apply_args_to_item cx item args = 
  let app params = 
	apply_ctxt cx params args item.Ast.span
  in
	match item.Ast.node with 
		Ast.MOD_ITEM_opaque_type td -> 
		  let cx = app td.Ast.decl_params in
			(cx, Ast.MOD_ITEM_opaque_type { td with Ast.decl_params = [| |] })
			  
	  | Ast.MOD_ITEM_public_type td -> 
		  let cx = app td.Ast.decl_params in
			(cx, Ast.MOD_ITEM_public_type { td with Ast.decl_params = [| |] })
			  
	| Ast.MOD_ITEM_pred pd -> 
		let cx = app pd.Ast.decl_params in
		  (cx, Ast.MOD_ITEM_pred { pd with Ast.decl_params = [| |] })
			
	| Ast.MOD_ITEM_mod md ->
		let cx = app md.Ast.decl_params in 
		  (cx, Ast.MOD_ITEM_mod { md with Ast.decl_params = [| |] })

	| Ast.MOD_ITEM_fn fd -> 
		let cx = app fd.Ast.decl_params in 
		  (cx, Ast.MOD_ITEM_fn { fd with Ast.decl_params = [| |] })

	| Ast.MOD_ITEM_prog pd ->
		let cx = app pd.Ast.decl_params in 
		  (cx, Ast.MOD_ITEM_prog { pd with Ast.decl_params = [| |] })

	| Ast.MOD_ITEM_slot _ -> 
		raise (err cx "applying types to slot")
;;


let apply_args_to_type_item cx tyitem args = 
  let app params = 
	apply_ctxt_ty cx params args tyitem.Ast.span
  in
	match tyitem.Ast.node with 
		Ast.MOD_TYPE_ITEM_opaque_type td -> 
		  let cx = app td.Ast.decl_params in
			(cx, Ast.MOD_TYPE_ITEM_opaque_type { td with Ast.decl_params = [| |] })
			  
	  | Ast.MOD_TYPE_ITEM_public_type td -> 
		  let cx = app td.Ast.decl_params in
			(cx, Ast.MOD_TYPE_ITEM_public_type { td with Ast.decl_params = [| |] })
			  
	| Ast.MOD_TYPE_ITEM_pred pd -> 
		let cx = app pd.Ast.decl_params in
		  (cx, Ast.MOD_TYPE_ITEM_pred { pd with Ast.decl_params = [| |] })
			
	| Ast.MOD_TYPE_ITEM_mod md ->
		let cx = app md.Ast.decl_params in 
		  (cx, Ast.MOD_TYPE_ITEM_mod { md with Ast.decl_params = [| |] })

	| Ast.MOD_TYPE_ITEM_fn fd -> 
		let cx = app fd.Ast.decl_params in 
		  (cx, Ast.MOD_TYPE_ITEM_fn { fd with Ast.decl_params = [| |] })

	| Ast.MOD_TYPE_ITEM_prog pd ->
		let cx = app pd.Ast.decl_params in 
		  (cx, Ast.MOD_TYPE_ITEM_prog { pd with Ast.decl_params = [| |] })

	| Ast.MOD_TYPE_ITEM_slot _ -> 
		raise (err cx "applying types to slot")
;;
		  

let rec lookup cx 
	(basefn : ctxt -> (ctxt * binding) -> (ctxt * 'a))
	(extfn : ctxt -> (ctxt * 'a) -> Ast.name_component -> (ctxt * 'a)) 
	name = 
  match name with 
	  Ast.NAME_base (Ast.BASE_ident id) -> basefn cx (lookup_ident cx id)
	| Ast.NAME_base (Ast.BASE_app (id, args)) -> 
		let (cx, binding) = lookup_ident cx id in
		  (match binding with 
			   BINDING_item (i, bi) -> 
				 let ((cx':ctxt), item) = apply_args_to_item cx bi args in 
				   basefn cx (cx', BINDING_item (i, {bi with Ast.node = item}))
			 | BINDING_type_item bti -> 
				 let ((cx':ctxt), tyitem) = apply_args_to_type_item cx bti args in 
				   basefn cx (cx', BINDING_type_item {bti with Ast.node = tyitem})
			 | BINDING_temp _ -> 
				 raise (err cx "applying types to temporary binding"))
	| Ast.NAME_base (Ast.BASE_temp temp) -> 
		basefn cx (lookup_temp cx temp)
	| Ast.NAME_ext (base, comp) -> 
		let base' = lookup cx basefn extfn base in
		  extfn cx base' comp 
;;


let lookup_type_item cx name = 
  let basefn cx (cx', binding) = 
	match binding with 
		BINDING_item (_, item) -> (cx', mod_type_item_of_mod_item item)
	  | BINDING_type_item tyitem -> (cx', tyitem)
	  | _ -> raise (err cx "unhandled case in Semant.lookup_type")
  in
  let extfn cx (cx', tyitem) comp = 
	type_component_of_type_item cx' tyitem comp 
  in
	lookup cx basefn extfn name
;;

let join_array sep arr = 
  let s = ref "" in
	for i = 0 to Array.length arr do
	  if i = 0
	  then s := arr.(i)
	  else s := (!s) ^ sep ^ arr.(i)
	done;
	(!s)
;;


let string_of_ty ty = 
  (* FIXME: possibly flesh this out, though it's just diagnostic. *)
  "T"
;;


let string_of_name_component comp = 
  match comp with 
	  Ast.COMP_ident id -> id
	| Ast.COMP_app (id, tys) -> 
		id ^ "[" ^ (join_array "," (Array.map string_of_ty tys)) ^ "]"
	| Ast.COMP_idx i -> 
		"{" ^ (string_of_int i) ^ "}"
;;


let rec string_of_name name = 
  match name with 
	  Ast.NAME_base (Ast.BASE_ident id) -> id
	| Ast.NAME_base (Ast.BASE_temp n) -> "<temp#" ^ (string_of_int n) ^ ">"
	| Ast.NAME_base (Ast.BASE_app (id, tys)) -> 
		id ^ "[" ^ (join_array "," (Array.map string_of_ty tys)) ^ "]"
	| Ast.NAME_ext (n, c) -> 
		(string_of_name n) ^ "." ^ (string_of_name_component c)
;;
		

let lookup_type cx name = 
  let parametric = 
	err cx "Semant.lookup_type found parametric binding, concrete type required"
  in
  let (cx', tyitem) = lookup_type_item cx name in 
	match tyitem.Ast.node with 
		Ast.MOD_TYPE_ITEM_opaque_type td -> 
		  if Array.length td.Ast.decl_params != 0
		  then raise parametric
		  else 
			let opaque = Ast.TY_opaque (next_ty_nonce ()) in
			  (match td.Ast.decl_item with 
				   Ast.LIMITED -> (cx', Ast.TY_lim opaque)
				 | Ast.UNLIMITED -> (cx', opaque))
	  | Ast.MOD_TYPE_ITEM_public_type td -> 
		  if Array.length td.Ast.decl_params != 0
		  then raise parametric
		  else (cx', td.Ast.decl_item)

	  | _ -> raise (err cx ((string_of_name name) ^ " names a non-type item"))
;;


let rec resolve_mod_items cx items = 
  let cx = extend_ctxt_scopes cx items in
	Hashtbl.iter (resolve_mod_item cx) items

		  
and resolve_mod_item cx id item =
  if cx.ctxt_sess.Session.sess_log_env
  then Printf.fprintf cx.ctxt_sess.Session.sess_log_out
	"resolving mod item %s\n" id
  else ();
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
		  (match s with 
			   Ast.SLOT_exterior t -> resolve_ty cx t
			 | Ast.SLOT_interior t -> resolve_ty cx t
			 | Ast.SLOT_read_alias t -> resolve_ty cx t
			 | Ast.SLOT_write_alias t -> resolve_ty cx t
			 | Ast.SLOT_auto -> raise (err cx "auto slot inference not implemented"));
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
  let cx' = 
	{ cx with ctxt_scopes = 
		block.Ast.block_scope :: cx.ctxt_scopes } 
  in
	Array.iter (resolve_stmt cx') block.Ast.block_stmts;
	layout_scope cx block.Ast.block_scope

  
and resolve_ty cx t = 
  match t with 
	  Ast.TY_any -> ()
	| Ast.TY_nil -> ()
	| Ast.TY_bool -> ()
	| Ast.TY_mach (tm, sz) -> ()
	| Ast.TY_int -> ()
	| Ast.TY_char -> ()
	| Ast.TY_str -> ()

	| Ast.TY_tup tt -> ()
	| Ast.TY_vec t -> ()
	| Ast.TY_rec tr -> ()

	| Ast.TY_fn tfn -> ()
	| Ast.TY_chan t -> ()
	| Ast.TY_port t -> ()
		
	| Ast.TY_named nm ->
		let
			(cx, defn) = lookup_type cx nm
		in
		  resolve_ty cx defn

	| Ast.TY_opaque non -> ()

	(* 
	   | Ast.TY_tag of ty_tag
	   | Ast.TY_iso of ty_iso
	   | Ast.TY_idx of int
	   
	   | Ast.TY_constrained (t, cstrs)
	   | Ast.TY_mod items -> ()
	   | Ast.TY_prog tp -> ()
	   | Ast.TY_lim t -> ()
	*)
	| _ -> ()
		
		
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
		let _ = lookup_ident cx id in
		  if cx.ctxt_sess.Session.sess_log_env
		  then Printf.fprintf cx.ctxt_sess.Session.sess_log_out			
			"lval: %s (resolved)\n" id
		  else ()
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
  let cx = { cx with ctxt_span = Some stmt.Ast.span } in
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
		  Array.iter (resolve_expr cx) args;
		  let cx' = 
			{ cx with ctxt_scopes = 
				f.Ast.foreach_scope :: cx.ctxt_scopes } 
		  in
			resolve_stmt cx' f.Ast.foreach_body;
			layout_scope cx f.Ast.foreach_scope;
			()
			  
	| Ast.STMT_for f -> 
		resolve_stmt cx f.Ast.for_init;
		let cx' = 
		  { cx with ctxt_scopes =
			  f.Ast.for_scope :: cx.ctxt_scopes }
		in
		  resolve_expr cx' f.Ast.for_test;
		  resolve_stmt cx' f.Ast.for_step;
		  resolve_stmt cx' f.Ast.for_body;
		  layout_scope cx f.Ast.for_scope;

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
		let scope = (match cx.ctxt_scopes with 
						 ((Ast.SCOPE_frame (_, f))::_) -> f
					   | _ -> raise (err cx "non-frame scope containing decl"))
		in
		  (match d with 
			   Ast.DECL_mod_item (id, item) -> 
				 Hashtbl.add scope.Ast.scope_items id (-1L, item);
				 resolve_mod_item cx id item
				   
			 | Ast.DECL_temp (slot, nonce, eo) -> 
				 let slot = match slot with 
					 Ast.SLOT_auto -> infer_slot_from_init cx eo 
				   | other -> other
				 in
				   Hashtbl.add scope.Ast.scope_temps nonce (-1L, slot, eo))

	| Ast.STMT_copy (lval, expr) -> 
		resolve_lval cx lval;
		resolve_expr cx expr;
		update_inferred_type cx lval (expr_type cx expr)
		  
	| Ast.STMT_call (dst, fn, args) -> 
		resolve_lval cx dst;
		resolve_lval cx fn;
		Array.iter (resolve_expr cx) args;
		update_inferred_type cx dst (lval_type cx fn)
		  
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
			Il.emit emit (Il.MOV Il.DATA32) dst lhs;
			Il.emit emit op dst rhs;
			dst

	  | Ast.EXPR_unary (unop, a) -> 
		  let src = trans_expr emit a in
		  let dst = Il.next_vreg emit in 
		  let op = match unop with
			  Ast.UNOP_not -> Il.LNOT
			| Ast.UNOP_neg -> Il.NEG
		  in
			Il.emit emit op dst src;
			dst
	  | _ -> raise (Invalid_argument "Semant.trans_expr: unimplemented translation")

let rec trans_stmt emit stmt = 
  match stmt.Ast.node with 
	  Ast.STMT_copy (lval, expr) -> 
		let dst = Il.Nil in
		let src = trans_expr emit expr in
		  Il.emit emit (Il.MOV Il.DATA32) dst src;
		  dst

	| _ -> raise (Invalid_argument "Semant.trans_stmt: unimplemented translation")
