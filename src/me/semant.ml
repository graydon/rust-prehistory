
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

open Common;;

let ty_nonce = ref 0 
;;

let next_ty_nonce _ = (ty_nonce := (!ty_nonce) + 1; !ty_nonce)
;;

type ctxt = 
	{ ctxt_frame_scopes: Ast.frame list;
	  ctxt_type_scopes: Ast.mod_type_items list;
	  ctxt_span: span option;
	  ctxt_sess: Session.sess;
	  ctxt_made_progress: bool ref;
	  ctxt_contains_autos: bool ref;
	  ctxt_contains_unresolved_types: bool ref;
	  ctxt_ptrsz: int64 }
;;

exception Semant_err of ((span option) * string)
;;

exception Auto_slot
;;

let err cx str = 
  (Semant_err (cx.ctxt_span, (str)))
;;

let	root_ctxt sess = { ctxt_frame_scopes = []; 
					   ctxt_type_scopes = [];
					   ctxt_span = None;
					   ctxt_sess = sess;
					   ctxt_made_progress = ref true;
                       ctxt_contains_autos = ref false;
                       ctxt_contains_unresolved_types = ref false;
					   ctxt_ptrsz = 4L }
;;

let log cx = 
  let sess = cx.ctxt_sess in
  let k1 s = 
    Printf.fprintf sess.Session.sess_log_out "env: %s\n%!" s
  in
  let k2 s = () in 
    Printf.ksprintf (if sess.Session.sess_log_env then k1 else k2)
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

let string_of_key k = 
  match k with 
      Ast.KEY_temp i -> "<temp#" ^ (string_of_int i) ^ ">"
    | Ast.KEY_ident i -> i
;;

let rec string_of_name_component comp = 
  match comp with 
	  Ast.COMP_ident id -> id
	| Ast.COMP_app (id, tys) -> 
		id ^ "[" ^ (join_array "," (Array.map string_of_ty tys)) ^ "]"
	| Ast.COMP_idx i -> 
		"{" ^ (string_of_int i) ^ "}"

and string_of_name name = 
  match name with 
	  Ast.NAME_base (Ast.BASE_ident id) -> id
	| Ast.NAME_base (Ast.BASE_temp n) -> "<temp#" ^ (string_of_int n) ^ ">"
	| Ast.NAME_base (Ast.BASE_app (id, tys)) -> 
		id ^ "[" ^ (join_array "," (Array.map string_of_ty tys)) ^ "]"
	| Ast.NAME_ext (n, c) -> 
		(string_of_name n) ^ "." ^ (string_of_name_component c)

and string_of_ty ty = 
  (* FIXME: possibly flesh this out, though it's just diagnostic. *)
  match ty with 
      Ast.TY_any -> "any"
    | Ast.TY_nil -> "nil"
    | Ast.TY_bool -> "bool"
    | Ast.TY_mach _ -> "mach"
    | Ast.TY_int -> "int"
    | Ast.TY_char -> "char"
    | Ast.TY_str -> "str"

    | Ast.TY_tup _ -> "tup"
    | Ast.TY_vec _ -> "vec"
    | Ast.TY_rec _ -> "rec"

    | Ast.TY_tag _ -> "tag"
    | Ast.TY_iso _ -> "iso"
    | Ast.TY_idx _ -> "idx"

    | Ast.TY_fn _ -> "fn"
    | Ast.TY_chan _ -> "chan"
    | Ast.TY_port _ -> "port"
        
    | Ast.TY_mod _ -> "mod"
    | Ast.TY_prog _ -> "prog"

    | Ast.TY_opaque _ -> "opaque"
    | Ast.TY_named name -> "named:" ^ (string_of_name name)
    | Ast.TY_type -> "ty"
      
    | Ast.TY_constrained _ -> "constrained"
    | Ast.TY_lim _ -> "lim"
;;

let rec size_of_ty cx t = 
  match t with 
	  Ast.TY_nil -> 0L
	| Ast.TY_bool -> 1L
	| Ast.TY_mach (_, n) -> Int64.of_int (n / 8)
	| Ast.TY_int -> cx.ctxt_ptrsz
	| Ast.TY_char -> 4L
	| Ast.TY_str -> cx.ctxt_ptrsz
	| Ast.TY_tup tys -> (Array.fold_left (fun n ty -> Int64.add n (slot_size cx ty)) 0L tys)
	| _ -> raise (err cx "unhandled type in size_of_ty")

and slot_size cx s = 
  match s with 
	  Ast.SLOT_exterior _ -> cx.ctxt_ptrsz
	| Ast.SLOT_read_alias _ -> cx.ctxt_ptrsz
	| Ast.SLOT_write_alias _ -> cx.ctxt_ptrsz
	| Ast.SLOT_interior t -> 
		size_of_ty cx t
	| Ast.SLOT_auto -> raise Auto_slot
;;

let slot_type cx s = 
  match s with 
	  Ast.SLOT_exterior t -> Some t
	| Ast.SLOT_read_alias t -> Some t
	| Ast.SLOT_write_alias t -> Some t
	| Ast.SLOT_interior t -> Some t
	| Ast.SLOT_auto -> None
;;

(* 
 * 'binding' is mostly just to permit returning a single ocaml type 
 * from a scope-based lookup; scopes (and the things found in them) 
 * have two separate flavours, those formed from modules and those 
 * formed from module *types*. The latter can nest in the former,
 * but not vice-versa.
 * 
 * All lookup functions should therefore consult the possibly-empty
 * nested type-scope before looking in 
 * the enclosing frame scope list.
 *)

type binding = 
	BINDING_item of (Ast.resolved_path * Ast.mod_item)
  | BINDING_slot of (Ast.resolved_path * ((Ast.slot ref) spanned))
  | BINDING_type_item of Ast.mod_type_item


(* 
 * Extend a context with bindings for the type parameters of a
 * module item or module-type item, given actual arguments. 
 *)
let apply_ctxt_generic 
    (cx:ctxt)
    (extend:ctxt -> ((Ast.ident,('a spanned)) Hashtbl.t) -> 'b)
    (ctor:Ast.ty Ast.decl -> 'a)
    (params:((Ast.ty_limit * Ast.ident) array))
    (args:Ast.ty array)
    (span:span) = 
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
			Hashtbl.add htab ident { node = item'; 
									 span = span }
		in
		  Array.iteri addty params;
		  extend cx htab


let rec extend_ctxt_by_mod_ty 
	(cx:ctxt) 
	(tyitems:Ast.mod_type_items) 
	: ctxt = 
  let scopes = tyitems :: cx.ctxt_type_scopes in
	{ cx with ctxt_type_scopes = scopes }
  
(* 
 * Extend a context with bindings for the type parameters of a
 * module item, mapping each to a new anonymous type.
 *)
and param_ctxt cx params span =
  let nparams = Array.length params in
	if nparams = 0
	then cx
	else 
	  let bind (lim, ident) = 
		let nonce = next_ty_nonce () in
		let item' = (Ast.MOD_ITEM_public_type
					   { Ast.decl_params = [| |];
						 Ast.decl_item = (match lim with 
											  Ast.LIMITED -> (Ast.TY_lim (Ast.TY_opaque nonce))
											| Ast.UNLIMITED -> (Ast.TY_opaque nonce))})
		in
		  (ident, { node = item'; span = span })
	  in
		extend_ctxt_by_frame cx (Array.map bind params)

and linearize_items_for_frame 
	(items:(Ast.ident,Ast.mod_item) Hashtbl.t)
	: ((Ast.ident * Ast.mod_item) array) = 
	let get_named_slot name item sz = ((name, item) :: sz) in
	let named_items = Hashtbl.fold get_named_slot items [] in
	  (Array.of_list 
		 (Sort.list 
			(fun (a, _) (b, _) -> a < b) named_items))

and apply_ctxt cx params args span = 
  apply_ctxt_generic cx 
	(fun cx items -> extend_ctxt_by_frame cx (linearize_items_for_frame items))
	(fun x -> Ast.MOD_ITEM_public_type x)
	params args span


and apply_ctxt_ty cx params args span = 
  apply_ctxt_generic cx 
	extend_ctxt_by_mod_ty
	(fun x -> Ast.MOD_TYPE_ITEM_public_type x)
	params args span


and lookup_ident 
	(cx:ctxt) 
	(fp:Ast.resolved_path) 
	(ident:Ast.ident)
	: (ctxt * binding) = 
  match cx.ctxt_type_scopes with 
	  (x::xs) -> 
		if Hashtbl.mem x ident
		then 
		  let tyitem = Hashtbl.find x ident in 
			({ cx with ctxt_span = Some tyitem.span }, 
			 BINDING_type_item tyitem)
		else 
		  lookup_ident 
			{ cx with ctxt_type_scopes = xs } fp ident
	| [] -> 
		(match cx.ctxt_frame_scopes with 
			 [] -> raise (err cx ("unknown identifier: '" ^ ident ^ "'"))
		   | (x::xs) -> 
			   let tab = x.Ast.frame_slots in
				 if Hashtbl.mem tab (Ast.KEY_ident ident)
				 then 
				   let (layout, slotr) = Hashtbl.find tab (Ast.KEY_ident ident) in
					 ({cx with ctxt_span = Some slotr.span}, 
					  BINDING_slot (Ast.RES_off (layout.layout_offset, fp), slotr))
				 else 
			       let tab = x.Ast.frame_items in
				     if Hashtbl.mem tab ident
				     then 
				       let (layout, item) = Hashtbl.find tab ident in
					     ({cx with ctxt_span = Some item.span}, 
					      BINDING_item (Ast.RES_off (layout.layout_offset, fp), item))
                     else 
				       lookup_ident 
					     { cx with ctxt_frame_scopes = xs } 
					     (Ast.RES_deref (Ast.RES_off (0L, fp))) ident)


and lookup_temp (cx:ctxt) 
	(fp:Ast.resolved_path) 
	(temp:Ast.nonce) 
	: (ctxt * binding) = 
  match cx.ctxt_frame_scopes with 
	  [] -> raise (err cx ("unknown temporary: '" ^ (string_of_int temp) ^ "'"))
	| (x::xs) -> 
		let tab = x.Ast.frame_slots in
		  if Hashtbl.mem tab (Ast.KEY_temp temp)
		  then 
		    let (layout, slot) = Hashtbl.find tab (Ast.KEY_temp temp) in 
			  ({ cx with ctxt_span = Some slot.span }, 
			   BINDING_slot (Ast.RES_off (layout.layout_offset, fp), slot))
		  else 
		    lookup_temp 
			  { cx with ctxt_frame_scopes = xs } 
			  (Ast.RES_deref (Ast.RES_off (0L, fp)))
			  temp
              

and lookup_base cx base = 
  match base with 
	  (Ast.BASE_ident id) -> lookup_ident cx Ast.RES_fp id
	| (Ast.BASE_temp t) -> lookup_temp cx Ast.RES_fp t
	| _ -> raise (err cx "unhandled name base variant in lookup_base")


and fmt_base cx base = 
  match base with 
	  Ast.BASE_ident id -> id
	| Ast.BASE_temp t -> ("temp#" ^ (string_of_int t))
	| _ -> raise (err cx "unhandled name base variant in fmt_base")


and mod_type_of_mod m = 
  let ty_items = Hashtbl.create 4 in 
  let add n i = Hashtbl.add ty_items n (mod_type_item_of_mod_item i) in
	Hashtbl.iter add m;
	ty_items

and prog_type_of_prog prog = 
  let init_ty = (match prog.Ast.prog_init with 
					 None -> None
				   | Some init -> Some init.Ast.init_sig)
  in
	{ Ast.prog_mod_ty = mod_type_of_mod prog.Ast.prog_mod;
	  Ast.prog_init_ty = init_ty; }

	  
and mod_type_item_of_mod_item item = 
  let decl params item = 
	{ Ast.decl_params = params;
	  Ast.decl_item = item }
  in
  let ty = 
	match item.node with 
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
          let prog_ty = prog_type_of_prog pd.Ast.decl_item in
	        Ast.MOD_TYPE_ITEM_prog (decl pd.Ast.decl_params prog_ty)
  in
	{ span = item.span;
	  node = ty }


and type_component_of_type_item cx tyitem comp = 
  match comp with 
	  Ast.COMP_ident id -> 
		(match tyitem.node with 
			 Ast.MOD_TYPE_ITEM_mod md -> 
			   let params = md.Ast.decl_params in 
			   let tyitems = md.Ast.decl_item in 				 
				 if Hashtbl.mem tyitems id
				 then 
				   let cx = param_ctxt cx params tyitem.span in
				   let cx = extend_ctxt_by_mod_ty cx tyitems in
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


and apply_args_to_item cx item args = 
  let app params = 
	apply_ctxt cx params args item.span
  in
	match item.node with 
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


and apply_args_to_type_item cx tyitem args = 
  let app params = 
	apply_ctxt_ty cx params args tyitem.span
  in
	match tyitem.node with 
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
		  

and lookup cx 
	(basefn : ctxt -> (ctxt * binding) -> (ctxt * 'a))
	(extfn : ctxt -> (ctxt * 'a) -> Ast.name_component -> (ctxt * 'a)) 
	name = 
  match name with 
	  Ast.NAME_base (Ast.BASE_ident id) -> basefn cx (lookup_ident cx Ast.RES_fp id)
	| Ast.NAME_base (Ast.BASE_app (id, args)) -> 
		let (cx, binding) = lookup_ident cx Ast.RES_fp id in
		  (match binding with 
			   BINDING_item (i, bi) -> 
				 let ((cx':ctxt), item) = apply_args_to_item cx bi args in 
				   basefn cx (cx', BINDING_item (i, {bi with node = item}))
			 | BINDING_type_item bti -> 
				 let ((cx':ctxt), tyitem) = apply_args_to_type_item cx bti args in 
				   basefn cx (cx', BINDING_type_item {bti with node = tyitem})
			 | BINDING_slot _ -> 
				 raise (err cx "applying types to slot"))
	| Ast.NAME_base (Ast.BASE_temp temp) -> 
		basefn cx (lookup_temp cx Ast.RES_fp temp)
	| Ast.NAME_ext (base, comp) -> 
		let base' = lookup cx basefn extfn base in
		  extfn cx base' comp 


and lookup_type_item cx name = 
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

and type_of_mod_item cx item = 
  let check_concrete params ty = 
    if Array.length params = 0 
    then ty 
    else raise (err cx "item has parametric type in type_of_mod_item")
  in
	match item.node with 
		Ast.MOD_ITEM_opaque_type td -> 
          check_concrete td.Ast.decl_params Ast.TY_type
			  
	  | Ast.MOD_ITEM_public_type td -> 
          check_concrete td.Ast.decl_params Ast.TY_type
			  
	  | Ast.MOD_ITEM_pred pd -> 
          check_concrete pd.Ast.decl_params 
            (Ast.TY_fn { Ast.fn_pure = true;
                         Ast.fn_lim = Ast.UNLIMITED;
                         Ast.fn_sig = pd.Ast.decl_item.Ast.pred_ty;
                         Ast.fn_proto = None })
			  
	  | Ast.MOD_ITEM_mod md ->
          check_concrete md.Ast.decl_params 
            (Ast.TY_mod (mod_type_of_mod md.Ast.decl_item))
              
	  | Ast.MOD_ITEM_fn fd -> 
          check_concrete fd.Ast.decl_params
            (Ast.TY_fn fd.Ast.decl_item.Ast.fn_ty)
            
	  | Ast.MOD_ITEM_prog pd ->
          check_concrete pd.Ast.decl_params
            (Ast.TY_prog (prog_type_of_prog pd.Ast.decl_item))


and lookup_type cx name = 
  let parametric = 
	err cx "Semant.lookup_type found parametric binding, concrete type required"
  in
  let (cx', tyitem) = lookup_type_item cx name in 
	match tyitem.node with 
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

and lval_type cx lval =
  match !(lval.Ast.lval_res) with 
      Some res -> 
        (match res.Ast.res_target with 
             (Ast.RES_slot slotr) -> slot_type cx !(slotr.node)
           | (Ast.RES_item item) -> Some (type_of_mod_item cx item))
	| _ -> None

and expr_type cx expr = 
  let concretize tyo = 
    match tyo with 
        Some (Ast.TY_named _) -> None
      | Some t -> Some t
      | None -> None
  in
    match expr with 
	    Ast.EXPR_literal lit -> 
		  (match lit with 
			   Ast.LIT_nil -> Some Ast.TY_nil
		     | Ast.LIT_bool _ -> Some Ast.TY_bool
		     | Ast.LIT_unsigned (n, _) -> Some (Ast.TY_mach (Ast.TY_unsigned, n))
		     | Ast.LIT_signed (n, _) -> Some (Ast.TY_mach (Ast.TY_signed, n))
		     | Ast.LIT_ieee_bfp _ -> Some (Ast.TY_mach (Ast.TY_ieee_bfp, 64))
		     | Ast.LIT_ieee_dfp _ -> Some (Ast.TY_mach (Ast.TY_ieee_dfp, 128))
		     | Ast.LIT_int _ -> Some Ast.TY_int
		     | Ast.LIT_char _ -> Some Ast.TY_char
		     | Ast.LIT_str _ -> Some Ast.TY_str
		     | Ast.LIT_custom _ -> None)
		    
	  (* FIXME: check appropriateness of applying op to type *)
	  | Ast.EXPR_binary (op, a, b) -> 
		  (match (concretize (lval_type cx a), 
                  concretize (lval_type cx b)) with 
			   (Some t1, Some t2) -> 
			     if t1 = t2 
			     then 
                   match op with 
                       Ast.BINOP_eq | Ast.BINOP_ne
                     | Ast.BINOP_lt | Ast.BINOP_le
                     | Ast.BINOP_gt | Ast.BINOP_ge -> Some Ast.TY_bool
                     | _ -> Some t1
			     else raise (err cx ("mismatched binary expression types in expr_type: "
                                     ^ (string_of_ty t1) ^ " vs. " ^ (string_of_ty t2)))
		     | _ -> (cx.ctxt_contains_unresolved_types := true; None))
            
	  | Ast.EXPR_unary (_, lv) -> concretize (lval_type cx lv)
	  | Ast.EXPR_lval lv -> concretize (lval_type cx lv)
	  | _ -> raise (err cx "unhandled expression type in expr_type")

and lval_fn_result_type cx fn =
  let rec f ty = 
    match ty with 
        Ast.TY_fn f -> 
          let slot = f.Ast.fn_sig.Ast.sig_output_slot in
            slot_type cx slot
      | Ast.TY_lim t -> f t
      | _ -> raise (err cx "non-function type in function context")
  in
    match lval_type cx fn with 
        Some t -> 
          let ft = f t in
          let s = (match ft with None -> "<none>" | Some t -> string_of_ty t) in
            log cx "function return type: %s" s;
            ft
      | _ -> None

and lval_fn_arg_type cx fn i = 
  let badcount = "argument-count mismatch in lval_fn_arg_type" in
  let rec f ty = 
    match ty with 
        Ast.TY_fn f -> 
          let slot = f.Ast.fn_sig.Ast.sig_input_slot in
            (match slot_type cx slot with
                 None -> None
               | Some (Ast.TY_tup tup) -> 
                   (if i >= 0 or i < Array.length tup
                    then slot_type cx tup.(i)
                    else raise (err cx badcount))
               | Some t -> 
                   (if i = 0 
                    then Some t
                    else raise (err cx badcount)))
      | _ -> raise (err cx "non-function type in lval_fn_arg_type")
  in
    match lval_type cx fn with 
        Some ty -> f ty
      | _ -> None


and layout_frame 
	(cx:ctxt) 
	(frame:Ast.frame) 
	: unit = 
  let get_slot key (l, s) sz = ((key, l, s) :: sz) in
  let slots = Hashtbl.fold get_slot frame.Ast.frame_slots [] in 
  let slots = 
		  Array.of_list 
			(Sort.list 
			   (fun (a, _, _) (b, _, _) -> a < b) slots)
  in
    (* FIXME: lay out the module items too. *)
    (* 
       let get_item name (l, i) sz = ((name, l, i) :: sz) in
       let items = Hashtbl.fold get_slot frame.Ast.frame_items [] in
       let items = 
	   Array.of_list 
	   (Sort.list 
	   (fun (a, _, _) (b, _, _) -> a < b) items) 
       in
    *)
	for i = 0 to (Array.length slots) - 1 do
	  let (_, _, s) = slots.(i) in
        resolve_slot_ref cx None s;
    done;          
	try 
	  frame.Ast.frame_layout.layout_size <- 0L;
	  for i = 0 to (Array.length slots) - 1 do
        let offset = frame.Ast.frame_layout.layout_size in
		let (key, layout, s) = slots.(i) in
          log cx "laying out slot %d (%s)" i (string_of_key key);
		  let sz = slot_size cx (!(s.node)) in
            log cx "  == %Ld bytes @ %Ld" sz offset;
            layout.layout_size <- sz;
            layout.layout_offset <- offset;
			frame.Ast.frame_layout.layout_size <- Int64.add offset sz
	  done;
	with 
		Auto_slot -> 
          log cx "hit auto slot";
		  cx.ctxt_contains_autos := true
            

and extend_ctxt_by_frame 
	(cx:ctxt)
	(items:(Ast.ident * Ast.mod_item) array)
	: ctxt = 
  log cx "extending ctxt by frame";
  let items' = Hashtbl.create (Array.length items) in
	for i = 0 to (Array.length items) - 1
	do
	  let (ident, item) = items.(i) in 
		Hashtbl.add items' ident (new_layout(), item)
	done;
	let frame = 
	  { Ast.frame_layout = new_layout();
		Ast.frame_slots = Hashtbl.create 0;
		Ast.frame_items = items'; }
	in
	  layout_frame cx frame;
	  { cx with ctxt_frame_scopes = (frame :: cx.ctxt_frame_scopes) }


and resolve_mod_items cx items = 
  let cx = extend_ctxt_by_frame cx (linearize_items_for_frame items) in
	Hashtbl.iter (resolve_mod_item cx) items

		  
and resolve_mod_item cx id item =
  log cx "resolving mod item %s" id;
  let span = item.span in
	match item.node with 
		Ast.MOD_ITEM_mod md ->
		  let cx = param_ctxt cx md.Ast.decl_params span in
			resolve_mod_items cx md.Ast.decl_item
			  
	  | Ast.MOD_ITEM_prog pd -> 
		  let cx = param_ctxt cx pd.Ast.decl_params span in
			resolve_prog cx pd.Ast.decl_item

	  | Ast.MOD_ITEM_fn fn -> 
		  let cx = param_ctxt cx fn.Ast.decl_params span in
			resolve_fn span cx fn.Ast.decl_item

	  | _ -> ()


and resolve_fn span cx fn = 
  let cx = 
	{ cx with ctxt_frame_scopes = 
		fn.Ast.fn_frame :: cx.ctxt_frame_scopes } 
  in
	resolve_stmt cx fn.Ast.fn_body

		
and resolve_prog cx prog = 
  let items = prog.Ast.prog_mod in
  let cx = extend_ctxt_by_frame cx (linearize_items_for_frame items) in
	Hashtbl.iter (resolve_mod_item cx) items;
	resolve_init cx prog.Ast.prog_init;
  	resolve_stmt_option cx prog.Ast.prog_main;
  	resolve_stmt_option cx prog.Ast.prog_fini;


and resolve_init cx init = 
  ()


and resolve_block cx block = 
  let cx' = 
	{ cx with ctxt_frame_scopes = 
		block.Ast.block_frame :: cx.ctxt_frame_scopes } 
  in
    log cx "resolving block with %d items, %d slots"
	  (Hashtbl.length block.Ast.block_frame.Ast.frame_items)
	  (Hashtbl.length block.Ast.block_frame.Ast.frame_slots);
	layout_frame cx block.Ast.block_frame;
	Array.iter (resolve_stmt cx') block.Ast.block_stmts

and resolve_slot 
    (cx:ctxt) 
    (tyo:Ast.ty option) 
    (slot:Ast.slot) 
    : Ast.slot = 
  let resolve_and_check_type ty = 
    let ty = resolve_ty cx ty in
      match tyo with 
          None -> ty
        | Some t -> 
            if ty = t
            then ty
            else raise (err cx ("mismatched types in resolve_slot: slot is " 
                                ^ (string_of_ty ty) 
                                ^ " constraint implies " 
                                ^ (string_of_ty t)))
  in
    match slot with 
	  Ast.SLOT_exterior ty -> 
        Ast.SLOT_exterior (resolve_and_check_type ty)
	| Ast.SLOT_interior ty -> 
        Ast.SLOT_interior (resolve_and_check_type ty)
	| Ast.SLOT_read_alias ty -> 
        Ast.SLOT_read_alias (resolve_and_check_type ty)
	| Ast.SLOT_write_alias ty -> 
        Ast.SLOT_write_alias (resolve_and_check_type ty)
	| Ast.SLOT_auto -> 
        (match tyo with 
             None -> Ast.SLOT_auto
           | Some t -> Ast.SLOT_interior t)

and resolve_slot_ref 
    (cx:ctxt) 
    (tyo:Ast.ty option) 
    (slotr:(Ast.slot ref) spanned) 
    : unit = 
  let slot = !(slotr.node) in
  let newslot = resolve_slot cx tyo slot in
    if slot = newslot
    then ()
    else (log cx "----- made progress ----";
          cx.ctxt_made_progress := true;
          slotr.node := newslot)
  
and resolve_ty 
    (cx:ctxt)
    (t:Ast.ty)
    : Ast.ty = 
  match t with 
	  Ast.TY_any | Ast.TY_nil | Ast.TY_bool 
    | Ast.TY_mach _ | Ast.TY_int | Ast.TY_char
	| Ast.TY_str | Ast.TY_opaque _ -> t

	| Ast.TY_tup tt ->
        Ast.TY_tup (Array.map (resolve_slot cx None) tt)
	| Ast.TY_vec t -> 
        Ast.TY_vec (resolve_ty cx t)
	| Ast.TY_rec tr ->
        let newt = Hashtbl.create (Hashtbl.length tr) in
          (Hashtbl.iter (fun k s -> Hashtbl.add newt k (resolve_slot cx None s)) tr;
           Ast.TY_rec newt)
	| Ast.TY_chan t -> 
        Ast.TY_chan (resolve_ty cx t)
	| Ast.TY_port t -> 
        Ast.TY_port (resolve_ty cx t)
		  
	| Ast.TY_named nm ->
		let
			(cx, defn) = lookup_type cx nm
		in
		  resolve_ty cx defn

	(* 
	   | Ast.TY_fn tfn -> ()
	   | Ast.TY_tag of ty_tag
	   | Ast.TY_iso of ty_iso
	   | Ast.TY_idx of int
	   
	   | Ast.TY_constrained (t, cstrs)
	   | Ast.TY_mod items -> ()
	   | Ast.TY_prog tp -> ()
	   | Ast.TY_lim t -> ()
	*)

	| _ -> raise (err cx "unhandled type in resolve_ty")
		
		
and resolve_expr cx expr = 
  match expr with 
	  Ast.EXPR_binary (_, a, b) -> 
		resolve_lval cx None a;
		resolve_lval cx None b
	| Ast.EXPR_unary (_, e) -> 
		resolve_lval cx None e
	| Ast.EXPR_lval lval -> 
		resolve_lval cx None lval
	| Ast.EXPR_rec htab -> 
		Hashtbl.iter (fun _ lv -> resolve_lval cx None lv) htab
	| Ast.EXPR_vec v -> 
		Array.iter (resolve_lval cx None) v
	| Ast.EXPR_tup v -> 
		Array.iter (resolve_lval cx None) v
	| Ast.EXPR_literal _ -> ()
		  

and resolve_lval cx tyo lval = 
  let bind_to_slot path slot = 
    let res = { Ast.res_path = path;
                Ast.res_target = Ast.RES_slot slot; } 
    in
      resolve_slot_ref cx tyo slot;
      lval.Ast.lval_res := Some res
  in
  let bind_to_item path item = 
    let res = { Ast.res_path = path;
                Ast.res_target = Ast.RES_item item; } 
    in
      lval.Ast.lval_res := Some res
  in
    match !(lval.Ast.lval_res) with 
        Some r -> (match r.Ast.res_target with
                       Ast.RES_slot slotr -> resolve_slot_ref cx tyo slotr
                     | Ast.RES_item _ -> ())
      | None -> 
	      (match lval.Ast.lval_src.node with 
	         | Ast.LVAL_base base -> 
		         let (_, binding) = lookup_base cx base in
                   log cx "resolved lval: %s" (fmt_base cx base);
			       (match binding with 
				        BINDING_item (path, item) -> bind_to_item path item
			          | BINDING_slot (path, slot) -> bind_to_slot path slot
			          | BINDING_type_item _ -> 
				          raise (err cx ("lval '" ^ (fmt_base cx base) ^ "' resolved to a type name")))
	         | _ -> raise (err cx ("unhandled lval form in resolve_lval")))

and resolve_stmt_option cx stmtopt = 
  match stmtopt with 
	  None -> ()
	| Some s -> resolve_stmt cx s


and resolve_expr_option cx expropt = 
  match expropt with 
	  None -> ()
	| Some e -> resolve_expr cx e

and resolve_lval_option cx lopt = 
  match lopt with 
	  None -> ()
	| Some lv -> resolve_lval cx None lv

and resolve_stmts cx stmts = 
  Array.iter (resolve_stmt cx) stmts
		
and resolve_stmt cx stmt = 
  let cx = { cx with ctxt_span = Some stmt.span } in
  match stmt.node with 
	  Ast.STMT_while w -> 
		let (stmts, lval) = w.Ast.while_lval in
		  resolve_lval cx (Some Ast.TY_bool) lval;
		  resolve_stmts cx stmts;
		  resolve_stmt cx w.Ast.while_body

	| Ast.STMT_do_while w -> 
		let (stmts, lval) = w.Ast.while_lval in
		  resolve_lval cx (Some Ast.TY_bool) lval;
		  resolve_stmts cx stmts;
		  resolve_stmt cx w.Ast.while_body

	| Ast.STMT_foreach f -> 
		(* FIXME: foreaches are a bit wrong at the moment. *)
		let (fn, args) = f.Ast.foreach_call in
		  resolve_lval cx None fn;
		  Array.iter (resolve_lval cx None) args;
		  let cx' = 
			{ cx with ctxt_frame_scopes = 
				f.Ast.foreach_frame :: cx.ctxt_frame_scopes } 
		  in
			layout_frame cx f.Ast.foreach_frame;
			resolve_stmt cx' f.Ast.foreach_body;
			()
			  
	| Ast.STMT_for f -> 
		resolve_stmt cx f.Ast.for_init;
		let cx' = 
		  { cx with ctxt_frame_scopes =
			  f.Ast.for_frame :: cx.ctxt_frame_scopes }
		in
		let (stmts, lval) = f.Ast.for_test in
		  layout_frame cx f.Ast.for_frame;
		  resolve_stmts cx' stmts;
		  resolve_lval cx' (Some Ast.TY_bool) lval;
		  resolve_stmt cx' f.Ast.for_step;
		  resolve_stmt cx' f.Ast.for_body;

	| Ast.STMT_if i -> 
		resolve_lval cx (Some Ast.TY_bool) i.Ast.if_test;
		resolve_stmt cx i.Ast.if_then;
		resolve_stmt_option cx i.Ast.if_else

	| Ast.STMT_try t -> 
		resolve_stmt cx t.Ast.try_body;
		resolve_stmt_option cx t.Ast.try_fail;
		resolve_stmt_option cx t.Ast.try_fini
		
	| Ast.STMT_put (_, lo) -> 
		resolve_lval_option cx lo

	| Ast.STMT_ret (_, lo) -> 
		resolve_lval_option cx lo

	| Ast.STMT_block b -> 
		resolve_block cx b

	| Ast.STMT_decl d -> 
		(match d with 
			 Ast.DECL_mod_item (id, item) -> 
			   resolve_mod_item cx id item
				 
		   | Ast.DECL_slot (key, slot) -> 
			   resolve_slot_ref cx None slot)
			
	| Ast.STMT_copy (lval, expr) -> 
		resolve_expr cx expr;
		resolve_lval cx (expr_type cx expr) lval;
		  
	| Ast.STMT_call (dst, fn, args) -> 
		resolve_lval cx None fn;
		Array.iteri (fun i -> resolve_lval cx (lval_fn_arg_type cx fn i)) args;
		resolve_lval cx (lval_fn_result_type cx fn) dst

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

let resolve_crate sess items = 
  let cx = root_ctxt sess in
	while !(cx.ctxt_made_progress) do
      log cx "";
      log cx "=== fresh resolution pass ===";
      cx.ctxt_contains_autos := false;
      cx.ctxt_contains_unresolved_types := false;
      cx.ctxt_made_progress := false;
	  resolve_mod_items cx items;
	done;
    if !(cx.ctxt_contains_autos) or
      !(cx.ctxt_contains_unresolved_types)
    then raise (err cx "progress wedged but crate incomplete")
    else ()


(* Translation *)

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

(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
