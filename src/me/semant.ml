
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

exception Semant_err of ((node_id option) * string)
;;

let err (idopt:node_id option) = 
  let k s = 
    raise (Semant_err (idopt, s))
  in
    Printf.ksprintf k
;;


(* Mappings between mod items and their respective types. *)

let rec ty_mod_of_mod (m:Ast.mod_items) : Ast.mod_type_items = 
  let ty_items = Hashtbl.create (Hashtbl.length m) in 
  let add n i = Hashtbl.add ty_items n (mod_type_item_of_mod_item i) in
	Hashtbl.iter add m;
	ty_items

and mod_type_item_of_mod_item (item:Ast.mod_item) : Ast.mod_type_item = 
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
			(decl pd.Ast.decl_params (ty_pred_of_pred pd.Ast.decl_item))
	  | Ast.MOD_ITEM_mod md ->
			Ast.MOD_TYPE_ITEM_mod 
			  (decl md.Ast.decl_params (ty_mod_of_mod md.Ast.decl_item))
	  | Ast.MOD_ITEM_fn fd -> 
		  Ast.MOD_TYPE_ITEM_fn
			(decl fd.Ast.decl_params (ty_fn_of_fn fd.Ast.decl_item))
	  | Ast.MOD_ITEM_prog pd -> 
	      Ast.MOD_TYPE_ITEM_prog 
            (decl pd.Ast.decl_params (ty_prog_of_prog pd.Ast.decl_item))
  in
	{ id = item.id;
	  node = ty }
      
and ty_prog_of_prog (prog:Ast.prog) : Ast.ty_prog = 
  let init_ty = 
    match prog.Ast.prog_init with 
		None -> None
	  | Some init -> Some (arg_slots init.node.Ast.init_input_slots)
  in
	{ Ast.prog_mod_ty = ty_mod_of_mod prog.Ast.prog_mod;
	  Ast.prog_init_ty = init_ty; }

and arg_slots (slots:((Ast.slot identified) * Ast.ident) array) : Ast.slot array = 
  Array.map (fun (sid,_) -> sid.node) slots

and ty_fn_of_fn (fn:Ast.fn) : Ast.ty_fn = 
  ({ Ast.sig_input_slots = arg_slots fn.Ast.fn_input_slots;
     Ast.sig_output_slot = fn.Ast.fn_output_slot.node },
   fn.Ast.fn_aux )

and ty_pred_of_pred (pred:Ast.pred) : Ast.ty_pred = 
  arg_slots pred.Ast.pred_input_slots

    
and ty_of_mod_item (item:Ast.mod_item) : Ast.ty = 
  let check_concrete params ty = 
    if Array.length params = 0 
    then ty 
    else err (Some item.id) "item has parametric type in type_of_mod_item"
  in
	match item.node with 
		Ast.MOD_ITEM_opaque_type td -> 
          check_concrete td.Ast.decl_params Ast.TY_type
			
	  | Ast.MOD_ITEM_public_type td -> 
          check_concrete td.Ast.decl_params Ast.TY_type
			
	  | Ast.MOD_ITEM_pred pd -> 
          check_concrete pd.Ast.decl_params 
            (Ast.TY_pred (ty_pred_of_pred pd.Ast.decl_item))
			  
	  | Ast.MOD_ITEM_mod md ->
          check_concrete md.Ast.decl_params 
            (Ast.TY_mod (ty_mod_of_mod md.Ast.decl_item))
              
	  | Ast.MOD_ITEM_fn fd -> 
          check_concrete fd.Ast.decl_params
            (Ast.TY_fn (ty_fn_of_fn fd.Ast.decl_item))
            
	  | Ast.MOD_ITEM_prog pd ->
          check_concrete pd.Ast.decl_params
            (Ast.TY_prog (ty_prog_of_prog pd.Ast.decl_item))


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
