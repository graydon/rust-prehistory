(* 
 * This module does the first (environment-sensitive) set of semantic lowerings:
 *
 *   - resolves lvals and types
 *   - lays out frames
 *   - performs type inference and checking
 * 
 * By the end of this pass, you should not need to do any further name-based lookups.
 *)

open Semant;;
open Common;;

type ctxt = 
	{ ctxt_frame_scopes: Ast.frame list;
	  ctxt_type_scopes: Ast.mod_type_items list;
	  ctxt_span: span option;
	  ctxt_sess: Session.sess;
	  ctxt_made_progress: bool ref;
	  ctxt_contains_autos: bool ref;
	  ctxt_contains_un_laid_out_frames: bool ref;
	  ctxt_contains_unresolved_types: bool ref;
	  ctxt_abi: Abi.abi }
;;

let	new_ctxt sess abi = 
  { ctxt_frame_scopes = []; 
	ctxt_type_scopes = [];
	ctxt_span = None;
	ctxt_sess = sess;
	ctxt_made_progress = ref true;
    ctxt_contains_autos = ref false;
    ctxt_contains_un_laid_out_frames = ref false;
    ctxt_contains_unresolved_types = ref false;
	ctxt_abi = abi }
;;

let log cx = Session.log "resolve" 
  cx.ctxt_sess.Session.sess_log_resolve
  cx.ctxt_sess.Session.sess_log_out
;;

let err cx str = 
  (Semant_err (cx.ctxt_span, (str)))
;;

exception Auto_slot;;
exception Un_laid_out_frame;;


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
	| Ast.TY_int -> cx.ctxt_abi.Abi.abi_ptr_sz
	| Ast.TY_char -> 4L
	| Ast.TY_str -> cx.ctxt_abi.Abi.abi_ptr_sz
	| Ast.TY_tup tys -> (Array.fold_left (fun n ty -> Int64.add n (slot_size cx ty)) 0L tys)
	| _ -> raise (err cx "unhandled type in size_of_ty")

and slot_size cx s = 
  match s with 
	  Ast.SLOT_exterior _ -> cx.ctxt_abi.Abi.abi_ptr_sz
	| Ast.SLOT_read_alias _ -> cx.ctxt_abi.Abi.abi_ptr_sz
	| Ast.SLOT_write_alias _ -> cx.ctxt_abi.Abi.abi_ptr_sz
	| Ast.SLOT_interior t -> size_of_ty cx t
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
	BINDING_item of ((Ast.resolved_path option) * Ast.mod_item)
  | BINDING_slot of ((Ast.resolved_path option) * Ast.local)
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

(* 
 * Our lookup system integrates notions of the layout of frames, but is presently
 * not quite correct. What we want to do is:
 * 
 *   - Frames look, broadly, like this (growing downward):
 * 
 *     +----------------------------+ <-- Rewind tail calls to here. If varargs are supported,
 *     |caller args                 |     must use memmove or similar "overlap-permitting" move,
 *     |...                         |     if supporting tail-calling. 
 *     |...                         |
 *     +----------------------------+ <-- fp + abi_frame_base_sz + abi_implicit_args_sz
 *     |caller non-reg ABI operands |
 *     |possibly empty, if fastcall |
 *     |  - process pointer?        |
 *     |  - runtime pointer?        |
 *     |  - yield pc or delta?      |
 *     |  - yield slot addr?        |
 *     |  - ret slot addr?          |
 *     +----------------------------+ <-- fp + abi_frame_base_sz
 *     |return pc pushed by machine |
 *     |plus any callee-save stuff  |
 *     +----------------------------+ <-- fp
 *     |frame-allocated stuff       |
 *     |determined in resolve       |
 *     |...                         |
 *     |...                         |
 *     |...                         |
 *     +----------------------------+ <-- fp - framesz
 *     |spills determined in ra     |
 *     |...                         |
 *     |...                         |
 *     +----------------------------+ <-- fp - (framesz + spillsz)
 * 
 *   - Divide slots into two classes:
 *     - Those that are never aliased and fit in a word, so are vreg-allocated
 *     - All others
 * 
 *   - Look up entries in the frame *before* layout, type-resolve, and mark as 
 *     aliased when we encounter an aliasing statement.
 * 
 *   - Lay out the frame *on exit* of the resolve pass, given what we now know
 *     wrt the slot types and aliasing:
 *     - Non-aliased, word-fitting slots consume no frame space *yet*; they are
 *       given a generic value that indicates "try a vreg". The register allocator
 *       may spill them later, if it needs to, but that's not our concern.
 *     - Aliased / too-big slots are frame-allocated, need to be laid out in the
 *       frame at fixed offsets, so need to be assigned Common.layout values. 
 *       (Is this true of aliased word-fitting? Can we not runtime-calculate the 
 *       position of a spill slot? Meh.)
 *)

and should_use_vreg (cx:ctxt) (local:Ast.local) : bool = 
  let slotr = local.Ast.local_slot in 
  let sz = slot_size cx (!(slotr.node)) in
    (Int64.compare sz cx.ctxt_abi.Abi.abi_ptr_sz > 0 ||
       (!(local.Ast.local_aliased)))

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
        begin
		  match cx.ctxt_frame_scopes with 
			  [] -> raise (err cx ("unknown identifier: '" ^ ident ^ "'"))
		    | (x::xs) -> 
                let local_opt = 
                  match x with 
                      Ast.FRAME_heavy hf -> 
                        let args = !(hf.Ast.heavy_frame_arg_slots) in
                          if List.exists (fun (k,_) -> k = ident) args
                          then Some (List.assoc ident args)
                          else None
                    | Ast.FRAME_light lf -> 
			            let tab = lf.Ast.light_frame_locals in
				          if Hashtbl.mem tab (Ast.KEY_ident ident)
				          then Some (Hashtbl.find tab (Ast.KEY_ident ident))
                          else None
                in
                  match local_opt with       
                      Some local -> 
                        let layout = local.Ast.local_layout in 
                        let slotr = local.Ast.local_slot in                          
                        let pathopt =
                          try
                            match x with 
                                Ast.FRAME_light _ -> 
                                  if should_use_vreg cx local 
                                  then Some (Ast.RES_member (layout, (Ast.RES_deref fp)))
                                  else Some (Ast.RES_vreg local.Ast.local_vreg)
                              | Ast.FRAME_heavy hf -> 
                                  Some (Ast.RES_member 
                                          (layout,
                                           (Ast.RES_member (hf.Ast.heavy_frame_layout, 
                                                            (Ast.RES_deref fp)))))
                          with 
                              Auto_slot -> 
                                begin 
                                  cx.ctxt_contains_autos := true; 
                                  None
                                end
                        in
					      ({cx with ctxt_span = Some slotr.span}, 
					       BINDING_slot (pathopt, local))
                    | None -> 
                        begin
                          match x with 
                              Ast.FRAME_heavy _ -> 
                                lookup_ident 
					              { cx with ctxt_frame_scopes = xs } 
					              (Ast.RES_deref fp) ident
                            | Ast.FRAME_light lf ->  
			                    let tab = lf.Ast.light_frame_items in
				                  if Hashtbl.mem tab ident
				                  then 
				                    let (layout, item) = Hashtbl.find tab ident in
                                    let pathopt = Some (Ast.RES_member (layout, (Ast.RES_deref fp))) in
					                  ({cx with ctxt_span = Some item.span}, 
					                   BINDING_item (pathopt, item))
                                  else                        
				                    lookup_ident 
					                  { cx with ctxt_frame_scopes = xs } fp ident
                        end
        end


and lookup_temp (cx:ctxt) 
	(fp:Ast.resolved_path) 
	(temp:Ast.nonce) 
	: (ctxt * binding) = 
  match cx.ctxt_frame_scopes with 
	  [] -> raise (err cx ("unknown temporary: '" ^ (string_of_int temp) ^ "'"))
	| (x::xs) -> 
        begin
          match x with 
              Ast.FRAME_light lf ->                 
		        let tab = lf.Ast.light_frame_locals in
		          if Hashtbl.mem tab (Ast.KEY_temp temp)
		          then 
		            let local = Hashtbl.find tab (Ast.KEY_temp temp) in 
                    let layout = local.Ast.local_layout in 
                    let slotr = local.Ast.local_slot in 
                    let pathopt = 
                      try
                        if should_use_vreg cx local
                        then Some (Ast.RES_member (layout, (Ast.RES_deref fp)))
                        else Some (Ast.RES_vreg local.Ast.local_vreg)
                      with 
                          Auto_slot -> 
                            begin 
                              cx.ctxt_contains_autos := true; 
                              None
                            end
                    in
                      log cx "found temporary temp %d" temp;
			          ({ cx with ctxt_span = Some slotr.span }, 
			           BINDING_slot (pathopt, local))
		          else 
		            lookup_temp { cx with ctxt_frame_scopes = xs } fp temp
            | _ -> 
                lookup_temp 
			      { cx with ctxt_frame_scopes = xs } (Ast.RES_deref fp) temp
        end

and lookup_base cx base = 
  match base with 
	  (Ast.BASE_ident id) -> lookup_ident cx (Ast.RES_pr FP) id
	| (Ast.BASE_temp t) -> lookup_temp cx (Ast.RES_pr FP) t
	| _ -> raise (err cx "unhandled name base variant in lookup_base")


and string_of_base cx base = 
  match base with 
	  Ast.BASE_ident id -> id
	| Ast.BASE_temp t -> ("temp#" ^ (string_of_int t))
	| _ -> raise (err cx "unhandled name base variant in string_of_base")


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
	  Ast.NAME_base (Ast.BASE_ident id) -> basefn cx (lookup_ident cx (Ast.RES_pr FP) id)
	| Ast.NAME_base (Ast.BASE_app (id, args)) -> 
		let (cx, binding) = lookup_ident cx (Ast.RES_pr FP) id in
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
		basefn cx (lookup_temp cx (Ast.RES_pr FP) temp)
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
  match !(lval.Ast.lval_res.Ast.res_target) with 
      Some (Ast.RES_slot local) -> slot_type cx !(local.Ast.local_slot.node)
    | Some (Ast.RES_item item) -> Some (type_of_mod_item cx item)
| _ -> None

and atom_type cx atom = 
  let concretize tyo = 
    match tyo with 
        Some (Ast.TY_named _) -> None
      | Some t -> Some t
      | None -> None
  in
    match atom with 
        Ast.ATOM_lval lv -> concretize (lval_type cx lv)
      | Ast.ATOM_literal lit -> 
		  (match lit.node with 
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

and expr_type cx expr = 
    match expr with 
	    Ast.EXPR_atom atom -> atom_type cx atom
		    
	  (* FIXME: check appropriateness of applying op to type *)
	  | Ast.EXPR_binary (op, a, b) -> 
		  (match (atom_type cx a, atom_type cx b) with 
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
            
	  | Ast.EXPR_unary (_, atom) -> atom_type cx atom
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

and layout_frame_inner
    (cx:ctxt)
    (heavy:bool)
    (frame_off:int64)
    (frame_sz:int64)
    (frame_layout:layout)
    (slots:(Ast.slot_key * Ast.local) array)
    : unit = 
      log cx "beginning%s frame layout for %Ld byte frame @ %Ld (done: %b)" 
        (if heavy then " heavy" else " light") frame_sz frame_off frame_layout.layout_done;
      let off = ref 0L in 
	    frame_layout.layout_size <- frame_sz;
        frame_layout.layout_offset <- frame_off;
	    for i = 0 to (Array.length slots) - 1 do          
		  let (key, local) = slots.(i) in
          let layout = local.Ast.local_layout in 
            log cx "laying out slot %d (%s)" i (string_of_key key);
		    let sz = slot_size cx (!(local.Ast.local_slot.node)) in
              log cx "  == %Ld bytes @ %Ld" sz (!off);
              layout.layout_size <- sz;
              layout.layout_offset <- (!off);
              layout.layout_done <- true;
              off := Int64.add (!off) sz
        done;
        if not frame_layout.layout_done 
        then 
          begin
            log cx "setting layout_done <- true";
            frame_layout.layout_done <- true;
            cx.ctxt_made_progress := true
          end
        else
          log cx "layout was already done"
  

and layout_frame 
	(cx:ctxt) 
	(frame:Ast.frame) 
	: unit = 
  try 
    let slots_sz slots = 
      Array.fold_left 
        (fun x (_,local) -> 
           Int64.add x (slot_size cx (!(local.Ast.local_slot.node)))) 
        0L slots
    in
      match frame with 
          Ast.FRAME_light lf -> 
            begin
              let _ = 
                Hashtbl.iter 
                  (fun k local -> resolve_slot_ref cx None local.Ast.local_slot) 
                  lf.Ast.light_frame_locals
              in
              let slots = 
                Array.of_list 
	              (Sort.list 
                     (fun (a, _) (b, _) -> a < b)
                     (List.filter 
                        (fun (k,local) -> not (should_use_vreg cx local))
                        (htab_pairs lf.Ast.light_frame_locals)))
              in
              let sz = slots_sz slots in 
              let offset = 
                match cx.ctxt_frame_scopes with 
                    [] -> 0L
                  | x::_ -> 
                      begin 
                        match x with 
                            Ast.FRAME_heavy hf -> 0L
                          | Ast.FRAME_light lf ->                               
                              let layout = lf.Ast.light_frame_layout in                            
                                if layout.layout_done
                                then (Int64.sub layout.layout_offset sz)
                                else raise Un_laid_out_frame
                      end
              in
                layout_frame_inner cx false offset sz lf.Ast.light_frame_layout slots
            end
        | Ast.FRAME_heavy hf -> 
            let offset = 
              Int64.add 
                cx.ctxt_abi.Abi.abi_frame_base_sz 
                cx.ctxt_abi.Abi.abi_implicit_args_sz 
            in
            let _ = 
              List.iter 
                (fun (_,local) -> resolve_slot_ref cx None local.Ast.local_slot) 
                (!(hf.Ast.heavy_frame_arg_slots))
            in
            let slots = 
              Array.of_list (List.map 
                               (fun (k,v) -> (Ast.KEY_ident k, v))
                               (!(hf.Ast.heavy_frame_arg_slots)))
            in
              layout_frame_inner cx true offset (slots_sz slots) hf.Ast.heavy_frame_layout slots
  with 
	  Auto_slot -> 
        log cx "hit auto slot";
		cx.ctxt_contains_autos := true
          
    | Un_laid_out_frame -> 
        log cx "hit un-laid-out frame";
        cx.ctxt_contains_un_laid_out_frames := true
          
and extend_ctxt_by_frame 
	(cx:ctxt)
	(items:(Ast.ident * Ast.mod_item) array)
	: ctxt = 
  (* 
   * FIXME: frames for type parameters (which is what these are) are
   * totally broken and need reworking into a sane part of the ABI.
   *)
  log cx "extending ctxt by frame";
  let items' = Hashtbl.create (Array.length items) in
	for i = 0 to (Array.length items) - 1
	do
	  let (ident, item) = items.(i) in 
		Hashtbl.add items' ident (new_layout(), item)
	done;
    let light = { Ast.light_frame_layout = new_layout();
		          Ast.light_frame_locals = Hashtbl.create 0;
		          Ast.light_frame_items = items'; }
    in
    let frame = Ast.FRAME_light light in        
      light.Ast.light_frame_layout.layout_done <- true;
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

and new_local (slot:(Ast.slot ref) spanned) = 
  { Ast.local_layout = new_layout();
    Ast.local_slot = slot;
    Ast.local_aliased = ref false;
    Ast.local_vreg = ref None; }

and resolve_fn span cx fn = 
  let cx = 
	{ cx with ctxt_frame_scopes = 
		(Ast.FRAME_heavy fn.Ast.fn_frame) :: cx.ctxt_frame_scopes } 
  in
	resolve_block cx fn.Ast.fn_body;
    layout_frame cx (Ast.FRAME_heavy fn.Ast.fn_frame);
    (* FIXME: ret/put slots are a mess. Clean up. *)
    let outslot = 
      resolve_slot cx None fn.Ast.fn_ty.Ast.fn_sig.Ast.sig_output_slot        
    in
    let outslotr = { node=ref outslot; span=span} in
    let outlocal = new_local outslotr in 
    let outlayout = outlocal.Ast.local_layout in 
      outlayout.layout_size <- slot_size cx outslot;
      outlayout.layout_offset <-  cx.ctxt_abi.Abi.abi_frame_base_sz;
      fn.Ast.fn_frame.Ast.heavy_frame_out_slot := Some outlocal
              
		    
and resolve_prog cx prog = 
  let items = prog.Ast.prog_mod in
  let cx = extend_ctxt_by_frame cx (linearize_items_for_frame items) in
	Hashtbl.iter (resolve_mod_item cx) items;
	resolve_init cx prog.Ast.prog_init;
  	resolve_block_option cx prog.Ast.prog_main;
  	resolve_block_option cx prog.Ast.prog_fini;


and resolve_init cx init = 
  ()


and resolve_block cx (block:Ast.block) = 
  let cx' = 
	{ cx with ctxt_frame_scopes = 
		(Ast.FRAME_light block.node.Ast.block_frame) :: cx.ctxt_frame_scopes } 
  in
    log cx "resolving block with %d items, %d slots"
	  (Hashtbl.length block.node.Ast.block_frame.Ast.light_frame_items)
	  (Hashtbl.length block.node.Ast.block_frame.Ast.light_frame_locals);
	Array.iter (resolve_stmt cx') block.node.Ast.block_stmts;
	layout_frame cx (Ast.FRAME_light block.node.Ast.block_frame)

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
		resolve_atom cx None a;
		resolve_atom cx None b
	| Ast.EXPR_unary (_, e) -> 
		resolve_atom cx None e
	| Ast.EXPR_atom atom -> 
		resolve_atom cx None atom
	| Ast.EXPR_rec htab -> 
		Hashtbl.iter (fun _ lv -> resolve_atom cx None lv) htab
	| Ast.EXPR_vec v -> 
		Array.iter (resolve_atom cx None) v
	| Ast.EXPR_tup v -> 
		Array.iter (resolve_atom cx None) v
		  

and resolve_atom cx tyo atom = 
  match atom with 
      Ast.ATOM_literal _ -> ()
    | Ast.ATOM_lval lv -> resolve_lval cx tyo lv


and resolve_lval cx tyo lval = 
  let bind_to_slot pathopt local = 
    lval.Ast.lval_res.Ast.res_path := pathopt;
    lval.Ast.lval_res.Ast.res_target := Some (Ast.RES_slot local);
    resolve_slot_ref cx tyo local.Ast.local_slot
  in
  let bind_to_item pathopt item = 
    lval.Ast.lval_res.Ast.res_path := pathopt;
    lval.Ast.lval_res.Ast.res_target := Some (Ast.RES_item item)
  in
    match !(lval.Ast.lval_res.Ast.res_path) with 
        Some pth -> 
          begin 
            match !(lval.Ast.lval_res.Ast.res_target) with
                Some (Ast.RES_slot local) -> resolve_slot_ref cx tyo local.Ast.local_slot
              | Some (Ast.RES_item _) -> ()
              | None -> raise (err cx "lval path resolved but no target?")
          end
      | None -> 
          begin
	        match lval.Ast.lval_src.node with 
	          | Ast.LVAL_base base -> 
                  let _ = 
                    begin
                      match !(lval.Ast.lval_res.Ast.res_target) with 
                          None -> log cx "first-pass resolving lval: %s" (string_of_base cx base)
                        | Some _ -> log cx "Nth-pass resolving lval: %s" (string_of_base cx base)
                    end
                  in
		          let (_, binding) = lookup_base cx base in
                    log cx "resolved lval: %s" (string_of_base cx base);
                    begin
			          match binding with 
				          BINDING_item (pathopt, item) -> bind_to_item pathopt item
			            | BINDING_slot (pathopt, local) -> bind_to_slot pathopt local
			            | BINDING_type_item _ -> 
				            raise (err cx ("lval '" ^ (string_of_base cx base) ^ "' resolved to a type name"))
                    end
	          | _ -> raise (err cx ("unhandled lval form in resolve_lval"))
          end

and resolve_block_option cx (blockopt:Ast.block option) = 
  match blockopt with 
	  None -> ()
	| Some s -> resolve_block cx s


and resolve_expr_option cx expropt = 
  match expropt with 
	  None -> ()
	| Some e -> resolve_expr cx e

and resolve_lval_option cx lopt = 
  match lopt with 
	  None -> ()
	| Some lv -> resolve_lval cx None lv

and resolve_atom_option cx aopt = 
  match aopt with 
	  None -> ()
	| Some atom -> resolve_atom cx None atom

and resolve_stmts cx stmts = 
  Array.iter (resolve_stmt cx) stmts
		
and resolve_stmt cx stmt = 
  let cx = { cx with ctxt_span = Some stmt.span } in
  match stmt.node with 
	  Ast.STMT_log a -> 
		  resolve_atom cx None a

	| Ast.STMT_while w -> 
		let (stmts, atom) = w.Ast.while_lval in
		  resolve_atom cx (Some Ast.TY_bool) atom;
		  resolve_stmts cx stmts;
		  resolve_block cx w.Ast.while_body

	| Ast.STMT_do_while w -> 
		let (stmts, atom) = w.Ast.while_lval in
		  resolve_atom cx (Some Ast.TY_bool) atom;
		  resolve_stmts cx stmts;
		  resolve_block cx w.Ast.while_body

	| Ast.STMT_foreach f -> 
		(* FIXME: foreaches are a bit wrong at the moment. *)
		let (fn, args) = f.Ast.foreach_call in
		  resolve_lval cx None fn;
		  Array.iter (resolve_lval cx None) args;
		  let cx' = 
			{ cx with ctxt_frame_scopes = 
				(Ast.FRAME_light f.Ast.foreach_frame) :: cx.ctxt_frame_scopes } 
		  in
			layout_frame cx (Ast.FRAME_light f.Ast.foreach_frame);
			resolve_block cx' f.Ast.foreach_body;
			()
			  
	| Ast.STMT_for f -> 
		resolve_stmt cx f.Ast.for_init;
		let cx' = 
		  { cx with ctxt_frame_scopes =
			  (Ast.FRAME_light f.Ast.for_frame) :: cx.ctxt_frame_scopes }
		in
		let (stmts, atom) = f.Ast.for_test in
		  layout_frame cx (Ast.FRAME_light f.Ast.for_frame);
		  resolve_stmts cx' stmts;
		  resolve_atom cx' (Some Ast.TY_bool) atom;
		  resolve_stmt cx' f.Ast.for_step;
		  resolve_stmt cx' f.Ast.for_body;

	| Ast.STMT_if i -> 
		resolve_atom cx (Some Ast.TY_bool) i.Ast.if_test;
		resolve_block cx i.Ast.if_then;
		resolve_block_option cx i.Ast.if_else

	| Ast.STMT_try t -> 
		resolve_block cx t.Ast.try_body;
		resolve_block_option cx t.Ast.try_fail;
		resolve_block_option cx t.Ast.try_fini
		
	| Ast.STMT_put (_, lo) -> 
		resolve_atom_option cx lo

	| Ast.STMT_ret (_, lo) -> 
		resolve_atom_option cx lo

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
		Array.iteri (fun i -> resolve_atom cx (lval_fn_arg_type cx fn i)) args;
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

let resolve_crate 
    (sess:Session.sess) 
    (abi:Abi.abi) 
    (items:Ast.mod_items) 
    : unit = 
  try 
    let cx = new_ctxt sess abi in
	  while !(cx.ctxt_made_progress) do
        log cx "";
        log cx "=== fresh resolution pass ===";
        cx.ctxt_contains_autos := false;
        cx.ctxt_contains_unresolved_types := false;
        cx.ctxt_contains_un_laid_out_frames := false;
        cx.ctxt_made_progress := false;
	    resolve_mod_items cx items;
	  done;
      if !(cx.ctxt_contains_autos) or
        !(cx.ctxt_contains_unresolved_types) or
        !(cx.ctxt_contains_un_laid_out_frames)
      then 
        raise (err cx "progress ceased, but crate incomplete")
      else ()
  with 
	  Semant_err (spano, str) -> 
        begin
		  match spano with 
			  None -> 
                Session.fail sess "Resolve error: %s\n%!" str
		    | Some span -> 			  
			    Session.fail sess "%s:E:Resolve error: %s\n%!" 
                  (Session.string_of_span span) str
        end
;;

(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
