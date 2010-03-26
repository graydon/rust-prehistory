
open Common;;

type slots_table = (Ast.slot_key,node_id) Hashtbl.t
type items_table = (Ast.ident,node_id) Hashtbl.t
type block_slots_table = (node_id,slots_table) Hashtbl.t
type block_items_table = (node_id,items_table) Hashtbl.t
;;


type code = {
  code_fixup: fixup;
  code_quads: Il.quads;
  code_vregs_and_spill: (int * fixup) option;
}
;;

type glue =
    GLUE_activate
  | GLUE_yield
  | GLUE_exit_main_proc
  | GLUE_exit_proc
  | GLUE_mark of Ast.ty
  | GLUE_drop of Ast.ty
  | GLUE_free of Ast.ty
  | GLUE_copy of Ast.ty      (* One-level copy. *)
  | GLUE_clone of Ast.ty     (* Deep copy. *)
  | GLUE_compare of Ast.ty
  | GLUE_hash of Ast.ty
  | GLUE_write of Ast.ty
  | GLUE_read of Ast.ty
  | GLUE_unwind
  | GLUE_get_next_pc
  | GLUE_mark_frame of node_id
  | GLUE_drop_frame of node_id
  | GLUE_reloc_frame of node_id
  | GLUE_bind_mod of node_id
  | GLUE_fn_binding of node_id
;;

type data =
    DATA_str of string
  | DATA_name of Ast.name
  | DATA_tydesc of Ast.ty
  | DATA_frame_glue_fns of node_id
  | DATA_mod_table of node_id
  | DATA_mod_pair of node_id
  | DATA_crate
;;

type defn =
    DEFN_slot of Ast.slot
  | DEFN_item of (Ast.ty_param identified, Ast.mod_item') Ast.decl
  | DEFN_ty_param of Ast.ty_param
;;

type glue_code = (glue, code) Hashtbl.t;;
type item_code = (node_id, code) Hashtbl.t;;
type file_code = (node_id, item_code) Hashtbl.t;;
type data_frags = (data, (fixup * Asm.frag)) Hashtbl.t;;

let string_of_name (n:Ast.name) : string =
  Ast.fmt_to_str Ast.fmt_name n
;;

(* The node_id in the Constr_pred constr_key is the innermost block_id
   of any of the constr's pred name or cargs; this is the *outermost*
   block_id in which its constr_id can possibly be shared. It serves
   only to uniquely identify the constr.
   
   The node_id in the Constr_init constr_key is just a slot id. 
   Constr_init is the builtin (un-named) constraint that means "slot is
   initialized". Not every slot is.
 *)
type constr_key =
    Constr_pred of (Ast.constr * node_id)
  | Constr_init of node_id

type ctxt =
    { ctxt_sess: Session.sess;
      ctxt_frame_args: (node_id,node_id list) Hashtbl.t;
      ctxt_frame_blocks: (node_id,node_id list) Hashtbl.t;
      ctxt_block_slots: block_slots_table;
      ctxt_block_items: block_items_table;
      ctxt_slot_is_arg: (node_id,unit) Hashtbl.t;
      ctxt_slot_keys: (node_id,Ast.slot_key) Hashtbl.t;
      ctxt_all_item_names: (node_id,Ast.name) Hashtbl.t;
      ctxt_all_item_types: (node_id,Ast.ty) Hashtbl.t;
      ctxt_all_lval_types: (node_id,Ast.ty) Hashtbl.t;
      ctxt_all_type_items: (node_id,Ast.ty) Hashtbl.t;
      ctxt_all_stmts: (node_id,Ast.stmt) Hashtbl.t;
      ctxt_item_files: (node_id,filename) Hashtbl.t;
      ctxt_all_defns: (node_id,defn) Hashtbl.t;                         (* definition id --> definition *)
      ctxt_lval_to_referent: (node_id,node_id) Hashtbl.t;               (* reference id --> definition id *)
      ctxt_imported_items: (node_id, (import_lib * nabi_conv)) Hashtbl.t;
      ctxt_pat_to_tag: (node_id,node_id) Hashtbl.t;

      (* Layout-y stuff. *)
      ctxt_slot_aliased: (node_id,unit) Hashtbl.t;
      ctxt_slot_is_module_state: (node_id,unit) Hashtbl.t;
      ctxt_slot_vregs: (node_id,((int option) ref)) Hashtbl.t;
      ctxt_slot_offsets: (node_id,size) Hashtbl.t;
      ctxt_frame_sizes: (node_id,size) Hashtbl.t;
      ctxt_call_sizes: (node_id,size) Hashtbl.t;

      (* Mutability and GC stuff. *)
      ctxt_mutable_slot_referent: (node_id,unit) Hashtbl.t;

      (* Typestate-y stuff. *)
      ctxt_constrs: (constr_id,constr_key) Hashtbl.t;
      ctxt_constr_ids: (constr_key,constr_id) Hashtbl.t;
      ctxt_preconditions: (node_id,Bits.t) Hashtbl.t;
      ctxt_postconditions: (node_id,Bits.t) Hashtbl.t;
      ctxt_prestates: (node_id,Bits.t) Hashtbl.t;
      ctxt_poststates: (node_id,Bits.t) Hashtbl.t;
      ctxt_call_lval_params: (node_id,Ast.ty array) Hashtbl.t;
      ctxt_copy_stmt_is_init: (node_id,unit) Hashtbl.t;

      (* Translation-y stuff. *)
      ctxt_fn_fixups: (node_id,fixup) Hashtbl.t;
      ctxt_mod_fixups: (node_id,fixup) Hashtbl.t;
      ctxt_block_fixups: (node_id,fixup) Hashtbl.t;
      ctxt_file_fixups: (node_id,fixup) Hashtbl.t;
      ctxt_spill_fixups: (node_id,fixup) Hashtbl.t;
      ctxt_abi: Abi.abi;
      ctxt_activate_fixup: fixup;
      ctxt_yield_fixup: fixup;
      ctxt_unwind_fixup: fixup;

      ctxt_debug_aranges_fixup: fixup;
      ctxt_debug_pubnames_fixup: fixup;
      ctxt_debug_info_fixup: fixup;
      ctxt_debug_abbrev_fixup: fixup;
      ctxt_debug_line_fixup: fixup;
      ctxt_debug_frame_fixup: fixup;

      ctxt_image_base_fixup: fixup;
      ctxt_crate_fixup: fixup;

      ctxt_file_code: file_code;
      ctxt_all_item_code: item_code;
      ctxt_glue_code: glue_code;
      ctxt_data: data_frags;

      ctxt_native_imports: (import_lib,((string,fixup) Hashtbl.t)) Hashtbl.t;
      ctxt_native_exports: (segment,((string, fixup) Hashtbl.t)) Hashtbl.t;

      ctxt_import_rust_sym_num: (node_id, int) Hashtbl.t;
      ctxt_import_c_sym_num: ((import_lib * string), int) Hashtbl.t;
      ctxt_import_lib_num: (import_lib, int) Hashtbl.t;

      ctxt_main_fn_fixup: fixup;
      ctxt_main_name: string;
      ctxt_main_exit_proc_glue_fixup: fixup;
    }
;;

let new_ctxt sess abi crate =
  { ctxt_sess = sess;
    ctxt_frame_args = Hashtbl.create 0;
    ctxt_frame_blocks = Hashtbl.create 0;
    ctxt_block_slots = Hashtbl.create 0;
    ctxt_block_items = Hashtbl.create 0;
    ctxt_slot_is_arg = Hashtbl.create 0;
    ctxt_slot_keys = Hashtbl.create 0;
    ctxt_all_item_names = Hashtbl.create 0;
    ctxt_all_item_types = Hashtbl.create 0;
    ctxt_all_lval_types = Hashtbl.create 0;
    ctxt_all_type_items = Hashtbl.create 0;
    ctxt_all_stmts = Hashtbl.create 0;
    ctxt_item_files = crate.Ast.crate_files;
    ctxt_all_defns = Hashtbl.create 0;
    ctxt_lval_to_referent = Hashtbl.create 0;
    ctxt_imported_items = crate.Ast.crate_imported;
    ctxt_pat_to_tag = Hashtbl.create 0;

    ctxt_mutable_slot_referent = Hashtbl.create 0;

    ctxt_constrs = Hashtbl.create 0;
    ctxt_constr_ids = Hashtbl.create 0;
    ctxt_preconditions = Hashtbl.create 0;
    ctxt_postconditions = Hashtbl.create 0;
    ctxt_prestates = Hashtbl.create 0;
    ctxt_poststates = Hashtbl.create 0;
    ctxt_copy_stmt_is_init = Hashtbl.create 0;
    ctxt_call_lval_params = Hashtbl.create 0;

    ctxt_slot_aliased = Hashtbl.create 0;
    ctxt_slot_is_module_state = Hashtbl.create 0;
    ctxt_slot_vregs = Hashtbl.create 0;
    ctxt_slot_offsets = Hashtbl.create 0;
    ctxt_frame_sizes = Hashtbl.create 0;
    ctxt_call_sizes = Hashtbl.create 0;

    ctxt_fn_fixups = Hashtbl.create 0;
    ctxt_mod_fixups = Hashtbl.create 0;
    ctxt_block_fixups = Hashtbl.create 0;
    ctxt_file_fixups = Hashtbl.create 0;
    ctxt_spill_fixups = Hashtbl.create 0;
    ctxt_abi = abi;
    ctxt_activate_fixup = new_fixup "activate glue";
    ctxt_yield_fixup = new_fixup "yield glue";
    ctxt_unwind_fixup = new_fixup "unwind glue";

    ctxt_debug_aranges_fixup = new_fixup "debug_aranges section";
    ctxt_debug_pubnames_fixup = new_fixup "debug_pubnames section";
    ctxt_debug_info_fixup = new_fixup "debug_info section";
    ctxt_debug_abbrev_fixup = new_fixup "debug_abbrev section";
    ctxt_debug_line_fixup = new_fixup "debug_line section";
    ctxt_debug_frame_fixup = new_fixup "debug_frame section";

    ctxt_image_base_fixup = new_fixup "loaded image base";
    ctxt_crate_fixup = new_fixup "root crate structure";
    ctxt_file_code = Hashtbl.create 0;
    ctxt_all_item_code = Hashtbl.create 0;
    ctxt_glue_code = Hashtbl.create 0;
    ctxt_data = Hashtbl.create 0;

    ctxt_native_imports = Hashtbl.create 0;
    ctxt_native_exports = Hashtbl.create 0;

    ctxt_import_rust_sym_num = Hashtbl.create 0;
    ctxt_import_c_sym_num = Hashtbl.create 0;
    ctxt_import_lib_num = Hashtbl.create 0;

    ctxt_main_fn_fixup = new_fixup (string_of_name crate.Ast.crate_main);
    ctxt_main_name = string_of_name crate.Ast.crate_main;
    ctxt_main_exit_proc_glue_fixup = new_fixup "main exit-proc glue"
  }
;;

exception Semant_err of ((node_id option) * string)
;;

let err (idopt:node_id option) =
  let k s =
    raise (Semant_err (idopt, s))
  in
    Printf.ksprintf k
;;

let report_err cx ido str =
  let sess = cx.ctxt_sess in
  let spano = match ido with
      None -> None
    | Some id -> (Session.get_span sess id)
  in
    match spano with
        None ->
          Session.fail sess "Error: %s\n%!" str
      | Some span ->
          Session.fail sess "%s:E:Error: %s\n%!"
            (Session.string_of_span span) str
;;

let bugi (cx:ctxt) (i:node_id) =
  let k s =
    report_err cx (Some i) s;
    failwith s
  in Printf.ksprintf k
;;

(* Convenience accessors. *)

(* resolve an lval reference id to the id of its definition *)
let lval_to_referent (cx:ctxt) (id:node_id) : node_id =
  if Hashtbl.mem cx.ctxt_lval_to_referent id
  then Hashtbl.find cx.ctxt_lval_to_referent id
  else bug () "unresolved lval"
;;

(* resolve an lval reference id to its definition *)
let resolve_lval_id (cx:ctxt) (id:node_id) : defn =
  Hashtbl.find cx.ctxt_all_defns (lval_to_referent cx id)
;;

let referent_is_slot (cx:ctxt) (id:node_id) : bool =
  match Hashtbl.find cx.ctxt_all_defns id with
      DEFN_slot _ -> true
    | _ -> false
;;

let referent_is_item (cx:ctxt) (id:node_id) : bool =
  match Hashtbl.find cx.ctxt_all_defns id with
      DEFN_item _ -> true
    | _ -> false
;;

(* coerce an lval definition id to a slot *)
let referent_to_slot (cx:ctxt) (id:node_id) : Ast.slot =
  match Hashtbl.find cx.ctxt_all_defns id with
      DEFN_slot slot -> slot
    | _ -> bugi cx id "unknown slot"
;;

(* coerce an lval reference id to its definition slot *)
let lval_to_slot (cx:ctxt) (id:node_id) : Ast.slot =
  match resolve_lval_id cx id with
      DEFN_slot slot -> slot
    | _ -> bugi cx id "unknown slot"
;;

let get_fn_fixup (cx:ctxt) (id:node_id) : fixup =
  if Hashtbl.mem cx.ctxt_fn_fixups id
  then Hashtbl.find cx.ctxt_fn_fixups id
  else bugi cx id "fn without fixup"
;;

let get_mod_fixup (cx:ctxt) (id:node_id) : fixup =
  if Hashtbl.mem cx.ctxt_mod_fixups id
  then Hashtbl.find cx.ctxt_mod_fixups id
  else bugi cx id "mod without fixup"
;;

let get_framesz (cx:ctxt) (id:node_id) : size =
  if Hashtbl.mem cx.ctxt_frame_sizes id
  then Hashtbl.find cx.ctxt_frame_sizes id
  else bugi cx id "missing framesz"
;;

let get_callsz (cx:ctxt) (id:node_id) : size =
  if Hashtbl.mem cx.ctxt_call_sizes id
  then Hashtbl.find cx.ctxt_call_sizes id
  else bugi cx id "missing callsz"
;;

let n_item_ty_params (cx:ctxt) (id:node_id) : int =
  match Hashtbl.find cx.ctxt_all_defns id with
      DEFN_item i -> Array.length i.Ast.decl_params
    | _ -> bugi cx id "n_item_ty_params on non-item"
;;

let get_spill (cx:ctxt) (id:node_id) : fixup =
  if Hashtbl.mem cx.ctxt_spill_fixups id
  then Hashtbl.find cx.ctxt_spill_fixups id
  else bugi cx id "missing spill fixup"
;;

let import_native (cx:ctxt) (lib:import_lib) (name:string) : fixup =
  let lib_tab = (htab_search_or_add cx.ctxt_native_imports lib
                   (fun _ -> Hashtbl.create 0))
  in
    htab_search_or_add lib_tab name
      (fun _ -> new_fixup ("import: " ^ name))
;;

let export_native (cx:ctxt) (seg:segment) (name:string) : fixup =
  let seg_tab = (htab_search_or_add cx.ctxt_native_exports seg
                   (fun _ -> Hashtbl.create 0))
  in
    htab_search_or_add seg_tab name
      (fun _ -> new_fixup ("export: " ^ name))
;;

let export_existing_native (cx:ctxt) (seg:segment) (name:string) (fix:fixup) : unit =
  let seg_tab = (htab_search_or_add cx.ctxt_native_exports seg
                   (fun _ -> Hashtbl.create 0))
  in
    htab_put seg_tab name fix
;;

let slot_ty (s:Ast.slot) : Ast.ty =
  match s.Ast.slot_ty with
      Some t -> t
    | None -> bug () "untyped slot"
;;

let defn_is_slot (d:defn) : bool =
  match d with
      DEFN_slot _ -> true
    | _ -> false
;;

let defn_is_item (d:defn) : bool =
  match d with
      DEFN_item _ -> true
    | _ -> false
;;

let slot_is_module_state (cx:ctxt) (sid:node_id) : bool =
  Hashtbl.mem cx.ctxt_slot_is_module_state sid
;;


(* determines whether d defines a statically-known value *)
let defn_is_static (d:defn) : bool =
  not (defn_is_slot d)
;;

let defn_is_callable (d:defn) : bool =
  match d with
      DEFN_slot { Ast.slot_ty = Some Ast.TY_fn _ }
    | DEFN_slot { Ast.slot_ty = Some Ast.TY_pred _ }
    | DEFN_item { Ast.decl_item = (Ast.MOD_ITEM_pred _ ) }
    | DEFN_item { Ast.decl_item = (Ast.MOD_ITEM_fn _ ) } -> true
    | _ -> false
;;

(* Constraint manipulation. *)

let rec apply_names_to_carg_path
    (names:(Ast.name_base option) array)
    (cp:Ast.carg_path)
    : Ast.carg_path =
  match cp with
      Ast.CARG_ext (Ast.CARG_base Ast.BASE_formal,
                    Ast.COMP_idx i) ->
        begin
          match names.(i) with
              Some nb ->
                Ast.CARG_base (Ast.BASE_named nb)
            | None -> bug () "Indexing off non-named carg"
        end
    | Ast.CARG_ext (cp', e) ->
        Ast.CARG_ext (apply_names_to_carg_path names cp', e)
    | _ -> cp
;;

let apply_names_to_carg
    (names:(Ast.name_base option) array)
    (carg:Ast.carg)
    : Ast.carg =
  match carg with
      Ast.CARG_path cp ->
        Ast.CARG_path (apply_names_to_carg_path names cp)
    | Ast.CARG_lit _ -> carg
;;

let apply_names_to_constr
    (names:(Ast.name_base option) array)
    (constr:Ast.constr)
    : Ast.constr =
  { constr with
      Ast.constr_args =
      Array.map (apply_names_to_carg names) constr.Ast.constr_args }
;;

let atoms_to_names (atoms:Ast.atom array)
    : (Ast.name_base option) array =
  Array.map
    begin
      fun atom ->
        match atom with
            Ast.ATOM_lval (Ast.LVAL_base nbi) -> Some nbi.node
          | _ -> None
    end
    atoms
;;

let rec lval_base_id (lv:Ast.lval) : node_id =
  match lv with
      Ast.LVAL_base nbi -> nbi.id
    | Ast.LVAL_ext (lv, _) -> lval_base_id lv
;;

let rec lval_base_slot (cx:ctxt) (lv:Ast.lval) : node_id option =
  match lv with
      Ast.LVAL_base nbi ->
        let referent = lval_to_referent cx nbi.id in
          if referent_is_slot cx referent
          then Some referent
          else None
    | Ast.LVAL_ext (lv, _) -> lval_base_slot cx lv
;;

let rec lval_slots (cx:ctxt) (lv:Ast.lval) : node_id array =
  match lv with
      Ast.LVAL_base nbi ->
        let referent = lval_to_referent cx nbi.id in
          if referent_is_slot cx referent
          then [| referent |]
          else [| |]
    | Ast.LVAL_ext (lv, Ast.COMP_named _) -> lval_slots cx lv
    | Ast.LVAL_ext (lv, Ast.COMP_atom a) ->
        Array.append (lval_slots cx lv) (atom_slots cx a)

and atom_slots (cx:ctxt) (a:Ast.atom) : node_id array =
  match a with
      Ast.ATOM_literal _ -> [| |]
    | Ast.ATOM_lval lv -> lval_slots cx lv
;;

let lval_option_slots (cx:ctxt) (lv:Ast.lval option) : node_id array =
  match lv with
      None -> [| |]
    | Some lv -> lval_slots cx lv
;;

let resolve_lval (cx:ctxt) (lv:Ast.lval) : defn =
  resolve_lval_id cx (lval_base_id lv)
;;

let atoms_slots (cx:ctxt) (az:Ast.atom array) : node_id array =
  Array.concat (List.map (atom_slots cx) (Array.to_list az))
;;

let modes_and_atoms_slots (cx:ctxt) (az:(Ast.mode * Ast.atom) array) : node_id array =
  Array.concat (List.map (fun (_,a) -> atom_slots cx a) (Array.to_list az))
;;

let entries_slots (cx:ctxt)
    (entries:(Ast.ident * Ast.mode * Ast.atom) array) : node_id array =
  Array.concat (List.map
                  (fun (_, _, atom) -> atom_slots cx atom)
                  (Array.to_list entries))
;;

let expr_slots (cx:ctxt) (e:Ast.expr) : node_id array =
    match e with
        Ast.EXPR_binary (_, a, b) ->
          Array.append (atom_slots cx a) (atom_slots cx b)
      | Ast.EXPR_unary (_, u) -> atom_slots cx u
      | Ast.EXPR_atom a -> atom_slots cx a
;;


(* Type extraction. *)

let interior_slot_full mut ty : Ast.slot =
  { Ast.slot_mode = Ast.MODE_interior mut;
    Ast.slot_ty = Some ty }
;;

let exterior_slot_full mut ty : Ast.slot =
  { Ast.slot_mode = Ast.MODE_exterior mut;
    Ast.slot_ty = Some ty }
;;

let interior_slot ty : Ast.slot = interior_slot_full Ast.IMMUTABLE ty
;;

let exterior_slot ty : Ast.slot = exterior_slot_full Ast.IMMUTABLE ty
;;


(* General folds of Ast.ty. *)

type ('ty, 'slot, 'slots, 'tag) ty_fold =
    {
      (* Functions that correspond to interior nodes in Ast.ty. *)
      ty_fold_slot : (Ast.mode * 'ty) -> 'slot;
      ty_fold_slots : ('slot array) -> 'slots;
      ty_fold_tags : (Ast.name, 'slots) Hashtbl.t -> 'tag;

      (* Functions that correspond to the Ast.ty constructors. *)
      ty_fold_any: unit -> 'ty;
      ty_fold_nil : unit -> 'ty;
      ty_fold_bool : unit -> 'ty;
      ty_fold_mach : ty_mach -> 'ty;
      ty_fold_int : unit -> 'ty;
      ty_fold_char : unit -> 'ty;
      ty_fold_str : unit -> 'ty;
      ty_fold_tup : 'slots -> 'ty;
      ty_fold_vec : 'slot -> 'ty;
      ty_fold_rec : (Ast.ident * 'slot) array -> 'ty;
      ty_fold_tag : 'tag -> 'ty;
      ty_fold_iso : (int * 'tag array) -> 'ty;
      ty_fold_idx : int -> 'ty;
      ty_fold_fn : (('slots * Ast.constrs * 'slot) * Ast.ty_fn_aux) -> 'ty;
      ty_fold_pred : ('slots * Ast.constrs) -> 'ty;
      ty_fold_chan : 'ty -> 'ty;
      ty_fold_port : 'ty -> 'ty;
      ty_fold_mod : (('slots * Ast.constrs) option * Ast.mod_type_items) -> 'ty;
      ty_fold_proc : unit -> 'ty;
      ty_fold_opaque : (opaque_id * Ast.mutability) -> 'ty;
      ty_fold_param : (int * opaque_id * Ast.mutability) -> 'ty;
      ty_fold_named : Ast.name -> 'ty;
      ty_fold_type : unit -> 'ty;
      ty_fold_constrained : ('ty * Ast.constrs) -> 'ty }
;;

let rec fold_ty (f:('ty, 'slot, 'slots, 'tag) ty_fold) (ty:Ast.ty) : 'ty =
  let fold_slot (s:Ast.slot) : 'slot =
    f.ty_fold_slot (s.Ast.slot_mode, fold_ty f (slot_ty s))
  in
  let fold_slots (slots:Ast.slot array) : 'slots =
    f.ty_fold_slots (Array.map fold_slot slots)
  in
  let fold_tags (ttag:Ast.ty_tag) : 'tag =
    f.ty_fold_tags (htab_map ttag (fun k v -> (k, fold_slots v)))
  in
  let fold_sig tsig =
    (fold_slots tsig.Ast.sig_input_slots,
     tsig.Ast.sig_input_constrs,
     fold_slot tsig.Ast.sig_output_slot)
  in
    match ty with
    Ast.TY_any -> f.ty_fold_any ()
  | Ast.TY_nil -> f.ty_fold_nil ()
  | Ast.TY_bool -> f.ty_fold_bool ()
  | Ast.TY_mach m -> f.ty_fold_mach m
  | Ast.TY_int -> f.ty_fold_int ()
  | Ast.TY_char -> f.ty_fold_char ()
  | Ast.TY_str -> f.ty_fold_str ()

  | Ast.TY_tup t -> f.ty_fold_tup (fold_slots t)
  | Ast.TY_vec s -> f.ty_fold_vec (fold_slot s)
  | Ast.TY_rec r -> f.ty_fold_rec (Array.map (fun (k,v) -> (k,fold_slot v)) r)

  | Ast.TY_tag tt -> f.ty_fold_tag (fold_tags tt)
  | Ast.TY_iso ti -> f.ty_fold_iso (ti.Ast.iso_index,
                                    (Array.map fold_tags ti.Ast.iso_group))
  | Ast.TY_idx i -> f.ty_fold_idx i

  | Ast.TY_fn (tsig,taux) -> f.ty_fold_fn (fold_sig tsig, taux)
  | Ast.TY_pred (slots, constrs) -> f.ty_fold_pred (fold_slots slots, constrs)
  | Ast.TY_chan t -> f.ty_fold_chan (fold_ty f t)
  | Ast.TY_port t -> f.ty_fold_port (fold_ty f t)

  | Ast.TY_mod (hdr, mti) ->
      let folded_hdr =
        match hdr with
            None -> None
          | Some (slots, constrs) -> Some (fold_slots slots, constrs)
      in
        f.ty_fold_mod (folded_hdr, mti)

  | Ast.TY_proc -> f.ty_fold_proc ()

  | Ast.TY_opaque x -> f.ty_fold_opaque x
  | Ast.TY_param x -> f.ty_fold_param x
  | Ast.TY_named n -> f.ty_fold_named n
  | Ast.TY_type -> f.ty_fold_type ()

  | Ast.TY_constrained (t, constrs) ->
      f.ty_fold_constrained (fold_ty f t, constrs)

;;

type 'a simple_ty_fold = ('a, 'a, 'a, 'a) ty_fold
;;

let ty_fold_default (default:'a) : 'a simple_ty_fold =
    { ty_fold_slot = (fun _ -> default);
      ty_fold_slots = (fun _ -> default);
      ty_fold_tags = (fun _ -> default);
      ty_fold_any = (fun _ -> default);
      ty_fold_nil = (fun _ -> default);
      ty_fold_bool = (fun _ -> default);
      ty_fold_mach = (fun _ -> default);
      ty_fold_int = (fun _ -> default);
      ty_fold_char = (fun _ -> default);
      ty_fold_str = (fun _ -> default);
      ty_fold_tup = (fun _ -> default);
      ty_fold_vec = (fun _ -> default);
      ty_fold_rec = (fun _ -> default);
      ty_fold_tag = (fun _ -> default);
      ty_fold_iso = (fun _ -> default);
      ty_fold_idx = (fun _ -> default);
      ty_fold_fn = (fun _ -> default);
      ty_fold_pred = (fun _ -> default);
      ty_fold_chan = (fun _ -> default);
      ty_fold_port = (fun _ -> default);
      ty_fold_mod = (fun _ -> default);
      ty_fold_proc = (fun _ -> default);
      ty_fold_opaque = (fun _ -> default);
      ty_fold_param = (fun _ -> default);
      ty_fold_named = (fun _ -> default);
      ty_fold_type = (fun _ -> default);
      ty_fold_constrained = (fun _ -> default) }
;;

let ty_fold_rebuild (id:Ast.ty -> Ast.ty)
    : (Ast.ty, Ast.slot, Ast.slot array, Ast.ty_tag) ty_fold =
  { ty_fold_slot = (fun (mode, t) -> { Ast.slot_mode = mode;
                                       Ast.slot_ty = Some t });
    ty_fold_slots = (fun slots -> slots);
    ty_fold_tags = (fun htab -> htab);
    ty_fold_any = (fun _ -> id Ast.TY_any);
    ty_fold_nil = (fun _ -> id Ast.TY_nil);
    ty_fold_bool = (fun _ -> id Ast.TY_bool);
    ty_fold_mach = (fun m -> id (Ast.TY_mach m));
    ty_fold_int = (fun _ -> id Ast.TY_int);
    ty_fold_char = (fun _ -> id Ast.TY_char);
    ty_fold_str = (fun _ -> id Ast.TY_str);
    ty_fold_tup =  (fun slots -> id (Ast.TY_tup slots));
    ty_fold_vec = (fun slot -> id (Ast.TY_vec slot));
    ty_fold_rec = (fun entries -> id (Ast.TY_rec entries));
    ty_fold_tag = (fun tag -> id (Ast.TY_tag tag));
    ty_fold_iso = (fun (i, tags) -> id (Ast.TY_iso { Ast.iso_index = i;
                                                     Ast.iso_group = tags }));
    ty_fold_idx = (fun i -> id (Ast.TY_idx i));
    ty_fold_fn = (fun ((islots, constrs, oslot), aux) ->
                    id (Ast.TY_fn ({ Ast.sig_input_slots = islots;
                                     Ast.sig_input_constrs = constrs;
                                     Ast.sig_output_slot = oslot }, aux)));
    ty_fold_pred = (fun (islots, constrs) ->
                      id (Ast.TY_pred (islots, constrs)));
    ty_fold_chan = (fun t -> id (Ast.TY_chan t));
    ty_fold_port = (fun t -> id (Ast.TY_port t));
    ty_fold_mod = (fun (hdr, mti) -> id (Ast.TY_mod (hdr, mti)));
    ty_fold_proc = (fun _ -> id Ast.TY_proc);
    ty_fold_opaque = (fun (opa, mut) -> id (Ast.TY_opaque (opa, mut)));
    ty_fold_param = (fun (i, oid, mut) -> id (Ast.TY_param (i, oid, mut)));
    ty_fold_named = (fun n -> id (Ast.TY_named n));
    ty_fold_type = (fun _ -> id (Ast.TY_type));
    ty_fold_constrained = (fun (t, constrs) -> id (Ast.TY_constrained (t, constrs))) }
;;

let associative_binary_op_ty_fold
    (default:'a)
    (fn:'a -> 'a -> 'a)
    : 'a simple_ty_fold =
  let base = ty_fold_default default in
  let reduce ls =
    match ls with
        [] -> default
      | x::xs -> List.fold_left fn x xs
  in
    { base with
        ty_fold_slots = (fun slots -> reduce (Array.to_list slots));
        ty_fold_slot = (fun (_, a) -> a);
        ty_fold_tags = (fun tab -> reduce (htab_vals tab));
        ty_fold_tup = (fun a -> a);
        ty_fold_vec = (fun a -> a);
        ty_fold_rec = (fun sz -> reduce (Array.to_list (Array.map (fun (_, s) -> s) sz)));
        ty_fold_tag = (fun a -> a);
        ty_fold_iso = (fun (_,iso) -> reduce (Array.to_list iso));
        ty_fold_fn = (fun ((islots, _, oslot), _) -> fn islots oslot);
        ty_fold_pred = (fun (islots, _) -> islots);
        ty_fold_chan = (fun a -> a);
        ty_fold_port = (fun a -> a);
        ty_fold_constrained = (fun (a, _) -> a) }

let ty_fold_bool_and (default:bool) : bool simple_ty_fold =
  associative_binary_op_ty_fold default (fun a b -> a & b)
;;

let ty_fold_bool_or (default:bool) : bool simple_ty_fold =
  associative_binary_op_ty_fold default (fun a b -> a || b)
;;

let ty_fold_list_concat _ : ('a list) simple_ty_fold =
  associative_binary_op_ty_fold [] (fun a b -> a @ b)
;;

(* Mutability analysis. *)

let mode_is_mutable (m:Ast.mode) : bool =
  match m with
      Ast.MODE_exterior Ast.MUTABLE
    | Ast.MODE_interior Ast.MUTABLE -> true
    | _ -> false
;;

let slot_is_mutable (s:Ast.slot) : bool =
  mode_is_mutable s.Ast.slot_mode
;;

let type_is_mutable (t:Ast.ty) : bool =
  let fold_slot (mode, b) =
    if b
    then true
    else mode_is_mutable mode
  in
  let fold = ty_fold_bool_or false in
  let fold = { fold with ty_fold_slot = fold_slot } in
    fold_ty fold t
;;

(* Analyze whether a type contains a channel, in which case we have to deep copy. *)

let type_contains_chan (t:Ast.ty) : bool =
  let fold_chan _ = true in
  let fold = ty_fold_bool_or false in
  let fold = { fold with ty_fold_chan = fold_chan } in
    fold_ty fold t
;;

(* GC analysis. *)

(* If a type is cyclic, it is managed by mark-sweep GC, not
 * refcounting.
 * 
 * A type T is "cyclic" iff it contains a mutable slot that contains an
 * un-captured TY_idx (that is, there is a 'mutability point' between
 * the TY_idx and its enclosing TY_iso).
 * 
 *)

let type_is_cyclic (t:Ast.ty) : bool =

  let contains_uncaptured_idx t =
    let fold_idx _ = true in
    let fold_iso _ = false in
    let fold = ty_fold_bool_or false in
    let fold = { fold with
                   ty_fold_idx = fold_idx;
                   ty_fold_iso = fold_iso }
    in
      fold_ty fold t
  in

  let is_cyclic = ref false in
  let fold_slot (mode, ty) =
    if mode_is_mutable mode && contains_uncaptured_idx ty
    then is_cyclic := true;
    { Ast.slot_mode = mode;
      Ast.slot_ty = Some t }
  in
  let fold = ty_fold_rebuild (fun t -> t) in
  let fold = { fold with ty_fold_slot = fold_slot } in
  let _ = fold_ty fold t in
    !is_cyclic
;;


let check_concrete params thing =
  if Array.length params = 0
  then thing
  else bug () "unhandled parametric binding"
;;


let ty_of_mod_type_item (mti:Ast.mod_type_item) : Ast.ty =
  check_concrete mti.Ast.decl_params
    begin
      match mti.Ast.decl_item with
          Ast.MOD_TYPE_ITEM_opaque_type mut ->
            (* 
             * FIXME (bug 541598): generate opaque_ids uniquely per
             * name (incl. type parameters)
             *)
            let opaque_id = Opaque 0 in
              Ast.TY_opaque (opaque_id, mut)
        | Ast.MOD_TYPE_ITEM_public_type ty -> ty
        | Ast.MOD_TYPE_ITEM_pred p -> Ast.TY_pred p
        | Ast.MOD_TYPE_ITEM_mod m -> Ast.TY_mod m
        | Ast.MOD_TYPE_ITEM_fn f -> Ast.TY_fn f
    end
;;


let project_type_to_slot (base_ty:Ast.ty) (comp:Ast.lval_component) : Ast.slot =
  match (base_ty, comp) with
      (Ast.TY_rec elts, Ast.COMP_named (Ast.COMP_ident id)) ->
        begin
          match atab_search elts id with
              Some slot -> slot
            | None -> err None "unknown record-member '%s'" id
        end

    | (Ast.TY_tup elts, Ast.COMP_named (Ast.COMP_idx i)) ->
        if 0 <= i && i < (Array.length elts)
        then elts.(i)
        else err None "out-of-range tuple index %d" i

    | (Ast.TY_vec slot, Ast.COMP_atom _) ->
        slot

    | (Ast.TY_str, Ast.COMP_atom _) ->
        interior_slot (Ast.TY_mach TY_u8)

    | (Ast.TY_mod (_, mtis), Ast.COMP_named (Ast.COMP_ident id)) ->
        begin
          match htab_search mtis id with
              Some mti -> interior_slot (ty_of_mod_type_item mti)
            | None -> err None "unknown module-member '%s'" id
        end

    | (_,_) ->
        bug () "unhandled form of lval-ext in Semant.project_slot: %a indexed by %a"
          Ast.sprintf_ty base_ty Ast.sprintf_lval_component comp
;;


let project_mod_type_to_type (base_ty:Ast.ty) (comp:Ast.lval_component) : Ast.ty =
  match base_ty with
      Ast.TY_mod (_, mtis) ->
        begin
          match comp with
              Ast.COMP_named (Ast.COMP_ident id) ->
                begin
                  match htab_search mtis id with
                      None -> err None "unknown module-type item '%s'" id
                    | Some mti -> ty_of_mod_type_item mti
                end
            | _ -> bug () "unhandled name-component type in Semant.project_mod_type_to_type"
        end
    | _ -> err None "non-module base type in Semant.project_mod_type_to_type"
;;


(* NB: this will fail if lval is not a slot. *)
let rec lval_slot (cx:ctxt) (lval:Ast.lval) : Ast.slot =
  match lval with
      Ast.LVAL_base nb -> lval_to_slot cx nb.id
    | Ast.LVAL_ext (base, comp) ->
        let base_ty = slot_ty (lval_slot cx base) in
          project_type_to_slot base_ty comp
;;

(* NB: this will fail if lval is not an item. *)
let rec lval_item (cx:ctxt) (lval:Ast.lval) : Ast.mod_item =
  match lval with
      Ast.LVAL_base nb ->
        begin
          let referent = lval_to_referent cx nb.id in
            match htab_search cx.ctxt_all_defns referent with
                (*
                 * FIXME: returning empty params this way is wrong.
                 * DEFN_item should hold an Ast.mod_item' decl.
                 *)
                Some (DEFN_item item) -> {node=item; id=referent}
              | _ -> bug () "lval does not name an item"
        end
    | Ast.LVAL_ext (base, comp) ->
        let base_item = lval_item cx base in
        match base_item.node.Ast.decl_item with
            Ast.MOD_ITEM_mod (_, items) ->
              begin
                match comp with
                    Ast.COMP_named (Ast.COMP_ident i) ->
                      begin
                        match htab_search items i with
                            None -> err None "unknown module item '%s'" i
                          | Some sub -> check_concrete base_item.node.Ast.decl_params sub
                      end
                  | _ ->
                      bug () "unhandled lval-component in Semant.lval_item"
              end
          | _ -> err None "lval base %a does not name a module" Ast.sprintf_lval base
;;

let lval_is_slot (cx:ctxt) (lval:Ast.lval) : bool =
  match resolve_lval cx lval with
      DEFN_slot _ -> true
    | _ -> false
;;

let lval_is_item (cx:ctxt) (lval:Ast.lval) : bool =
  match resolve_lval cx lval with
      DEFN_item _ -> true
    | _ -> false
;;

let lval_is_direct_fn (cx:ctxt) (lval:Ast.lval) : bool =
  let defn = resolve_lval cx lval in
    (defn_is_static defn) && (defn_is_callable defn)
;;

let lval_is_direct_mod (cx:ctxt) (lval:Ast.lval) : bool =
  let defn = resolve_lval cx lval in
    if not (defn_is_static defn)
    then false
    else
      match defn with
          DEFN_item { Ast.decl_item = Ast.MOD_ITEM_mod (None, _) } -> true
        | _ -> false
;;

let lval_is_static (cx:ctxt) (lval:Ast.lval) : bool =
  defn_is_static (resolve_lval cx lval)
;;

let lval_is_callable (cx:ctxt) (lval:Ast.lval) : bool =
  defn_is_callable (resolve_lval cx lval)
;;


let rec lval_ty (cx:ctxt) (lval:Ast.lval) : Ast.ty =
  let base_id = lval_base_id lval in
    Hashtbl.find cx.ctxt_all_lval_types base_id
;;

let rec atom_type (cx:ctxt) (at:Ast.atom) : Ast.ty =
  match at with
      Ast.ATOM_literal {node=(Ast.LIT_int _); id=_} -> Ast.TY_int
    | Ast.ATOM_literal {node=(Ast.LIT_bool _); id=_} -> Ast.TY_bool
    | Ast.ATOM_literal {node=(Ast.LIT_char _); id=_} -> Ast.TY_char
    | Ast.ATOM_literal {node=(Ast.LIT_nil); id=_} -> Ast.TY_nil
    | Ast.ATOM_literal {node=(Ast.LIT_mach (m,_,_)); id=_} -> Ast.TY_mach m
    | Ast.ATOM_literal _ -> bug () "unhandled form of literal '%a', in atom_type" Ast.sprintf_atom at
    | Ast.ATOM_lval lv -> lval_ty cx lv
;;

let expr_type (cx:ctxt) (e:Ast.expr) : Ast.ty =
  match e with
      Ast.EXPR_binary (op, a, _) ->
        begin
          match op with
              Ast.BINOP_eq | Ast.BINOP_ne | Ast.BINOP_lt  | Ast.BINOP_le
            | Ast.BINOP_ge | Ast.BINOP_gt -> Ast.TY_bool
            | _ -> atom_type cx a
        end
    | Ast.EXPR_unary (Ast.UNOP_not, _) -> Ast.TY_bool
    | Ast.EXPR_unary (_, a) -> atom_type cx a
    | Ast.EXPR_atom a -> atom_type cx a
;;


(* Mappings between mod items and their respective types. *)

let rec ty_mod_of_mod
    (inside:bool)
    (m:(Ast.mod_header option * Ast.mod_items))
    : Ast.ty_mod =
  let (hdr, mis) = m in
  let hdr_type =
    match hdr with
        None -> None
      | Some (slots, constrs) -> Some (arg_slots slots, constrs)
  in
  let ty_items = Hashtbl.create (Hashtbl.length mis) in
  let add n i =
    match mod_type_item_of_mod_item inside i with
        None -> ()
      | Some mty -> Hashtbl.add ty_items n mty
  in
    Hashtbl.iter add mis;
    (hdr_type, ty_items)

and mod_type_item_of_mod_item
    (inside:bool)
    (item:Ast.mod_item)
    : Ast.mod_type_item option =
  let item_opt =
    match item.node.Ast.decl_item with
        Ast.MOD_ITEM_opaque_type t ->
          if inside
          then
            Some (Ast.MOD_TYPE_ITEM_public_type t)
          else
            let mut =
              if type_is_mutable t
              then Ast.MUTABLE
              else Ast.IMMUTABLE
            in
              Some (Ast.MOD_TYPE_ITEM_opaque_type mut)
      | Ast.MOD_ITEM_public_type t ->
          Some (Ast.MOD_TYPE_ITEM_public_type t)
      | Ast.MOD_ITEM_pred p ->
          Some (Ast.MOD_TYPE_ITEM_pred (ty_pred_of_pred p))
      | Ast.MOD_ITEM_mod m ->
          Some (Ast.MOD_TYPE_ITEM_mod (ty_mod_of_mod true m))
      | Ast.MOD_ITEM_fn f ->
          Some (Ast.MOD_TYPE_ITEM_fn (ty_fn_of_fn f))
      | Ast.MOD_ITEM_tag _ -> None
  in
    match item_opt with
        None -> None
      | Some item' ->
          Some { Ast.decl_params = Array.map (fun i -> i.node) item.node.Ast.decl_params;
                 Ast.decl_item = item' }

and arg_slots (slots:Ast.header_slots) : Ast.slot array =
  Array.map (fun (sid,_) -> sid.node) slots

and tup_slots (slots:Ast.header_tup) : Ast.slot array =
  Array.map (fun sid -> sid.node) slots

and ty_fn_of_fn (fn:Ast.fn) : Ast.ty_fn =
  ({ Ast.sig_input_slots = arg_slots fn.Ast.fn_input_slots;
     Ast.sig_input_constrs = fn.Ast.fn_input_constrs;
     Ast.sig_output_slot = fn.Ast.fn_output_slot.node },
   fn.Ast.fn_aux )

and ty_pred_of_pred (pred:Ast.pred) : Ast.ty_pred =
  (arg_slots pred.Ast.pred_input_slots,
   pred.Ast.pred_input_constrs)

and ty_of_mod_item (inside:bool) (item:Ast.mod_item) : Ast.ty =
  match item.node.Ast.decl_item with
      Ast.MOD_ITEM_opaque_type _
    | Ast.MOD_ITEM_public_type _ -> Ast.TY_type
    | Ast.MOD_ITEM_pred p -> (Ast.TY_pred (ty_pred_of_pred p))
    | Ast.MOD_ITEM_mod m -> (Ast.TY_mod (ty_mod_of_mod inside m))
    | Ast.MOD_ITEM_fn f -> (Ast.TY_fn (ty_fn_of_fn f))
    | Ast.MOD_ITEM_tag (htup, ttag, _) ->
        let taux = { Ast.fn_purity = Ast.PURE;
                     Ast.fn_proto = None }
        in
        let tsig = { Ast.sig_input_slots = tup_slots htup;
                     Ast.sig_input_constrs = [| |];
                     Ast.sig_output_slot = interior_slot (Ast.TY_tag ttag) }
        in
          (Ast.TY_fn (tsig, taux))
;;

(* Scopes and the visitor that builds them. *)

type scope =
    SCOPE_block of node_id
  | SCOPE_mod_item of Ast.mod_item
  | SCOPE_mod_type_item of Ast.mod_type_item
  | SCOPE_crate of Ast.crate
;;

let id_of_scope (sco:scope) : node_id =
  match sco with
      SCOPE_block id -> id
    | SCOPE_mod_item i -> i.id
    | SCOPE_mod_type_item _ -> bug () "id_of_scope within mod_type_item"
    | SCOPE_crate c -> c.id
;;

let scope_stack_managing_visitor
    (scopes:(scope list) ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let push s =
    scopes := s :: (!scopes)
  in
  let pop _ =
    scopes := List.tl (!scopes)
  in
  let visit_block_pre b =
    push (SCOPE_block b.id);
    inner.Walk.visit_block_pre b
  in
  let visit_block_post b =
    inner.Walk.visit_block_post b;
    pop();
  in
  let visit_mod_item_pre n p i =
    push (SCOPE_mod_item i);
    inner.Walk.visit_mod_item_pre n p i
  in
  let visit_mod_item_post n p i =
    inner.Walk.visit_mod_item_post n p i;
    pop();
  in
  let visit_mod_type_item_pre n p i =
    push (SCOPE_mod_type_item i);
    inner.Walk.visit_mod_type_item_pre n p i
  in
  let visit_mod_type_item_post n p i =
    inner.Walk.visit_mod_type_item_post n p i;
    pop();
  in
  let visit_crate_pre c =
    push (SCOPE_crate c);
    inner.Walk.visit_crate_pre c
  in
  let visit_crate_post c =
    inner.Walk.visit_crate_post c;
    pop()
  in
    { inner with
        Walk.visit_block_pre = visit_block_pre;
        Walk.visit_block_post = visit_block_post;
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post;
        Walk.visit_mod_type_item_pre = visit_mod_type_item_pre;
        Walk.visit_mod_type_item_post = visit_mod_type_item_post;
        Walk.visit_crate_pre = visit_crate_pre;
        Walk.visit_crate_post = visit_crate_post; }
;;

(* Generic lookup, used for slots, items, types, etc. *)

let lookup_by_ident
    (cx:ctxt)
    (scopes:scope list)
    (ident:Ast.ident)
    : ((scope list * node_id) option) =
  let check_items items =
    match htab_search items ident with
        None -> None
      | Some i -> Some i.id
  in
  let check_input_slot islots =
    arr_search islots
      (fun _ (sloti,ident') ->
         if ident = ident'
         then Some sloti.id
         else None)
  in
  let check_params params =
    arr_search params
      (fun _ {node=(i,_); id=id} ->
         if i = ident then Some id else None)
  in
  let check_scope scope =
    match scope with
        SCOPE_block block_id ->
          let block_slots = Hashtbl.find cx.ctxt_block_slots block_id in
          let block_items = Hashtbl.find cx.ctxt_block_items block_id in
            begin
              match htab_search block_slots (Ast.KEY_ident ident) with
                  Some id -> Some id
                | None -> htab_search block_items ident
            end

      | SCOPE_crate crate ->
          check_items crate.node.Ast.crate_items

      | SCOPE_mod_item item ->
          begin
            let item_match =
              match item.node.Ast.decl_item with
                  Ast.MOD_ITEM_fn f ->
                    check_input_slot f.Ast.fn_input_slots

                | Ast.MOD_ITEM_pred p ->
                    check_input_slot p.Ast.pred_input_slots

                | Ast.MOD_ITEM_mod (hdr, md) ->
                    begin
                      match check_items md with
                          Some m -> Some m
                        | None ->
                            begin
                              match hdr with
                                  None -> None
                                | Some (h, _) ->
                                    check_input_slot h
                            end
                    end
                | _ -> None
            in
              match item_match with
                  Some _ -> item_match
                | None -> check_params item.node.Ast.decl_params
          end

      | SCOPE_mod_type_item _ -> None
  in
    list_search_ctxt scopes check_scope
;;

let lookup_by_temp
    (cx:ctxt)
    (scopes:scope list)
    (temp:temp_id)
    : ((scope list * node_id) option) =
  let check_scope scope =
    match scope with
        SCOPE_block block_id ->
          let block_slots = Hashtbl.find cx.ctxt_block_slots block_id in
            htab_search block_slots (Ast.KEY_temp temp)
      | _ -> None
  in
    list_search_ctxt scopes check_scope
;;

let lookup
    (cx:ctxt)
    (scopes:scope list)
    (key:Ast.slot_key)
    : ((scope list * node_id) option) =
  match key with
      Ast.KEY_temp temp -> lookup_by_temp cx scopes temp
    | Ast.KEY_ident ident -> lookup_by_ident cx scopes ident
;;


let run_passes
    (cx:ctxt)
    (name:string)
    (path:Ast.name_component Stack.t)
    (passes:Walk.visitor array)
    (log:string->unit)
    (crate:Ast.crate)
    : unit =
  let do_pass i pass =
    let logger s = log (Printf.sprintf "pass %d: %s" i s) in
      Walk.walk_crate
        (Walk.path_managing_visitor path
           (Walk.mod_item_logging_visitor logger path pass))
        crate
  in
  let sess = cx.ctxt_sess in
    if sess.Session.sess_failed
    then ()
    else
      try
        Session.time_inner name sess
          (fun _ -> Array.iteri do_pass passes)
      with
          Semant_err (ido, str) -> report_err cx ido str
;;

(* Rust type -> IL type conversion. *)

let word_sty (abi:Abi.abi) : Il.scalar_ty =
  Il.ValTy abi.Abi.abi_word_bits
;;

let word_rty (abi:Abi.abi) : Il.referent_ty =
  Il.ScalarTy (word_sty abi)
;;

let rec referent_type (abi:Abi.abi) (t:Ast.ty) : Il.referent_ty =
  let s t = Il.ScalarTy t in
  let v b = Il.ValTy b in
  let p t = Il.AddrTy t in
  let sv b = s (v b) in
  let sp t = s (p t) in

  let word = word_rty abi in
  let ptr = sp Il.OpaqueTy in
  let rc_ptr = sp (Il.StructTy [| word; Il.OpaqueTy |]) in
  let codeptr = sp Il.CodeTy in
  let tup ttup = Il.StructTy (Array.map (slot_referent_type abi) ttup) in
  let tag ttag =
    let union =
      Il.UnionTy
        (Array.map
           (fun key -> tup (Hashtbl.find ttag key))
           (sorted_htab_keys ttag))
    in
    let discriminant = word in
      Il.StructTy [| discriminant; union |]
  in

    match t with
        Ast.TY_any -> Il.StructTy [| word;  ptr |]
      | Ast.TY_nil -> Il.NilTy
      | Ast.TY_int -> word
          (* FIXME (bug 541563): bool should be 8 bit, not word-sized. *)
      | Ast.TY_bool -> word

      | Ast.TY_mach (TY_u8)
      | Ast.TY_mach (TY_s8) -> sv Il.Bits8

      | Ast.TY_mach (TY_u16)
      | Ast.TY_mach (TY_s16) -> sv Il.Bits16

      | Ast.TY_mach (TY_u32)
      | Ast.TY_mach (TY_s32)
      | Ast.TY_mach (TY_f32)
      | Ast.TY_char -> sv Il.Bits32

      | Ast.TY_mach (TY_u64)
      | Ast.TY_mach (TY_s64)
      | Ast.TY_mach (TY_f64) -> sv Il.Bits64

      | Ast.TY_str -> sp (Il.StructTy [| word; word; word; ptr |])
      | Ast.TY_vec _ -> sp (Il.StructTy [| word; word; word; ptr |])
      | Ast.TY_tup tt -> tup tt
      | Ast.TY_rec tr -> tup (Array.map snd tr)

      | Ast.TY_fn _
      | Ast.TY_pred _ ->
          let fn_closure_ptr = sp (Il.StructTy [| word; ptr |]) in
            Il.StructTy [| codeptr; fn_closure_ptr |]

      | Ast.TY_mod _ ->
          let mod_closure_ptr = sp (Il.StructTy [| word; ptr |]) in
            Il.StructTy [| ptr; mod_closure_ptr |]

      | Ast.TY_tag ttag -> tag ttag
      | Ast.TY_iso tiso -> tag tiso.Ast.iso_group.(tiso.Ast.iso_index)

      | Ast.TY_idx _ -> Il.OpaqueTy

      | Ast.TY_chan _
      | Ast.TY_port _
      | Ast.TY_proc
      | Ast.TY_type -> rc_ptr

      | Ast.TY_param (i, _, _) -> Il.ParamTy i

      | Ast.TY_opaque _ -> bug () "opaque type in referent_type"
      | Ast.TY_named _ -> bug () "named type in referent_type"
      | Ast.TY_constrained (t, _) -> referent_type abi t

and slot_referent_type (abi:Abi.abi) (sl:Ast.slot) : Il.referent_ty =
  let s t = Il.ScalarTy t in
  let v b = Il.ValTy b in
  let p t = Il.AddrTy t in
  let sv b = s (v b) in
  let sp t = s (p t) in

  let word = sv abi.Abi.abi_word_bits in

  let rty = referent_type abi (slot_ty sl) in
  match sl.Ast.slot_mode with
      Ast.MODE_exterior _ -> sp (Il.StructTy [| word; rty |])
    | Ast.MODE_interior _ -> rty
    | Ast.MODE_read_alias -> sp rty
    | Ast.MODE_write_alias -> sp rty
;;

let proc_rty (abi:Abi.abi) : Il.referent_ty =
  Il.StructTy
    begin
      Array.init
        Abi.n_visible_proc_fields
        (fun _ -> word_rty abi)
    end
;;

let tydesc_rty (abi:Abi.abi) : Il.referent_ty =
  (* 
   * NB: must match corresponding tydesc structure
   * in trans and offsets in ABI exactly.
   *)
  Il.StructTy
    [|
      word_rty abi;                      (* Abi.tydesc_field_size      *)
      word_rty abi;                      (* Abi.tydesc_field_align     *)
      Il.ScalarTy (Il.AddrTy Il.CodeTy); (* Abi.tydesc_field_copy_glue *)
      Il.ScalarTy (Il.AddrTy Il.CodeTy); (* Abi.tydesc_field_drop_glue *)
      Il.ScalarTy (Il.AddrTy Il.CodeTy); (* Abi.tydesc_field_free_glue *)
    |]
;;

let call_args_referent_type_full
    (abi:Abi.abi)
    (out_slot:Ast.slot)
    (n_ty_params:int)
    (in_slots:Ast.slot array)
    (extra_arg_rtys:Il.referent_ty array)
    : Il.referent_ty =
  let out_slot_rty = slot_referent_type abi out_slot in
  let out_ptr_rty = Il.ScalarTy (Il.AddrTy out_slot_rty) in
  let proc_ptr_rty = Il.ScalarTy (Il.AddrTy (proc_rty abi)) in
  let ty_param_rtys =
    let td = Il.ScalarTy (Il.AddrTy (tydesc_rty abi)) in
      Il.StructTy (Array.init n_ty_params (fun _ -> td))
  in
  let arg_rtys = Il.StructTy (Array.map (slot_referent_type abi) in_slots) in
    (* 
     * NB: must match corresponding calltup structure in trans and
     * member indices in ABI exactly.
     *)
    Il.StructTy
      [|
        out_ptr_rty;                (* Abi.calltup_elt_out_ptr    *)
        proc_ptr_rty;               (* Abi.calltup_elt_proc_ptr   *)
        ty_param_rtys;              (* Abi.calltup_elt_ty_params  *)
        arg_rtys;                   (* Abi.calltup_elt_args       *)
        Il.StructTy extra_arg_rtys  (* Abi.calltup_elt_extra_args *)
      |]
;;

let call_args_referent_type
    (cx:ctxt)
    (n_ty_params:int)
    (callee_ty:Ast.ty)
    (closure:Il.referent_ty option)
    : Il.referent_ty =
  let with_closure e =
    match closure with
        None -> e
      | Some c ->
          Array.append e
            [|
              Il.ScalarTy (Il.AddrTy c) (* Abi.extra_args_elt_closure *)
            |]
  in
    match callee_ty with
        Ast.TY_fn (tsig, taux) ->
          let extras =
            match taux.Ast.fn_proto with
                None -> with_closure [| |]
                  (* FIXME: extra-args will expand with non-empty call-protocol *)
              | _ -> bug cx "nonempty call protocols not yet implemented"
          in
            call_args_referent_type_full
              cx.ctxt_abi
              tsig.Ast.sig_output_slot
              n_ty_params
              tsig.Ast.sig_input_slots
              extras

      | Ast.TY_pred (in_args, _) ->
          call_args_referent_type_full
            cx.ctxt_abi
            (interior_slot Ast.TY_bool)
            n_ty_params
            in_args
            (with_closure [| |])

      | Ast.TY_mod (Some (in_args, _), mtis) ->
          call_args_referent_type_full
            cx.ctxt_abi
            (interior_slot (Ast.TY_mod (None, mtis)))
            n_ty_params
            in_args
            (with_closure [| |])

      | _ -> bug cx "Semant.call_args_referent_type on non-callable type"
;;

let indirect_call_args_referent_type
    (cx:ctxt)
    (n_ty_params:int)
    (callee_ty:Ast.ty)
    (closure:Il.referent_ty)
    : Il.referent_ty =
  call_args_referent_type cx n_ty_params callee_ty (Some closure)
;;

let direct_call_args_referent_type
    (cx:ctxt)
    (n_ty_params:int)
    (callee_node:node_id)
    : Il.referent_ty =
  let ity = Hashtbl.find cx.ctxt_all_item_types callee_node in
    call_args_referent_type cx n_ty_params ity None
;;

let ty_sz (abi:Abi.abi) (t:Ast.ty) : int64 =
  force_sz (Il.referent_ty_size abi.Abi.abi_word_bits (referent_type abi t))
;;

let ty_align (abi:Abi.abi) (t:Ast.ty) : int64 =
  force_sz (Il.referent_ty_align abi.Abi.abi_word_bits (referent_type abi t))
;;

let slot_sz (abi:Abi.abi) (s:Ast.slot) : int64 =
  force_sz (Il.referent_ty_size abi.Abi.abi_word_bits (slot_referent_type abi s))
;;

let word_slot (abi:Abi.abi) : Ast.slot =
  interior_slot (Ast.TY_mach abi.Abi.abi_word_ty)
;;

let word_write_alias_slot (abi:Abi.abi) : Ast.slot =
  { Ast.slot_mode = Ast.MODE_write_alias;
    Ast.slot_ty = Some (Ast.TY_mach abi.Abi.abi_word_ty) }
;;

(* FIXME: eliminate this, it duplicates logic elsewhere. *)
let fn_call_tup (abi:Abi.abi) (inputs:Ast.ty_tup) : Ast.ty_tup =
  let proc_ptr = word_slot abi in
  let out_ptr = word_slot abi in
    Array.append [| out_ptr; proc_ptr |] inputs
;;


let mk_ty_fn
    (out_slot:Ast.slot)
    (arg_slots:Ast.slot array)
    : Ast.ty =
  (* In some cases we don't care what aux or constrs are. *)
  let taux = { Ast.fn_purity = Ast.PURE;
               Ast.fn_proto = None; }
  in
  let tsig = { Ast.sig_input_slots = arg_slots;
               Ast.sig_input_constrs = [| |];
               Ast.sig_output_slot = out_slot; }
  in
    Ast.TY_fn (tsig, taux)
;;

let mk_simple_ty_fn
    (arg_slots:Ast.slot array)
    : Ast.ty =
  (* In some cases we don't care what the output slot is. *)
  let out_slot = interior_slot Ast.TY_nil in
    mk_ty_fn out_slot arg_slots
;;


(* name mangling support. *)

let item_name (cx:ctxt) (id:node_id) : Ast.name =
  Hashtbl.find cx.ctxt_all_item_names id
;;

let item_str (cx:ctxt) (id:node_id) : string =
    string_of_name (item_name cx id)
;;

let ty_str (ty:Ast.ty) : string =
  let base = associative_binary_op_ty_fold "" (fun a b -> a ^ b) in
  let fold_slot (mode,ty) =
    match mode with
        Ast.MODE_exterior Ast.IMMUTABLE -> "E" ^ ty
      | Ast.MODE_interior Ast.IMMUTABLE -> ty
      | Ast.MODE_exterior Ast.MUTABLE -> "M" ^ ty
      | Ast.MODE_interior Ast.MUTABLE -> "m" ^ ty
      | Ast.MODE_read_alias -> "r" ^ ty
      | Ast.MODE_write_alias -> "w" ^ ty
  in
  let num n = (string_of_int n) ^ "$" in
  let len a = num (Array.length a) in
  let join az = Array.fold_left (fun a b -> a ^ b) "" az in
  let fold_slots slots =
    "t"
    ^ (len slots)
    ^ (join slots)
  in
  let fold_rec entries =
    "r"
    ^ (len entries)
    ^ (Array.fold_left
         (fun str (ident, s) -> str ^ "$" ^ ident ^ "$" ^ s)
         "" entries)
  in
  let fold_tags tags =
    "g"
    ^ (num (Hashtbl.length tags))
    ^ (Array.fold_left
         (fun str key -> str ^ (string_of_name key) ^ (Hashtbl.find tags key))
         "" (sorted_htab_keys tags))
  in
  let fold_iso (n, tags) =
    "G"
    ^ (num n)
    ^ (len tags)
    ^ (join tags)
  in
  let fold_mach m =
    match m with
        TY_u8 -> "U0"
      | TY_u16 -> "U1"
      | TY_u32 -> "U2"
      | TY_u64 -> "U3"
      | TY_s8 -> "S0"
      | TY_s16 -> "S1"
      | TY_s32 -> "S2"
      | TY_s64 -> "S3"
      | TY_f32 -> "F2"
      | TY_f64 -> "F3"
  in
  let fold =
     { base with
         ty_fold_slot = fold_slot;
         ty_fold_slots = fold_slots;
         ty_fold_tags = fold_tags;
         ty_fold_rec = fold_rec;
         ty_fold_any = (fun _ -> "a");
         ty_fold_nil = (fun _ -> "n");
         ty_fold_bool = (fun _ -> "b");
         ty_fold_mach = fold_mach;
         ty_fold_int = (fun _ -> "i");
         ty_fold_char = (fun _ -> "c");
         ty_fold_str = (fun _ -> "s");
         ty_fold_vec = (fun s -> "v" ^ s);
         ty_fold_iso = fold_iso;
         ty_fold_idx = (fun i -> "x" ^ (string_of_int i));
         (* FIXME: encode constrs, aux as well. *)
         ty_fold_fn = (fun ((ins,_,out),_) -> "f" ^ ins ^ out);
         ty_fold_pred = (fun (ins,_) -> "p" ^ ins);
         ty_fold_chan = (fun t -> "H" ^ t);
         ty_fold_port = (fun t -> "R" ^ t);
         (* FIXME: encode module types. *)
         ty_fold_mod = (fun _ -> "m");
         ty_fold_proc = (fun _ -> "P");
         (* FIXME: encode opaque and param numbers. *)
         ty_fold_opaque = (fun _ -> "Q");
         ty_fold_param = (fun _ -> "A");
         ty_fold_named = (fun _ -> bug () "string-encoding named type");
         ty_fold_type = (fun _ -> "T");
         (* FIXME: encode constrs as well. *)
         ty_fold_constrained = (fun (t,_)-> t) }
  in
    fold_ty fold ty
;;

let glue_str (cx:ctxt) (g:glue) : string =
  match g with
      GLUE_activate -> "glue$activate"
    | GLUE_yield -> "glue$yield"
    | GLUE_exit_main_proc -> "glue$exit_main_proc"
    | GLUE_exit_proc -> "glue$exit_proc"
    | GLUE_mark ty -> "glue$mark$" ^ (ty_str ty)
    | GLUE_drop ty -> "glue$drop$" ^ (ty_str ty)
    | GLUE_free ty -> "glue$free$" ^ (ty_str ty)
    | GLUE_copy ty -> "glue$copy$" ^ (ty_str ty)
    | GLUE_clone ty -> "glue$clone$" ^ (ty_str ty)
    | GLUE_compare ty -> "glue$compare$" ^ (ty_str ty)
    | GLUE_hash ty -> "glue$hash$" ^ (ty_str ty)
    | GLUE_write ty -> "glue$write$" ^ (ty_str ty)
    | GLUE_read ty -> "glue$read$" ^ (ty_str ty)
    | GLUE_unwind -> "glue$unwind"
    | GLUE_get_next_pc -> "glue$get_next_pc"
    | GLUE_mark_frame i -> "glue$mark_frame$" ^ (item_str cx i)
    | GLUE_drop_frame i -> "glue$drop_frame$" ^ (item_str cx i)
    | GLUE_reloc_frame i -> "glue$reloc_frame$" ^ (item_str cx i)
    | GLUE_bind_mod i -> "glue$bind_mod$" ^ (item_str cx i)
        (* 
         * FIXME: the node_id here isn't an item, it's a statement; 
         * lookup bind target and encode bound arg tuple type.
         *)
    | GLUE_fn_binding _ -> "glue$fn_binding"
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
