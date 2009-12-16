open Semant;;
open Common;;

(*
 * Resolution passes:
 *
 *   - build multiple 'scope' hashtables mapping slot_key -> node_id
 *   - build single 'type inference' hashtable mapping node_id -> slot
 *
 *   (note: not every slot is identified; only those that are declared
 *    in statements and/or can participate in local type inference.
 *    Those in function signatures are not, f.e. Also no type values
 *    are identified, though module items are. )
 *
 *)


let log cx = Session.log "resolve"
  cx.ctxt_sess.Session.sess_log_resolve
  cx.ctxt_sess.Session.sess_log_out
;;


let block_scope_forming_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_block_pre b =
    if not (Hashtbl.mem cx.ctxt_block_items b.id)
    then htab_put cx.ctxt_block_items b.id (Hashtbl.create 0);
    if not (Hashtbl.mem cx.ctxt_block_slots b.id)
    then htab_put cx.ctxt_block_slots b.id (Hashtbl.create 0);
    inner.Walk.visit_block_pre b
  in
    { inner with Walk.visit_block_pre = visit_block_pre }
;;


let stmt_collecting_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  let block_ids = Stack.create () in
  let visit_block_pre (b:Ast.block) =
    Stack.push b.id block_ids;
    inner.Walk.visit_block_pre b
  in
  let visit_block_post (b:Ast.block) =
    inner.Walk.visit_block_post b;
    ignore (Stack.pop block_ids)
  in
  let visit_stmt_pre stmt =
    begin
      htab_put cx.ctxt_all_stmts stmt.id stmt;
      match stmt.node with
          Ast.STMT_decl d ->
            begin
              let bid = Stack.top block_ids in
              let items = Hashtbl.find cx.ctxt_block_items bid in
              let slots = Hashtbl.find cx.ctxt_block_slots bid in
              let check_and_log_ident id ident =
                if Hashtbl.mem items ident ||
                  Hashtbl.mem slots (Ast.KEY_ident ident)
                then
                  err (Some id)
                    "duplicate declaration '%s' in block" ident
                else
                  log cx "found decl of '%s' in block" ident
              in
              let check_and_log_tmp id tmp =
                if Hashtbl.mem slots (Ast.KEY_temp tmp)
                then
                  err (Some id)
                    "duplicate declaration of temp #%d in block" (int_of_temp tmp)
                else
                  log cx "found decl of temp #%d in block" (int_of_temp tmp)
              in
              let check_and_log_key id key =
                match key with
                    Ast.KEY_ident i -> check_and_log_ident id i
                  | Ast.KEY_temp t -> check_and_log_tmp id t
              in
                match d with
                    Ast.DECL_mod_item (ident, item) ->
                      check_and_log_ident item.id ident;
                      htab_put items ident item.id
                  | Ast.DECL_slot (key, sid) ->
                      check_and_log_key sid.id key;
                      htab_put slots key sid.id;
                      htab_put cx.ctxt_slot_owner sid.id bid;
                      htab_put cx.ctxt_slot_keys sid.id key
            end
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre stmt
  in
    { inner with
        Walk.visit_block_pre = visit_block_pre;
        Walk.visit_block_post = visit_block_post;
        Walk.visit_stmt_pre = visit_stmt_pre }
;;

let all_item_collecting_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =

  let visit_native_mod_item_pre n i =
    htab_put cx.ctxt_all_native_items i.id i.node;
    htab_put cx.ctxt_item_names i.id n;
    log cx "collected native item #%d: %s" (int_of_node i.id) n;
    inner.Walk.visit_native_mod_item_pre n i
  in

  let visit_mod_item_pre n p i =
    let owned owner sloti =
      htab_put cx.ctxt_slot_owner sloti owner
    in
    let note_header owner =
      Array.iter
        (fun (sloti,ident) ->
           owned owner sloti.id;
           htab_put cx.ctxt_slot_keys sloti.id (Ast.KEY_ident ident))
    in
      htab_put cx.ctxt_all_items i.id i.node;
      htab_put cx.ctxt_item_names i.id n;
      log cx "collected item #%d: %s" (int_of_node i.id) n;
      begin
        (* FIXME: this is incomplete. *)
        match i.node with
            Ast.MOD_ITEM_fn fd ->
              begin
                note_header i.id fd.Ast.decl_item.Ast.fn_input_slots;
                owned i.id fd.Ast.decl_item.Ast.fn_output_slot.id
              end
          | Ast.MOD_ITEM_pred pd ->
              begin
                note_header i.id pd.Ast.decl_item.Ast.pred_input_slots
              end
          | Ast.MOD_ITEM_prog pd ->
              begin
                Hashtbl.iter (fun _ sloti -> owned i.id sloti.id)
                  pd.Ast.decl_item.Ast.prog_slots;
                match pd.Ast.decl_item.Ast.prog_init with
                    None -> ()
                  | Some init ->
                      begin
                        note_header init.id init.node.Ast.init_input_slots;
                        owned init.id init.node.Ast.init_proc_input.id;
                        owned init.id init.node.Ast.init_output_slot.id
                      end
              end
          | _ -> ()
      end;
      inner.Walk.visit_mod_item_pre n p i
  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_native_mod_item_pre = visit_native_mod_item_pre;
    }
;;


type iso_group = IsoGroup of int;;
let int_of_iso_group ig =
  match ig with
      IsoGroup i -> i
;;


let rec get_representative
    (joins:(iso_group,(iso_group option)) Hashtbl.t)
    (i:iso_group)
    : iso_group =
  match htab_search joins i with
      None -> (htab_put joins i None; i)
    | Some None -> i
    | Some (Some other) -> get_representative joins other
;;

type recur_info =
    { recur_all_nodes: node_id list;
      recur_curr_iso: iso_group option; }
;;

let empty_recur_info =
  { recur_all_nodes = [];
    recur_curr_iso = None }
;;

let push_node r n =
  { r with recur_all_nodes = n :: r.recur_all_nodes }
;;

let set_iso r i =
  { r with recur_curr_iso = Some i }
;;


(*
 * iso-recursion groups are very complicated.
 * 
 *   - iso groups are always rooted at *named* ty_tag nodes
 * 
 *   - consider: 
 * 
 *    type colour = tag(red, green, blue);
 *    type list = tag(cons(colour, @list), nil())
 * 
 *    this should include list as an iso but not colour,
 *    should result in:
 * 
 *    type list = iso[<0>:tag(cons(tag(red,green,blue),@#1))]
 * 
 *   - consider:
 * 
 *    type colour = tag(red, green, blue);
 *    type tree = tag(children(@list), leaf(colour))
 *    type list = tag(cons(@tree, @list), nil())
 * 
 *    this should result in:
 * 
 *    type list = iso[<0>:tag(cons(@#2, @#1),nil());
 *                    1: tag(children(@#1),leaf(tag(red,green,blue)))]
 * 
 *  - how can you calculate these?
 * 
 *    - start by making a map from named-tag-node-id -> referenced-other-nodes
 *    - for each member in the set, if you can get from itself to itself, keep it,
 *      otherwise it's non-recursive => non-interesting, delete it.
 *    - group the members (now all recursive) by dependency
 *    - assign index-number to each elt of group
 *    - fully resolve each elt of group, turning names into numbers or chasing through to
 *      fully-resolving targets as necessary
 *    - place group in iso, store differently-indexed value in table for each
 * 
 * 
 *  - what are the illegal forms?
 *    - recursion that takes indefinite storage to form a tag, eg.
 * 
 *      type t = tag(foo(t));
 *
 *    - recursion that makes a tag unconstructable, eg:
 * 
 *      type t = tag(foo(@t));
 *)

let get_ty_idxs (h:(int,unit)Hashtbl.t) (t:Ast.ty) : unit =
  let base = ty_fold_default () in
  let fold = { base with
                 ty_fold_idx = (fun i -> Hashtbl.replace h i ()) }
  in
    fold_ty fold t
;;


let lookup_type_by_ident
    (cx:ctxt)
    (scopes:scope list)
    (ident:Ast.ident)
    : ((scope list) * node_id * Ast.ty) =
  let res = lookup cx scopes (Ast.KEY_ident ident) in
    match res with
        None -> err None "identifier '%s' does not resolve to a type" ident
      | Some (scopes, id) ->
          begin
            if Hashtbl.mem cx.ctxt_all_items id
            then
              begin
                let ty =
                  match Hashtbl.find cx.ctxt_all_items id with
                      Ast.MOD_ITEM_opaque_type td -> td.Ast.decl_item
                    | Ast.MOD_ITEM_public_type td -> td.Ast.decl_item
                    | _ -> err None "identifier '%s' resolves to non-type" ident
                in
                  (scopes, id, ty)
              end
            else err None "identifier '%s' resolves to a non-type" ident
          end
;;


let lookup_type_by_name
    (cx:ctxt)
    (scopes:scope list)
    (name:Ast.name)
    : ((scope list) * node_id * Ast.ty) =
    match name with
        (Ast.NAME_base (Ast.BASE_ident ident)) ->
          lookup_type_by_ident cx scopes ident
      | _ -> err None "unhandled form of name in Resolve.lookup_type_by_name"
;;


let get_ty_references
    (t:Ast.ty)
    (cx:ctxt)
    (scopes:scope list)
    : node_id list =
  let base = ty_fold_list_concat () in
  let ty_fold_named n =
    let (_, node, _) = lookup_type_by_name cx scopes n in
      [ node ]
  in
  let fold = { base with ty_fold_named = ty_fold_named } in
    fold_ty fold t
;;


let tag_reference_extracting_visitor
    (cx:ctxt)
    (scopes:(scope list) ref)
    (tag_node_to_referenced_nodes:(node_id,node_id list) Hashtbl.t)
    (inner:Walk.visitor)
    : Walk.visitor =

  let visit_mod_item_pre id params item =
    begin
      match item.node with
          Ast.MOD_ITEM_public_type td
        | Ast.MOD_ITEM_opaque_type td
          ->
            begin
              let ty = td.Ast.decl_item in
                match ty with
                    Ast.TY_tag _ ->
                      log cx "extracting references for tag node %d" (int_of_node item.id);
                      let referenced = get_ty_references ty cx (!scopes) in
                        List.iter (fun i -> log cx "tag node %d references sub-tag %d"
                                     (int_of_node item.id) (int_of_node i)) referenced;
                        htab_put tag_node_to_referenced_nodes item.id referenced
                  | _ -> ()
            end
        | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre id params item
  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre }
;;


let group_array_of
    (cx:ctxt)
    (node_to_group:(node_id,iso_group) Hashtbl.t)
    (group_to_nodes:(iso_group, (node_id,unit) Hashtbl.t) Hashtbl.t)
    (n:node_id)
    : node_id array =
  let group = Hashtbl.find node_to_group n in
  let group_table = Hashtbl.find group_to_nodes group in
  let group_array = Array.of_list (htab_keys group_table) in
  let compare_types a_id b_id =
    let a_ty = Hashtbl.find cx.ctxt_all_items a_id in
    let b_ty = Hashtbl.find cx.ctxt_all_items b_id in
      compare a_ty b_ty
  in
    Array.sort compare_types group_array;
    group_array
;;


let rec resolve_type
    (cx:ctxt)
    (scopes:(scope list))
    (recur:recur_info)
    (t:Ast.ty)
    : Ast.ty =
  let base = ty_fold_rebuild in
  let ty_fold_named name =
    let (scopes, node, t) = lookup_type_by_name cx scopes name in
      if List.mem node recur.recur_all_nodes
      then (err (Some node) "infinite recursive type definition: '%a'"
              Ast.sprintf_name name)
      else resolve_type cx scopes (push_node recur node) t
  in
  let fold = { base with ty_fold_named = ty_fold_named } in
    fold_ty fold t
;;


let type_resolving_visitor
    (cx:ctxt)
    (scopes:(scope list) ref)
    (tag_node_to_referenced_nodes:(node_id,node_id list) Hashtbl.t)
    (inner:Walk.visitor)
    : Walk.visitor =

  let resolve_slot_identified (s:Ast.slot identified) : (Ast.slot identified) =
    try
      match s.node.Ast.slot_ty with
          None -> s
        | Some ty ->
            let ty = resolve_type cx (!scopes) empty_recur_info ty in
            let node = { s.node with Ast.slot_ty = Some ty } in
              { s with node = node }
    with
        Semant_err (None, e) -> raise (Semant_err ((Some s.id), e))
  in

  let visit_slot_identified_pre slot =
    let slot = resolve_slot_identified slot in
      htab_put cx.ctxt_all_slots slot.id slot.node;
      log cx "collected resolved slot #%d with type %s" (int_of_node slot.id)
        (match slot.node.Ast.slot_ty with
             None -> "??"
           | Some t -> (Ast.fmt_to_str Ast.fmt_ty t));
      inner.Walk.visit_slot_identified_pre slot
  in

  let visit_mod_item_pre id params item =
    begin
      try
        let ty = match (item.node, (ty_of_mod_item true item)) with
            (*
            (Ast.MOD_ITEM_tag {Ast.decl_item=(_, ttag, nid)},
             Ast.TY_fn (tsig, taux)) ->
              let r = empty_recur_info in
              let input_slots = Array.map (resolve_slot r (!scopes)) tsig.Ast.sig_input_slots in
              let output_slot = resolve_slot r (!scopes) tsig.Ast.sig_output_slot in
                Ast.TY_fn ({tsig with
                              Ast.sig_input_slots = input_slots;
                              Ast.sig_output_slot = output_slot }, taux)
            *)
          | (_e, t) -> resolve_type cx (!scopes) empty_recur_info t
        in
          log cx "resolved item %s, type %s" id (Ast.fmt_to_str Ast.fmt_ty ty);
          htab_put cx.ctxt_all_item_types item.id ty
      with
          Semant_err (None, e) -> raise (Semant_err ((Some item.id), e))
    end;
    inner.Walk.visit_mod_item_pre id params item
  in

  let visit_native_mod_item_pre id item =
    begin
      let ty = resolve_type cx (!scopes) empty_recur_info (ty_of_native_mod_item item) in
        htab_put cx.ctxt_all_item_types item.id ty
    end;
    inner.Walk.visit_native_mod_item_pre id item
  in
    { inner with
        Walk.visit_slot_identified_pre = visit_slot_identified_pre;
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_native_mod_item_pre = visit_native_mod_item_pre }
;;


let lval_base_resolving_visitor
    (cx:ctxt)
    (scopes:(scope list) ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let lookup_slot_by_ident id ident =
    log cx "looking up slot or item with ident '%s'" ident;
    match lookup cx (!scopes) (Ast.KEY_ident ident) with
        None -> err (Some id) "unresolved identifier '%s'" ident
      | Some (scope, id) -> (log cx "resolved to node id #%d" (int_of_node id); id)
  in
  let lookup_slot_by_temp id temp =
    log cx "looking up temp slot #%d" (int_of_temp temp);
    let res = lookup cx (!scopes) (Ast.KEY_temp temp) in
      match res with
          None -> err (Some id) "unresolved temp node #%d" (int_of_temp temp)
        | Some (scope, id) -> (log cx "resolved to node id #%d" (int_of_node id); id)
  in
  let lookup_slot_by_name_base id nb =
    match nb with
        Ast.BASE_ident ident -> lookup_slot_by_ident id ident
      | Ast.BASE_temp temp -> lookup_slot_by_temp id temp
      | Ast.BASE_app _ -> err (Some id) "unhandled name base case BASE_app"
  in

  let visit_lval_pre lv =
    let rec lookup_lval lv =
      match lv with
          Ast.LVAL_ext (base, ext) ->
            begin
              lookup_lval base;
              match ext with
                  Ast.COMP_atom (Ast.ATOM_lval lv') -> lookup_lval lv'
                | _ -> ()
            end
        | Ast.LVAL_base nb ->
            let slot_id = lookup_slot_by_name_base nb.id nb.node in
              log cx "resolved lval #%d to slot #%d" (int_of_node nb.id) (int_of_node slot_id);
              htab_put cx.ctxt_lval_to_referent nb.id slot_id
    in
      lookup_lval lv;
      inner.Walk.visit_lval_pre lv
  in
    { inner with
        Walk.visit_lval_pre = visit_lval_pre }
;;


let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  let (scopes:(scope list) ref) = ref [] in
  let path = Stack.create () in

  let node_to_references = Hashtbl.create 0 in

  let passes =
    [|
      (block_scope_forming_visitor cx
         (stmt_collecting_visitor cx
            (all_item_collecting_visitor cx
               Walk.empty_visitor)));
      (scope_stack_managing_visitor scopes
         (tag_reference_extracting_visitor
            cx scopes node_to_references
            Walk.empty_visitor));
      (scope_stack_managing_visitor scopes
         (type_resolving_visitor cx scopes
            node_to_references
            (lval_base_resolving_visitor cx scopes
               Walk.empty_visitor)));
    |]
  in
    run_passes cx path passes (log cx "%s") crate
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)

