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
 *   - iso groups are always rooted at ty_tag nodes
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

let iso_grouping_visitor
    (cx:ctxt)
    (scopes:(scope list) ref)
    (node_to_group:(node_id,iso_group) Hashtbl.t)
    (group_joins:(iso_group,(iso_group option)) Hashtbl.t)
    (inner:Walk.visitor)
    : Walk.visitor =

  let iso_group_of (n:node_id) : iso_group =
    match htab_search node_to_group n with
        Some g -> g
      | None ->
          let group = IsoGroup (Hashtbl.length node_to_group) in
            Hashtbl.add node_to_group n group;
            group
  in

  let rec join_groups (i:iso_group) (j:iso_group) : unit =
    if (int_of_iso_group j) < (int_of_iso_group i)
    then join_groups j i
    else
      begin
        let i_rep = get_representative group_joins i in
        let j_rep = get_representative group_joins j in
          if i_rep != j_rep
          then Hashtbl.replace group_joins j_rep (Some i_rep)
      end
  in

  let rec group_slot
      (recur:recur_info)
      (scopes:scope list)
      (slot:Ast.slot)
      : unit =
    match slot.Ast.slot_ty with
        Some t -> group_ty recur scopes t
      | None -> ()

  and group_ty_sig
      (recur:recur_info)
      (scopes:scope list)
      (tsig:Ast.ty_sig)
      : unit =
    Array.iter (group_slot recur scopes) tsig.Ast.sig_input_slots;
    group_slot recur scopes tsig.Ast.sig_output_slot

  and group_ty_fn
      (recur:recur_info)
      (scopes:scope list)
      (f:Ast.ty_fn)
      : unit =
    let (tsig,_) = f in
      group_ty_sig recur scopes tsig

  and group_mod_type_item
      (recur:recur_info)
      (scopes:scope list)
      (ident:Ast.ident)
      (item:Ast.mod_type_item)
      : unit =
    let scopes = (SCOPE_mod_type_item item) :: scopes in
      match item.node with
          Ast.MOD_TYPE_ITEM_opaque_type _ -> ()
        | Ast.MOD_TYPE_ITEM_public_type td ->
            (group_ty recur scopes td.Ast.decl_item)
        | Ast.MOD_TYPE_ITEM_pred pd ->
            let (slots, _) = pd.Ast.decl_item in
              Array.iter (group_slot recur scopes) slots
        | Ast.MOD_TYPE_ITEM_mod md ->
            group_mod_type_items recur scopes md.Ast.decl_item
        | Ast.MOD_TYPE_ITEM_fn fd ->
            group_ty_fn recur scopes fd.Ast.decl_item
        | Ast.MOD_TYPE_ITEM_prog pd ->
            group_ty_sig recur scopes pd.Ast.decl_item

  and group_mod_type_items
      (recur:recur_info)
      (scopes:scope list)
      (mtis:Ast.mod_type_items)
      : unit =
    Hashtbl.iter (group_mod_type_item recur scopes) mtis

  and group_ty_tup
      (recur:recur_info)
      (scopes:scope list)
      (ttup:Ast.ty_tup)
      : unit =
    Array.iter (group_slot recur scopes) ttup

  and group_ty
      (recur:recur_info)
      (scopes:scope list)
      (t:Ast.ty)
      : unit =
    match t with
        Ast.TY_any | Ast.TY_nil | Ast.TY_bool | Ast.TY_mach _
      | Ast.TY_int | Ast.TY_char | Ast.TY_str | Ast.TY_type
      | Ast.TY_idx _ | Ast.TY_opaque _ | Ast.TY_proc -> ()

      | Ast.TY_tup ttup ->
          group_ty_tup recur scopes ttup

      | Ast.TY_rec trec ->
          Array.iter
            (fun (_, s) -> group_slot recur scopes s)
            trec

      | Ast.TY_tag ttag ->
          Hashtbl.iter
            (fun _ ttup -> group_ty_tup recur scopes ttup)
            ttag

      | Ast.TY_iso tiso ->
          Array.iter
            begin
              fun ttag -> Hashtbl.iter
                (fun _ ttup -> group_ty_tup recur scopes ttup)
                ttag
            end
            tiso.Ast.iso_group

      | Ast.TY_vec slot -> group_slot recur scopes slot
      | Ast.TY_chan ty -> group_ty recur scopes ty
      | Ast.TY_port ty -> group_ty recur scopes ty
      | Ast.TY_constrained (ty, _) -> group_ty recur scopes ty
      | Ast.TY_fn tfn -> group_ty_fn recur scopes tfn
      | Ast.TY_pred tp ->
          let (slots, _) = tp in
            Array.iter (group_slot recur scopes) slots
      | Ast.TY_prog tprog -> group_ty_sig recur scopes tprog
      | Ast.TY_mod mtis -> group_mod_type_items recur scopes mtis
      | Ast.TY_named (Ast.NAME_base (Ast.BASE_ident ident)) ->
          let (scopes, id, ty) = lookup_type_by_ident cx scopes ident in
            if List.mem id recur.recur_all_nodes
            then () (* join_groups group other_group *)
            else group_ty (push_node recur id) scopes ty
      | Ast.TY_named _ -> err None "unhandled form of type name"
  in

  let visit_mod_item_pre id params item =
    begin
      match item.node with
          Ast.MOD_ITEM_public_type td
        | Ast.MOD_ITEM_opaque_type td
          ->
            begin
              let ty = td.Ast.decl_item in
              (* let group = iso_group_of item.id in *)
                group_ty (push_node empty_recur_info item.id) (!scopes) ty;
            end
        | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre id params item
  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre }
;;


let iso_group_rewriting_visitor
    (cx:ctxt)
    (node_to_group:(node_id,iso_group) Hashtbl.t)
    (group_joins:(iso_group,(iso_group option)) Hashtbl.t)
    (group_to_nodes:(iso_group, (node_id,unit) Hashtbl.t) Hashtbl.t)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_mod_item_pre id params item =
    begin
      match item.node with
          Ast.MOD_ITEM_public_type td
        | Ast.MOD_ITEM_opaque_type td
          -> ()
            (*
            begin
              let group = Hashtbl.find node_to_group item.id in
              let rep = get_representative group_joins group in
              let ty = td.Ast.decl_item in
                Hashtbl.replace node_to_group item.id rep;
                log cx "type %s placed in iso-recursion group %d"
                  id (int_of_iso_group rep);
                match ty with
                    Ast.TY_tag ttag ->
                      begin
                        let group_table =
                          match htab_search group_to_nodes rep with
                              None ->
                                let tab = Hashtbl.create 0 in
                                  Hashtbl.add group_to_nodes rep tab;
                                  tab
                            | Some t -> t
                        in
                          Hashtbl.replace group_table item.id ()
                      end
                  | _ -> ()
            end *)
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


let tag_collecting_visitor
    (cx:ctxt)
    (node_to_group:(node_id,iso_group) Hashtbl.t)
    (group_to_nodes:(iso_group, (node_id,unit) Hashtbl.t) Hashtbl.t)
    (tag_node_to_idx:(node_id,int) Hashtbl.t)
    (inner:Walk.visitor)
    : Walk.visitor =

  let note_tag_type nid ty =
    match ty with
        Ast.TY_tag tt ->
          log cx "collected tag type %s = node #%d" (Ast.fmt_to_str Ast.fmt_tag tt) (int_of_node nid);
          let group_array = group_array_of cx node_to_group group_to_nodes nid in
          let n = ref (-1) in
            for i = 0 to (Array.length group_array) - 1
            do
              if group_array.(i) = nid
              then n := i
              else ()
            done;
            assert ((!n) != -1);
            log cx "tag type %a (node #%d) is idx#%d in iso group #%d"
              Ast.sprintf_tag tt
              (int_of_node nid) (!n) (int_of_iso_group (Hashtbl.find node_to_group nid));
            Hashtbl.replace tag_node_to_idx nid (!n)
      | _ -> ()
  in

  let visit_mod_item_pre id params item =
    begin
      match item.node with
          Ast.MOD_ITEM_opaque_type td
        | Ast.MOD_ITEM_public_type td ->
            note_tag_type item.id td.Ast.decl_item
        | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre id params item
  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre }
;;


let type_resolving_visitor
    (cx:ctxt)
    (scopes:(scope list) ref)
    (node_to_group:(node_id,iso_group) Hashtbl.t)
    (group_to_nodes:(iso_group, (node_id,unit) Hashtbl.t) Hashtbl.t)
    (tag_node_to_idx:(node_id,int) Hashtbl.t)
    (inner:Walk.visitor)
    : Walk.visitor =

  let rec resolve_slot
      (recur:recur_info)
      (scopes:scope list)
      (slot:Ast.slot)
      : Ast.slot =
    { slot with
        Ast.slot_ty = (match slot.Ast.slot_ty with
                           None -> None
                         | Some t -> Some (resolve_ty recur scopes t)) }

  and resolve_ty_sig
      (recur:recur_info)
      (scopes:scope list)
      (tsig:Ast.ty_sig)
      : Ast.ty_sig =
    { Ast.sig_input_slots = Array.map (resolve_slot recur scopes) tsig.Ast.sig_input_slots;
      Ast.sig_input_constrs = tsig.Ast.sig_input_constrs;
      Ast.sig_output_slot = resolve_slot recur scopes tsig.Ast.sig_output_slot }

  and resolve_ty_fn
      (recur:recur_info)
      (scopes:scope list)
      (f:Ast.ty_fn) : Ast.ty_fn =
    let (tsig,taux) = f in
      (resolve_ty_sig recur scopes tsig, taux)

  and resolve_mod_type_item
      (recur:recur_info)
      (scopes:scope list)
      (ident:Ast.ident)
      (item:Ast.mod_type_item)
      : (Ast.ident * Ast.mod_type_item) =
    log cx "resolving mod type item %s" ident;
    let decl params item =
      { Ast.decl_params = params;
        Ast.decl_item = item }
    in
    let scopes = (SCOPE_mod_type_item item) :: scopes in
      let item' =
        match item.node with
            Ast.MOD_TYPE_ITEM_opaque_type td ->
              Ast.MOD_TYPE_ITEM_opaque_type td
          | Ast.MOD_TYPE_ITEM_public_type td ->
              Ast.MOD_TYPE_ITEM_public_type
                (decl
                   td.Ast.decl_params
                   (resolve_ty recur scopes td.Ast.decl_item))
          | Ast.MOD_TYPE_ITEM_pred pd ->
              let (slots, constrs) = pd.Ast.decl_item in
                Ast.MOD_TYPE_ITEM_pred
                  (decl pd.Ast.decl_params
                     ((Array.map (resolve_slot recur scopes) slots), constrs))
          | Ast.MOD_TYPE_ITEM_mod md ->
              Ast.MOD_TYPE_ITEM_mod
                (decl md.Ast.decl_params
                   (resolve_mod_type_items recur scopes md.Ast.decl_item))
          | Ast.MOD_TYPE_ITEM_fn fd ->
              Ast.MOD_TYPE_ITEM_fn
                (decl fd.Ast.decl_params
                   (resolve_ty_fn recur scopes fd.Ast.decl_item))
          | Ast.MOD_TYPE_ITEM_prog pd ->
              Ast.MOD_TYPE_ITEM_prog
                (decl pd.Ast.decl_params
                   (resolve_ty_sig recur scopes pd.Ast.decl_item))
      in
        (ident, {item with node=item'})


  and resolve_mod_type_items
      (recur:recur_info)
      (scopes:scope list)
      (mtis:Ast.mod_type_items)
      : Ast.mod_type_items =
    (htab_map mtis (resolve_mod_type_item recur scopes))

  and resolve_ty_tup
      (recur:recur_info)
      (scopes:scope list)
      (ttup:Ast.ty_tup)
      : Ast.ty_tup =
    Array.map (resolve_slot recur scopes) ttup

  and resolve_ttag
      (recur:recur_info)
      (scopes:scope list)
      (ttag:Ast.ty_tag)
      : Ast.ty_tag =
    htab_map ttag
      (fun i ttup -> (i,resolve_ty_tup recur scopes ttup))

  and resolve_ty
      (recur:recur_info)
      (scopes:scope list)
      (t:Ast.ty)
      : Ast.ty =
    match t with
        Ast.TY_any | Ast.TY_nil | Ast.TY_bool | Ast.TY_mach _
      | Ast.TY_int | Ast.TY_char | Ast.TY_str | Ast.TY_type
      | Ast.TY_idx _ | Ast.TY_opaque _ | Ast.TY_proc -> t

      | Ast.TY_tup ttup ->
          Ast.TY_tup (resolve_ty_tup recur scopes ttup)

      | Ast.TY_rec trec ->
          Ast.TY_rec
            (Array.map
               (fun (n, s) -> (n, resolve_slot recur scopes s))
               trec)

      | Ast.TY_tag ttag ->
          (* If we get here we're *not* in an iso group. *)
          Ast.TY_tag (resolve_ttag recur scopes ttag)

      | Ast.TY_iso tiso ->
          Ast.TY_iso
            { tiso with
                Ast.iso_group =
                Array.map (fun ttag -> (resolve_ttag recur scopes ttag))
                  tiso.Ast.iso_group }

      | Ast.TY_vec slot -> Ast.TY_vec (resolve_slot recur scopes slot)
      | Ast.TY_chan ty -> Ast.TY_chan (resolve_ty recur scopes ty)
      | Ast.TY_port ty -> Ast.TY_port (resolve_ty recur scopes ty)

      | Ast.TY_constrained (ty, constrs) ->
          Ast.TY_constrained ((resolve_ty recur scopes ty),constrs)

      | Ast.TY_fn tfn -> Ast.TY_fn (resolve_ty_fn recur scopes tfn)
      | Ast.TY_pred tp ->
          let (slots, constrs) = tp in
            Ast.TY_pred ((Array.map (resolve_slot recur scopes) slots), constrs)
      | Ast.TY_prog tprog -> Ast.TY_prog (resolve_ty_sig recur scopes tprog)
      | Ast.TY_mod mtis -> Ast.TY_mod (resolve_mod_type_items recur scopes mtis)
      | Ast.TY_named (Ast.NAME_base (Ast.BASE_ident ident)) ->
          let (scopes, id, ty) = lookup_type_by_ident cx scopes ident in
            (* We have to decide whether to replace a name with an idx or its expansion. 
             * An idx is appropriate iff:
             * 
             *   - the id we've resolved is in an iso group
             *   - we're currently in an iso group
             *   - those two iso groups are the same
             * 
             * Along the way we're going to decide whether we're in an infinite-recursion
             * case or not.
             *   
             *)
            begin
              match htab_search node_to_group id with
                  None ->
                    if List.mem id recur.recur_all_nodes
                    then (err (Some id) "Infinite type recursion on type node #%d, %a"
                            (int_of_node id) Ast.sprintf_ty ty)
                    else
                      resolve_ty (push_node recur id) scopes ty
                | Some other_iso ->
                    begin
                      match recur.recur_curr_iso with
                          Some curr_iso when curr_iso = other_iso ->
                            let idx = Hashtbl.find tag_node_to_idx id in
                              Ast.TY_idx idx
                        | _  ->
                            resolve_ty (set_iso recur other_iso) scopes ty
                    end
            end
      | Ast.TY_named _ -> err None "unhandled form of type name"
  in

  let resolve_slot_identified (s:Ast.slot identified) : (Ast.slot identified) =
    try
      { s with node = resolve_slot empty_recur_info (!scopes) s.node }
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

  let iso_visit_mod_item_pre id params item =
    begin
      try
        match item.node with
            Ast.MOD_ITEM_public_type td
          | Ast.MOD_ITEM_opaque_type td ->
              begin
                let ty = td.Ast.decl_item in
                let group_array = group_array_of cx node_to_group group_to_nodes item.id in
                  ()
              end
          | _ -> ()
      with
          Semant_err (None, e) -> raise (Semant_err ((Some item.id), e))
    end;
    inner.Walk.visit_mod_item_pre id params item
  in


  let visit_mod_item_pre id params item =
    begin
      try
        let ty = match (item.node, (ty_of_mod_item true item)) with
            (Ast.MOD_ITEM_tag {Ast.decl_item=(_, ttag, nid)},
             Ast.TY_fn (tsig, taux)) ->
              let r = empty_recur_info in
              let input_slots = Array.map (resolve_slot r (!scopes)) tsig.Ast.sig_input_slots in
              let output_slot = resolve_slot r (!scopes) tsig.Ast.sig_output_slot in
                Ast.TY_fn ({tsig with
                              Ast.sig_input_slots = input_slots;
                              Ast.sig_output_slot = output_slot }, taux)
          | (_, t) ->
              resolve_ty empty_recur_info (!scopes) t
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
      let ty = resolve_ty empty_recur_info (!scopes) (ty_of_native_mod_item item) in
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

  let node_to_group = Hashtbl.create 0 in
  let group_joins = Hashtbl.create 0 in
  let group_to_nodes = Hashtbl.create 0 in

  let tag_to_idx = Hashtbl.create 0 in

  let passes =
    [|
      (block_scope_forming_visitor cx
         (stmt_collecting_visitor cx
            (all_item_collecting_visitor cx
               Walk.empty_visitor)));
      (scope_stack_managing_visitor scopes
         (iso_grouping_visitor
            cx scopes node_to_group group_joins
            Walk.empty_visitor));
      (iso_group_rewriting_visitor cx
         node_to_group group_joins group_to_nodes
         Walk.empty_visitor);
      (tag_collecting_visitor cx
         node_to_group group_to_nodes tag_to_idx
         Walk.empty_visitor);
      (scope_stack_managing_visitor scopes
         (type_resolving_visitor cx scopes
            node_to_group group_to_nodes tag_to_idx
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
