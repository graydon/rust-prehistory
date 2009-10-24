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
    log cx "collected native item #%d (%s)" (int_of_node i.id) n;
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
      log cx "collected item #%d (%s)" (int_of_node i.id) n;
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

let type_resolving_visitor
    (cx:ctxt)
    (scopes:scope Stack.t)
    (inner:Walk.visitor)
    : Walk.visitor =

  let lookup_type_by_ident ident =
    let res = lookup cx scopes (Ast.KEY_ident ident) in
      match res with
          None -> err None "identifier '%s' does not resolve to a type" ident
        | Some (scope, id) ->
            begin
              if Hashtbl.mem cx.ctxt_all_items id
              then
                begin
                  match Hashtbl.find cx.ctxt_all_items id with
                      Ast.MOD_ITEM_opaque_type td -> td.Ast.decl_item
                    | Ast.MOD_ITEM_public_type td -> td.Ast.decl_item
                    | _ -> err None "identifier '%s' resolves to non-type" ident
                end
              else err None "identifier '%s' resolves to a non-type" ident
            end
  in

  let rec resolve_slot (slot:Ast.slot) : Ast.slot =
    { slot with
        Ast.slot_ty = (match slot.Ast.slot_ty with
                           None -> None
                         | Some t -> Some (resolve_ty t)) }

  and resolve_ty_sig (tsig:Ast.ty_sig) : Ast.ty_sig =
    { Ast.sig_input_slots = Array.map resolve_slot tsig.Ast.sig_input_slots;
      Ast.sig_input_constrs = tsig.Ast.sig_input_constrs;
      Ast.sig_output_slot = resolve_slot tsig.Ast.sig_output_slot }

  and resolve_ty_fn (f:Ast.ty_fn) : Ast.ty_fn =
    let (tsig,taux) = f in
      (resolve_ty_sig tsig, taux)

  and resolve_mod_type_item (ident:Ast.ident) (item:Ast.mod_type_item)
      : (Ast.ident * Ast.mod_type_item) =
    log cx "resolving mod type item %s" ident;
    let decl params item =
      { Ast.decl_params = params;
        Ast.decl_item = item }
    in
      log cx "pushing mod type item %s" ident;
      Stack.push (SCOPE_mod_type_item item) scopes;
      let item' =
        match item.node with
            Ast.MOD_TYPE_ITEM_opaque_type td ->
              Ast.MOD_TYPE_ITEM_opaque_type td
          | Ast.MOD_TYPE_ITEM_public_type td ->
              Ast.MOD_TYPE_ITEM_public_type
                (decl
                   td.Ast.decl_params
                   (resolve_ty td.Ast.decl_item))
          | Ast.MOD_TYPE_ITEM_pred pd ->
              let (slots, constrs) = pd.Ast.decl_item in
                Ast.MOD_TYPE_ITEM_pred
                  (decl pd.Ast.decl_params
                     ((Array.map resolve_slot slots), constrs))
          | Ast.MOD_TYPE_ITEM_mod md ->
              Ast.MOD_TYPE_ITEM_mod
                (decl md.Ast.decl_params
                   (resolve_mod_type_items md.Ast.decl_item))
          | Ast.MOD_TYPE_ITEM_fn fd ->
              Ast.MOD_TYPE_ITEM_fn
                (decl fd.Ast.decl_params
                   (resolve_ty_fn fd.Ast.decl_item))
          | Ast.MOD_TYPE_ITEM_prog pd ->
              Ast.MOD_TYPE_ITEM_prog
                (decl pd.Ast.decl_params
                   (resolve_ty_sig pd.Ast.decl_item))
      in
        log cx "popping mod type item %s" ident;
        ignore (Stack.pop scopes);
        (ident, {item with node=item'})


  and resolve_mod_type_items (mtis:Ast.mod_type_items) : Ast.mod_type_items =
    (htab_map mtis resolve_mod_type_item)

  and resolve_ty_tup ttup = Array.map resolve_slot ttup

  and resolve_ty (t:Ast.ty) : Ast.ty =
    match t with
        Ast.TY_any | Ast.TY_nil | Ast.TY_bool | Ast.TY_mach _
      | Ast.TY_int | Ast.TY_char | Ast.TY_str | Ast.TY_type
      | Ast.TY_idx _ | Ast.TY_opaque _ | Ast.TY_proc -> t

      | Ast.TY_tup ttup -> Ast.TY_tup (resolve_ty_tup ttup)
      | Ast.TY_rec trec -> Ast.TY_rec (Array.map (fun (n, s) -> (n, resolve_slot s)) trec)

      | Ast.TY_tag ttag -> Ast.TY_tag (htab_map ttag (fun i ttup -> (i, resolve_ty_tup ttup)))
      | Ast.TY_iso tiso ->
          Ast.TY_iso
            { tiso with
                Ast.iso_group =
                Array.map (fun ttag -> htab_map ttag
                             (fun i ttup -> (i, resolve_ty_tup ttup)))
                  tiso.Ast.iso_group }

      | Ast.TY_vec ty -> Ast.TY_vec (resolve_ty ty)
      | Ast.TY_chan ty -> Ast.TY_chan (resolve_ty ty)
      | Ast.TY_port ty -> Ast.TY_port (resolve_ty ty)

      | Ast.TY_constrained (ty, constrs) ->
          Ast.TY_constrained ((resolve_ty ty),constrs)

      | Ast.TY_fn tfn -> Ast.TY_fn (resolve_ty_fn tfn)
      | Ast.TY_pred tp ->
          let (slots, constrs) = tp in
            Ast.TY_pred ((Array.map resolve_slot slots), constrs)
      | Ast.TY_prog tprog -> Ast.TY_prog (resolve_ty_sig tprog)
      | Ast.TY_mod mtis -> Ast.TY_mod (resolve_mod_type_items mtis)
      | Ast.TY_named (Ast.NAME_base (Ast.BASE_ident ident)) ->
          resolve_ty (lookup_type_by_ident ident)
      | Ast.TY_named _ -> err None "unhandled form of type name"
  in

  let resolve_slot_identified (s:Ast.slot identified) : (Ast.slot identified) =
    try
      { s with node = resolve_slot s.node }
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
        let ty = resolve_ty (ty_of_mod_item true item) in
          htab_put cx.ctxt_all_item_types item.id ty
      with
          Semant_err (None, e) -> raise (Semant_err ((Some item.id), e))
    end;
    inner.Walk.visit_mod_item_pre id params item
  in

  let visit_native_mod_item_pre id item =
    begin
      match item.node with
          (* FIXME: this is incomplete. *)
          Ast.NATIVE_fn tsig ->
            begin
              let taux = { Ast.fn_pure = false;
                           Ast.fn_proto = None }
              in
                try
                  let ty = resolve_ty (Ast.TY_fn (tsig, taux)) in
                    htab_put cx.ctxt_all_item_types item.id ty
              with
                  Semant_err (None, e) -> raise (Semant_err ((Some item.id), e))
            end
        | _ -> ()
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
    (scopes:scope Stack.t)
    (inner:Walk.visitor)
    : Walk.visitor =
  let lookup_slot_by_ident id ident =
    log cx "looking up slot or item with ident '%s'" ident;
    match lookup cx scopes (Ast.KEY_ident ident)with
        None -> err (Some id) "unresolved identifier '%s'" ident
      | Some (scope, id) -> (log cx "resolved to node id #%d" (int_of_node id); id)
  in
  let lookup_slot_by_temp id temp =
    log cx "looking up temp slot #%d" (int_of_temp temp);
    let res = lookup cx scopes (Ast.KEY_temp temp) in
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
  let (scopes:scope Stack.t) = Stack.create () in
  let passes =
    [|
      (block_scope_forming_visitor cx
         (stmt_collecting_visitor cx
            (all_item_collecting_visitor cx
               Walk.empty_visitor)));
      (scope_stack_managing_visitor scopes
         (type_resolving_visitor cx scopes
            (lval_base_resolving_visitor cx scopes
               Walk.empty_visitor)));
    |]
  in
    run_passes cx passes (log cx "%s") crate
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
