open Semant;;
open Common;;


let log cx = Session.log "auto"
  cx.ctxt_sess.Session.sess_log_auto
  cx.ctxt_sess.Session.sess_log_out
;;


let auto_inference_visitor
    (cx:ctxt)
    (progress:bool ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let check_ty_eq (t1:Ast.ty) (t2:Ast.ty) : unit =
    if not (t1 = t2)
    then err None "mismatched types: %s vs. %s "
      (Ast.string_of_ty t1) (Ast.string_of_ty t2)
  in
  let unify_ty (tyo:Ast.ty option) (ty:Ast.ty) : Ast.ty option =
    match tyo with
        None -> Some ty
      | Some t -> (check_ty_eq t ty; Some t)
  in
  let unify_slot (tyo:Ast.ty option) (id:node_id) (s:Ast.slot) : Ast.ty option =
    match (tyo, s.Ast.slot_ty) with
        (None, None) -> None
      | (Some t, None) ->
          log cx "setting type of slot #%d to %s" (int_of_node id) (Ast.string_of_ty t);
          Hashtbl.replace cx.ctxt_all_slots id { s with Ast.slot_ty = (Some t) };
          progress := true;
          Some t
      | (tyo, Some t) -> unify_ty tyo t
  in
  let unify_lval (tyo:Ast.ty option) (lval:Ast.lval) : Ast.ty option =
    match lval with
        Ast.LVAL_base nb ->
          let referent = Hashtbl.find cx.ctxt_lval_to_referent nb.id in
            begin
              match htab_search cx.ctxt_all_slots referent with
                  Some s -> unify_slot tyo referent s
                | None ->
                    unify_ty tyo
                      (ty_of_mod_item
                         { node = (Hashtbl.find cx.ctxt_all_items referent);
                           id = referent })
            end
      | _ -> (* FIXME: full-name unification? Oh, that'll be complex... *) None
  in
  let unify_lit (tyo:Ast.ty option) (lit:Ast.lit) : Ast.ty option =
    match lit with
      | Ast.LIT_nil -> unify_ty tyo Ast.TY_nil
      | Ast.LIT_bool _ -> unify_ty tyo Ast.TY_bool
      | Ast.LIT_mach (m, _) -> unify_ty tyo (Ast.TY_mach m)
      | Ast.LIT_int _ -> unify_ty tyo Ast.TY_int
      | Ast.LIT_char _ -> unify_ty tyo Ast.TY_char
      | Ast.LIT_str _ -> unify_ty tyo Ast.TY_str
      | Ast.LIT_custom _ -> tyo
  in
  let unify_atom (tyo:Ast.ty option) (atom:Ast.atom) : Ast.ty option =
    match atom with
        Ast.ATOM_literal lit -> unify_lit tyo lit.node
      | Ast.ATOM_lval lval -> unify_lval tyo lval
  in
  let unify_expr (tyo:Ast.ty option) (expr:Ast.expr) : Ast.ty option =
    match expr with
        Ast.EXPR_binary (op, a, b) ->
          begin
            match op with
                Ast.BINOP_eq | Ast.BINOP_ne
              | Ast.BINOP_lt | Ast.BINOP_le
              | Ast.BINOP_gt | Ast.BINOP_ge ->
                  begin
                    ignore (unify_atom (unify_atom None a) b);
                    ignore (unify_atom (unify_atom None b) a);
                    unify_ty tyo Ast.TY_bool
                  end
              | _ ->
                  begin
                    ignore (unify_atom (unify_atom tyo a) b);
                    unify_atom (unify_atom tyo b) a
                  end
          end
      | Ast.EXPR_unary (_, atom) -> unify_atom tyo atom
      | Ast.EXPR_atom atom -> unify_atom tyo atom
      | _ -> err None "unhandled expression type in expr_ty"
  in
  let visit_stmt_pre (s:Ast.stmt) =
    begin
      match s.node with
          Ast.STMT_copy (lval,expr) ->
            ignore (unify_lval (unify_expr None expr) lval);
            ignore (unify_expr (unify_lval None lval) expr);
        | Ast.STMT_call (dst,fn,args) ->
            begin
              match unify_lval None fn with
                  None -> ()
                | Some (Ast.TY_fn (tsig, _)) ->
                    begin
                      ignore (unify_lval tsig.Ast.sig_output_slot.Ast.slot_ty dst);
                      let islots = tsig.Ast.sig_input_slots in
                        if Array.length islots != Array.length args
                        then err (Some s.id) "argument count mismatch";
                        for i = 0 to (Array.length islots) - 1
                        do
                          ignore (unify_atom islots.(i).Ast.slot_ty args.(i));
                        done
                    end
                | _ -> err (Some s.id) "STMT_call fn resolved to non-function type"
            end
        | Ast.STMT_spawn prog ->
            begin
              match unify_atom None prog with
                  None -> ()
                | Some (Ast.TY_prog _) -> ()
                | Some _ -> err (Some s.id) "STMT_spawn prog resolved to non-program type"
            end
        | Ast.STMT_if i ->
            ignore (unify_atom (Some Ast.TY_bool) i.Ast.if_test)
        | Ast.STMT_while w ->
            let (_, atom) = w.Ast.while_lval in
              ignore (unify_atom (Some Ast.TY_bool) atom)
        | _ -> () (* FIXME: plenty more to handle here. *)
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;


let process_crate
    (cx:ctxt)
    (items:Ast.mod_items)
    : unit =
  let auto_queue = Queue.create () in
  let enqueue_auto_slot id slot =
    match slot.Ast.slot_ty with
        None ->
          log cx "enqueueing auto slot #%d" (int_of_node id);
          Queue.add id auto_queue
      | _ -> ()
  in
  let progress = ref true in
  let auto_pass = ref 0 in
    Hashtbl.iter enqueue_auto_slot cx.ctxt_all_slots;
    while not (Queue.is_empty auto_queue) do
      if not (!progress)
      then err None "auto inference pass wedged";
      let tmpq = Queue.copy auto_queue in
        log cx "auto inference pass %d on %d remaining auto slots"
          (!auto_pass)
          (Queue.length auto_queue);
        Queue.clear auto_queue;
        progress := false;
        Walk.walk_mod_items
          (Walk.mod_item_logging_visitor
             (log cx "auto inference pass %d: %s" (!auto_pass))
             (auto_inference_visitor cx progress Walk.empty_visitor))
          items;
        Queue.iter
          (fun id -> enqueue_auto_slot id
             (Hashtbl.find cx.ctxt_all_slots id))
          tmpq;
        incr auto_pass;
    done
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
