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
  let fmt_mode m =
    match m with
        Ast.MODE_interior Ast.MUTABLE -> "interior-mutable"
      | Ast.MODE_interior Ast.IMMUTABLE -> "interior-immutable"
      | Ast.MODE_exterior Ast.MUTABLE -> "exterior-mutable"
      | Ast.MODE_exterior Ast.IMMUTABLE -> "exterior-immutable"
      | Ast.MODE_read_alias -> "read-alias"
      | Ast.MODE_write_alias -> "write-alias"
  in
  let check_mode_eq (m1:Ast.mode) (m2:Ast.mode) : unit =
    if not (m1 = m2)
    then err None "mismatched mode: %s vs. %s "
      (fmt_mode m1) (fmt_mode m2)
  in
  let check_ty_eq (t1:Ast.ty) (t2:Ast.ty) : unit =
    if not (t1 = t2)
    then err None "mismatched types: %s vs. %s "
      (Ast.fmt_to_str Ast.fmt_ty t1) (Ast.fmt_to_str Ast.fmt_ty t2)
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
          log cx "setting type of slot #%d to %s" (int_of_node id) (Ast.fmt_to_str Ast.fmt_ty t);
          Hashtbl.replace cx.ctxt_all_slots id { s with Ast.slot_ty = (Some t) };
          progress := true;
          Some t
      | (tyo, Some t) -> unify_ty tyo t
  in
  let rec unify_lval (tyo:Ast.ty option) (lval:Ast.lval) : Ast.ty option =
    match lval with
        Ast.LVAL_base nb ->
          let referent = Hashtbl.find cx.ctxt_lval_to_referent nb.id in
            begin
              match htab_search cx.ctxt_all_slots referent with
                  Some s -> unify_slot tyo referent s
                | None ->
                    unify_ty tyo
                      (Hashtbl.find cx.ctxt_all_item_types referent)
            end
      | Ast.LVAL_ext (base, comp) ->
          begin
            (* We are being asked to unify the full lval with a given type,
             * which means we do not know anything in particular about its
             * prefix type -- type T could be embedded in any number of containing
             * structures -- so we unify the prefix-lval with None and only
             * extend it if we get a real type back.
             *)
            match unify_lval None base with
                None -> None
              | Some prefix_type ->
                  if lval_is_slot cx base
                  then
                    let slot = project_type_to_slot prefix_type comp in
                      slot.Ast.slot_ty
                  else
                    Some (project_mod_type_to_type prefix_type comp)
          end
  in
  let unify_lit (tyo:Ast.ty option) (lit:Ast.lit) : Ast.ty option =
    match lit with
      | Ast.LIT_nil -> unify_ty tyo Ast.TY_nil
      | Ast.LIT_bool _ -> unify_ty tyo Ast.TY_bool
      | Ast.LIT_mach (m, _, _) -> unify_ty tyo (Ast.TY_mach m)
      | Ast.LIT_int _ -> unify_ty tyo Ast.TY_int
      | Ast.LIT_char _ -> unify_ty tyo Ast.TY_char
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
  in
  let unify_header_slots id hdr args =
      if Array.length hdr != Array.length args
      then err (Some id) "argument count mismatch: %d given, %d expected"
        (Array.length args) (Array.length hdr);
      for i = 0 to (Array.length hdr) - 1
      do
        ignore (unify_atom hdr.(i).Ast.slot_ty args.(i));
      done
  in
  let unify_tsig id tsig dst args =
    ignore (unify_lval tsig.Ast.sig_output_slot.Ast.slot_ty dst);
    unify_header_slots id tsig.Ast.sig_input_slots args
  in
  let rec visit_stmt_pre (s:Ast.stmt) =
    try
      visit_stmt_pre_full s
    with
        Semant_err (None, msg) -> raise (Semant_err ((Some s.id), msg))

  and visit_stmt_pre_full (s:Ast.stmt) =
    begin
      match s.node with
          Ast.STMT_copy (lval,expr,_) ->
            ignore (unify_lval (unify_expr None expr) lval);
            ignore (unify_expr (unify_lval None lval) expr)

        | Ast.STMT_init_str (lval, _) ->
            ignore (unify_lval (Some Ast.TY_str) lval)

        | Ast.STMT_init_vec (lval, slot, atoms) ->
            let unify_init tyo =
              match tyo with
                  None ->
                    begin
                      let lim = Array.length atoms in
                      let rec step i tyo =
                        if i = lim
                        then (Some (Ast.TY_vec {slot with Ast.slot_ty=tyo}))
                        else
                          let at = atoms.(i) in
                          match unify_atom tyo at with
                              None -> None
                            | Some t ->
                                step (i+1) (Some t)
                      in
                        step 0 None
                    end
                | Some (Ast.TY_vec aslot) ->
                    for i = 0 to (Array.length atoms) - 1
                    do
                      let at = atoms.(i) in
                        ignore (unify_atom aslot.Ast.slot_ty at)
                    done;
                    tyo
                | Some _ -> err None "Non-vector type for vector initializer"
            in
              ignore (unify_lval (unify_init None) lval);
              ignore (unify_init (unify_lval None lval))

        | Ast.STMT_init_chan (chan, Some port) ->
            begin
              match unify_lval None port with
                  Some (Ast.TY_port t) ->
                    ignore (unify_lval (Some (Ast.TY_chan t)) chan)
                | Some _ -> err (Some s.id) "non-port type passed to channel"
                | None -> ()
            end

        | Ast.STMT_init_tup (lval, atoms) ->
            let unify_init tyo =
              match tyo with
                  None ->
                    begin
                      let lim = Array.length atoms in
                      let rec step i accum =
                        if i = lim
                        then (Some (Ast.TY_tup (Array.of_list (List.rev accum))))
                        else
                          let (mode, at) = atoms.(i) in
                          match unify_atom None at with
                              None -> None
                            | Some t ->
                                step (i+1) ({Ast.slot_mode=mode;
                                             Ast.slot_ty=Some t} :: accum)
                      in
                        step 0 []
                    end
                | Some (Ast.TY_tup tt) ->
                    if Array.length atoms != Array.length tt
                    then err None "mismatched tuple length"
                    else
                      for i = 0 to (Array.length atoms) - 1
                      do
                        let (mode, at) = atoms.(i) in
                        let slot = tt.(i) in
                          check_mode_eq slot.Ast.slot_mode mode;
                          ignore (unify_atom slot.Ast.slot_ty at)
                      done;
                    tyo
                | Some _ -> err None "Non-tuple type for tuple initializer"
            in
              ignore (unify_lval (unify_init None) lval);
              ignore (unify_init (unify_lval None lval))

        | Ast.STMT_init_rec (lval,atab) ->
            let unify_init tyo =
              match tyo with
                  None ->
                    begin
                      let lim = Array.length atab in
                      let rec step i accum =
                        if i = lim
                        then (Some (Ast.TY_rec (Array.of_list (List.rev accum))))
                        else
                          let (id, mode, at) = atab.(i) in
                          match unify_atom None at with
                              None -> None
                            | Some t ->
                                let accum = ltab_put accum id {Ast.slot_mode=mode;
                                                               Ast.slot_ty=Some t}
                                in
                                  step (i+1) accum
                      in
                        step 0 []
                    end
                | Some (Ast.TY_rec rt) ->
                    begin
                      let unify_rec_elt (id, mode, at) =
                        match atab_search rt id with
                            Some slot ->
                              check_mode_eq slot.Ast.slot_mode mode;
                              ignore (unify_atom slot.Ast.slot_ty at)
                          | None -> err None "Unexpected record-member '%s'" id
                      in
                        Array.iter unify_rec_elt atab;
                        tyo
                  end
                | Some _ -> err None "Non-record type for record initializer"
            in
              ignore (unify_lval (unify_init None) lval);
              ignore (unify_init (unify_lval None lval))

        | Ast.STMT_call (dst,fn,args) ->
            begin
              match unify_lval None fn with
                  None -> ()
                | Some (Ast.TY_fn (tsig, _)) -> unify_tsig s.id tsig dst args
                | Some (Ast.TY_mod tmod) ->
                    begin
                      match tmod with
                          (Some (hdr, _), _) ->
                            ignore (unify_lval (Some (Ast.TY_mod tmod)) dst);
                            unify_header_slots s.id hdr args
                        | _ ->
                            err (Some s.id) "STMT_call on non-stateful module type"
                    end
                | Some (Ast.TY_pred (hdr, _)) -> unify_header_slots s.id hdr args
                | _ -> err (Some s.id) "STMT_call on unexpected type"
            end
        | Ast.STMT_spawn (dst,_,fn,_) ->
            begin
              match unify_lval None fn with
                  None -> ()
                | Some (Ast.TY_fn _) -> ignore (unify_lval (Some Ast.TY_proc) dst)
                | Some _ -> err (Some s.id) "STMT_spawn prog resolved to non-function type"
            end
        | Ast.STMT_if i ->
            ignore (unify_expr (Some Ast.TY_bool) i.Ast.if_test)
        | Ast.STMT_while w ->
            let (_, e) = w.Ast.while_lval in
              ignore (unify_expr (Some Ast.TY_bool) e)
        | _ -> () (* FIXME (bug 541570): plenty more to handle here. *)
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;


let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  try
    let path = Stack.create () in
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
          Walk.walk_crate
            (Walk.path_managing_visitor path
               (Walk.mod_item_logging_visitor
                  (log cx "auto inference pass %d: %s" (!auto_pass))
                  path
                  (auto_inference_visitor cx progress Walk.empty_visitor)))
            crate;
          Queue.iter
            (fun id -> enqueue_auto_slot id
               (Hashtbl.find cx.ctxt_all_slots id))
            tmpq;
          incr auto_pass;
      done
  with
      Semant_err (ido, str) -> report_err cx ido str
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
