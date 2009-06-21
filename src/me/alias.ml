open Semant;;
open Common;;

let log cx = Session.log "alias"
  cx.ctxt_sess.Session.sess_log_alias
  cx.ctxt_sess.Session.sess_log_out
;;

let alias_analysis_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  let alias lval =
    match lval with
        Ast.LVAL_base nb ->
          let referent = Hashtbl.find cx.ctxt_lval_to_referent nb.id in
            if Hashtbl.mem cx.ctxt_all_slots referent
            then
              begin
                log cx "noting slot #%d as aliased" (int_of_node referent);
                Hashtbl.replace cx.ctxt_slot_aliased referent ()
              end
      | _ -> err None "unhandled form of lval in alias analysis"
  in
  let visit_stmt_pre s =
    begin
      match s.node with
          (*
           * FIXME: must expand this analysis to cover alias-forming arg slots, when
           * they are supported.
           *)
          Ast.STMT_call (dst, _, _) -> alias dst
        | Ast.STMT_spawn (dst, _, _) -> alias dst
        | Ast.STMT_send (_, src) -> alias src
        | Ast.STMT_recv (dst, _) -> alias dst
        | Ast.STMT_init_port (dst) -> alias dst
        | Ast.STMT_init_chan (dst, _) -> alias dst
        | Ast.STMT_init_vec (dst, _) -> alias dst
        | _ -> () (* FIXME: plenty more to handle here. *)
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with Walk.visit_stmt_pre = visit_stmt_pre }
;;

let process_crate
    (cx:ctxt)
    (items:Ast.mod_items)
    : unit =
  let passes =
    [|
      (alias_analysis_visitor cx
         Walk.empty_visitor);
    |]
  in
    run_passes cx passes (log cx "%s") items
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
