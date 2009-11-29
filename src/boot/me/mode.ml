open Semant;;
open Common;;

let log cx = Session.log "mode"
  cx.ctxt_sess.Session.sess_log_mode
  cx.ctxt_sess.Session.sess_log_out
;;

let mode_check_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  let check_write id dst =
    let slot_ids = lval_slots cx dst in
      Array.iter
        (fun slot_id ->
           let slot = Hashtbl.find cx.ctxt_all_slots slot_id in
             match slot.Ast.slot_mode with
                 Ast.MODE_read_alias -> err (Some id) "writing to read-alias"
               | _ -> ())
        slot_ids
  in
  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_copy (dst, _) -> check_write s.id dst
        | Ast.STMT_call (dst, _, _) -> check_write s.id dst
        | Ast.STMT_init_tup (dst, _) -> check_write s.id dst
        | Ast.STMT_init_rec (dst, _) -> check_write s.id dst
        | Ast.STMT_init_vec (dst, _, _) -> check_write s.id dst
        | Ast.STMT_init_str (dst, _) -> check_write s.id dst
        | Ast.STMT_recv (dst, _) -> check_write s.id dst
        | _ -> ()
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
  let passes =
    [|
      (mode_check_visitor cx
         Walk.empty_visitor)
    |]
  in
    run_passes cx passes (log cx "%s") crate;
()
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
