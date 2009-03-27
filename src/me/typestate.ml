open Semant;;
open Common;;


let log cx = Session.log "typestate"
  cx.ctxt_sess.Session.sess_log_typestate
  cx.ctxt_sess.Session.sess_log_out
;;


let typestate_visitor
    (cx:ctxt)
    (progress:bool ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_stmt_pre s =
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }


let process_crate
    (cx:ctxt)
    (items:Ast.mod_items)
    : unit =
  let progress = ref true in
  let typestate_pass = ref 0 in
    while !progress do
      log cx "typestate pass %d" (!typestate_pass);
      progress := false
    done
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
