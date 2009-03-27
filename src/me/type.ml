open Semant;;
open Common;;


let log cx = Session.log "type"
  cx.ctxt_sess.Session.sess_log_type
  cx.ctxt_sess.Session.sess_log_out
;;


let process_crate
    (cx:ctxt)
    (items:Ast.mod_items)
    : unit =
()
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
