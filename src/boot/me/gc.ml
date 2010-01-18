open Semant;;
open Common;;

let log cx = Session.log "gc"
  cx.ctxt_sess.Session.sess_log_gc
  cx.ctxt_sess.Session.sess_log_out
;;


let gc_analysis_visitor
    (_:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =

  (* The GC analysis runs after the mutability rules.
   * 
   * - Mutability alone doesn't necessitate living in GC memory. If you
   * declare a record full of mutable ints, it can live outside GC
   * memory because it's not cyclic. Likewise frames.
   * 
   * - A cyclic value can point to an acyclic value, but not vice-versa
   *   (since it would never be collected).
   *)

  inner
;;


let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  let path = Stack.create () in
  let passes =
    [|
      (gc_analysis_visitor cx
         Walk.empty_visitor);
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
