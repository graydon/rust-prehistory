open Semant;;
open Common;;

let log cx = Session.log "gc"
  cx.ctxt_sess.Session.sess_log_gc
  cx.ctxt_sess.Session.sess_log_out
;;

let gc_analysis_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =

  (* The GC analysis runs after the mutability rules.
   * 
   * - Mutability alone doesn't necessitate living in GC memory. If you
   * declare a record full of mutable ints, it can live outside GC
   * memory because it's not scanned. Likewise frames.
   * 
   * - We do not know *values* at compile-time though, so we don't
   * analyze them; we analyze slots. Each slot is classified as
   * pointing-to-GC-memory or not.
   * 
   * - A slot points into GC memory if it is exterior and "of mutable
   * type". We have marked all the points-to-mutable-type
   * identified slots in the mutability pass, which means we can
   * just use a semant query on the slot identity to tell when an init
   * is a "from GC" allocation.
   * 
   * The point of this pass is to work out the set of slots that need
   * to be marked as part of each GC-slot-pointing frame.
   *)
  inner
;;


let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  let passes =
    [|
      (gc_analysis_visitor cx
         Walk.empty_visitor);
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
