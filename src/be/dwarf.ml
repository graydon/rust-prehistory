(* 
 * Walk crate and generate DWARF-3 records. This file might also go in 
 * the me/ directory; it's half-middle-end, half-back-end. Debug info is
 * like that.
 * 
 * Some notes about DWARF:
 * 
 *   - Records form an ownership tree. The tree is serialized in
 *     depth-first pre-order with child lists ending with null
 *     records. When a node type is defined to have no children, no null
 *     child record is provided; it's implied.
 * 
 *               [parent]
 *                /    \           
 *          [child1]  [child2]
 *              |
 *          [grandchild1]
 * 
 *     serializes as:    
 * 
 *          [parent][child1][grandchild1][null][child2][null][null]
 * 
 *   - Sometimes you want to make it possible to scan through a sibling
 *     list quickly while skipping the sub-children of each (such as
 *     skipping the 'grandchild' above); this can be done with a
 *     DW_AT_sibling attribute that points forward to the next same-level
 *     sibling.
 * 
 *   - A DWARF consumer contains a little stack-machine interpreter for
 *     a micro-language that you can embed in DWARF records to compute
 *     values algorithmically.
 * 
 *   - DWARF is not "officially" supported by any Microsoft tools in PE
 *     files, but the Microsoft debugging information formats are
 *     proprietary and ever-shifting, and not clearly sufficient for our
 *     needs; by comparison DWARF is widely supported, stable, flexible,
 *     and required everywhere *else*. We are using DWARF to support major
 *     components of the rust runtime (reflection, unwinding, profiling) so
 *     it's helpful to not have to span technologies, just focus on DWARF.
 *     Luckily the MINGW/Cygwin communities have worked out a convention
 *     for PE, and taught BFD (thus most tools) how to digest DWARF
 *     sections trailing after the .idata section of a normal PE
 *     file. Seems to work fine.  
 * 
 *)

open Semant;;
open Common;;

let log cx = Session.log "dwarf"
  cx.ctxt_sess.Session.sess_log_dwarf
  cx.ctxt_sess.Session.sess_log_out
;;


let process_crate
    (cx:ctxt)
    (items:Ast.mod_items)
    : (Asm.item list) =
  let passes =
    [|
      Walk.empty_visitor
    |];
  in
    log cx "emitting DWARF records";
    run_passes cx passes (log cx "%s") items;
    [Asm.ZSTRING "DWARF DATA TASTES GREAT (PE doesn't like empty sections)"]
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)

