open Semant;;
open Common;;

let log cx = Session.log "mutable"
  cx.ctxt_sess.Session.sess_log_mutable
  cx.ctxt_sess.Session.sess_log_out
;;


let mutability_analysis_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =

  (* 
   * This visitor classifies every identified slot as containing-a-mutable-type
   * or not, on the basis of the following analysis:
   * 
   * - A slot is mutable if it is qualified with 'mutable'.
   * - A type is mutable if it can contain a mutable slot (transitively). 
   * 
   *)
  let visit_slot_identified_post s =
    (* Pick up the auto type-resolved slot. *)
    let slot = Hashtbl.find cx.ctxt_all_slots s.id in
      begin
        if type_is_mutable (slot_ty slot)
        then Hashtbl.replace cx.ctxt_mutable_slot_referent s.id ();
      end;
      inner.Walk.visit_slot_identified_post s;
  in
    { inner with Walk.visit_slot_identified_post = visit_slot_identified_post }
;;


let mutability_checking_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  (* 
   * This visitor enforces the following rules:
   * 
   * - A predicate can't apply to a slot holding a mutable type.
   *   (note: A predicate *can* apply to a mutable slot holding an
   *    immutable type.)
   * 
   * - A channel type carrying a mutable type is illegal.
   * 
   * - Writing to an immutable slot is illegal.
   * 
   * - Forming a write-alias to an immutable slot is illegal.
   * 
   *)
  let visit_ty_pre t =
    match t with
        Ast.TY_chan t' when type_is_mutable t' ->
          err None "channel of mutable type: %a " Ast.sprintf_ty t'
      | _ -> ()
  in

  let check_write id dst =
    let dst_slot = lval_slot cx dst in
      if ((slot_is_mutable dst_slot) or
            (Hashtbl.mem cx.ctxt_copy_stmt_is_init id))
      then ()
      else err (Some id) "writing to non-mutable slot"
  in
    (* FIXME: enforce the no-write-alias-to-immutable-slot rule. *)
  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_copy (dst, _) -> check_write s.id dst
        | Ast.STMT_call (dst, _, _) -> check_write s.id dst
        | Ast.STMT_recv (dst, _) -> check_write s.id dst
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in

    { inner with
        Walk.visit_ty_pre = visit_ty_pre;
        Walk.visit_stmt_pre = visit_stmt_pre }
;;


let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  let path = Stack.create () in
  let passes =
    [|
      (mutability_checking_visitor cx
         (mutability_analysis_visitor cx
            Walk.empty_visitor));
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
