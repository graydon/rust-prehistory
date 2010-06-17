open Semant;;
open Common;;

let log cx = Session.log "effect"
  cx.ctxt_sess.Session.sess_log_effect
  cx.ctxt_sess.Session.sess_log_out
;;


let mutability_checking_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  (* 
   * This visitor enforces the following rules:
   * 
   * - A channel type carrying a mutable type is illegal.
   * 
   * - Writing to an immutable slot is illegal.
   * 
   * - Forming a mutable alias to an immutable slot is illegal.
   * 
   *)
  let visit_ty_pre t =
    match t with
        Ast.TY_chan t' when type_has_state t' ->
          err None "channel of mutable type: %a " Ast.sprintf_ty t'
      | _ -> ()
  in

  let check_write id dst =
    let dst_slot = lval_slot cx dst in
      if (dst_slot.Ast.slot_mutable or
            (Hashtbl.mem cx.ctxt_copy_stmt_is_init id))
      then ()
      else err (Some id) "writing to non-mutable slot"
  in
    (* FIXME: enforce the no-write-alias-to-immutable-slot rule. *)
  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_copy (dst, _) -> check_write s.id dst
        | Ast.STMT_copy_binop (dst, _, _) -> check_write s.id dst
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

let function_effect_propagation_visitor
    ((*cx*)_:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  (* 
   * This visitor calculates the effect of each function according to
   * its statements:
   * 
   *    - Communication lowers to 'io'
   *    - Native calls lower to 'unsafe'
   *    - Calling a function with effect e lowers to e.
   *)
  inner
;;

let binding_effect_propagation_visitor
    ((*cx*)_:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  (* This visitor lowers the effect of an object or binding according
   * to its slots: holding a 'state' slot lowers any obj item, or
   * bind-stmt LHS, to 'state'. *)
  inner
;;

let effect_checking_visitor
    ((*cx*)_:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  (*
   * This visitor checks that each type, item and obj declares
   * effects consistent with what we calculated.
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
      (mutability_checking_visitor cx
         Walk.empty_visitor);
      (function_effect_propagation_visitor cx
         Walk.empty_visitor);
      (binding_effect_propagation_visitor cx
         Walk.empty_visitor);
      (effect_checking_visitor cx
         Walk.empty_visitor);
    |]
  in
    run_passes cx "effect" path passes (log cx "%s") crate
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
