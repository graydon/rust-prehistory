open Semant;;
open Common;;

let log cx = Session.log "mutable"
  cx.ctxt_sess.Session.sess_log_mutable
  cx.ctxt_sess.Session.sess_log_out
;;

let slot_is_mutable (s:Ast.slot) : bool =
  match s.Ast.slot_mode with
      Ast.MODE_exterior Ast.MUTABLE
    | Ast.MODE_interior Ast.MUTABLE -> true
    | _ -> false
;;

let rec type_is_mutable (t:Ast.ty) : bool =
  match t with
      Ast.TY_any
    | Ast.TY_nil
    | Ast.TY_bool
    | Ast.TY_mach _
    | Ast.TY_int
    | Ast.TY_char
    | Ast.TY_str -> false

    | Ast.TY_tup ttup -> ty_tup_is_mutable ttup
    | Ast.TY_vec t' -> type_is_mutable t'
    | Ast.TY_rec trec ->
        Array.fold_left
          (fun b (_, s) ->
             if b then b else slot_or_type_is_mutable s)
          false trec

    | Ast.TY_tag ttag -> ty_tag_is_mutable ttag
    | Ast.TY_idx idx ->
        err None "unimiplemented idx-type in type_is_mutable"

    | Ast.TY_iso tiso ->
        Array.fold_left
          (fun b t' ->
             if b then b else ty_tag_is_mutable t')
          false tiso.Ast.iso_group

    | Ast.TY_fn (_, taux) ->
        (match taux.Ast.fn_purity with
             Ast.PURE -> false
           | Ast.IMPURE Ast.MUTABLE -> true
           | Ast.IMPURE Ast.IMMUTABLE -> false)

    | Ast.TY_pred _
    | Ast.TY_chan _
    | Ast.TY_prog _
    | Ast.TY_type -> false

    | Ast.TY_port _
    | Ast.TY_proc -> true

    | Ast.TY_constrained (t', _) -> type_is_mutable t'
    | Ast.TY_opaque (_, Ast.MUTABLE) -> true
    | Ast.TY_opaque (_, Ast.IMMUTABLE) -> false

    | Ast.TY_named _ ->
        err None "unresolved named type in type_is_mutable"

    | Ast.TY_mod mtis ->
        err None "unimplemented mod-type in type_is_mutable"

and slot_or_type_is_mutable (s:Ast.slot) : bool =
  if slot_is_mutable s
  then true
  else type_is_mutable (slot_ty s)

and ty_tag_is_mutable (ttag:Ast.ty_tag) : bool =
  htab_fold
    (fun _ t' b ->
       if b then b else ty_tup_is_mutable t')
    false ttag

and ty_tup_is_mutable (ttup:Ast.ty_tup) : bool =
  Array.fold_left
    (fun b s ->
       if b then b else slot_or_type_is_mutable s)
    false ttup
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
  let passes =
    [|
      (mutability_checking_visitor cx
         (mutability_analysis_visitor cx
            Walk.empty_visitor));
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
