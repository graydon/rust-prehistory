open Semant;;
open Common;;


let log cx = Session.log "type"
  cx.ctxt_sess.Session.sess_log_type
  cx.ctxt_sess.Session.sess_log_out
;;

let type_check_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  let ty_err (ty:Ast.ty) (expected:string) : unit =
    err None "mismatched types: found %s, expected %s "
      (Ast.fmt_to_str Ast.fmt_ty ty) expected
  in

  let check_ty_eq (t1:Ast.ty) (t2:Ast.ty) : unit =
    if not (t1 = t2) then ty_err t1 (Ast.fmt_to_str Ast.fmt_ty t2)
  in

  let loggable (ty:Ast.ty) : bool =
    match ty with
        Ast.TY_str -> true
      | Ast.TY_int -> true
      | Ast.TY_char -> true
      | Ast.TY_mach (TY_u8) -> true
      | Ast.TY_mach (TY_u16) -> true
      | Ast.TY_mach (TY_u32) -> true
      | Ast.TY_mach (TY_s8) -> true
      | Ast.TY_mach (TY_s16) -> true
      | Ast.TY_mach (TY_s32) -> true
      | _ -> false
  in

  let visit_stmt_pre_full (s:Ast.stmt) : unit =
    begin
      match s.node with
          Ast.STMT_init_rec (lval,atab) ->
            let rtype = Array.map
              begin
                fun (id, mode, at) ->
                  (id, {Ast.slot_mode=mode;
                        Ast.slot_ty=Some (atom_type cx at)})
              end
              atab
            in
              check_ty_eq (lval_ty cx lval) (Ast.TY_rec rtype)

        | Ast.STMT_init_tup (lval,mode_atoms) ->
            let ttype = Array.map
              begin
                fun (mode, at) ->
                  {Ast.slot_mode=mode;
                   Ast.slot_ty=Some (atom_type cx at)}
              end
              mode_atoms
            in
              check_ty_eq (lval_ty cx lval) (Ast.TY_tup ttype)

        | Ast.STMT_init_str (lval, _) ->
            check_ty_eq (lval_ty cx lval) Ast.TY_str

        | Ast.STMT_copy (lval, expr, _) ->
            check_ty_eq (lval_ty cx lval) (expr_type cx expr)

        | Ast.STMT_call (out, callee, args) ->
            begin
              let _ = out in
              let callee_ty = lval_ty cx callee in
              let check_ins in_slots =
                if (Array.length in_slots) != (Array.length args)
                then err (Some s.id) "argument count mismatch: %d given, %d expected"
                  (Array.length args) (Array.length in_slots);
                for i = 0 to (Array.length args) - 1 do
                  check_ty_eq (slot_ty in_slots.(i)) (atom_type cx args.(i))
                done
              in
              let check_out out_slot =
                check_ty_eq (slot_ty out_slot) (lval_ty cx out)
              in
                match callee_ty with
                    Ast.TY_fn (tsig, _(*taux*)) ->
                      begin
                        check_ins tsig.Ast.sig_input_slots;
                        check_out tsig.Ast.sig_output_slot
                      end
                  | Ast.TY_pred (in_slots, _(*constrs*)) ->
                      begin
                        check_ins in_slots;
                        check_out (interior_slot Ast.TY_bool)
                      end
                  | Ast.TY_mod (Some (in_slots, _(*constrs*)), mtis) ->
                      begin
                        check_ins in_slots;
                        check_out (interior_slot (Ast.TY_mod (None, mtis)));
                      end
                  | _ ->
                      err (Some s.id) "call to non-callable lval: %a of type %a"
                        Ast.sprintf_lval callee
                        Ast.sprintf_ty callee_ty
              end

        | Ast.STMT_while w ->
            let (_, e) = w.Ast.while_lval in
              check_ty_eq Ast.TY_bool (expr_type cx e)

        | Ast.STMT_if i ->
            check_ty_eq Ast.TY_bool (expr_type cx i.Ast.if_test)

        | Ast.STMT_log atom ->
            let atom_ty = atom_type cx atom in
            if not (loggable atom_ty) then ty_err atom_ty "loggable"

        (* FIXME (bug 541531): plenty more to handle here. *)
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in

  let visit_stmt_pre (s:Ast.stmt) : unit =
    try
      visit_stmt_pre_full s
    with
        Semant_err (None, msg) -> raise (Semant_err ((Some s.id), msg))
  in

  let visit_expr_pre (e:Ast.expr) : unit =
    begin
      match e with
          Ast.EXPR_binary (_, a, b) ->
            check_ty_eq (atom_type cx a) (atom_type cx b)
        | _ -> ()
    end;
    inner.Walk.visit_expr_pre e
  in
    { inner with
        Walk.visit_expr_pre = visit_expr_pre;
        Walk.visit_stmt_pre = visit_stmt_pre }
;;


let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  let path = Stack.create () in
  let passes =
    [|
      (type_check_visitor cx
         Walk.empty_visitor);
    |]
  in
    run_passes cx "type" path passes (log cx "%s") crate
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
