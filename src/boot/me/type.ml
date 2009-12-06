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
  let check_ty_eq (t1:Ast.ty) (t2:Ast.ty) : unit =
    if not (t1 = t2)
    then err None "mismatched types: %s vs. %s "
      (Ast.fmt_to_str Ast.fmt_ty t1) (Ast.fmt_to_str Ast.fmt_ty t2)
  in

  let visit_stmt_pre_full (s:Ast.stmt) : unit =
    begin
      match s.node with
          Ast.STMT_copy (lval,expr) ->
            check_ty_eq (lval_ty cx lval) (expr_type cx expr)

        | Ast.STMT_init_str (lval, _) ->
            check_ty_eq (lval_ty cx lval) Ast.TY_str

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

        | Ast.STMT_init_rec (lval,atab) ->
            let rtype = Array.map
              begin
                fun (id, mode, at) ->
                  (id, {Ast.slot_mode=mode;
                        Ast.slot_ty=Some (atom_type cx at)})
              end
              atab
            in
              check_ty_eq (lval_ty cx lval) (Ast.TY_rec rtype)

        | Ast.STMT_if i ->
            check_ty_eq Ast.TY_bool (expr_type cx i.Ast.if_test)

        | Ast.STMT_while w ->
            let (_, e) = w.Ast.while_lval in
              check_ty_eq Ast.TY_bool (expr_type cx e)

        (* FIXME: plenty more to handle here. *)
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
