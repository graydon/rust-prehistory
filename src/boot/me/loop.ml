(*
 * Computes iterator-loop nesting depths and max depth of each function.
 *)

open Semant;;
open Common;;

let log cx = Session.log "loop"
  cx.ctxt_sess.Session.sess_log_mode
  cx.ctxt_sess.Session.sess_log_out
;;

type fn_ctxt = { current_depth: int;
                 max_depth: int; }
;;

let incr_depth (fcx:fn_ctxt) =
  let depth = fcx.current_depth + 1 in
    { current_depth = depth;
      max_depth = max fcx.max_depth depth; }
;;

let decr_depth (fcx:fn_ctxt) =
  { fcx with current_depth = fcx.current_depth - 1; }
;;

let top_fcx = { current_depth = 0;
                max_depth = 0; }
;;

let loop_depth_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =

  let (fcxs : fn_ctxt Stack.t) = Stack.create () in

  let push_loop () =
    let fcx = Stack.pop fcxs in
      Stack.push (incr_depth fcx) fcxs
  in

  let pop_loop () =
    let fcx = Stack.pop fcxs in
      Stack.push (decr_depth fcx) fcxs
  in

  let visit_mod_item_pre
      (ident:Ast.ident)
      (ty_params:(Ast.ty_param identified) array)
      (item:Ast.mod_item)
      : unit =
    Stack.push top_fcx fcxs;
    inner.Walk.visit_mod_item_pre ident ty_params item
  in

  let visit_mod_item_post
      (ident:Ast.ident)
      (ty_params:(Ast.ty_param identified) array)
      (item:Ast.mod_item)
      : unit =
    inner.Walk.visit_mod_item_post ident ty_params item;
    let fcx = Stack.pop fcxs in
      htab_put cx.ctxt_fn_loop_depths item.id fcx.max_depth
  in

  let visit_obj_fn_pre
      (obj:Ast.obj identified)
      (ident:Ast.ident)
      (fn:Ast.fn identified)
      : unit =
    Stack.push top_fcx fcxs;
    inner.Walk.visit_obj_fn_pre obj ident fn
  in

  let visit_obj_fn_post
      (obj:Ast.obj identified)
      (ident:Ast.ident)
      (fn:Ast.fn identified)
      : unit =
    inner.Walk.visit_obj_fn_pre obj ident fn;
    let fcx = Stack.pop fcxs in
      htab_put cx.ctxt_fn_loop_depths fn.id fcx.max_depth
  in

  let visit_obj_drop_pre
      (obj:Ast.obj identified)
      (b:Ast.block)
      : unit =
    Stack.push top_fcx fcxs;
    inner.Walk.visit_obj_drop_pre obj b
  in

  let visit_obj_drop_post
      (obj:Ast.obj identified)
      (b:Ast.block)
      : unit =
    inner.Walk.visit_obj_drop_post obj b;
    let fcx = Stack.pop fcxs in
      htab_put cx.ctxt_fn_loop_depths b.id fcx.max_depth
  in

  let visit_stmt_pre s =
    begin
      match s.node with
        | Ast.STMT_for_each _ ->
            let fcx = Stack.top fcxs in
              begin
                htab_put cx.ctxt_loop_depths s.id fcx.current_depth;
                push_loop ()
              end
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in

  let visit_stmt_post s =
    inner.Walk.visit_stmt_post s;
    match s.node with
      | Ast.STMT_for_each _ -> pop_loop ()
      | _ -> ()

  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post;
        Walk.visit_obj_fn_pre = visit_obj_fn_pre;
        Walk.visit_obj_fn_post = visit_obj_fn_post;
        Walk.visit_obj_drop_pre = visit_obj_drop_pre;
        Walk.visit_obj_drop_post = visit_obj_drop_post;
        Walk.visit_stmt_pre = visit_stmt_pre;
        Walk.visit_stmt_post = visit_stmt_post }
;;

let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  let path = Stack.create () in
  let passes =
    [|
      (loop_depth_visitor cx
         Walk.empty_visitor)
    |]
  in

    run_passes cx "loop" path passes (log cx "%s") crate;
    ()
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
