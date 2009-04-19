open Semant;;
open Common;;


let log cx = Session.log "typestate"
  cx.ctxt_sess.Session.sess_log_typestate
  cx.ctxt_sess.Session.sess_log_out
;;


let id_of_scope (sco:scope) : node_id =
  match sco with
      SCOPE_block id -> id
    | SCOPE_mod_item i -> i.id
    | SCOPE_mod_type_item ti -> ti.id
;;


let determine_constr_key
    (cx:ctxt)
    (c:Ast.constr)
    (scopes:scope Stack.t)
    (scope_ids:node_id list)
    : constr_key =
  (* 
   * The idea here is to work out the innermost scope of either 
   * the constraint itself or any of the slots used as arguments.
   * 
   * The combination of that, plus the constr itself, forms a 
   * unique key for idenfitying a predicate.
   *)
  let cident =
    (* FIXME: handle other forms of const name. *)
    match c.Ast.constr_name with
        Ast.NAME_base (Ast.BASE_ident ident) -> ident
      | _ -> err None "unhandled form of constraint-name"
  in
  let cid =
    ref (match lookup cx scopes (Ast.KEY_ident cident) with
             Some (scope, _) -> id_of_scope scope
           | None -> err None "unresolved constraint '%s'" cident)
  in
  let rec tighten_to id sids =
    match sids with
        [] -> ()
      | x::_ when x = (!cid) -> ()
      | x::_ when x = id -> cid := id
      | _::xs -> tighten_to id xs
  in
  let tighten_to_carg carg =
    match carg with
        (* FIXME: handle other forms of constr-arg. *)
        Ast.CARG_path (Ast.CARG_base (Ast.BASE_formal)) -> ()
      | Ast.CARG_path (Ast.CARG_ext (Ast.CARG_base (Ast.BASE_formal), _)) -> ()
      | Ast.CARG_lit _ -> ()
      | Ast.CARG_path (Ast.CARG_base (Ast.BASE_named (Ast.BASE_ident argident))) ->
          begin
            match lookup cx scopes (Ast.KEY_ident argident) with
                Some (scope, _) -> tighten_to (id_of_scope scope) scope_ids
              | None -> err None "unresolved constraint-arg '%s'" argident
          end
      | _ -> err None "unhandled form of constraint-arg name"
  in
    Array.iter tighten_to_carg c.Ast.constr_args;
    (c, !cid)
;;

let constr_id_assigning_visitor
    (cx:ctxt)
    (scopes:scope Stack.t)
    (inner:Walk.visitor)
    : Walk.visitor =
  let idref = ref 0 in
  let visit_one_constr c =
    let scope_ids = List.map id_of_scope (stk_elts_from_top scopes) in
    let key = determine_constr_key cx c scopes scope_ids in
    let cid = Constr (!idref) in
      log cx "assigning constr id #%d to constr %s"
        (!idref)  (Ast.fmt_to_str Ast.fmt_constr c);
      incr idref;
      htab_put cx.ctxt_constrs cid key;
      htab_put cx.ctxt_constr_ids key cid
  in
  let visit_check constrs =
    Array.iter visit_one_constr constrs
  in
  let visit_check_if constrs _ =
    Array.iter visit_one_constr constrs
  in
  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_check constrs ->
            visit_check constrs
        | Ast.STMT_check_if (constrs, stmt) ->
            visit_check_if constrs stmt
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;

let typestate_flow_visitor
    (cx:ctxt)
    (scopes:scope Stack.t)
    (progress:bool ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_one_constr c =
    let scope_ids = List.map id_of_scope (stk_elts_from_top scopes) in
    let key = determine_constr_key cx c scopes scope_ids in
      ()
  in
  let visit_check constrs =
    Array.iter visit_one_constr constrs
  in
  let visit_check_if constrs stmt = () in
  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_check constrs ->
            visit_check constrs
        | Ast.STMT_check_if (constrs, stmt) ->
            visit_check_if constrs stmt
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;


let typestate_verify_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_prove constrs = () in
  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_prove constrs ->
            visit_prove constrs
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;

let process_crate
    (cx:ctxt)
    (items:Ast.mod_items)
    : unit =
  let progress = ref true in
  let (scopes:scope Stack.t) = Stack.create () in
  let setup_passes =
    [|
      (scope_stack_managing_visitor scopes
         (constr_id_assigning_visitor cx scopes
            Walk.empty_visitor))
    |]
  in
  let flow_passes =
    [|
      (scope_stack_managing_visitor scopes
         (typestate_flow_visitor cx scopes progress
            Walk.empty_visitor))
    |]
  in
  let verify_passes =
    [|
      (scope_stack_managing_visitor scopes
         (typestate_verify_visitor cx
            Walk.empty_visitor))
    |]
  in
    run_passes cx setup_passes (log cx "%s") items;
    let typestate_flow_pass = ref 0 in
      while !progress do
        log cx "typestate flow pass %d" (!typestate_flow_pass);
        progress := false;
        run_passes cx flow_passes (log cx "%s") items
      done;
      run_passes cx verify_passes (log cx "%s") items
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
