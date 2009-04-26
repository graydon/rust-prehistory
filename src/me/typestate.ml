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
    (* FIXME: handle other forms of constr name. *)
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
    (idref:int ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let scope_ids = List.map id_of_scope (stk_elts_from_top scopes) in
  let visit_constr_pre c =
    let key = determine_constr_key cx c scopes scope_ids in
      if not (Hashtbl.mem cx.ctxt_constr_ids key)
      then
        begin
          let cid = Constr (!idref) in
            log cx "assigning constr id #%d to constr %s"
              (!idref)  (Ast.fmt_to_str Ast.fmt_constr c);
            incr idref;
            htab_put cx.ctxt_constrs cid key;
            htab_put cx.ctxt_constr_ids key cid;
        end;
      inner.Walk.visit_constr_pre c
  in
    (* 
     * We also want to generate, for any call site, a variant of 
     * the callee's entry typestate specialized to the arguments
     * that the caller passes.
     *)
  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_call (_, (Ast.LVAL_base nb), args) ->
            let referent_ty = lval_ty cx nb.id in
              begin
                match referent_ty with
                    Ast.TY_fn (tsig,_) ->
                      let constrs = tsig.Ast.sig_input_constrs in
                      let constrs' = Array.map (apply_atoms_to_constr args) constrs in
                        Array.iter visit_constr_pre constrs'
                  | _ -> ()
              end
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre;
        Walk.visit_constr_pre = visit_constr_pre }
;;

let bitmap_assigning_visitor
    (cx:ctxt)
    (idref:int ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_stmt_pre s =
    log cx "building %d-entry bitmap for node %d" (!idref) (int_of_node s.id);
    htab_put cx.ctxt_preconditions s.id (Bitv.create (!idref) false);
    htab_put cx.ctxt_postconditions s.id (Bitv.create (!idref) false);
    htab_put cx.ctxt_prestates s.id (Bitv.create (!idref) false);
    htab_put cx.ctxt_poststates s.id (Bitv.create (!idref) false);
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;

let condition_assigning_visitor
    (cx:ctxt)
    (scopes:scope Stack.t)
    (inner:Walk.visitor)
    : Walk.visitor =
  let scope_ids = List.map id_of_scope (stk_elts_from_top scopes) in
  let set_condition bitv constrs =
    Array.iter
      (fun constr ->
         let key = determine_constr_key cx constr scopes scope_ids in
         let cid = Hashtbl.find cx.ctxt_constr_ids key in
         let i = int_of_constr cid in
           log cx "setting bit %d, constraint %s"
             i (Ast.fmt_to_str Ast.fmt_constr constr);
           Bitv.set bitv (int_of_constr cid) true)
      constrs
  in
  let set_postcondition id constrs =
    let bitv = Hashtbl.find cx.ctxt_postconditions id in
      set_condition bitv constrs
  in
  let set_precondition id constrs =
    let bitv = Hashtbl.find cx.ctxt_preconditions id in
      set_condition bitv constrs
  in
  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_check constrs ->
            log cx "setting postcondition for check stmt %d"
              (int_of_node s.id);
            set_postcondition s.id constrs
        | Ast.STMT_call (_, (Ast.LVAL_base nb), args) ->
            let referent_ty = lval_ty cx nb.id in
              begin
                match referent_ty with
                    Ast.TY_fn (tsig,_) ->
                      let constrs = tsig.Ast.sig_input_constrs in
                      let constrs' = Array.map (apply_atoms_to_constr args) constrs in
                        set_precondition s.id constrs'
                  | _ -> ()
              end
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;

let lset_add x xs =
  if List.mem x xs
  then xs
  else x::xs
;;

let lset_fmt lset =
  "[" ^
    (String.concat ", "
       (List.map
          (fun n -> string_of_int (int_of_node n)) lset)) ^
    "]"
;;

let graph_building_visitor
    (cx:ctxt)
    (graph:(node_id, (node_id list)) Hashtbl.t)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_stmt_pre s =
    begin
      if not (Hashtbl.mem graph s.id)
      then htab_put graph s.id []
      else ()
    end;
    inner.Walk.visit_stmt_pre s
  in
  let visit_block_pre b =
    for i = 0 to (Array.length b.node) - 2
    do
      let stmt = b.node.(i) in
      let next = b.node.(i+1) in
      let dests =
        if Hashtbl.mem graph stmt.id
        then Hashtbl.find graph stmt.id
        else []
      in
        log cx "stmt edge %d -> %d"
          (int_of_node stmt.id) (int_of_node next.id);
        Hashtbl.replace graph stmt.id (lset_add next.id dests);
    done;
    inner.Walk.visit_block_pre b
  in
  let visit_stmt_post s =
    begin
      (* Rewire blocks, loops and conditionals a bit. *)
      match s.node with
          Ast.STMT_block b when not ((Array.length b.node) = 0) ->
            let dests = Hashtbl.find graph b.id in
            let first = b.node.(0).id in
            let last = b.node.((Array.length b.node) - 1) in
              log cx "block entry edge %d -> %d"
                (int_of_node b.id) (int_of_node first);
              log cx "block exit edge %d -> %s"
                (int_of_node b.id) (lset_fmt dests);
              Hashtbl.replace graph b.id [first];
              Hashtbl.replace graph last.id dests
        | _ -> ()
    end;
    inner.Walk.visit_stmt_post s
  in
    { inner with
        Walk.visit_block_pre = visit_block_pre;
        Walk.visit_stmt_pre = visit_stmt_pre;
        Walk.visit_stmt_post = visit_stmt_post }
;;

let run_dataflow cx sz inverse_graph =
  let nodes = Array.of_list (htab_keys inverse_graph) in
  let progress = ref true in
  let fmt_constr_bitv bitv =
    String.concat ", "
      (List.map
         (fun i ->
            let (constr, _) = Hashtbl.find cx.ctxt_constrs (Constr i) in
              Ast.fmt_to_str Ast.fmt_constr constr)
         (Bitv.to_list bitv))
  in
  let set_bits dst src =
    Bitv.iteri_true (fun i ->
                       if Bitv.get dst i
                       then ()
                       else
                         (progress := true;
                          Bitv.set dst i true)) src
  in
  let intersect_bits dst src =
    Bitv.iteri (fun i b ->
                  let curr = Bitv.get dst i in
                  let comb = curr && b in
                    if curr != comb
                    then
                      (progress := true;
                       Bitv.set dst i comb)) src
  in
  let iter = ref 0 in
    Array.sort compare nodes;
    while !progress do
      incr iter;
      progress := false;
      log cx "dataflow pass %d" (!iter);
      Array.iter
        (fun node ->
           let prestate = Hashtbl.find cx.ctxt_prestates node in
           let postcond = Hashtbl.find cx.ctxt_postconditions node in
           let poststate = Hashtbl.find cx.ctxt_poststates node in
           let predecessors = Hashtbl.find inverse_graph node in
           let i = int_of_node node in
             log cx "in-edges for %d: %s" i (lset_fmt predecessors);
             begin
               match predecessors with
                   p :: ps ->
                     set_bits prestate (Hashtbl.find cx.ctxt_poststates p);
                     List.iter
                       (fun p ->
                          let p_poststate = Hashtbl.find cx.ctxt_poststates p in
                            intersect_bits p_poststate prestate)
                       ps
                 | _ -> ()
             end;
             log cx "stmt %d prestate %s" i (fmt_constr_bitv prestate);
             set_bits poststate prestate;
             set_bits poststate postcond;
             log cx "stmt %d poststate %s" i (fmt_constr_bitv poststate);
)
        nodes
    done
;;

let typestate_verify_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_stmt_pre s =
    let prestate = Hashtbl.find cx.ctxt_prestates s.id in
    let precond = Hashtbl.find cx.ctxt_preconditions s.id in
      Bitv.iteri_true
        (fun i ->
           if not (Bitv.get prestate i)
           then
             let (constr, _) = Hashtbl.find cx.ctxt_constrs (Constr i) in
             let constr_str = Ast.fmt_to_str Ast.fmt_constr constr in
               err (Some s.id) "Unsatisfied precondition constraint: %s " constr_str)
        precond;
      inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;

let invert_graph
    (graph:(node_id, (node_id list)) Hashtbl.t)
    (inverse_graph:(node_id, (node_id list)) Hashtbl.t)
    : unit =
  Hashtbl.iter
    (fun src dsts ->
       List.iter
         (fun dst ->
            let srcs =
              if Hashtbl.mem inverse_graph dst
              then Hashtbl.find inverse_graph dst
              else []
            in
              Hashtbl.replace inverse_graph dst (lset_add src srcs))
         dsts)
    graph
;;

let process_crate
    (cx:ctxt)
    (items:Ast.mod_items)
    : unit =
  let (scopes:scope Stack.t) = Stack.create () in
  let constr_id = ref 0 in
  let (graph:(node_id, (node_id list)) Hashtbl.t) = Hashtbl.create 0 in
  let (inverse_graph:(node_id, (node_id list)) Hashtbl.t) = Hashtbl.create 0 in
  let setup_passes =
    [|
      (scope_stack_managing_visitor scopes
         (constr_id_assigning_visitor cx scopes constr_id
            Walk.empty_visitor));
      (bitmap_assigning_visitor cx constr_id
         Walk.empty_visitor);
      (scope_stack_managing_visitor scopes
         (condition_assigning_visitor cx scopes
            Walk.empty_visitor));
      (graph_building_visitor cx graph
         Walk.empty_visitor)
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
    invert_graph graph inverse_graph;
    run_dataflow cx (!constr_id) inverse_graph;
    run_passes cx verify_passes (log cx "%s") items
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
