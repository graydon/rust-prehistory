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
    (scopes:scope Stack.t)
    (scope_ids:node_id list)
    (c:Ast.constr)
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
    Constr_pred (c, !cid)
;;

let fmt_constr_key ckey =
  match ckey with
      Constr_pred (constr, _) -> Ast.fmt_to_str Ast.fmt_constr constr
    | Constr_init n -> Printf.sprintf "<init #%d>" (int_of_node n)
;;


let rec lval_slots (cx:ctxt) (lv:Ast.lval) : node_id array =
  match lv with
      Ast.LVAL_base nbi ->
        let referent = lval_to_referent cx nbi.id in
          if Hashtbl.mem cx.ctxt_all_slots referent
          then [| referent |]
          else [| |]
    | Ast.LVAL_ext (lv, _) -> lval_slots cx lv
;;

let atom_slots (cx:ctxt) (a:Ast.atom) : node_id array =
  match a with
      Ast.ATOM_literal _ -> [| |]
    | Ast.ATOM_lval lv -> lval_slots cx lv
;;

let expr_slots (cx:ctxt) (e:Ast.expr) : node_id array =
    match e with
        Ast.EXPR_binary (_, a, b) ->
          Array.append (atom_slots cx a) (atom_slots cx b)
      | Ast.EXPR_unary (_, u) -> atom_slots cx u
      | Ast.EXPR_atom a -> atom_slots cx a
;;

let constr_id_assigning_visitor
    (cx:ctxt)
    (scopes:scope Stack.t)
    (idref:int ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let scope_ids = List.map id_of_scope (stk_elts_from_top scopes) in

  let note_constr_key key =
    if not (Hashtbl.mem cx.ctxt_constr_ids key)
    then
      begin
        let cid = Constr (!idref) in
          log cx "assigning constr id #%d to constr %s"
            (!idref) (fmt_constr_key key);
          incr idref;
          htab_put cx.ctxt_constrs cid key;
          htab_put cx.ctxt_constr_ids key cid;
      end
  in

  let visit_constr_pre c =
    let key = determine_constr_key cx scopes scope_ids c in
      note_constr_key key;
      inner.Walk.visit_constr_pre c
  in
    (* 
     * We want to generate, for any call site, a variant of 
     * the callee's entry typestate specialized to the arguments
     * that the caller passes.
     * 
     * Also, for any slot-decl node, we have to generate a 
     * variant of Constr_init for the slot (because the slot is
     * the sort of thing that can vary in init-ness over time).
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

        | Ast.STMT_decl (Ast.DECL_slot (skey, sloti)) ->
            note_constr_key (Constr_init sloti.id)

        | Ast.STMT_copy (dst, src) ->
            let precond = Array.map (fun s -> Constr_init s) (expr_slots cx src) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              Array.iter note_constr_key precond;
              Array.iter note_constr_key postcond

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
  let scope_ids _ = List.map id_of_scope (stk_elts_from_top scopes) in
  let set_condition (bitv:Bitv.t) (keys:constr_key array) : unit =
    Array.iter
      (fun key ->
         let cid = Hashtbl.find cx.ctxt_constr_ids key in
         let i = int_of_constr cid in
           log cx "setting bit %d, constraint %s"
             i (fmt_constr_key key);
           Bitv.set bitv (int_of_constr cid) true)
      keys
  in
  let set_postcondition (id:node_id) (keys:constr_key array) : unit =
    let bitv = Hashtbl.find cx.ctxt_postconditions id in
      set_condition bitv keys
  in
  let set_precondition (id:node_id) (keys:constr_key array) : unit =
    let bitv = Hashtbl.find cx.ctxt_preconditions id in
      set_condition bitv keys
  in

  let resolve_constr_to_key (constr:Ast.constr) : constr_key =
    determine_constr_key cx scopes (scope_ids()) constr
  in

  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_check constrs ->
            log cx "setting postcondition for check stmt %d"
              (int_of_node s.id);
            let keys = Array.map resolve_constr_to_key constrs in
              set_postcondition s.id keys

        | Ast.STMT_copy (dst, src) ->
            let precond = Array.map (fun s -> Constr_init s) (expr_slots cx src) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              set_precondition s.id precond;
              set_postcondition s.id postcond

        | Ast.STMT_call (_, (Ast.LVAL_base nb), args) ->
            let referent_ty = lval_ty cx nb.id in
              begin
                match referent_ty with
                    Ast.TY_fn (tsig,_) ->
                      let formal_constrs = tsig.Ast.sig_input_constrs in
                      let constrs = Array.map (apply_atoms_to_constr args) formal_constrs in
                      let keys = Array.map resolve_constr_to_key constrs in
                        set_precondition s.id keys
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
            let dests = Hashtbl.find graph s.id in
            let first = b.node.(0) in
            let last = b.node.((Array.length b.node) - 1) in
              log cx "block entry edge %d -> %d"
                (int_of_node s.id) (int_of_node first.id);
              log cx "block exit edge %d -> %s"
                (int_of_node last.id) (lset_fmt dests);
              Hashtbl.replace graph s.id [first.id];
              Hashtbl.replace graph last.id dests

        | Ast.STMT_while sw ->
            begin
              let (stmts, at) = sw.Ast.while_lval in
              let dests = Hashtbl.find graph s.id in
              let pre_loop_stmt_id =
                let slen = Array.length stmts in
                  if slen != 0
                  then
                    begin
                      let first = stmts.(0) in
                      let last = stmts.(slen - 1) in
                        log cx "while stmt, pre-loop entry edge %d -> %d"
                          (int_of_node s.id) (int_of_node first.id);
                        Hashtbl.replace graph s.id [first.id];
                        last.id
                    end
                  else
                    s.id
              in
              let block = sw.Ast.while_body.node in
              let blen = Array.length block in
                if blen != 0
                then
                  begin
                    let first = block.(0) in
                    let last = block.(blen - 1) in
                    let new_dests = first.id :: dests in
                      log cx "while stmt, block entry edge %d -> %d"
                        (int_of_node pre_loop_stmt_id) (int_of_node first.id);
                      log cx "while stmt, block exit edge %d -> %s"
                        (int_of_node last.id) (lset_fmt new_dests);
                      Hashtbl.replace graph pre_loop_stmt_id [first.id];
                      Hashtbl.replace graph last.id new_dests
                  end
                else
                  Hashtbl.replace graph pre_loop_stmt_id dests
            end

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
         (fun i -> fmt_constr_key (Hashtbl.find cx.ctxt_constrs (Constr i)))
         (Bitv.to_list bitv))
  in
  let intersection bitvs =
    let n = ref (Bitv.create sz false) in
      List.iter (fun bv -> n := Bitv.bw_and (!n) bv) bitvs;
      !n
  in
  let set_bits dst src =
    Bitv.iteri (fun i b ->
                  if (Bitv.get dst i) = b
                  then ()
                  else
                    (progress := true;
                     log cx "made progress setting bit %d" i;
                     Bitv.set dst i b)) src
  in
  let raise_bits dst src =
    Bitv.iteri_true (fun i ->
                       if Bitv.get dst i
                       then ()
                       else
                         (progress := true;
                          log cx "made progress raising bit %d" i;
                          Bitv.set dst i true)) src
  in
  let iter = ref 0 in
    Array.sort compare nodes;
    while !progress do
      incr iter;
      progress := false;
      log cx "dataflow pass %d" (!iter);
      Array.iter
        begin
          fun node ->
            let prestate = Hashtbl.find cx.ctxt_prestates node in
            let postcond = Hashtbl.find cx.ctxt_postconditions node in
            let poststate = Hashtbl.find cx.ctxt_poststates node in
            let predecessors = Hashtbl.find inverse_graph node in
            let i = int_of_node node in
              log cx "in-edges for %d: %s" i (lset_fmt predecessors);
              set_bits prestate
                (intersection
                   (List.map
                      (Hashtbl.find cx.ctxt_poststates) predecessors));
              log cx "stmt %d prestate %s" i (fmt_constr_bitv prestate);
              raise_bits poststate prestate;
              raise_bits poststate postcond;
              log cx "stmt %d poststate %s" i (fmt_constr_bitv poststate);
        end
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
             let ckey = Hashtbl.find cx.ctxt_constrs (Constr i) in
             let constr_str = fmt_constr_key ckey in
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
