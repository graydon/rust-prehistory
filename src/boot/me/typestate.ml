open Semant;;
open Common;;


let log cx = Session.log "typestate"
  cx.ctxt_sess.Session.sess_log_typestate
  cx.ctxt_sess.Session.sess_log_out
;;

let iflog cx thunk =
  if cx.ctxt_sess.Session.sess_log_typestate
  then thunk ()
  else ()
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

let fmt_constr_key cx ckey =
  match ckey with
      Constr_pred (constr, _) -> Ast.fmt_to_str Ast.fmt_constr constr
    | Constr_init n when Hashtbl.mem cx.ctxt_slot_keys n ->
        Printf.sprintf "<init #%d = %s>"
          (int_of_node n)
          (Ast.fmt_to_str Ast.fmt_slot_key (Hashtbl.find cx.ctxt_slot_keys n))
    | Constr_init n ->
        Printf.sprintf "<init #%d>" (int_of_node n)
;;

let entry_keys header constrs resolver =
  let init_keys =
    Array.map
      (fun (sloti, _) -> (Constr_init sloti.id))
      header
  in
  let names =
    Array.map
      (fun (_, ident) -> (Some (Ast.BASE_ident ident)))
      header
  in
  let input_constrs =
    Array.map (apply_names_to_constr names) constrs in
  let input_keys = Array.map resolver input_constrs in
    (input_keys, init_keys)
;;

let prog_slot_keys prog =
  Array.of_list
    (reduce_hash_to_list
       begin
         fun ident sloti ->
           Constr_init sloti.id
       end
       prog.Ast.decl_item.Ast.prog_slots)
;;


let fn_decl_keys fd resolver =
  let fn = fd.Ast.decl_item in
    entry_keys fn.Ast.fn_input_slots fn.Ast.fn_input_constrs resolver
;;

let pred_decl_keys pd resolver =
  let pred = pd.Ast.decl_item in
    entry_keys pred.Ast.pred_input_slots pred.Ast.pred_input_constrs resolver

let prog_decl_init_keys prog resolver =
  match prog.Ast.decl_item.Ast.prog_init with
      None -> ([||], [||])
    | Some i ->
        entry_keys i.node.Ast.init_input_slots i.node.Ast.init_input_constrs resolver
;;


let constr_id_assigning_visitor
    (cx:ctxt)
    (scopes:scope Stack.t)
    (idref:int ref)
    (inner:Walk.visitor)
    : Walk.visitor =

  let scope_ids _ = List.map id_of_scope (stk_elts_from_top scopes) in

  let resolve_constr_to_key (constr:Ast.constr) : constr_key =
    determine_constr_key cx scopes (scope_ids()) constr
  in

  let note_constr_key key =
    if not (Hashtbl.mem cx.ctxt_constr_ids key)
    then
      begin
        let cid = Constr (!idref) in
          iflog cx
            (fun _ -> log cx "assigning constr id #%d to constr %s"
               (!idref) (fmt_constr_key cx key));
          incr idref;
          htab_put cx.ctxt_constrs cid key;
          htab_put cx.ctxt_constr_ids key cid;
      end
  in

  let note_keys = Array.iter note_constr_key in

  let visit_mod_item_pre n p i =
    begin
    match i.node with
        Ast.MOD_ITEM_fn fd ->
          let (input_keys, init_keys) = fn_decl_keys fd resolve_constr_to_key in
            note_keys input_keys;
            note_keys init_keys
      | Ast.MOD_ITEM_pred pd ->
          let (input_keys, init_keys) = pred_decl_keys pd resolve_constr_to_key in
            note_keys input_keys;
            note_keys init_keys
      | Ast.MOD_ITEM_prog pd ->
          let (input_keys, init_keys) = prog_decl_init_keys pd resolve_constr_to_key in
          let slot_init_keys = prog_slot_keys pd in
            note_keys input_keys;
            note_keys init_keys;
            note_keys slot_init_keys;
      | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in

  let visit_constr_pre c =
    let key = determine_constr_key cx scopes (scope_ids()) c in
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
          Ast.STMT_call (_, lv, args) ->
            let referent_ty = lval_ty cx lv in
              begin
                match referent_ty with
                    Ast.TY_fn (tsig,_) ->
                      let constrs = tsig.Ast.sig_input_constrs in
                      let names = atoms_to_names args in
                      let constrs' = Array.map (apply_names_to_constr names) constrs in
                        Array.iter visit_constr_pre constrs'
                  | _ -> ()
              end

        | Ast.STMT_decl (Ast.DECL_slot (skey, sloti)) ->
            note_constr_key (Constr_init sloti.id)

        | Ast.STMT_init_rec (dst, entries) ->
            let precond = Array.map (fun s -> Constr_init s) (entries_slots cx entries) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              note_keys precond;
              note_keys postcond

        | Ast.STMT_init_tup (dst, modes_atoms) ->
            let precond = Array.map (fun s -> Constr_init s) (modes_and_atoms_slots cx modes_atoms) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              note_keys precond;
              note_keys postcond

        | Ast.STMT_init_vec (dst, _, atoms) ->
            let precond = Array.map (fun s -> Constr_init s) (atoms_slots cx atoms) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              note_keys precond;
              note_keys postcond

        | Ast.STMT_init_port dst ->
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              note_keys postcond

        | Ast.STMT_init_chan (dst, port) ->
            let precond = Array.map (fun s -> Constr_init s) (lval_option_slots cx port) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              note_keys precond;
              note_keys postcond

        | Ast.STMT_copy (dst, src) ->
            let precond = Array.map (fun s -> Constr_init s) (expr_slots cx src) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              note_keys precond;
              note_keys postcond

        | Ast.STMT_recv (dst, src) ->
            let precond = Array.map (fun s -> Constr_init s) (lval_slots cx src) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              note_keys precond;
              note_keys postcond

        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_stmt_pre = visit_stmt_pre;
        Walk.visit_constr_pre = visit_constr_pre }
;;

let bitmap_assigning_visitor
    (cx:ctxt)
    (idref:int ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_stmt_pre s =
    iflog cx (fun _ -> log cx "building %d-entry bitmap for node %d"
                (!idref) (int_of_node s.id));
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

  let raise_bits (bitv:Bitv.t) (keys:constr_key array) : unit =
    Array.iter
      (fun key ->
         let cid = Hashtbl.find cx.ctxt_constr_ids key in
         let i = int_of_constr cid in
           iflog cx (fun _ -> log cx "setting bit %d, constraint %s"
                       i (fmt_constr_key cx key));
           Bitv.set bitv (int_of_constr cid) true)
      keys
  in

  let raise_prestate (id:node_id) (keys:constr_key array) : unit =
    let bitv = Hashtbl.find cx.ctxt_prestates id in
      raise_bits bitv keys
  in

  let raise_postcondition (id:node_id) (keys:constr_key array) : unit =
    let bitv = Hashtbl.find cx.ctxt_postconditions id in
      raise_bits bitv keys
  in

  let raise_precondition (id:node_id) (keys:constr_key array) : unit =
    let bitv = Hashtbl.find cx.ctxt_preconditions id in
      raise_bits bitv keys
  in

  let resolve_constr_to_key (constr:Ast.constr) : constr_key =
    determine_constr_key cx scopes (scope_ids()) constr
  in

  let visit_mod_item_pre n p i =
    let raise_entry_state input_keys init_keys block =
      if (Array.length block.node) != 0
      then
        begin
          iflog cx (fun _ -> log cx
                      "setting entry state as stmt %d prestate"
                      (int_of_node block.node.(0).id));
          raise_prestate block.node.(0).id input_keys;
          raise_prestate block.node.(0).id init_keys;
          iflog cx (fun _ -> log cx "done propagating fn entry state")
        end
    in
    begin
    match i.node with
        Ast.MOD_ITEM_fn fd ->
          let (input_keys, init_keys) = fn_decl_keys fd resolve_constr_to_key in
            raise_entry_state input_keys init_keys fd.Ast.decl_item.Ast.fn_body

      | Ast.MOD_ITEM_pred pd ->
          let (input_keys, init_keys) = pred_decl_keys pd resolve_constr_to_key in
            raise_entry_state input_keys init_keys pd.Ast.decl_item.Ast.pred_body

      | Ast.MOD_ITEM_prog pd ->
          (* FIXME: need to enforce prog slot init keys on exit edges from init() *)
          let slot_init_keys = prog_slot_keys pd in
            begin
              match pd.Ast.decl_item.Ast.prog_init with
                  None -> ()
                | Some i ->
                    let (input_keys, init_keys) = prog_decl_init_keys pd resolve_constr_to_key in
                      raise_entry_state input_keys init_keys i.node.Ast.init_body;
            end;
            begin
              match pd.Ast.decl_item.Ast.prog_main with
                  None -> ()
                | Some i -> raise_entry_state [||] slot_init_keys i
            end
      | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in

  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_check constrs ->
            let postcond = Array.map resolve_constr_to_key constrs in
              raise_postcondition s.id postcond

        | Ast.STMT_copy (dst, src) ->
            let precond = Array.map (fun s -> Constr_init s) (expr_slots cx src) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_precondition s.id precond;
              raise_postcondition s.id postcond

        | Ast.STMT_recv (dst, src) ->
            let precond = Array.map (fun s -> Constr_init s) (lval_slots cx src) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_precondition s.id precond;
              raise_postcondition s.id postcond

        | Ast.STMT_init_rec (dst, entries) ->
            let precond = Array.map (fun s -> Constr_init s) (entries_slots cx entries) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_precondition s.id precond;
              raise_postcondition s.id postcond

        | Ast.STMT_init_tup (dst, modes_atoms) ->
            let precond = Array.map (fun s -> Constr_init s) (modes_and_atoms_slots cx modes_atoms) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_precondition s.id precond;
              raise_postcondition s.id postcond

        | Ast.STMT_init_vec (dst, _, atoms) ->
            let precond = Array.map (fun s -> Constr_init s) (atoms_slots cx atoms) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_precondition s.id precond;
              raise_postcondition s.id postcond

        | Ast.STMT_init_port dst ->
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_postcondition s.id postcond

        | Ast.STMT_init_chan (dst, port) ->
            let precond = Array.map (fun s -> Constr_init s) (lval_option_slots cx port) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_precondition s.id precond;
              raise_postcondition s.id postcond

        | Ast.STMT_call (dst, lv, args) ->
            let referent_ty = lval_ty cx lv in
              begin
                match referent_ty with
                    Ast.TY_fn (tsig,_) ->
                      let formal_constrs = tsig.Ast.sig_input_constrs in
                      let names = atoms_to_names args in
                      let constrs = Array.map (apply_names_to_constr names) formal_constrs in
                      let keys = Array.map resolve_constr_to_key constrs in
                        raise_precondition s.id keys
                  | _ -> ()
              end;
              begin
                let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
                  raise_postcondition s.id postcond
              end
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
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

  let visit_stmts stmts =
    for i = 0 to (Array.length stmts) - 2
    do
      let stmt = stmts.(i) in
      let next = stmts.(i+1) in
      let dests =
        if Hashtbl.mem graph stmt.id
        then Hashtbl.find graph stmt.id
        else []
      in
        log cx "stmt edge %d -> %d"
          (int_of_node stmt.id) (int_of_node next.id);
        Hashtbl.replace graph stmt.id (lset_add next.id dests);
    done;
  in

  let visit_stmt_pre s =
    begin
      if not (Hashtbl.mem graph s.id)
      then htab_put graph s.id []
      else ()
    end;
    begin
      match s.node with
          Ast.STMT_while sw ->
            let (stmts, _) = sw.Ast.while_lval in
              visit_stmts stmts
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in

  let visit_block_pre b = visit_stmts b.node in

  let rec last_of (stmt:Ast.stmt) =
    let last_of_stmts (best:Ast.stmt) (stmts:Ast.stmt array) : Ast.stmt =
      let len = Array.length stmts in
        if len = 0 then best else last_of (stmts.(len - 1))
    in
      match stmt.node with
          Ast.STMT_block b -> last_of_stmts stmt b.node
        | Ast.STMT_if sif ->
            let then_last = last_of_stmts stmt sif.Ast.if_then.node in
              begin
                match sif.Ast.if_else with
                    None -> then_last
                  | Some b -> last_of_stmts then_last b.node
              end

        | Ast.STMT_while sw ->
            let (stmts, at) = sw.Ast.while_lval in
            let pre_last = last_of_stmts stmt stmts in
              last_of_stmts pre_last sw.Ast.while_body.node

        | _ -> stmt
  in

  let visit_stmt_post s =
    begin
      (* Rewire blocks, loops and conditionals a bit. *)
      match s.node with
          Ast.STMT_block b when not ((Array.length b.node) = 0) ->
            let dests = Hashtbl.find graph s.id in
            let first = b.node.(0) in
            let last = last_of (b.node.((Array.length b.node) - 1)) in
              log cx "block entry edge %d -> %d"
                (int_of_node s.id) (int_of_node first.id);
              log cx "block exit edge %d -> %s"
                (int_of_node last.id) (lset_fmt dests);
              Hashtbl.replace graph s.id [first.id];
              Hashtbl.replace graph last.id dests

        | Ast.STMT_if sif ->
            begin
              let dests = Hashtbl.find graph s.id in
              let tlen = Array.length sif.Ast.if_then.node in
              let nonempty_else =
                match sif.Ast.if_else with
                    None -> None
                  | Some b ->
                      if Array.length b.node = 0
                      then None
                      else Some b.node
              in
                match (tlen != 0, nonempty_else) with
                    (false, None) -> ()
                  | (true, None) ->
                      begin
                        let first = sif.Ast.if_then.node.(0) in
                        let last = last_of sif.Ast.if_then.node.(tlen - 1) in
                          log cx "rewiring if stmt %d to enter 'then' block at %d" 
                            (int_of_node s.id) (int_of_node first.id);
                          Hashtbl.replace graph s.id [first.id];
                          Hashtbl.replace graph last.id dests
                      end
                  | (false, Some e) ->
                      begin
                        let elen = Array.length e in
                        let first = e.(0) in
                        let last = last_of e.(elen - 1) in
                          Hashtbl.replace graph s.id [first.id];
                          Hashtbl.replace graph last.id dests
                      end
                  | (true, Some e) ->
                      begin
                        let elen = Array.length e in
                        let t_first = sif.Ast.if_then.node.(0) in
                        let t_last = last_of sif.Ast.if_then.node.(tlen - 1) in
                        let e_first = e.(0) in
                        let e_last = last_of e.(elen - 1) in
                          Hashtbl.replace graph s.id [t_first.id; e_first.id];
                          Hashtbl.replace graph t_last.id dests;
                          Hashtbl.replace graph e_last.id dests
                      end
            end

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
                      let last = last_of stmts.(slen - 1) in
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
                    let last = last_of block.(blen - 1) in
                    let new_dests = s.id :: dests in
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

let find_roots
    (graph:(node_id, (node_id list)) Hashtbl.t)
    : (node_id,unit) Hashtbl.t =
  let roots = Hashtbl.create 0 in
    Hashtbl.iter (fun src _ -> Hashtbl.replace roots src ()) graph;
    Hashtbl.iter (fun _ dsts ->
                    List.iter (fun d -> Hashtbl.remove roots d) dsts) graph;
    roots
;;

let run_dataflow cx sz graph =
  (* let nodes = Array.of_list (htab_keys graph) in *)
  let roots = find_roots graph in
  let nodes = Queue.create () in
  let progress = ref true in
  let fmt_constr_bitv bitv =
    String.concat ", "
      (List.map
         (fun i -> fmt_constr_key cx (Hashtbl.find cx.ctxt_constrs (Constr i)))
         (Bitv.to_list bitv))
  in
  let set_bits dst src =
    Bitv.iteri
      begin
        fun i b ->
          if (Bitv.get dst i) = b
          then ()
          else
            (progress := true;
             iflog cx (fun _ -> log cx "made progress setting bit %d" i);
             Bitv.set dst i b)
      end
      src
  in
  let intersect_bits dst src =
    Bitv.iteri
      begin
        fun i b ->
          if (not b) && (Bitv.get dst i)
          then
            (progress := true;
             iflog cx (fun _ -> log cx
                         "made progress clearing bit %d" i);
             Bitv.set dst i b)
      end
      src
  in
  let raise_bits dst src =
    Bitv.iteri_true
      begin
        fun i ->
          if Bitv.get dst i
          then ()
          else
            (progress := true;
             iflog cx (fun _ -> log cx
                         "made progress raising bit %d" i);
             Bitv.set dst i true)
      end
      src
  in
  let iter = ref 0 in
  let written = Hashtbl.create 0 in
    Hashtbl.iter (fun n _ -> Queue.push n nodes) roots;
    while !progress do
      incr iter;
      progress := false;
      iflog cx (fun _ -> log cx "dataflow pass %d" (!iter));
      Queue.iter
        begin
          fun node ->
            let prestate = Hashtbl.find cx.ctxt_prestates node in
            let postcond = Hashtbl.find cx.ctxt_postconditions node in
            let poststate = Hashtbl.find cx.ctxt_poststates node in
              iflog cx (fun _ -> log cx "stmt %d: '%s'" (int_of_node node)
                          (Ast.fmt_to_str Ast.fmt_stmt
                             (Hashtbl.find cx.ctxt_all_stmts node)));
              iflog cx (fun _ -> log cx "stmt %d:" (int_of_node node));
              iflog cx (fun _ -> log cx
                          "    prestate %s" (fmt_constr_bitv prestate));
              raise_bits poststate prestate;
              raise_bits poststate postcond;
              iflog cx (fun _ -> log cx
                          "    poststate %s" (fmt_constr_bitv poststate));
              Hashtbl.replace written node ();
            let successors = Hashtbl.find graph node in
            let i = int_of_node node in
              iflog cx (fun _ -> log cx
                          "out-edges for %d: %s" i (lset_fmt successors));
              List.iter
                begin
                  fun succ ->
                    if Hashtbl.mem written succ
                    then
                      begin
                        intersect_bits (Hashtbl.find cx.ctxt_prestates succ) poststate;
                        Hashtbl.replace written succ ()
                      end
                    else
                      begin
                        progress := true;
                        Queue.push succ nodes;
                        set_bits (Hashtbl.find cx.ctxt_prestates succ) poststate
                      end
                end
                successors
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
             let constr_str = fmt_constr_key cx ckey in
               err (Some s.id)
                 "Unsatisfied precondition constraint %s at stmt %d: %s"
                 constr_str
                 (int_of_node s.id)
                 (Ast.fmt_to_str Ast.fmt_stmt
                    (Hashtbl.find cx.ctxt_all_stmts s.id)))
        precond;
      inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;

let lifecycle_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor =
  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_copy (lv_dst, _)
        | Ast.STMT_call (lv_dst, _, _)
        | Ast.STMT_recv (lv_dst, _) ->
            let prestate = Hashtbl.find cx.ctxt_prestates s.id in
            let poststate = Hashtbl.find cx.ctxt_poststates s.id in
            let dst_slots = lval_slots cx lv_dst in
            let is_initializing slot =
              let cid = Hashtbl.find cx.ctxt_constr_ids (Constr_init slot) in
              let i = int_of_constr cid in
                (not (Bitv.get prestate i)) && (Bitv.get poststate i)
            in
            let initializing = List.exists is_initializing (Array.to_list dst_slots) in
              if initializing
              then Hashtbl.add cx.ctxt_copy_stmt_is_init s.id ()
              else ()
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
;;

let process_crate
    (cx:ctxt)
    (crate:Ast.crate)
    : unit =
  let (scopes:scope Stack.t) = Stack.create () in
  let constr_id = ref 0 in
  let (graph:(node_id, (node_id list)) Hashtbl.t) = Hashtbl.create 0 in
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
  let aux_passes =
    [|
      (lifecycle_visitor cx
         Walk.empty_visitor)
    |]
  in
    run_passes cx setup_passes (log cx "%s") crate;
    run_dataflow cx (!constr_id) graph;
    run_passes cx verify_passes (log cx "%s") crate;
    run_passes cx aux_passes (log cx "%s") crate
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
