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

let name_base_to_slot_key (nb:Ast.name_base) : Ast.slot_key =
  match nb with
      Ast.BASE_ident ident -> Ast.KEY_ident ident
    | Ast.BASE_temp tmp -> Ast.KEY_temp tmp
    | Ast.BASE_app _ -> bug () "name_base_to_slot_key on parametric name"
;;

let determine_constr_key
    (cx:ctxt)
    (scopes:(scope list))
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
  let key =
    (* FIXME (bug 541527): handle other forms of constr name. *)
    match c.Ast.constr_name with
        Ast.NAME_base nb -> name_base_to_slot_key nb
      | x ->
          err None "unhandled form of constraint-name: %a"
            Ast.sprintf_name x
  in
  let cid =
    ref (match lookup cx scopes key with
             Some (scope::_, _) -> id_of_scope scope
           | _ ->
               err None "unresolved constraint '%a'"
                 Ast.sprintf_slot_key key)
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
        (* FIXME (bug 541527): handle other forms of constr-arg. *)
        Ast.CARG_path (Ast.CARG_base (Ast.BASE_formal)) -> ()
      | Ast.CARG_path (Ast.CARG_ext (Ast.CARG_base (Ast.BASE_formal), _)) -> ()
      | Ast.CARG_lit _ -> ()
      | Ast.CARG_path (Ast.CARG_base (Ast.BASE_named nb)) ->
          begin
            let key = name_base_to_slot_key nb in
              match lookup cx scopes key with
                  Some (scope::_, _) -> tighten_to (id_of_scope scope) scope_ids
                | _ ->
                    err None "unresolved constraint-arg '%a'"
                      Ast.sprintf_slot_key key
          end
      | _ ->
          err None "unhandled form of constraint-arg name: %a"
            Ast.sprintf_carg carg
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

let obj_keys ob resolver =
    entry_keys ob.Ast.obj_state ob.Ast.obj_constrs resolver
;;

let fn_keys fn resolver =
    entry_keys fn.Ast.fn_input_slots fn.Ast.fn_input_constrs resolver
;;

let pred_keys pred resolver =
    entry_keys pred.Ast.pred_input_slots pred.Ast.pred_input_constrs resolver
;;

let constr_id_assigning_visitor
    (cx:ctxt)
    (scopes:(scope list) ref)
    (idref:int ref)
    (inner:Walk.visitor)
    : Walk.visitor =

  let scope_ids _ = List.map id_of_scope (!scopes) in

  let resolve_constr_to_key (constr:Ast.constr) : constr_key =
    determine_constr_key cx (!scopes) (scope_ids()) constr
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
    match i.node.Ast.decl_item with
        Ast.MOD_ITEM_fn f ->
          let (input_keys, init_keys) = fn_keys f resolve_constr_to_key in
            note_keys input_keys;
            note_keys init_keys
      | Ast.MOD_ITEM_obj ob ->
          let (input_keys, init_keys) = obj_keys ob resolve_constr_to_key in
            note_keys input_keys;
            note_keys init_keys
      | Ast.MOD_ITEM_pred p ->
          let (input_keys, init_keys) = pred_keys p resolve_constr_to_key in
            note_keys input_keys;
            note_keys init_keys
      | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in

  let visit_constr_pre c =
    let key = determine_constr_key cx (!scopes) (scope_ids()) c in
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

                  | Ast.TY_pred (_, constrs) ->
                      let names = atoms_to_names args in
                      let constrs' = Array.map (apply_names_to_constr names) constrs in
                        Array.iter visit_constr_pre constrs'

                  | _ -> ()
              end

        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in

  let visit_slot_identified_pre s =
    note_constr_key (Constr_init s.id);
    inner.Walk.visit_slot_identified_pre s
  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_slot_identified_pre = visit_slot_identified_pre;
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
    htab_put cx.ctxt_preconditions s.id (Bits.create (!idref) false);
    htab_put cx.ctxt_postconditions s.id (Bits.create (!idref) false);
    htab_put cx.ctxt_prestates s.id (Bits.create (!idref) false);
    htab_put cx.ctxt_poststates s.id (Bits.create (!idref) false);
    inner.Walk.visit_stmt_pre s
  in
  let visit_block_pre b =
    iflog cx (fun _ -> log cx "building %d-entry bitmap for node %d"
                (!idref) (int_of_node b.id));
    htab_put cx.ctxt_preconditions b.id (Bits.create (!idref) false);
    htab_put cx.ctxt_postconditions b.id (Bits.create (!idref) false);
    htab_put cx.ctxt_prestates b.id (Bits.create (!idref) false);
    htab_put cx.ctxt_poststates b.id (Bits.create (!idref) false);
    inner.Walk.visit_block_pre b
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre;
        Walk.visit_block_pre = visit_block_pre }
;;

let condition_assigning_visitor
    (cx:ctxt)
    (scopes:(scope list) ref)
    (inner:Walk.visitor)
    : Walk.visitor =
  let scope_ids _ = List.map id_of_scope (!scopes) in

  let raise_bits (bitv:Bits.t) (keys:constr_key array) : unit =
    Array.iter
      (fun key ->
         let cid = Hashtbl.find cx.ctxt_constr_ids key in
         let i = int_of_constr cid in
           iflog cx (fun _ -> log cx "setting bit %d, constraint %s"
                       i (fmt_constr_key cx key));
           Bits.set bitv (int_of_constr cid) true)
      keys
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
    determine_constr_key cx (!scopes) (scope_ids()) constr
  in

  let raise_entry_state input_keys init_keys block =
    iflog cx (fun _ -> log cx
                "setting entry state as block %d postcondition (block-entry prestate)"
                (int_of_node block.id));
    raise_postcondition block.id input_keys;
    raise_postcondition block.id init_keys;
    iflog cx (fun _ -> log cx "done setting block postcondition")
  in

  let visit_mod_item_pre n p i =
    begin
      match i.node.Ast.decl_item with
          Ast.MOD_ITEM_fn f ->
            let (input_keys, init_keys) = fn_keys f resolve_constr_to_key in
              raise_entry_state input_keys init_keys f.Ast.fn_body

        | Ast.MOD_ITEM_pred p ->
            let (input_keys, init_keys) = pred_keys p resolve_constr_to_key in
              raise_entry_state input_keys init_keys p.Ast.pred_body

        | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in

  let visit_obj_fn_pre obj ident fn =
    let (obj_input_keys, obj_init_keys) = obj_keys obj.node resolve_constr_to_key in
    let (fn_input_keys, fn_init_keys) = fn_keys fn.node resolve_constr_to_key in
      raise_entry_state obj_input_keys obj_init_keys fn.node.Ast.fn_body;
      raise_entry_state fn_input_keys fn_init_keys fn.node.Ast.fn_body;
      inner.Walk.visit_obj_fn_pre obj ident fn
  in

  let visit_stmt_pre s =
    begin
      match s.node with
          Ast.STMT_check (constrs, _) ->
            let postcond = Array.map resolve_constr_to_key constrs in
              raise_postcondition s.id postcond

        | Ast.STMT_recv (dst, src) ->
            let precond = Array.map (fun s -> Constr_init s) (lval_slots cx src) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_precondition s.id precond;
              raise_postcondition s.id postcond

        | Ast.STMT_send (dst, src) ->
            let precond = Array.append
              (Array.map (fun s -> Constr_init s) (lval_slots cx dst))
              (Array.map (fun s -> Constr_init s) (lval_slots cx src))
            in
              raise_precondition s.id precond;

        | Ast.STMT_init_rec (dst, entries, base) ->
            let base_slots =
              begin
                match base with
                    None -> [| |]
                  | Some lval -> lval_slots cx lval
              end
            in
            let precond = Array.map (fun s -> Constr_init s) (Array.append (entries_slots cx entries) base_slots) in
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

        | Ast.STMT_init_str (dst, _) ->
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_postcondition s.id postcond

        | Ast.STMT_init_port dst ->
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_postcondition s.id postcond

        | Ast.STMT_init_chan (dst, port) ->
            let precond = Array.map (fun s -> Constr_init s) (lval_option_slots cx port) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_precondition s.id precond;
              raise_postcondition s.id postcond

        | Ast.STMT_copy (dst, src) ->
            let precond = Array.map (fun s -> Constr_init s) (expr_slots cx src) in
            let postcond = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
              raise_precondition s.id precond;
              raise_postcondition s.id postcond

        | Ast.STMT_copy_binop (dst, _, src) ->
            let dst_init = Array.map (fun s -> Constr_init s) (lval_slots cx dst) in
            let src_init = Array.map (fun s -> Constr_init s) (atom_slots cx src) in
            let precond = Array.append dst_init src_init in
              raise_precondition s.id precond;

        | Ast.STMT_spawn (dst, _, lv, args)
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
                  | Ast.TY_pred (_,formal_constrs) ->
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

        | Ast.STMT_ret (Some at) ->
            let precond = Array.map (fun s -> Constr_init s) (atom_slots cx at) in
              raise_precondition s.id precond

        | Ast.STMT_put (Some at) ->
            let precond = Array.map (fun s -> Constr_init s) (atom_slots cx at) in
              raise_precondition s.id precond

        | Ast.STMT_join lval ->
            let precond = Array.map (fun s -> Constr_init s) (lval_slots cx lval) in
              raise_precondition s.id precond

        | Ast.STMT_log atom ->
            let precond = Array.map (fun s -> Constr_init s) (atom_slots cx atom) in
              raise_precondition s.id precond

        | Ast.STMT_check_expr expr ->
            let precond = Array.map (fun s -> Constr_init s) (expr_slots cx expr) in
              raise_precondition s.id precond

        | Ast.STMT_while sw ->
            let (_, expr) = sw.Ast.while_lval in
            let precond = Array.map (fun s -> Constr_init s) (expr_slots cx expr) in
              raise_precondition s.id precond

        | Ast.STMT_alt_tag at ->
            let precond = Array.map (fun s -> Constr_init s) (lval_slots cx at.Ast.alt_tag_lval) in
            let visit_arm { node = (_, header_slots, block) } =
              (* FIXME: propagate tag-carried constrs here. *)
              let (input_keys, init_keys) = entry_keys header_slots [| |] resolve_constr_to_key in
                raise_entry_state input_keys init_keys block
            in
              raise_precondition s.id precond;
              Array.iter visit_arm at.Ast.alt_tag_arms

        | Ast.STMT_for_each fe ->
            let (si, _) = fe.Ast.for_each_slot in
            let block_entry_state = [| Constr_init si.id |] in
              raise_postcondition fe.Ast.for_each_body.id block_entry_state

        | Ast.STMT_for fo ->
            let (si, _) = fo.Ast.for_slot in
            let block_entry_state = [| Constr_init si.id |] in
              raise_postcondition fo.Ast.for_body.id block_entry_state

        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in
    { inner with
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_obj_fn_pre = visit_obj_fn_pre;
        Walk.visit_stmt_pre = visit_stmt_pre }
;;

let lset_add (x:node_id) (xs:node_id list) : node_id list =
  if List.mem x xs
  then xs
  else x::xs
;;

let lset_remove (x:node_id) (xs:node_id list) : node_id list =
  List.filter (fun a -> not (a = x)) xs
;;

let lset_union (xs:node_id list) (ys:node_id list) : node_id list =
  List.fold_left (fun ns n -> lset_add n ns) xs ys
;;

let lset_diff (xs:node_id list) (ys:node_id list) : node_id list =
  List.fold_left (fun ns n -> lset_remove n ns) xs ys
;;

let lset_fmt lset =
  "[" ^
    (String.concat ", "
       (List.map
          (fun n -> string_of_int (int_of_node n)) lset)) ^
    "]"
;;

type node_graph = (node_id, (node_id list)) Hashtbl.t;;

let graph_sequence_building_visitor
    (cx:ctxt)
    (graph:node_graph)
    (inner:Walk.visitor)
    : Walk.visitor =

  (* Flow each stmt to its sequence-successor. *)
  let visit_stmts stmts =
    let len = Array.length stmts in
      for i = 0 to len - 2
      do
        let stmt = stmts.(i) in
        let next = stmts.(i+1) in
          log cx "sequential stmt edge %d -> %d"
            (int_of_node stmt.id) (int_of_node next.id);
          htab_put graph stmt.id [next.id]
      done;
      (* Flow last node to nowhere. *)
      if len > 0
      then htab_put graph stmts.(len-1).id []
  in

  let visit_stmt_pre s =
    (* Sequence the prelude nodes on special stmts. *)
    begin
      match s.node with
          Ast.STMT_while sw ->
            let (stmts, _) = sw.Ast.while_lval in
              visit_stmts stmts
        | _ -> ()
    end;
    inner.Walk.visit_stmt_pre s
  in

  let visit_block_pre b =
    visit_stmts b.node;
    inner.Walk.visit_block_pre b
  in

    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre;
        Walk.visit_block_pre = visit_block_pre }
;;

let add_flow_edges (graph:node_graph) (n:node_id) (dsts:node_id list) : unit =
  let existing = Hashtbl.find graph n in
    Hashtbl.replace graph n (lset_union existing dsts)
;;

let remove_flow_edges (graph:node_graph) (n:node_id) (dsts:node_id list) : unit =
  let existing = Hashtbl.find graph n in
    Hashtbl.replace graph n (lset_diff existing dsts)
;;

let graph_general_block_structure_building_visitor
    ((*cx*)_:ctxt)
    (graph:node_graph)
    (inner:Walk.visitor)
    : Walk.visitor =

  let stmts = Stack.create () in

  let visit_stmt_pre s =
    Stack.push s stmts;
    inner.Walk.visit_stmt_pre s
  in

  let visit_stmt_post s =
    inner.Walk.visit_stmt_post s;
    ignore (Stack.pop stmts)
  in

  let visit_block_pre b =
    begin
    let len = Array.length b.node in

    (* Flow container-stmt to block, save existing out-edges for below. *)
    let dsts =
      if Stack.is_empty stmts
      then []
      else
        let s = Stack.top stmts in
        let dsts = Hashtbl.find graph s.id in
          add_flow_edges graph s.id [b.id];
          dsts
    in

      (*
       * If block has len, 
       * then flow block to block.node.(0) and block.node.(len-1) to dsts
       * else flow block to dsts
       * 
       * so AST:
       * 
       *   block#n{ stmt#0 ... stmt#k };
       *   stmt#j;
       * 
       * turns into graph:
       * 
       *   block#n -> stmt#0 -> ... -> stmt#k -> stmt#j
       * 
       *)

      if len > 0
      then
        begin
          htab_put graph b.id [b.node.(0).id];
          add_flow_edges graph b.node.(len-1).id dsts
        end
      else
        htab_put graph b.id dsts
    end;
    inner.Walk.visit_block_pre b
  in

    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre;
        Walk.visit_stmt_post = visit_stmt_post;
        Walk.visit_block_pre = visit_block_pre }
;;


let graph_special_block_structure_building_visitor
    ((*cx*)_:ctxt)
    (graph:(node_id, (node_id list)) Hashtbl.t)
    (inner:Walk.visitor)
    : Walk.visitor =

  let visit_stmt_pre s =
    begin
      match s.node with

        | Ast.STMT_if sif ->
            (* Drop implicit stmt-bypass edge(s); can only flow to inner block(s). *)
            let block_ids =
              [sif.Ast.if_then.id] @
                match sif.Ast.if_else with
                    None -> []
                  | Some eb -> [eb.id]
            in
              Hashtbl.replace graph s.id block_ids

        | Ast.STMT_while sw ->
            (* There are a bunch of rewirings to do on 'while' nodes. *)

            begin
              let dsts = Hashtbl.find graph s.id in
              let body = sw.Ast.while_body in
              let succ_stmts = List.filter (fun x -> not (x = body.id)) dsts in

              let (pre_loop_stmts, _) = sw.Ast.while_lval in
              let loop_head_id =
                (* Splice loop prelude into flow graph, save loop-head node. *)
                let slen = Array.length pre_loop_stmts in
                  if slen > 0
                  then
                    begin
                      remove_flow_edges graph s.id [body.id];
                      add_flow_edges graph s.id [pre_loop_stmts.(0).id];
                      add_flow_edges graph pre_loop_stmts.(slen-1).id [body.id];
                      pre_loop_stmts.(slen - 1).id
                    end
                  else
                    body.id
              in

                (* Always flow s into the loop prelude; prelude may end loop. *)
                remove_flow_edges graph s.id succ_stmts;
                add_flow_edges graph loop_head_id succ_stmts;

                (* Flow loop-end to loop-head. *)
                let blen = Array.length body.node in
                  if blen > 0
                  then add_flow_edges graph body.node.(blen - 1).id [loop_head_id]
                  else add_flow_edges graph body.id [loop_head_id]
            end

        | Ast.STMT_alt_tag at ->
            let dsts = Hashtbl.find graph s.id in
            let arm_blocks =
              let arm_block_id { node = (_, _, block) } = block.id in
              Array.to_list (Array.map arm_block_id at.Ast.alt_tag_arms)
            in
            let succ_stmts = List.filter (fun x -> not (List.mem x arm_blocks)) dsts in
              remove_flow_edges graph s.id succ_stmts

        | _ -> ()
    end;
    inner.Walk.visit_stmt_post s
  in
    { inner with
        Walk.visit_stmt_pre = visit_stmt_pre }
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

let run_dataflow cx graph : unit =
  let roots = find_roots graph in
  let nodes = Queue.create () in
  let progress = ref true in
  let fmt_constr_bitv bitv =
    String.concat ", "
      (List.map
         (fun i -> fmt_constr_key cx (Hashtbl.find cx.ctxt_constrs (Constr i)))
         (Bits.to_list bitv))
  in
  let set_bits dst src =
    if Bits.copy dst src
    then (progress := true;
          iflog cx (fun _ -> log cx "made progress setting bits"))
  in
  let intersect_bits dst src =
    if Bits.intersect dst src
    then (progress := true;
          iflog cx (fun _ -> log cx
                      "made progress intersecting bits"))
  in
  let raise_bits dst src =
    if Bits.union dst src
    then (progress := true;
          iflog cx (fun _ -> log cx
                      "made progress unioning bits"))
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
                       (match htab_search cx.ctxt_all_stmts node with
                            None -> "??"
                          | Some stmt -> Ast.fmt_to_str Ast.fmt_stmt stmt));
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
      List.iter
        (fun i ->
           if not (Bits.get prestate i)
           then
             let ckey = Hashtbl.find cx.ctxt_constrs (Constr i) in
             let constr_str = fmt_constr_key cx ckey in
               err (Some s.id)
                 "Unsatisfied precondition constraint %s at stmt %d: %s"
                 constr_str
                 (int_of_node s.id)
                 (Ast.fmt_to_str Ast.fmt_stmt
                    (Hashtbl.find cx.ctxt_all_stmts s.id)))
        (Bits.to_list precond);
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
                (not (Bits.get prestate i)) && (Bits.get poststate i)
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
  let path = Stack.create () in
  let (scopes:(scope list) ref) = ref [] in
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
      (graph_sequence_building_visitor cx graph
         Walk.empty_visitor);
      (graph_general_block_structure_building_visitor cx graph
         Walk.empty_visitor);
      (graph_special_block_structure_building_visitor cx graph
         Walk.empty_visitor);
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
    run_passes cx "typestate setup" path setup_passes (log cx "%s") crate;
    run_dataflow cx graph;
    run_passes cx "typestate verify" path verify_passes (log cx "%s") crate;
    run_passes cx "typestate aux" path aux_passes (log cx "%s") crate
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
