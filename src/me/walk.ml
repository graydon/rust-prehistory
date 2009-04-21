
open Common;;

(*
 * The purpose of this module is just to decouple the AST from the
 * various passes that are interested in visiting "parts" of it.
 * If the AST shifts, we have better odds of the shift only affecting
 * this module rather than all of its clients. Similarly if the
 * clients only need to visit part, they only have to define the
 * part of the walk they're interested in, making it cheaper to define
 * multiple passes.
 *)

type visitor =
    {
      visit_stmt_pre: Ast.stmt -> unit;
      visit_stmt_post: Ast.stmt -> unit;
      visit_slot_identified_pre: (Ast.slot identified) -> unit;
      visit_slot_identified_post: (Ast.slot identified) -> unit;
      visit_expr_pre: Ast.expr -> unit;
      visit_expr_post: Ast.expr -> unit;
      visit_ty_pre: Ast.ty -> unit;
      visit_ty_post: Ast.ty -> unit;
      visit_constr_pre: Ast.constr -> unit;
      visit_constr_post: Ast.constr -> unit;
      visit_block_pre: Ast.block -> unit;
      visit_block_post: Ast.block -> unit;
      visit_lit_pre: Ast.lit -> unit;
      visit_lit_post: Ast.lit -> unit;
      visit_lval_pre: Ast.lval -> unit;
      visit_lval_post: Ast.lval -> unit;
      visit_mod_item_pre:
        Ast.ident -> ((Ast.ty_limit * Ast.ident) array) -> Ast.mod_item -> unit;
      visit_mod_item_post:
        Ast.ident -> ((Ast.ty_limit * Ast.ident) array) -> Ast.mod_item -> unit;
      visit_mod_type_item_pre:
        Ast.ident -> ((Ast.ty_limit * Ast.ident) array) -> Ast.mod_type_item -> unit;
      visit_mod_type_item_post:
        Ast.ident -> ((Ast.ty_limit * Ast.ident) array) -> Ast.mod_type_item -> unit;
    }
;;


let empty_visitor =
  { visit_stmt_pre = (fun _ -> ());
    visit_stmt_post = (fun _ -> ());
    visit_slot_identified_pre = (fun _ -> ());
    visit_slot_identified_post = (fun _ -> ());
    visit_expr_pre = (fun _ -> ());
    visit_expr_post = (fun _ -> ());
    visit_ty_pre = (fun _ -> ());
    visit_ty_post = (fun _ -> ());
    visit_constr_pre = (fun _ -> ());
    visit_constr_post = (fun _ -> ());
    visit_block_pre = (fun _ -> ());
    visit_block_post = (fun _ -> ());
    visit_lit_pre = (fun _ -> ());
    visit_lit_post = (fun _ -> ());
    visit_lval_pre = (fun _ -> ());
    visit_lval_post = (fun _ -> ());
    visit_mod_item_pre = (fun _ _ _ -> ());
    visit_mod_item_post = (fun _ _ _ -> ());
    visit_mod_type_item_pre = (fun _ _ _ -> ());
    visit_mod_type_item_post = (fun _ _ _ -> ()); }
;;


let mod_item_logging_visitor
    (logfn:string->unit)
    (inner:visitor)
    : visitor =
  let names = Stack.create () in
  let visit_mod_item_pre name params item =
    Stack.push name names;
    logfn (Printf.sprintf "entering %s" (String.concat "." (stk_elts_from_bot names)));
    inner.visit_mod_item_pre name params item
  in
  let visit_mod_item_post name params item =
    logfn (Printf.sprintf "leaving %s" (String.concat "." (stk_elts_from_bot names)));
    inner.visit_mod_item_post name params item;
    ignore (Stack.pop names)
  in
    { inner with
        visit_mod_item_pre = visit_mod_item_pre;
        visit_mod_item_post = visit_mod_item_post }
;;


let walk_bracketed
    (pre:'a -> unit)
    (children:unit -> unit)
    (post:'a -> unit)
    (x:'a)
    : unit =
  begin
    pre x;
    children ();
    post x
  end
;;


let walk_option
    (walker:'a -> unit)
    (opt:'a option)
    : unit =
  match opt with
      None -> ()
    | Some v -> walker v
;;


let rec walk_mod_items
    (v:visitor)
    (items:Ast.mod_items)
    : unit =
  Hashtbl.iter (walk_mod_item v) items


and walk_mod_item
    (v:visitor)
    (name:Ast.ident)
    (item:Ast.mod_item)
    : unit =
  let (params,children) =
    match item.node with
        Ast.MOD_ITEM_opaque_type td ->
          (td.Ast.decl_params, (fun _ -> walk_ty v td.Ast.decl_item))
      | Ast.MOD_ITEM_public_type td ->
          (td.Ast.decl_params, (fun _ -> walk_ty v td.Ast.decl_item))
      | Ast.MOD_ITEM_pred pd ->
          (pd.Ast.decl_params, (fun _ -> walk_pred v pd.Ast.decl_item))
      | Ast.MOD_ITEM_mod md ->
          (md.Ast.decl_params, (fun _ -> walk_mod_items v md.Ast.decl_item))
      | Ast.MOD_ITEM_fn fd ->
          (fd.Ast.decl_params, (fun _ -> walk_fn v fd.Ast.decl_item))
      | Ast.MOD_ITEM_prog pd ->
          (pd.Ast.decl_params, (fun _ -> walk_prog v pd.Ast.decl_item))
  in
    walk_bracketed
      (v.visit_mod_item_pre name params)
      children
      (v.visit_mod_item_post name params)
      item


and walk_ty
    (v:visitor)
    (ty:Ast.ty)
    : unit =
  let walk_ttag ttag =
    Hashtbl.iter (fun _ t -> walk_ty v t) ttag
  in
  let children _ =
    match ty with
        Ast.TY_tup ttup -> Array.iter (walk_slot v) ttup
      | Ast.TY_vec t -> walk_ty v t
      | Ast.TY_rec trec -> Array.iter (fun (_, s) -> walk_slot v s) trec
      | Ast.TY_tag ttag -> walk_ttag ttag
      | Ast.TY_iso tiso -> Array.iter walk_ttag tiso.Ast.iso_group
      | Ast.TY_fn tfn -> walk_ty_fn v tfn
      | Ast.TY_pred (slots, constrs) ->
          begin
            Array.iter (walk_slot v) slots;
            walk_constrs v constrs
          end
      | Ast.TY_chan t -> walk_ty v t
      | Ast.TY_port t -> walk_ty v t
      | Ast.TY_mod mt -> walk_mod_type_items v mt
      | Ast.TY_prog tp -> walk_ty_prog v tp
      | Ast.TY_constrained (t,cs) ->
          begin
            walk_ty v t;
            walk_constrs v cs
          end
      | Ast.TY_lim t -> walk_ty v t
      | Ast.TY_named _ -> ()
      | Ast.TY_opaque _ -> ()
      | Ast.TY_idx _ -> ()
      | Ast.TY_mach _ -> ()
      | Ast.TY_type -> ()
      | Ast.TY_str -> ()
      | Ast.TY_char -> ()
      | Ast.TY_int -> ()
      | Ast.TY_bool -> ()
      | Ast.TY_nil -> ()
      | Ast.TY_any -> ()
  in
    walk_bracketed
      v.visit_ty_pre
      children
      v.visit_ty_post
      ty

and walk_ty_sig
    (v:visitor)
    (s:Ast.ty_sig)
    : unit =
  begin
    Array.iter (walk_slot v) s.Ast.sig_input_slots;
    walk_constrs v s.Ast.sig_input_constrs;
    walk_slot v s.Ast.sig_output_slot;
  end


and walk_ty_fn
    (v:visitor)
    (tfn:Ast.ty_fn)
    : unit =
  let (tsig, taux) = tfn in
  walk_ty_sig v tsig


and walk_ty_prog
    (v:visitor)
    (tprog:Ast.ty_prog)
    : unit =
  walk_option (Array.iter (walk_slot v)) tprog


and walk_mod_type_item
    (v:visitor)
    (name:Ast.ident)
    (item:Ast.mod_type_item)
    : unit =
  let (params,children) =
    match item.node with
        Ast.MOD_TYPE_ITEM_opaque_type td ->
          (td.Ast.decl_params, (fun _ -> ()))
      | Ast.MOD_TYPE_ITEM_public_type td ->
          (td.Ast.decl_params, (fun _ -> walk_ty v td.Ast.decl_item))
      | Ast.MOD_TYPE_ITEM_pred pd ->
          let (slots, constrs) = pd.Ast.decl_item in
            (pd.Ast.decl_params, (fun _ -> (Array.iter (walk_slot v) slots;
                                            walk_constrs v constrs)))
      | Ast.MOD_TYPE_ITEM_mod md ->
          (md.Ast.decl_params, (fun _ -> walk_mod_type_items v md.Ast.decl_item))
      | Ast.MOD_TYPE_ITEM_fn fd ->
          (fd.Ast.decl_params, (fun _ -> walk_ty_fn v fd.Ast.decl_item))
      | Ast.MOD_TYPE_ITEM_prog pd ->
          (pd.Ast.decl_params, (fun _ -> walk_ty_prog v pd.Ast.decl_item))
  in
    walk_bracketed
      (v.visit_mod_type_item_pre name params)
      children
      (v.visit_mod_type_item_post name params)
      item


and walk_mod_type_items
    (v:visitor)
    (items:Ast.mod_type_items)
    : unit =
  Hashtbl.iter (walk_mod_type_item v) items


and walk_constrs
    (v:visitor)
    (cs:Ast.constrs)
    : unit =
  Array.iter (walk_constr v) cs


and walk_constr
    (v:visitor)
    (c:Ast.constr)
    : unit =
  walk_bracketed
    v.visit_constr_pre
    (fun _ -> ())
    v.visit_constr_post
    c


and walk_pred
    (v:visitor)
    (p:Ast.pred)
    : unit =
  Array.iter (fun (s,_) -> walk_slot_identified v s) p.Ast.pred_input_slots;
  walk_constrs v p.Ast.pred_input_constrs;
  walk_block v p.Ast.pred_body


and walk_fn
    (v:visitor)
    (f:Ast.fn)
    : unit =
  Array.iter (fun (s,_) -> walk_slot_identified v s) f.Ast.fn_input_slots;
  walk_constrs v f.Ast.fn_input_constrs;
  walk_slot_identified v f.Ast.fn_output_slot;
  walk_block v f.Ast.fn_body


and walk_init
    (v:visitor)
    (i:Ast.init identified)
    : unit =
  Array.iter (fun (s,_) -> walk_slot_identified v s) i.node.Ast.init_input_slots;
  walk_block v i.node.Ast.init_body


and walk_prog
    (v:visitor)
    (p:Ast.prog)
    : unit =
  walk_option (walk_init v) p.Ast.prog_init;
  walk_option (walk_block v) p.Ast.prog_main;
  walk_option (walk_block v) p.Ast.prog_fini;
  walk_mod_items v p.Ast.prog_mod


and walk_slot_identified
    (v:visitor)
    (s:Ast.slot identified)
    : unit =
  walk_bracketed
    v.visit_slot_identified_pre
    (fun _ -> walk_slot v s.node)
    v.visit_slot_identified_post
    s


and walk_slot
    (v:visitor)
    (s:Ast.slot)
    : unit =
  walk_option (walk_ty v) s.Ast.slot_ty


and walk_stmt
    (v:visitor)
    (s:Ast.stmt)
    : unit =
  let walk_stmt_while
      (s:Ast.stmt_while)
      : unit =
    let (ss,a) = s.Ast.while_lval in
      Array.iter (walk_stmt v) ss;
      walk_atom v a;
      walk_block v s.Ast.while_body
  in
  let children _ =
    match s.node with
        Ast.STMT_log a ->
          walk_atom v a

      | Ast.STMT_spawn a ->
          walk_atom v a

      | Ast.STMT_init_rec (lv, atab) ->
          walk_lval v lv;
          Array.iter (fun (_, a) -> walk_atom v a) atab

      | Ast.STMT_init_vec (lv, atoms) ->
          walk_lval v lv;
          Array.iter (walk_atom v) atoms

      | Ast.STMT_init_tup (lv, atoms) ->
          walk_lval v lv;
          Array.iter (walk_atom v) atoms

      | Ast.STMT_while w ->
          walk_stmt_while w

      | Ast.STMT_do_while w ->
          walk_stmt_while w

      | Ast.STMT_if i ->
          begin
            walk_atom v i.Ast.if_test;
            walk_block v i.Ast.if_then;
            walk_option (walk_block v) i.Ast.if_else
          end

      | Ast.STMT_block b ->
          walk_block v b

      | Ast.STMT_copy (lv,e) ->
          walk_lval v lv;
          walk_expr v e

      | Ast.STMT_call (dst,f,az) ->
          walk_lval v dst;
          walk_lval v f;
          Array.iter (walk_atom v) az

      | Ast.STMT_ret (_, ao) ->
          walk_option (walk_atom v) ao

      | Ast.STMT_put (_, at) ->
          walk_option (walk_atom v) at

      (* FIXME: this should have a param array, and invoke the visitors. *)
      | Ast.STMT_decl (Ast.DECL_mod_item (id, mi)) ->
          walk_mod_item v id mi

      | Ast.STMT_decl (Ast.DECL_slot (_, slot)) ->
          walk_slot_identified v slot

      | Ast.STMT_send (lv,at) ->
          walk_lval v lv;
          walk_atom v at

      | Ast.STMT_recv (dst,src) ->
          walk_lval v dst;
          walk_lval v src

      | Ast.STMT_be (_, lv, ats) ->
          walk_lval v lv;
          Array.iter (walk_atom v) ats

      | Ast.STMT_check_expr at ->
          walk_atom v at

      | Ast.STMT_check cs ->
          walk_constrs v cs

      | Ast.STMT_check_if (cs,b) ->
          walk_constrs v cs;
          walk_block v b

      | Ast.STMT_prove cs ->
          walk_constrs v cs

      (* FIXME: finish this as needed. *)
      | Ast.STMT_foreach f -> ()
      | Ast.STMT_for f -> ()
      | Ast.STMT_try t -> ()
      | Ast.STMT_alt_tag sat -> ()
      | Ast.STMT_alt_type sat -> ()
      | Ast.STMT_alt_port sap -> ()
      | Ast.STMT_use _ -> ()
  in
    walk_bracketed
      v.visit_stmt_pre
      children
      v.visit_stmt_post
      s


and walk_expr
    (v:visitor)
    (e:Ast.expr)
    : unit =
  let children _ =
    match e with
        Ast.EXPR_binary (_,aa,ab) ->
          walk_atom v aa;
          walk_atom v ab
      | Ast.EXPR_unary (_,a) ->
          walk_atom v a
      | Ast.EXPR_atom a ->
          walk_atom v a
  in
  walk_bracketed
    v.visit_expr_pre
    children
    v.visit_expr_post
    e


and walk_atom
    (v:visitor)
    (a:Ast.atom)
    : unit =
  match a with
      Ast.ATOM_literal ls -> walk_lit v ls.node
    | Ast.ATOM_lval lv -> walk_lval v lv


and walk_lit
    (v:visitor)
    (li:Ast.lit)
    : unit =
  walk_bracketed
    v.visit_lit_pre
    (fun _ -> ())
    v.visit_lit_post
    li


and walk_lval
    (v:visitor)
    (lv:Ast.lval)
    : unit =
  walk_bracketed
    v.visit_lval_pre
    (fun _ -> ())
    v.visit_lval_post
    lv


and walk_block
    (v:visitor)
    (b:Ast.block)
    : unit =
  walk_bracketed
    v.visit_block_pre
    (fun _ -> (Array.iter (walk_stmt v) b.node))
    v.visit_block_post
    b


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
