open Common;;
open Semant;;

type tyspec =
    TYSPEC_equiv of tyvar
  | TYSPEC_all
  | TYSPEC_resolved of Ast.ty
  | TYSPEC_callable of (tyvar * tyvar array)   (* out, ins *)
  | TYSPEC_collection of tyvar                 (* homogeneous ordered collection *)
  | TYSPEC_comparable                          (* comparable with = and != *)
  | TYSPEC_plusable                            (* nums, vecs, and strings *)
  | TYSPEC_dictionary of dict
  | TYSPEC_integral                            (* integer-like *)
  | TYSPEC_loggable
  | TYSPEC_numeric                             (* integer-like or float-like *)
  | TYSPEC_ordered                             (* comparable with < and friends *)
  | TYSPEC_record of dict
  | TYSPEC_tuple of tyvar array                (* heterogeneous tuple *)
  | TYSPEC_vector of tyvar
  | TYSPEC_parametric of (tyvar * typarams)

and typarams = ((tyvar array) option) ref

and dict = (Ast.ident, tyvar) Hashtbl.t

and tyvar = tyspec ref;;

(* Signatures for binary operators. *)
type binopsig =
    BINOPSIG_bool_bool_bool     (* bool * bool -> bool *)
  | BINOPSIG_comp_comp_bool     (* comparable a * comparable a -> bool *)
  | BINOPSIG_ord_ord_bool       (* ordered a * ordered a -> bool *)
  | BINOPSIG_integ_integ_integ  (* integral a * integral a -> integral a *)
  | BINOPSIG_num_num_num        (* numeric a * numeric a -> numeric a *)
  | BINOPSIG_plus_plus_plus     (* plusable a * plusable a -> plusable a *)
;;

let rec tyspec_to_str (ts:tyspec) : string =

  let join (strings:string list) = String.concat ", " strings in

  let format_dict (dct:dict) =
    let format_name ident tv tail =
      (ident ^ " : " ^ (tyspec_to_str !tv))::tail
    in
      join (Hashtbl.fold format_name dct [])
  in

  let join_tvs tvs =
    let tv_to_str tv = tyspec_to_str !tv in
      join (Array.to_list (Array.map tv_to_str tvs))
  in
    match ts with
        TYSPEC_equiv tv -> ":" ^ (tyspec_to_str !tv)
      | TYSPEC_all -> "<?>"
      | TYSPEC_resolved ty -> Ast.sprintf_ty () ty
      | TYSPEC_callable (out, ins) ->
          Printf.sprintf "<callable fn(%s) -> %s>"
            (join_tvs ins) (tyspec_to_str !out)
      | TYSPEC_collection tv -> "<collection of " ^ (tyspec_to_str !tv) ^ ">"
      | TYSPEC_comparable -> "<comparable>"
      | TYSPEC_plusable -> "<plusable>"
      | TYSPEC_dictionary dct ->
          "<dictionary with members " ^ (format_dict dct) ^ ">"
      | TYSPEC_integral -> "<integral>"
      | TYSPEC_loggable -> "<loggable>"
      | TYSPEC_numeric -> "<numeric>"
      | TYSPEC_ordered -> "<ordered>"
      | TYSPEC_parametric (tv, params) ->
          begin
            match !params with
                None ->
                  Printf.sprintf "<parametric (%s)[?]>" (tyspec_to_str !tv)
              | Some tvs ->
                  Printf.sprintf "<parametric (%s)[%s]>" (tyspec_to_str !tv) (join_tvs tvs)
          end
      | TYSPEC_record dct -> "<record with fields " ^ (format_dict dct) ^ ">"
      | TYSPEC_tuple tvs -> "(" ^ (join_tvs tvs) ^ ")"
      | TYSPEC_vector tv -> "vec[" ^ (tyspec_to_str !tv) ^ "]"
;;

let iflog cx thunk =
  if cx.ctxt_sess.Session.sess_log_type
  then thunk ()
  else ()
;;

let process_crate (cx:ctxt) (crate:Ast.crate) : unit =
  let log cx = Session.log "type"
    cx.ctxt_sess.Session.sess_log_type
    cx.ctxt_sess.Session.sess_log_out
  in
  let retval_tv_r = ref (ref TYSPEC_all) in
  let (bindings:(node_id, tyvar) Hashtbl.t) = Hashtbl.create 10 in
  let (item_params:(node_id, tyvar array) Hashtbl.t) = Hashtbl.create 10 in
  let (lval_tyvars:(node_id, tyvar) Hashtbl.t) = Hashtbl.create 0 in
  let rec resolve_tyvar (tv:tyvar) : tyvar =
    match !tv with
        TYSPEC_equiv subtv -> resolve_tyvar subtv
      | _ -> tv
  in
  let visitor (cx:ctxt) (inner:Walk.visitor) : Walk.visitor =

    (* 
     * The stack of ty_params we're visiting while *walking*; new param sets get pushed
     * on here and popped off as walking traverses through a parametric mod_item.
     *)
    let (item_params_stk:((Ast.ty_param identified) array) Stack.t) = Stack.create () in

    (* 
     * The stack of typarams we're visiting while *unifying*; new param sets get pushed
     * on here and popped off as unification traverses through a TYSPEC_parametric.
     *)
    let (spec_params_stk:typarams Stack.t) = Stack.create () in

    let rec unify_slot
        (slot:Ast.slot)
        (id_opt:node_id option)
        (tv:tyvar) : unit =
      match id_opt with
          Some id -> unify_tyvars (Hashtbl.find bindings id) tv
        | None ->
            match slot.Ast.slot_ty with
                None -> bug () "untyped unidentified slot"
              | Some ty -> unify_ty ty tv

    and check_sane_tyvar tv =
      match !tv with
          TYSPEC_resolved (Ast.TY_named _) -> bug () "named-type in type checker"
        | _ -> ()

    and unify_tyvars  (av:tyvar) (bv:tyvar) : unit =
      iflog cx (fun _ ->
                  log cx "unifying types: %s vs. %s"
                    (tyspec_to_str !av) (tyspec_to_str !bv));
      check_sane_tyvar av;
      check_sane_tyvar bv;
      unify_tyvars' av bv;
      iflog cx (fun _ ->
                  log cx "    unified to: %s and %s"
                    (tyspec_to_str !av) (tyspec_to_str !bv));
      check_sane_tyvar av;
      check_sane_tyvar bv;

    and unify_tyvars' (av:tyvar) (bv:tyvar) : unit =
      let (a, b) = ((resolve_tyvar av), (resolve_tyvar bv)) in
      let fail () =
        err None "mismatched types: %s vs. %s" (tyspec_to_str !av)
          (tyspec_to_str !bv);
      in

      let unify_typarams (ap:typarams) (bp:typarams) : unit =
        match (!ap, !bp) with
            (None, Some b) -> ap := Some b
          | (Some a, None) -> bp := Some a
          | (Some a, Some b) ->
              if Array.length a != Array.length b
              then fail ()
              else Array.iteri (fun i av -> unify_tyvars av b.(i)) a
          | (None, None) -> ()
      in

      let merge_dicts a b =
        let c = Hashtbl.create ((Hashtbl.length a) + (Hashtbl.length b)) in
        let merge ident tv_a =
          if Hashtbl.mem b ident
          then unify_tyvars (Hashtbl.find b ident) tv_a;
          Hashtbl.add c ident tv_a
        in
          Hashtbl.iter merge a;
          c
      in

      let unify_dict_with_record_fields (dct:dict) (fields:Ast.ty_rec) : unit =
        let rec find_slot (query:Ast.ident) i : Ast.slot =
          if i = Array.length fields
          then fail ()
          else match fields.(i) with
              (ident, slot) ->
                if ident = query then slot
                else find_slot query (i + 1)
        in

        let check_entry ident tv =
          unify_slot (find_slot ident 0) None tv
        in
          Hashtbl.iter check_entry dct
      in

      let unify_dict_with_obj_fns
          (dct:dict)
          (fns:(Ast.ident,Ast.ty_fn) Hashtbl.t) : unit =
        let check_entry (query:Ast.ident) tv : unit =
          match htab_search fns query with
              None -> fail ()
            | Some fn -> unify_ty (Ast.TY_fn fn) tv
        in
          Hashtbl.iter check_entry dct
      in

      let rec is_comparable_or_ordered (comparable:bool) (ty:Ast.ty) : bool =
        match ty with
            Ast.TY_mach _ | Ast.TY_int | Ast.TY_char | Ast.TY_str -> true
          | Ast.TY_any | Ast.TY_nil | Ast.TY_bool | Ast.TY_chan _
          | Ast.TY_port _ | Ast.TY_proc | Ast.TY_tup _ | Ast.TY_vec _
          | Ast.TY_rec _ | Ast.TY_tag _ | Ast.TY_iso _ | Ast.TY_idx _ ->
              comparable
          | Ast.TY_fn _ | Ast.TY_pred _ | Ast.TY_obj _ | Ast.TY_opaque _
          | Ast.TY_param _ | Ast.TY_type -> false
          | Ast.TY_named _ -> bug () "is_comparable_or_ordered: TY_named TODO"
          | Ast.TY_constrained (ty, _) ->
              is_comparable_or_ordered comparable ty
      in

      let floating (ty:Ast.ty) : bool =
        match ty with
            Ast.TY_mach TY_f32 | Ast.TY_mach TY_f64 -> true
          | _ -> false
      in

      let integral (ty:Ast.ty) : bool =
        match ty with
            Ast.TY_int | Ast.TY_mach TY_u8 | Ast.TY_mach TY_u16
          | Ast.TY_mach TY_u32 | Ast.TY_mach TY_u64 | Ast.TY_mach TY_s8
          | Ast.TY_mach TY_s16 | Ast.TY_mach TY_s32
          | Ast.TY_mach TY_s64 ->
              true
          | _ -> false
      in

      let numeric (ty:Ast.ty) : bool = (integral ty) || (floating ty) in

      let plusable (ty:Ast.ty) : bool =
        match ty with
            Ast.TY_str -> true
          | _ -> numeric ty
      in

      let loggable (ty:Ast.ty) : bool =
        match ty with
            Ast.TY_str | Ast.TY_bool | Ast.TY_int | Ast.TY_char | Ast.TY_mach TY_u8
          | Ast.TY_mach TY_u16 | Ast.TY_mach TY_u32 | Ast.TY_mach TY_s8
          | Ast.TY_mach TY_s16 | Ast.TY_mach TY_s32 -> true
          | _ -> false
      in

      let rebuild_ty_under_params ty params =
        let unresolved = ref false in
        let substituted = ref false in
        let base = ty_fold_rebuild (fun t -> t) in
        let ty_fold_param (i, oid, mut) =
          let ty = Ast.TY_param (i, oid, mut) in
            match !params with
                None -> unresolved := true; ty
              | Some params ->
                  if i < Array.length params
                  then
                    match !(resolve_tyvar params.(i)) with
                        TYSPEC_resolved ty -> substituted := true; ty
                      | _ -> unresolved := true; ty
                    else
                      (unresolved := true; ty)
        in
        let fold = { base with ty_fold_param = ty_fold_param } in
        let ty' = fold_ty fold ty in
          (* 
           * FIXME: "substituted" and "ty'" here are only required
           * because the current equality-comparison code uses <> and
           * will judge some cases, such as rebuilt tags, as unequal
           * simply due to the different hashtable order in the
           * fold.
           *)
          if !substituted
          then (ty', !unresolved)
          else (ty, !unresolved)
      in

      let result =
        match (!a, !b) with
            (TYSPEC_equiv _, _) | (_, TYSPEC_equiv _) ->
              bug () "equiv found even though tyvar was resolved"

          | (TYSPEC_all, other) | (other, TYSPEC_all) -> other

          (* resolved *)

          | (TYSPEC_resolved ty_a, TYSPEC_resolved ty_b) ->
              let param oid n =
                let spec_ty_param _ =
                  let params =
                    if Stack.is_empty spec_params_stk
                    then bug () "indexed spec-param access outside parametric type"
                    else
                      Stack.top spec_params_stk
                  in
                    match !params with
                        None -> fail ()
                      | Some params ->
                          if n < Array.length params
                          then params.(n)
                          else fail()
                in
                  if not (Stack.is_empty item_params_stk)
                  then
                    let item_params = Stack.top item_params_stk in
                    let search_param_by_oid (_:int) (param:Ast.ty_param identified) : Ast.ty option =
                      let (_, (i, oid', mut)) = param.node in
                      if oid = oid'
                      then Some (Ast.TY_param (i, oid, mut))
                      else None
                    in
                      match arr_search item_params search_param_by_oid with
                          Some t -> ref (TYSPEC_resolved t)
                        | None -> spec_ty_param ()
                  else
                    spec_ty_param ()
              in
                begin
                  match ty_a, ty_b with
                      (Ast.TY_param (m,moid,_), Ast.TY_param (n,noid,_)) ->
                        if m != n || moid != noid
                        then fail ()
                        else !(param noid n)
                    | (Ast.TY_param (n,noid,_), _) ->
                        unify_ty ty_b (param noid n);
                        TYSPEC_resolved ty_b
                    | (_, Ast.TY_param (n,noid,_)) ->
                        unify_ty ty_a (param noid n);
                        TYSPEC_resolved ty_a
                    | _ ->
                        if ty_a <> ty_b
                        then fail ()
                        else TYSPEC_resolved ty_a
                end

          | (TYSPEC_resolved ty, TYSPEC_callable (out_tv, in_tvs))
          | (TYSPEC_callable (out_tv, in_tvs), TYSPEC_resolved ty) ->
              let unify_in_slot i in_slot =
                unify_slot in_slot None in_tvs.(i)
              in
                begin
                  match ty with
                      Ast.TY_fn ({
                                   Ast.sig_input_slots = in_slots;
                                   Ast.sig_output_slot = out_slot
                                 }, _) ->
                        if Array.length in_slots != Array.length in_tvs
                        then fail ();
                        unify_slot out_slot None out_tv;
                        Array.iteri unify_in_slot in_slots
                    | Ast.TY_pred (slots, _) ->
                        unify_ty Ast.TY_bool out_tv;
                        Array.iteri unify_in_slot slots
                    | _ -> fail ()
                end;
                TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_collection tv)
          | (TYSPEC_collection tv, TYSPEC_resolved ty) ->
              begin
                match ty with
                    Ast.TY_vec slot -> unify_slot slot None tv
                  | Ast.TY_str -> unify_ty (Ast.TY_mach TY_u8) tv
                  | _ -> fail ()
              end;
              TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_comparable)
          | (TYSPEC_comparable, TYSPEC_resolved ty) ->
              if not (is_comparable_or_ordered true ty) then fail ()
              else TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_plusable)
          | (TYSPEC_plusable, TYSPEC_resolved ty) ->
              if not (plusable ty) then fail ()
              else TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_dictionary dct)
          | (TYSPEC_dictionary dct, TYSPEC_resolved ty) ->
              begin
                match ty with
                    Ast.TY_rec fields ->
                      unify_dict_with_record_fields dct fields
                  | Ast.TY_obj fns ->
                      unify_dict_with_obj_fns dct fns
                  | _ -> fail ()
              end;
              TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_integral)
          | (TYSPEC_integral, TYSPEC_resolved ty) ->
              if not (integral ty) then fail () else TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_loggable)
          | (TYSPEC_loggable, TYSPEC_resolved ty) ->
              if not (loggable ty) then fail () else TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_numeric)
          | (TYSPEC_numeric, TYSPEC_resolved ty) ->
              if not (numeric ty) then fail ()
              else TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_ordered)
          | (TYSPEC_ordered, TYSPEC_resolved ty) ->
              if not (is_comparable_or_ordered false ty) then fail ()
              else TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_parametric (tv, params))
          | (TYSPEC_parametric (tv, params), TYSPEC_resolved ty) ->
              let (ty, unresolved) = rebuild_ty_under_params ty params in
                unify_ty ty tv;
                if unresolved
                then TYSPEC_parametric (tv, params)
                else TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_record dct)
          | (TYSPEC_record dct, TYSPEC_resolved ty) ->
              begin
                match ty with
                    Ast.TY_rec fields ->
                      unify_dict_with_record_fields dct fields
                  | _ -> fail ()
              end;
              TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_tuple tvs)
          | (TYSPEC_tuple tvs, TYSPEC_resolved ty) ->
              begin
                match ty with
                    Ast.TY_tup (elem_slots:Ast.slot array) ->
                      if (Array.length elem_slots) < (Array.length tvs)
                      then fail ()
                      else
                        let check_elem i tv =
                          unify_slot (elem_slots.(i)) None tv
                        in
                          Array.iteri check_elem tvs
                  | _ -> fail ()
              end;
              TYSPEC_resolved ty

          | (TYSPEC_resolved ty, TYSPEC_vector tv)
          | (TYSPEC_vector tv, TYSPEC_resolved ty) ->
              begin
                match ty with
                    Ast.TY_vec slot ->
                      unify_slot slot None tv;
                      TYSPEC_resolved ty
                  | _ -> fail ()
              end

          (* callable *)

          | (TYSPEC_callable (a_out_tv, a_in_tvs),
             TYSPEC_callable (b_out_tv, b_in_tvs)) ->
              unify_tyvars a_out_tv b_out_tv;
              let check_in_tv i a_in_tv =
                unify_tyvars a_in_tv b_in_tvs.(i)
              in
                Array.iteri check_in_tv a_in_tvs;
                TYSPEC_callable (a_out_tv, a_in_tvs)

          | (TYSPEC_callable _, TYSPEC_collection _)
          | (TYSPEC_callable _, TYSPEC_comparable)
          | (TYSPEC_callable _, TYSPEC_plusable)
          | (TYSPEC_callable _, TYSPEC_dictionary _)
          | (TYSPEC_callable _, TYSPEC_integral)
          | (TYSPEC_callable _, TYSPEC_loggable)
          | (TYSPEC_callable _, TYSPEC_numeric)
          | (TYSPEC_callable _, TYSPEC_ordered)
          | (TYSPEC_callable _, TYSPEC_parametric _)
          | (TYSPEC_callable _, TYSPEC_record _)
          | (TYSPEC_callable _, TYSPEC_tuple _)
          | (TYSPEC_callable _, TYSPEC_vector _)
          | (TYSPEC_collection _, TYSPEC_callable _)
          | (TYSPEC_comparable, TYSPEC_callable _)
          | (TYSPEC_plusable, TYSPEC_callable _)
          | (TYSPEC_dictionary _, TYSPEC_callable _)
          | (TYSPEC_integral, TYSPEC_callable _)
          | (TYSPEC_loggable, TYSPEC_callable _)
          | (TYSPEC_numeric, TYSPEC_callable _)
          | (TYSPEC_ordered, TYSPEC_callable _)
          | (TYSPEC_parametric _, TYSPEC_callable _)
          | (TYSPEC_record _, TYSPEC_callable _)
          | (TYSPEC_tuple _, TYSPEC_callable _)
          | (TYSPEC_vector _, TYSPEC_callable _) -> fail ()

          (* collection *)

          | (TYSPEC_collection av, TYSPEC_collection bv) ->
              unify_tyvars av bv;
              TYSPEC_collection av

          | (TYSPEC_collection av, TYSPEC_comparable)
          | (TYSPEC_comparable, TYSPEC_collection av) ->
              TYSPEC_collection av

          | (TYSPEC_collection v, TYSPEC_plusable)
          | (TYSPEC_plusable, TYSPEC_collection v) -> TYSPEC_collection v

          | (TYSPEC_collection _, TYSPEC_dictionary _)
          | (TYSPEC_collection _, TYSPEC_integral)
          | (TYSPEC_collection _, TYSPEC_loggable)
          | (TYSPEC_collection _, TYSPEC_numeric)
          | (TYSPEC_collection _, TYSPEC_ordered)
          | (TYSPEC_collection _, TYSPEC_parametric _)
          | (TYSPEC_collection _, TYSPEC_record _)
          | (TYSPEC_collection _, TYSPEC_tuple _)
          | (TYSPEC_dictionary _, TYSPEC_collection _)
          | (TYSPEC_integral, TYSPEC_collection _)
          | (TYSPEC_loggable, TYSPEC_collection _)
          | (TYSPEC_numeric, TYSPEC_collection _)
          | (TYSPEC_ordered, TYSPEC_collection _)
          | (TYSPEC_parametric _, TYSPEC_collection _)
          | (TYSPEC_record _, TYSPEC_collection _)
          | (TYSPEC_tuple _, TYSPEC_collection _) -> fail ()

          | (TYSPEC_collection av, TYSPEC_vector bv)
          | (TYSPEC_vector bv, TYSPEC_collection av) ->
              unify_tyvars av bv;
              TYSPEC_vector av

          (* comparable *)

          | (TYSPEC_comparable, TYSPEC_comparable) -> TYSPEC_comparable

          | (TYSPEC_comparable, TYSPEC_plusable)
          | (TYSPEC_plusable, TYSPEC_comparable) -> TYSPEC_plusable

          | (TYSPEC_comparable, TYSPEC_dictionary dict)
          | (TYSPEC_dictionary dict, TYSPEC_comparable) ->
              TYSPEC_dictionary dict

          | (TYSPEC_comparable, TYSPEC_integral)
          | (TYSPEC_integral, TYSPEC_comparable) -> TYSPEC_integral

          | (TYSPEC_comparable, TYSPEC_loggable)
          | (TYSPEC_loggable, TYSPEC_comparable) -> TYSPEC_loggable

          | (TYSPEC_comparable, TYSPEC_numeric)
          | (TYSPEC_numeric, TYSPEC_comparable) -> TYSPEC_numeric

          | (TYSPEC_comparable, TYSPEC_ordered)
          | (TYSPEC_ordered, TYSPEC_comparable) -> TYSPEC_ordered

          | (TYSPEC_comparable, TYSPEC_parametric _)
          | (TYSPEC_parametric _, TYSPEC_comparable) -> fail ()

          | (TYSPEC_comparable, TYSPEC_record r)
          | (TYSPEC_record r, TYSPEC_comparable) -> TYSPEC_record r

          | (TYSPEC_comparable, TYSPEC_tuple t)
          | (TYSPEC_tuple t, TYSPEC_comparable) -> TYSPEC_tuple t

          | (TYSPEC_comparable, TYSPEC_vector v)
          | (TYSPEC_vector v, TYSPEC_comparable) -> TYSPEC_vector v

          (* plusable *)

          | (TYSPEC_plusable, TYSPEC_plusable) -> TYSPEC_plusable

          | (TYSPEC_plusable, TYSPEC_dictionary _)
          | (TYSPEC_dictionary _, TYSPEC_plusable) -> fail ()

          | (TYSPEC_plusable, TYSPEC_integral)
          | (TYSPEC_integral, TYSPEC_plusable) -> TYSPEC_integral

          | (TYSPEC_plusable, TYSPEC_loggable)
          | (TYSPEC_loggable, TYSPEC_plusable) -> TYSPEC_plusable

          | (TYSPEC_plusable, TYSPEC_numeric)
          | (TYSPEC_numeric, TYSPEC_plusable) -> TYSPEC_numeric

          | (TYSPEC_plusable, TYSPEC_ordered)
          | (TYSPEC_ordered, TYSPEC_plusable) -> TYSPEC_plusable

          | (TYSPEC_plusable, TYSPEC_record _)
          | (TYSPEC_record _, TYSPEC_plusable) -> fail ()

          | (TYSPEC_plusable, TYSPEC_tuple _)
          | (TYSPEC_tuple _, TYSPEC_plusable) -> fail ()

          | (TYSPEC_plusable, TYSPEC_vector v)
          | (TYSPEC_vector v, TYSPEC_plusable) -> TYSPEC_vector v

          | (TYSPEC_plusable, TYSPEC_parametric _)
          | (TYSPEC_parametric _, TYSPEC_plusable) -> fail ()

          (* dictionary *)

          | (TYSPEC_dictionary da, TYSPEC_dictionary db) ->
              TYSPEC_dictionary (merge_dicts da db)

          | (TYSPEC_dictionary _, TYSPEC_integral)
          | (TYSPEC_dictionary _, TYSPEC_loggable)
          | (TYSPEC_dictionary _, TYSPEC_numeric)
          | (TYSPEC_dictionary _, TYSPEC_ordered)
          | (TYSPEC_dictionary _, TYSPEC_parametric _)
          | (TYSPEC_integral, TYSPEC_dictionary _)
          | (TYSPEC_loggable, TYSPEC_dictionary _)
          | (TYSPEC_numeric, TYSPEC_dictionary _)
          | (TYSPEC_ordered, TYSPEC_dictionary _)
          | (TYSPEC_parametric _, TYSPEC_dictionary _) -> fail ()

          | (TYSPEC_dictionary d, TYSPEC_record r)
          | (TYSPEC_record r, TYSPEC_dictionary d) ->
              TYSPEC_record (merge_dicts d r)

          | (TYSPEC_dictionary _, TYSPEC_tuple _)
          | (TYSPEC_dictionary _, TYSPEC_vector _)
          | (TYSPEC_tuple _, TYSPEC_dictionary _)
          | (TYSPEC_vector _, TYSPEC_dictionary _) -> fail ()

          (* integral *)

          | (TYSPEC_integral, TYSPEC_integral)
          | (TYSPEC_integral, TYSPEC_loggable)
          | (TYSPEC_integral, TYSPEC_numeric)
          | (TYSPEC_integral, TYSPEC_ordered)
          | (TYSPEC_loggable, TYSPEC_integral)
          | (TYSPEC_numeric, TYSPEC_integral)
          | (TYSPEC_ordered, TYSPEC_integral) -> TYSPEC_integral

          | (TYSPEC_integral, TYSPEC_parametric _)
          | (TYSPEC_integral, TYSPEC_record _)
          | (TYSPEC_integral, TYSPEC_tuple _)
          | (TYSPEC_integral, TYSPEC_vector _)
          | (TYSPEC_parametric _, TYSPEC_integral)
          | (TYSPEC_record _, TYSPEC_integral)
          | (TYSPEC_tuple _, TYSPEC_integral)
          | (TYSPEC_vector _, TYSPEC_integral) -> fail ()

          (* loggable *)

          | (TYSPEC_loggable, TYSPEC_loggable) -> TYSPEC_loggable

          | (TYSPEC_loggable, TYSPEC_numeric)
          | (TYSPEC_numeric, TYSPEC_loggable) -> TYSPEC_numeric

          | (TYSPEC_loggable, TYSPEC_ordered)
          | (TYSPEC_ordered, TYSPEC_loggable) -> TYSPEC_ordered

          | (TYSPEC_loggable, TYSPEC_parametric _)
          | (TYSPEC_loggable, TYSPEC_record _)
          | (TYSPEC_loggable, TYSPEC_tuple _)
          | (TYSPEC_loggable, TYSPEC_vector _)
          | (TYSPEC_parametric _, TYSPEC_loggable)
          | (TYSPEC_record _, TYSPEC_loggable)
          | (TYSPEC_tuple _, TYSPEC_loggable)
          | (TYSPEC_vector _, TYSPEC_loggable) -> fail ()

          (* numeric *)

          | (TYSPEC_numeric, TYSPEC_numeric) -> TYSPEC_numeric

          | (TYSPEC_numeric, TYSPEC_ordered)
          | (TYSPEC_ordered, TYSPEC_numeric) -> TYSPEC_ordered

          | (TYSPEC_numeric, TYSPEC_parametric _)
          | (TYSPEC_numeric, TYSPEC_record _)
          | (TYSPEC_numeric, TYSPEC_tuple _)
          | (TYSPEC_numeric, TYSPEC_vector _)
          | (TYSPEC_parametric _, TYSPEC_numeric)
          | (TYSPEC_record _, TYSPEC_numeric)
          | (TYSPEC_tuple _, TYSPEC_numeric)
          | (TYSPEC_vector _, TYSPEC_numeric) -> fail ()

          (* ordered *)

          | (TYSPEC_ordered, TYSPEC_ordered) -> TYSPEC_ordered

          | (TYSPEC_ordered, TYSPEC_parametric _)
          | (TYSPEC_ordered, TYSPEC_record _)
          | (TYSPEC_ordered, TYSPEC_tuple _)
          | (TYSPEC_ordered, TYSPEC_vector _)
          | (TYSPEC_parametric _, TYSPEC_ordered)
          | (TYSPEC_record _, TYSPEC_ordered)
          | (TYSPEC_tuple _, TYSPEC_ordered)
          | (TYSPEC_vector _, TYSPEC_ordered) -> fail ()

          (* parametric *)

          | (TYSPEC_parametric (tv_a, params_a),
             TYSPEC_parametric (tv_b, params_b)) ->
              unify_typarams params_a params_b;
              Stack.push params_a spec_params_stk;
              unify_tyvars tv_a tv_b;
              ignore (Stack.pop spec_params_stk);
              begin
                match !(resolve_tyvar tv_a) with
                    TYSPEC_resolved ty ->
                      let (ty, unresolved) = rebuild_ty_under_params ty params_a in
                        if unresolved
                        then TYSPEC_parametric (tv_a, params_a)
                        else TYSPEC_resolved ty
                  | _ -> TYSPEC_parametric (tv_a, params_a)
              end

          | (TYSPEC_parametric _, TYSPEC_record _)
          | (TYSPEC_parametric _, TYSPEC_tuple _)
          | (TYSPEC_parametric _, TYSPEC_vector _)
          | (TYSPEC_record _, TYSPEC_parametric _)
          | (TYSPEC_tuple _, TYSPEC_parametric _)
          | (TYSPEC_vector _, TYSPEC_parametric _) -> fail ()

          (* record *)

          | (TYSPEC_record da, TYSPEC_record db) ->
              TYSPEC_record (merge_dicts da db)

          | (TYSPEC_record _, TYSPEC_tuple _)
          | (TYSPEC_record _, TYSPEC_vector _)
          | (TYSPEC_tuple _, TYSPEC_record _)
          | (TYSPEC_vector _, TYSPEC_record _) -> fail ()

          (* tuple *)

          | (TYSPEC_tuple tvs_a, TYSPEC_tuple tvs_b) ->
              let len_a = Array.length tvs_a in
              let len_b = Array.length tvs_b in
              let max_len = max len_a len_b in
              let init_tuple_elem i =
                if i >= len_a
                then tvs_b.(i)
                else if i >= len_b
                then tvs_a.(i)
                else begin
                  unify_tyvars tvs_a.(i) tvs_b.(i);
                  tvs_a.(i)
                end
              in
                TYSPEC_tuple (Array.init max_len init_tuple_elem)

          | (TYSPEC_tuple _, TYSPEC_vector _)
          | (TYSPEC_vector _, TYSPEC_tuple _) -> fail ()

          (* vector *)

          | (TYSPEC_vector av, TYSPEC_vector bv) ->
              unify_tyvars av bv;
              TYSPEC_vector av
      in
      let c = ref result in
        a := TYSPEC_equiv c;
        b := TYSPEC_equiv c

    and unify_ty (ty:Ast.ty) (tv:tyvar) : unit =
      unify_tyvars (ref (TYSPEC_resolved ty)) tv
    in

    let rec unify_atom (atom:Ast.atom) (tv:tyvar) : unit =
      match atom with
          Ast.ATOM_literal { node = literal; id = _ } ->
            let ty = match literal with
                Ast.LIT_nil -> Ast.TY_nil
              | Ast.LIT_bool _ -> Ast.TY_bool
              | Ast.LIT_mach (mty, _, _) -> Ast.TY_mach mty
              | Ast.LIT_int (_, _) -> Ast.TY_int
              | Ast.LIT_char _ -> Ast.TY_char
              | _ -> bug () "unimplemented atom literal"
            in
              unify_ty ty tv
        | Ast.ATOM_lval lval -> unify_lval lval tv

    and unify_expr (expr:Ast.expr) (tv:tyvar) : unit =
      match expr with
          Ast.EXPR_binary (binop, lhs, rhs) ->
            let binop_sig = match binop with
                Ast.BINOP_or -> BINOPSIG_bool_bool_bool
              | Ast.BINOP_and -> BINOPSIG_bool_bool_bool
              | Ast.BINOP_eq -> BINOPSIG_comp_comp_bool
              | Ast.BINOP_ne -> BINOPSIG_comp_comp_bool
              | Ast.BINOP_lt -> BINOPSIG_ord_ord_bool
              | Ast.BINOP_le -> BINOPSIG_ord_ord_bool
              | Ast.BINOP_ge -> BINOPSIG_ord_ord_bool
              | Ast.BINOP_gt -> BINOPSIG_ord_ord_bool
              | Ast.BINOP_lsl -> BINOPSIG_integ_integ_integ
              | Ast.BINOP_lsr -> BINOPSIG_integ_integ_integ
              | Ast.BINOP_asr -> BINOPSIG_integ_integ_integ
              | Ast.BINOP_add -> BINOPSIG_plus_plus_plus
              | Ast.BINOP_sub -> BINOPSIG_num_num_num
              | Ast.BINOP_mul -> BINOPSIG_num_num_num
              | Ast.BINOP_div -> BINOPSIG_num_num_num
              | Ast.BINOP_mod -> BINOPSIG_num_num_num
              | Ast.BINOP_send -> bug () "BINOP_send found in expr"
            in
              begin
                match binop_sig with
                    BINOPSIG_bool_bool_bool ->
                      unify_atom lhs (ref (TYSPEC_resolved Ast.TY_bool));
                      unify_atom rhs (ref (TYSPEC_resolved Ast.TY_bool));
                      unify_ty Ast.TY_bool tv
                  | BINOPSIG_comp_comp_bool ->
                      let tv_a = ref TYSPEC_comparable in
                        unify_atom lhs tv_a;
                        unify_atom rhs tv_a;
                        unify_ty Ast.TY_bool tv
                  | BINOPSIG_ord_ord_bool ->
                      let tv_a = ref TYSPEC_ordered in
                        unify_atom lhs tv_a;
                        unify_atom rhs tv_a;
                        unify_ty Ast.TY_bool tv
                  | BINOPSIG_integ_integ_integ ->
                      let tv_a = ref TYSPEC_integral in
                        unify_atom lhs tv_a;
                        unify_atom rhs tv_a;
                        unify_tyvars tv tv_a
                  | BINOPSIG_num_num_num ->
                      let tv_a = ref TYSPEC_numeric in
                        unify_atom lhs tv_a;
                        unify_atom rhs tv_a;
                        unify_tyvars tv tv_a
                  | BINOPSIG_plus_plus_plus ->
                      let tv_a = ref TYSPEC_plusable in
                        unify_atom lhs tv_a;
                        unify_atom rhs tv_a;
                        unify_tyvars tv tv_a
              end
        | Ast.EXPR_unary (unop, atom) ->
            begin
              match unop with
                  Ast.UNOP_not ->
                    unify_atom atom (ref (TYSPEC_resolved Ast.TY_bool));
                    unify_ty Ast.TY_bool tv
                | Ast.UNOP_neg ->
                    let tv_a = ref TYSPEC_numeric in
                      unify_atom atom tv_a;
                      unify_tyvars tv tv_a
            end
        | Ast.EXPR_atom atom -> unify_atom atom tv

    and unify_lval' (lval:Ast.lval) (tv:tyvar) : unit =
      match lval with
          Ast.LVAL_base { id = id } ->
            let referent = Hashtbl.find cx.ctxt_lval_to_referent id in
              begin
                match Hashtbl.find cx.ctxt_all_defns referent with
                    DEFN_slot slot -> unify_slot slot (Some referent) tv
                  | _ ->
                      let spec = (!(Hashtbl.find bindings referent)) in
                        unify_tyvars (ref spec) tv
              end
        | Ast.LVAL_ext (base, comp) ->
            let base_ts = match comp with
                Ast.COMP_named (Ast.COMP_ident id) ->
                  let names = Hashtbl.create 1 in
                    Hashtbl.add names id tv;
                    TYSPEC_dictionary names

              | Ast.COMP_named (Ast.COMP_app (id, tys)) ->
                  let vars = Array.map (fun t -> ref (TYSPEC_resolved t)) tys in
                  let tv = ref (TYSPEC_parametric (tv, ref (Some vars))) in
                  let names = Hashtbl.create 1 in
                    Hashtbl.add names id tv;
                    TYSPEC_dictionary names

              | Ast.COMP_named (Ast.COMP_idx i) ->
                  let init j = if i + 1 == j then tv else ref TYSPEC_all in
                    TYSPEC_tuple (Array.init (i + 1) init)

              | Ast.COMP_atom atom ->
                  unify_atom atom (ref (TYSPEC_resolved Ast.TY_int));
                  TYSPEC_collection tv
            in
              unify_lval' base (ref base_ts)

    and unify_lval (lval:Ast.lval) (tv:tyvar) : unit =
      let id = lval_base_id lval in
      (* Fetch lval with type components resolved. *)
        iflog cx (fun _ -> log cx
                    "fetching resolved version of lval #%d"
                    (int_of_node id));
        let lval = Hashtbl.find cx.ctxt_all_lvals id in
          Hashtbl.add lval_tyvars id tv;
          unify_lval' lval tv

    in
    let gen_atom_tvs atoms =
      let gen_atom_tv atom =
        let tv = ref TYSPEC_all in
          unify_atom atom tv;
          tv
      in
        Array.map gen_atom_tv atoms
    in
    let visit_stmt_pre_full (stmt:Ast.stmt) : unit =

      let check_callable out_tv callee args =
        let param_tys = ref None in
        let in_tvs = gen_atom_tvs args in
        let callee_tv = ref (TYSPEC_callable (out_tv, in_tvs)) in
          (* 
           * NB: calls are all implicitly 'unknown type-parametric' to permit 
           * resolving against whatever sort of parametric callee shows up
           * in the callee lval.
           *)
        let full_callee_tv = ref (TYSPEC_parametric (callee_tv, param_tys)) in
          unify_lval callee full_callee_tv;
          match !param_tys with
              None -> ()
            | Some specs ->
                Hashtbl.add cx.ctxt_call_lval_params (lval_base_id callee)
                  begin
                    Array.mapi
                      begin
                        fun i tv ->
                          match !(resolve_tyvar tv) with
                              TYSPEC_resolved t ->
                                log cx "recording type parameter %d = %a"
                                  i Ast.sprintf_ty t; t
                            | ts -> err (Some stmt.id)
                                "unresolved type parameter %s"
                                  (tyspec_to_str ts)
                      end
                      specs
                  end
      in
      match stmt.node with
          Ast.STMT_spawn (out, _, callee, args) ->
            let out_tv = ref (TYSPEC_resolved Ast.TY_nil) in
              unify_lval out (ref (TYSPEC_resolved Ast.TY_proc));
              check_callable out_tv callee args

        | Ast.STMT_init_rec (lval, fields, Some base) ->
            let dct = Hashtbl.create 10 in
            let tvrec = ref (TYSPEC_record dct) in
            let add_field (ident, _, atom) =
              let tv = ref TYSPEC_all in
                unify_atom atom tv;
                Hashtbl.add dct ident tv
            in
              Array.iter add_field fields;
              let tvbase = ref TYSPEC_all in
                unify_lval base tvbase;
                unify_tyvars tvrec tvbase;
                unify_lval lval tvrec

        | Ast.STMT_init_rec (lval, fields, None) ->
            let dct = Hashtbl.create 10 in
            let add_field (ident, _, atom) =
              let tv = ref TYSPEC_all in
                unify_atom atom tv;
                Hashtbl.add dct ident tv
            in
              Array.iter add_field fields;
              unify_lval lval (ref (TYSPEC_record dct))

        | Ast.STMT_init_tup (lval, members) ->
            let member_to_tv (_, atom) =
              let tv = ref TYSPEC_all in
                unify_atom atom tv;
                tv
            in
            let member_tvs = Array.map member_to_tv members in
              unify_lval lval (ref (TYSPEC_tuple member_tvs))

        | Ast.STMT_init_vec (lval, _, atoms) ->
            let tv = ref TYSPEC_all in
            let unify_with_tv atom = unify_atom atom tv in
              Array.iter unify_with_tv atoms;
              unify_lval lval (ref (TYSPEC_vector tv))

        | Ast.STMT_init_str (lval, _) ->
            unify_lval lval (ref (TYSPEC_resolved Ast.TY_str))

        | Ast.STMT_copy (lval, expr) ->
            let tv = ref TYSPEC_all in
              unify_expr expr tv;
              unify_lval lval tv

        | Ast.STMT_copy_binop (lval, binop, at) ->
            let tv = ref TYSPEC_all in
              unify_expr (Ast.EXPR_binary (binop, Ast.ATOM_lval lval, at)) tv;
              unify_lval lval tv;

        | Ast.STMT_call (out, callee, args) ->
            let out_tv = ref TYSPEC_all in
              unify_lval out out_tv;
              check_callable out_tv callee args

        | Ast.STMT_log atom -> unify_atom atom (ref TYSPEC_loggable)

        | Ast.STMT_check_expr expr ->
            unify_expr expr (ref (TYSPEC_resolved Ast.TY_bool))

        | Ast.STMT_check (_, check_calls) ->
            let out_tv = ref (TYSPEC_resolved Ast.TY_bool) in
              Array.iter
                (fun (callee, args) ->
                   check_callable out_tv callee args)
                check_calls

        | Ast.STMT_while { Ast.while_lval = (_, expr); Ast.while_body = _ } ->
            unify_expr expr (ref (TYSPEC_resolved Ast.TY_bool))

        | Ast.STMT_if { Ast.if_test = if_test } ->
            unify_expr if_test (ref (TYSPEC_resolved Ast.TY_bool));

        | Ast.STMT_decl _ -> ()

        | Ast.STMT_ret (_, atom_opt) ->
            begin
              match atom_opt with
                  None -> unify_ty Ast.TY_nil !retval_tv_r
                | Some atom -> unify_atom atom !retval_tv_r
            end

        | Ast.STMT_be (_, callee, args) ->
            check_callable (!retval_tv_r) callee args

        | Ast.STMT_bind (bound, callee, arg_opts) ->
            (* FIXME: handle binding type parameters eventually. *)
            let out_tv = ref TYSPEC_all in
            let residue = ref [] in
            let gen_atom_opt_tvs atoms =
              let gen_atom_tv atom_opt =
                let tv = ref TYSPEC_all in
                  begin
                    match atom_opt with
                        None -> residue := tv :: (!residue);
                      | Some atom -> unify_atom atom tv
                  end;
                  tv
              in
                Array.map gen_atom_tv atoms
            in

            let in_tvs = gen_atom_opt_tvs arg_opts in
            let arg_residue_tvs = Array.of_list (List.rev (!residue)) in
            let callee_tv = ref (TYSPEC_callable (out_tv, in_tvs)) in
            let bound_tv = ref (TYSPEC_callable (out_tv, arg_residue_tvs)) in
              unify_lval callee callee_tv;
              unify_lval bound bound_tv

        (* FIXME (bug 541531): plenty more to handle here. *)
        | _ ->
            log cx "warning: not typechecking stmt %s\n"
              (Ast.sprintf_stmt () stmt)
    in

    let visit_stmt_pre (stmt:Ast.stmt) : unit =
      try
        visit_stmt_pre_full stmt;
        (* 
         * Reset any item-parameters that were resolved to types
         * during inference for this statement.
         *)
        Hashtbl.iter
          (fun _ params -> Array.iter (fun tv -> tv := TYSPEC_all) params)
          item_params;
      with
          Semant_err (None, msg) ->
            raise (Semant_err ((Some stmt.id), msg))
    in

    let visit_mod_item_pre n p mod_item =
      Stack.push mod_item.node.Ast.decl_params item_params_stk;
      begin
        try
          match mod_item.node.Ast.decl_item with
              Ast.MOD_ITEM_fn
                { Ast.fn_output_slot = { node = node; id = id } } ->
                  retval_tv_r := ref TYSPEC_all;
                  unify_slot node (Some id) !retval_tv_r

            | Ast.MOD_ITEM_pred _ ->
                retval_tv_r := ref (TYSPEC_resolved Ast.TY_bool)

            | _ -> ()
        with Semant_err (None, msg) ->
          raise (Semant_err ((Some mod_item.id), msg))
      end;
      inner.Walk.visit_mod_item_pre n p mod_item
    in

    let visit_mod_item_post n p mod_item =
      inner.Walk.visit_mod_item_post n p mod_item;
      ignore (Stack.pop item_params_stk)
    in

      {
        inner with
          Walk.visit_mod_item_pre = visit_mod_item_pre;
          Walk.visit_mod_item_post = visit_mod_item_post;
          Walk.visit_stmt_pre = visit_stmt_pre
      }

  in
    try
      let path = Stack.create () in
      let auto_queue = Queue.create () in
      let init_slot_tyvar id defn =
        match defn with
            DEFN_slot { Ast.slot_mode = _; Ast.slot_ty = None } ->
              Queue.add id auto_queue;
              Hashtbl.add bindings id (ref TYSPEC_all)
          | DEFN_slot { Ast.slot_mode = _; Ast.slot_ty = Some ty } ->
              let _ = iflog cx (fun _ -> log cx "initial slot #%d type: %a"
                                  (int_of_node id) Ast.sprintf_ty ty)
              in
                Hashtbl.add bindings id (ref (TYSPEC_resolved ty))
          | _ -> ()
      in
      let init_item_tyvar id ty =
        let _ = iflog cx (fun _ -> log cx "initial item #%d type: %a"
                            (int_of_node id) Ast.sprintf_ty ty)
        in
        let n_params =
          match Hashtbl.find cx.ctxt_all_defns id with
              DEFN_item i -> Array.length i.Ast.decl_params
            | _ -> err (Some id) "expected item defn for item tyvar"
        in
        let resolved = (TYSPEC_resolved ty) in
        let spec =
          if n_params = 0
          then resolved
          else
            let params = (Array.init n_params (fun _ -> ref TYSPEC_all)) in
              htab_put item_params id params;
              TYSPEC_parametric (ref resolved, ref (Some params))
        in
          Hashtbl.add bindings id (ref spec)
      in

      let init_mod_dict id defn =
        let rec tv_of_item id item =
          match item.Ast.decl_item with
              Ast.MOD_ITEM_mod items ->
                if Hashtbl.mem bindings id
                then Hashtbl.find bindings id
                else
                  let dict = htab_map items
                    (fun i item -> (i, tv_of_item item.id item.node))
                  in
                  let spec = TYSPEC_dictionary dict in
                  let tv = ref spec in
                    Hashtbl.add bindings id tv;
                    tv
            | _ ->
                Hashtbl.find bindings id
        in
          match defn with
              DEFN_item ({ Ast.decl_item=Ast.MOD_ITEM_mod _ } as item) ->
                ignore (tv_of_item id item)
            | _ -> ()
      in
        Hashtbl.iter init_slot_tyvar cx.ctxt_all_defns;
        Hashtbl.iter init_item_tyvar cx.ctxt_all_item_types;
        Hashtbl.iter init_mod_dict cx.ctxt_all_defns;
        Walk.walk_crate
          (Walk.path_managing_visitor path
             (Walk.mod_item_logging_visitor
                (log cx "typechecking pass: %s")
                path
                (visitor cx Walk.empty_visitor)))
          crate;
        let update_auto_tyvar id ty =
          let defn = Hashtbl.find cx.ctxt_all_defns id in
            match defn with
                DEFN_slot slot_defn ->
                  Hashtbl.replace cx.ctxt_all_defns id
                    (DEFN_slot { slot_defn with Ast.slot_ty = Some ty })
              | _ -> bug () "check_auto_tyvar: no slot defn"
        in
        let check_auto_tyvar id =
          let ts = !(resolve_tyvar (Hashtbl.find bindings id)) in
            match ts with
                TYSPEC_resolved ty -> update_auto_tyvar id ty
              | _ -> err (Some id) "unresolved type %s (%d)" (tyspec_to_str ts)
                  (int_of_node id)
        in
        let record_lval_ty id tv =
          let ts = !(resolve_tyvar tv) in
            match ts with
                TYSPEC_resolved ty -> Hashtbl.add cx.ctxt_all_lval_types id ty
              | _ -> err (Some id) "unresolved type %s" (tyspec_to_str ts)
        in
          Queue.iter check_auto_tyvar auto_queue;
          Hashtbl.iter record_lval_ty lval_tyvars;
    with Semant_err (ido, str) -> report_err cx ido str
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)

