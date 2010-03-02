
open Common;;
open Token;;
open Parser;;

(* NB: cexps (crate-expressions / constant-expressions) are only used
 * transiently during compilation: they are the outermost
 * expression-language describing crate configuration and
 * constants. They are completely evaluated at compile-time, in a
 * little micro-interpreter defined here, with the results of
 * evaluation being the crate structure full of items passed to the
 * rest of the compiler.
 * 
 * Cexps, like pexps, do not escape the language front-end.
 * 
 * You can think of the AST as a statement-language called "item"
 * sandwiched between two expression-languages, "cexp" on the outside
 * and "pexp" on the inside. The front-end evaluates cexp on the
 * outside in order to get one big item term, evaluating those parts of
 * pexp that are directly used by cexp in passing, and desugaring those
 * remaining parts of pexp that are embedded within the calculated item
 * term.
 * 
 * The rest of the compiler only deals with the item term, which is
 * what most of AST describes ("most" because the type-grammar spans
 * both items and pexps).
 * 
 *)


(*
 * FIXME: shift from interpret-while-parsing model to parse-then-interpret
 * model for cexps. Currently type 'cexp' here is feeling lonely.
 *)

type cexp =
    CEXP_alt of cexp_alt
  | CEXP_let of cexp_let
  | CEXP_src_mod of cexp_src
  | CEXP_dir_mod of cexp_dir
  | CEXP_use_mod of cexp_use
  | CEXP_nat_mod of cexp_nat
  | CEXP_pexp of Pexp.pexp

and cexp_alt =
    { alt_val: cexp;
      alt_arms: (cexp * cexp) array }

and cexp_let =
    { let_ident: cexp;
      let_value: cexp;
      let_body: cexp; }

and cexp_src =
    { src_name: cexp;
      src_path: cexp }

and cexp_dir =
    { dir_name: cexp;
      dir_path: cexp;
      dir_mods: cexp array }

and cexp_use =
    { use_name: cexp;
      use_path: cexp;
      use_meta: (cexp * cexp) array }

and cexp_nat =
    { nat_name: cexp;
      nat_path: cexp;
      nat_meta: (cexp * cexp) array;
      (* 
       * FIXME: possibly support embedding optional strings as
       * symbol-names, to handle mangling schemes that aren't
       * Token.IDENT values
       *)
      nat_items: Ast.native_mod_items;
    }
;;



(* Cexp grammar. *)


let rec parse_crate_mod_entry
    (prefix:string)
    (files:(node_id,filename) Hashtbl.t)
    (mod_items:Ast.mod_items)
    (native_mod_items:Ast.native_mod_items)
    (ps:pstate)
    : unit =
  match peek ps with
      NATIVE ->
        begin
            bump ps;
            let (ident, item) = Item.parse_native_mod_item ps in
              htab_put native_mod_items ident item;
              htab_put files item.id ps.pstate_file
        end
    | _ ->
        begin
          expect ps MOD;
          let apos = lexpos ps in
          let name = ctxt "mod: name" Pexp.parse_ident ps in
          let fname =
            match peek ps with
                EQ ->
                  bump ps;
                  (match peek ps with
                       LIT_STR s -> bump ps; s
                     | _ -> raise (unexpected ps))
              | _ ->
                  begin
                    match peek ps with
                        LBRACE -> name
                      | SEMI -> name ^ ".rs"
                      | _ -> raise (unexpected ps)
                  end
          in
          let full_fname = Filename.concat prefix fname in
          let (items,is_cu) =
            match peek ps with
                SEMI ->
                  bump ps;
                  let p =
                    make_parser
                      ps.pstate_temp_id
                      ps.pstate_node_id
                      ps.pstate_opaque_id
                      ps.pstate_sess
                      ps.pstate_lexfun
                      ps.pstate_get_ty_mod
                      ps.pstate_infer_crate_filename
                      full_fname
                  in
                    (Item.parse_mod_items p EOF, true)
              | LBRACE ->
                  bump ps;
                  let items =
                    parse_crate_mod_entries full_fname files ps
                  in
                    (items, false)

              | _ -> raise (unexpected ps)
          in
          let bpos = lexpos ps in
          let item_mod =
            (* FIXME: permit type-parametric top-level modules. *)
            span ps apos bpos (Ast.MOD_ITEM_mod { Ast.decl_params = arr [];
                                                  Ast.decl_item = (None, items) })
          in
            if is_cu
            then htab_put files item_mod.id full_fname;
            htab_put mod_items name item_mod
        end

and parse_crate_mod_entries
    (prefix:string)
    (files:(node_id,filename) Hashtbl.t)
    (ps:pstate)
    : Ast.mod_items =
  let items = Hashtbl.create 4 in
  let nitems = Hashtbl.create 4 in
    while (not (peek ps = RBRACE))
    do
      parse_crate_mod_entry prefix files items nitems ps
    done;
    expect ps RBRACE;
    items

and parse_crate_import
    (imports:(import_lib, (Ast.ident * Ast.mod_type_item * span)) Hashtbl.t)
    (ps:pstate)
    : unit =
  let apos = lexpos ps in
    expect ps USE;
    let ident = Pexp.parse_ident ps in
      expect ps SEMI;
      let filename = ps.pstate_infer_crate_filename ident in
      let ilib = { import_libname = filename;
                   import_prefix = 1 }
      in
      let tmod = ps.pstate_get_ty_mod filename in
        iflog ps
          begin
            fun _ ->
              log ps "extracted mod type from %s (binding to %s)" filename ident;
              log ps "%a" Ast.sprintf_ty (Ast.TY_mod tmod);
          end;
        let mti = Ast.MOD_TYPE_ITEM_mod {Ast.decl_params = [| |];
                                         Ast.decl_item = tmod}
        in
        let bpos = lexpos ps in
          Hashtbl.add imports ilib (ident, mti, {lo=apos;hi=bpos})

and expand_imports
    (ps:pstate)
    (crate:Ast.crate)
    : unit =
  let wrap span i =
    let id = next_node_id ps in
      htab_put ps.pstate_sess.Session.sess_spans id span;
      { node = i; id = id }
  in

  let rec extract_item
      (span:span)
      (ilib:import_lib)
      (mti:Ast.mod_type_item)
      : Ast.mod_item' =

    let wrap i = wrap span i in

    let form_header_slots slots =
      Array.mapi
        (fun i slot -> (wrap slot, "_" ^ (string_of_int i)))
        slots
    in

    match mti with
        Ast.MOD_TYPE_ITEM_opaque_type { Ast.decl_item=(_, mut);
                                        Ast.decl_params=params } ->
          Ast.MOD_ITEM_opaque_type
            { Ast.decl_item=Ast.TY_opaque ((next_opaque_id ps), mut);
              Ast.decl_params=params }

      | Ast.MOD_TYPE_ITEM_public_type { Ast.decl_item=ty;
                                        Ast.decl_params=params } ->
          Ast.MOD_ITEM_public_type
            { Ast.decl_item=ty;
              Ast.decl_params=params }

      | Ast.MOD_TYPE_ITEM_pred { Ast.decl_item=tpred;
                                 Ast.decl_params=params } ->
          let (slots, constrs) = tpred in
          let pred =
            { Ast.pred_input_slots = form_header_slots slots;
              Ast.pred_input_constrs = constrs;
              Ast.pred_body = wrap [| |] }
          in
            Ast.MOD_ITEM_pred
              { Ast.decl_item=pred;
                Ast.decl_params=params }

      | Ast.MOD_TYPE_ITEM_mod { Ast.decl_item=tmod;
                                Ast.decl_params=params } ->
          begin
            let hdr, mtis = tmod in
              match hdr with
                  None ->
                    Ast.MOD_ITEM_mod
                      { Ast.decl_item=(None, extract_mod span ilib mtis);
                        Ast.decl_params=params }
                | Some (slots, constrs) ->
                    Ast.MOD_ITEM_mod
                      { Ast.decl_item=(Some (form_header_slots slots, constrs),
                                       extract_mod span ilib mtis);
                        Ast.decl_params=params }
          end

      | Ast.MOD_TYPE_ITEM_fn { Ast.decl_item=tfn;
                               Ast.decl_params=params } ->
          let (tsig, taux) = tfn in
          let fn =
            { Ast.fn_input_slots=form_header_slots tsig.Ast.sig_input_slots;
              Ast.fn_input_constrs=tsig.Ast.sig_input_constrs;
              Ast.fn_output_slot=wrap tsig.Ast.sig_output_slot;
              Ast.fn_aux=taux;
              Ast.fn_body=wrap [| |]; }
          in
            Ast.MOD_ITEM_fn
              { Ast.decl_item=fn;
                Ast.decl_params=params }


  and wrap_item
      (span:span)
      (ilib:import_lib)
      (item':Ast.mod_item')
      : Ast.mod_item =
    let wrapped = wrap span item' in
      htab_put crate.node.Ast.crate_imported wrapped.id ilib;
      wrapped

  and extract_mod
      (span:span)
      (ilib:import_lib)
      (mtis:Ast.mod_type_items)
      : Ast.mod_items =
    htab_map
      mtis
      begin
        fun ident mti -> (ident, wrap_item span ilib (extract_item span ilib mti))
      end
  in

  let extract_items
      (ilib:import_lib)
      ((ident:Ast.ident), (import_mti:Ast.mod_type_item), (span:span))
      : (Ast.ident * Ast.mod_item) =
    let imported_mod = extract_item span ilib import_mti in
      (ident, wrap_item span ilib imported_mod)
  in

  let mis = htab_map crate.node.Ast.crate_imports extract_items in
    Hashtbl.iter (htab_put crate.node.Ast.crate_items) mis


and parse_root_crate_entries
    (fname:string)
    (prefix:string)
    (files:(node_id,filename) Hashtbl.t)
    (ps:pstate)
    : Ast.crate =
  let items = Hashtbl.create 4 in
  let nitems = Hashtbl.create 4 in
  let imports = Hashtbl.create 4 in
  let imported = Hashtbl.create 4 in
  let apos = lexpos ps in
    log ps "reading crate entries from %s" fname;
    while (not (peek ps = EOF))
    do
      match peek ps with
          NATIVE | MOD ->
            parse_crate_mod_entry prefix files items nitems ps
        | USE ->
            parse_crate_import imports ps
        | _ -> raise (unexpected ps)
    done;
    expect ps EOF;
    let main = find_main_fn ps items in
    let bpos = lexpos ps in
    let crate =
      span ps apos bpos
        { Ast.crate_items = items;
          Ast.crate_imports = imports;
          Ast.crate_imported = imported;
          Ast.crate_native_items = nitems;
          Ast.crate_main = main;
          Ast.crate_files = files }
    in
      htab_put files crate.id fname;
      crate

and find_main_fn
    (ps:pstate)
    (crate_items:Ast.mod_items)
    : Ast.name =
  let fns = ref [] in
  let extend prefix_name ident =
    match prefix_name with
        None -> Ast.NAME_base (Ast.BASE_ident ident)
      | Some n -> Ast.NAME_ext (n, Ast.COMP_ident ident)
  in
  let rec dig prefix_name items =
    Hashtbl.iter (extract_fn prefix_name) items
  and extract_fn prefix_name ident item =
    match item.node with
        Ast.MOD_ITEM_mod md ->
          if Array.length md.Ast.decl_params = 0 &&
            (fst md.Ast.decl_item = None)
          then dig (Some (extend prefix_name ident)) (snd md.Ast.decl_item)
          else ()
      | Ast.MOD_ITEM_fn fd ->
          if Array.length fd.Ast.decl_params = 0 && ident = "main"
          then fns := (extend prefix_name ident) :: (!fns)
          else ()
      | _ -> ()
  in
    dig None crate_items;
    match !fns with
        [] -> raise (err "no 'main' function found" ps)
      | [x] -> x
      | _ -> raise (err "multiple 'main' functions found" ps)
;;

let parse_root_with_parse_fn
    (suffix:string)
    fn
    (sess:Session.sess)
    tok
    (get_ty_mod:(filename ->  Ast.ty_mod))
    (infer_crate_filename:(Ast.ident -> filename))
    : Ast.crate =
  let files = Hashtbl.create 0 in
  let fname = Session.filename_of sess.Session.sess_in in
  let tref = ref (Temp 0) in
  let nref = ref (Node 0) in
  let oref = ref (Opaque 0) in
  let ps = make_parser tref nref oref sess tok get_ty_mod infer_crate_filename fname
  in
  let apos = lexpos ps in
    try
      let crate =
        if Filename.check_suffix fname suffix
        then fn fname (Filename.dirname fname) files ps
        else raise (err "parsing wrong kind of file" ps)
      in
        expand_imports ps crate;
        crate
    with
        Parse_err (ps, str) ->
          Session.fail sess "Parse error: %s\n%!" str;
          List.iter
            (fun (cx,pos) ->
               Session.fail sess "%s:E (parse context): %s\n%!"
                 (Session.string_of_pos pos) cx)
            ps.pstate_ctxt;
          span ps apos apos
            { Ast.crate_items = Hashtbl.create 0;
              Ast.crate_imports = Hashtbl.create 0;
              Ast.crate_imported = Hashtbl.create 0;
              Ast.crate_native_items = Hashtbl.create 0;
              Ast.crate_main = Ast.NAME_base (Ast.BASE_ident "none");
              Ast.crate_files = files }

let parse_root_srcfile_entries
    (fname:string)
    ((*prefix*)_:string)
    (files:(node_id,filename) Hashtbl.t)
    (ps:pstate)
    : Ast.crate =
  let stem = Filename.chop_suffix (Filename.basename fname) ".rs" in
  let apos = lexpos ps in
  let items = Item.parse_mod_items ps EOF in
  let bpos = lexpos ps in
  let modi = span ps apos bpos (Ast.MOD_ITEM_mod { Ast.decl_params = arr [];
                                                   Ast.decl_item = (None, items) })
  in
  let mitems = Hashtbl.create 0 in
    htab_put mitems stem modi;
    let crate =
      span ps apos bpos { Ast.crate_items = mitems;
                          Ast.crate_imports = Hashtbl.create 0;
                          Ast.crate_imported = Hashtbl.create 0;
                          Ast.crate_native_items = Hashtbl.create 0;
                          Ast.crate_main = find_main_fn ps mitems;
                          Ast.crate_files = files }
    in
      htab_put files crate.id fname;
      crate
;;

let parse_crate = parse_root_with_parse_fn ".rc" parse_root_crate_entries;;
let parse_srcfile = parse_root_with_parse_fn ".rs" parse_root_srcfile_entries;;



(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
