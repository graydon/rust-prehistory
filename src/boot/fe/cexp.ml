
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
    CEXP_alt of cexp_alt identified
  | CEXP_let of cexp_let identified
  | CEXP_src_mod of cexp_src identified
  | CEXP_dir_mod of cexp_dir identified
  | CEXP_use_mod of cexp_use identified
  | CEXP_nat_mod of cexp_nat identified
  | CEXP_pexp of Pexp.pexp

and cexp_alt =
    { alt_val: cexp;
      alt_arms: (cexp * cexp) array;
      alt_else: cexp }

and cexp_let =
    { let_ident: cexp;
      let_value: cexp;
      let_body: cexp; }

and cexp_src =
    { src_name: cexp;
      src_path: cexp option }

and cexp_dir =
    { dir_name: cexp;
      dir_path: cexp option;
      dir_mods: cexp array }

and cexp_use =
    { use_name: cexp;
      use_path: cexp option;
      use_meta: (cexp * cexp) array; }

and cexp_nat =
    { nat_name: cexp;
      nat_path: cexp option;
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


let rec parse_cexp (ps:pstate) : cexp =

  let apos = lexpos ps in
    match peek ps with
        MOD ->
          begin
            bump ps;
            let name = ctxt "mod: name" parse_cexp ps in
            let path = ctxt "mod: path" parse_eq_cexp_opt ps
            in
              match peek ps with
                  SEMI ->
                    bump ps;
                    let bpos = lexpos ps in
                      CEXP_src_mod
                        (span ps apos bpos { src_name = name;
                                             src_path = path })
                | LBRACE ->
                    let mods =
                      bracketed_zero_or_more LBRACE RBRACE
                        None parse_cexp ps
                    in
                    let bpos = lexpos ps in
                      CEXP_dir_mod
                        (span ps apos bpos { dir_name = name;
                                             dir_path = path;
                                             dir_mods = mods })
                | _ -> raise (unexpected ps)
        end

      | NATIVE ->
          begin
            bump ps;
            expect ps MOD;
            let name = ctxt "native mod: name" parse_cexp ps in
            let path = ctxt "native mod: path" parse_eq_cexp_opt ps in
            let meta = [| |] in
            let items = Hashtbl.create 0 in
            let get_item ps =
              let (ident, item) = Item.parse_native_mod_item ps in
                (*
                 * FIXME: don't forget to do this when evaluating native items: 
                 * 
                 *   'htab_put files item.id ps.pstate_file'
                 *)
                htab_put items ident item;
            in
              ignore (bracketed_zero_or_more
                        LBRACE RBRACE None get_item ps);
              let bpos = lexpos ps in
                CEXP_nat_mod
                  (span ps apos bpos { nat_name = name;
                                       nat_path = path;
                                       nat_meta = meta;
                                       nat_items = items })
          end

      | USE ->
          begin
            bump ps;
            let name = ctxt "use mod: name" parse_cexp ps in
            let path = ctxt "use mod: path" parse_eq_cexp_opt ps in
            let meta = [| |] in
            let bpos = lexpos ps in
              CEXP_use_mod
                (span ps apos bpos { use_name = name;
                                     use_path = path;
                                     use_meta = meta })
          end

      | _ -> CEXP_pexp (Pexp.parse_pexp ps)


and  parse_eq_cexp_opt (ps:pstate) : cexp option =
  match peek ps with
      EQ ->
        begin
          bump ps;
          Some (parse_cexp ps)
        end
    | _ -> None
;;


(*
 * Dynamic-typed micro-interpreter for the cexp language.
 * 
 * FIXME: probably want to expand this to handle simple records and sequences too,
 * possibly type-polymorphic '+' operator for concatenation as well.
 *)

type cval =
    CVAL_str of string
  | CVAL_num of int64
  | CVAL_bool of bool
  | CVAL_ident of Ast.ident
  | CVAL_mod of (Ast.ident * Ast.mod_items)
  | CVAL_native_mod of (Ast.ident * Ast.native_mod_items)
;;

type env = { env_bindings: (Ast.ident * cval) list;
             env_prefix: filename list;
             env_items: (filename, Ast.mod_items) Hashtbl.t }

let unexpected_val (expected:string) (v:cval)  =
  let got =
    match v with
        CVAL_str s -> "str \"" ^ (String.escaped s) ^ "\""
      | CVAL_num i -> "num " ^ (Int64.to_string i)
      | CVAL_bool b -> if b then "bool true" else "bool false"
      | CVAL_ident i -> "ident " ^ i
      | CVAL_mod (name, mis) ->
          (Printf.sprintf "mod '%s' with %d items"
             name (Hashtbl.length mis))
      | CVAL_native_mod (name, nmis) ->
          (Printf.sprintf "native mod '%s' with %d items"
             name (Hashtbl.length nmis))
  in
    (* FIXME: proper error reporting, please. *)
    failwith ("expected " ^ expected ^ ", got " ^ got)
;;


let rec eval_cexp (env:env) (exp:cexp) : cval =
  match exp with
      CEXP_alt {node=ca} ->
        let v = eval_cexp env ca.alt_val in
        let rec try_arm i =
          if i >= Array.length ca.alt_arms
          then ca.alt_else
          else
            let (arm_head, arm_body) = ca.alt_arms.(i) in
            let v' = eval_cexp env arm_head in
              if v' = v
              then arm_body
              else try_arm (i+1)
        in
          eval_cexp env (try_arm 0)

    | CEXP_let {node=cl} ->
        let ident = eval_cexp_to_ident env cl.let_ident in
        let v = eval_cexp env cl.let_value in
        let env = { env with
                      env_bindings = ((ident,v)::env.env_bindings ) }
        in
          eval_cexp env cl.let_body

    | CEXP_src_mod {node=s} ->
        let items = Hashtbl.create 0 in
        let name = eval_cexp_to_ident env s.src_name in
          CVAL_mod (name, items)

    | CEXP_dir_mod {node=d; id=id} ->
        let items = Hashtbl.create 0 in
        let name = eval_cexp_to_ident env d.dir_name in
        let path =
          match d.dir_path with
              None -> name
            | Some p -> eval_cexp_to_str env p
        in
        let env = { env with
                      env_prefix = path :: env.env_prefix } in
        let sub_items = Array.map (eval_cexp_to_mod env) d.dir_mods in
        let add (k,v) =
          let v = Ast.MOD_ITEM_mod { Ast.decl_params = [| |];
                                     Ast.decl_item = (None, v) }
          in
            Hashtbl.add items k {id=id; node=v}
        in
          Array.iter add sub_items;
          CVAL_mod (name, items)

    | CEXP_use_mod {node=u} ->
        let items = Hashtbl.create 0 in
        let name = eval_cexp_to_ident env u.use_name in
          CVAL_mod (name, items)

    | CEXP_nat_mod {node=cn} ->
        let name = eval_cexp_to_ident env cn.nat_name in
          CVAL_native_mod (name, cn.nat_items)

    | CEXP_pexp exp ->
        eval_pexp env exp



and eval_cexp_to_str (env:env) (exp:cexp) : string =
  match eval_cexp env exp with
      CVAL_str s -> s
    | v -> unexpected_val "str" v

and eval_cexp_to_num (env:env) (exp:cexp) : int64 =
  match eval_cexp env exp with
      CVAL_num n -> n
    | v -> unexpected_val "num" v

and eval_cexp_to_bool (env:env) (exp:cexp) : bool =
  match eval_cexp env exp with
      CVAL_bool b -> b
    | v -> unexpected_val "bool" v

and eval_cexp_to_ident (env:env) (exp:cexp) : Ast.ident =
  match eval_cexp env exp with
      CVAL_ident i -> i
    | v -> unexpected_val "ident" v

and eval_cexp_to_mod (env:env) (exp:cexp) : (Ast.ident * Ast.mod_items) =
  match eval_cexp env exp with
      CVAL_mod mis -> mis
    | v -> unexpected_val "mod" v

and eval_cexp_to_native_mod (env:env) (exp:cexp) : (Ast.ident * Ast.native_mod_items) =
  match eval_cexp env exp with
      CVAL_native_mod nmis -> nmis
    | v -> unexpected_val "native mod" v


and eval_pexp (env:env) (exp:Pexp.pexp) : cval =
  match exp.node with
    | Pexp.PEXP_binop (bop, a, b) ->
        begin
          let av = eval_pexp_to_num env a in
          let bv = eval_pexp_to_num env b in
            CVAL_num
              begin
                match bop with
                    Ast.BINOP_add -> Int64.add av bv
                  | Ast.BINOP_sub -> Int64.sub av bv
                  | Ast.BINOP_mul -> Int64.mul av bv
                  | Ast.BINOP_div -> Int64.div av bv
                  | _ -> failwith "unhandled arithmetic op while evaluating pexp"
              end
        end

    | Pexp.PEXP_unop (uop, a) ->
        begin
          match uop with
              Ast.UNOP_not ->
                CVAL_bool (not (eval_pexp_to_bool env a))
            | Ast.UNOP_neg ->
                CVAL_num (Int64.neg (eval_pexp_to_num env a))
        end

    | Pexp.PEXP_lval (Pexp.PLVAL_ident ident) ->
        begin
          match ltab_search env.env_bindings ident with
              None -> CVAL_ident ident
            | Some v -> v
        end

    | Pexp.PEXP_lit (Ast.LIT_bool b) ->
        CVAL_bool b

    | Pexp.PEXP_lit (Ast.LIT_int (i, _)) ->
        CVAL_num i

    | Pexp.PEXP_str s ->
        CVAL_str s

    | _ -> failwith "evaluating unhandled pexp type"


and eval_pexp_to_str (env:env) (exp:Pexp.pexp) : string =
  match eval_pexp env exp with
      CVAL_str s -> s
    | v -> unexpected_val "str" v

and eval_pexp_to_num (env:env) (exp:Pexp.pexp) : int64 =
  match eval_pexp env exp with
      CVAL_num n -> n
    | v -> unexpected_val "num" v

and eval_pexp_to_bool (env:env) (exp:Pexp.pexp) : bool =
  match eval_pexp env exp with
      CVAL_bool b -> b
    | v -> unexpected_val "bool" v

;;


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
                      ps.pstate_infer_lib_name
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
      let filename = ps.pstate_infer_lib_name ident in
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
    (infer_lib_name:(Ast.ident -> filename))
    : Ast.crate =
  let files = Hashtbl.create 0 in
  let fname = Session.filename_of sess.Session.sess_in in
  let tref = ref (Temp 0) in
  let nref = ref (Node 0) in
  let oref = ref (Opaque 0) in
  let ps = make_parser tref nref oref sess tok get_ty_mod infer_lib_name fname
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
