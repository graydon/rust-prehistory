
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
    { nat_abi: cexp;
      nat_name: cexp;
      nat_path: cexp option;
      nat_meta: (cexp * cexp) array;
      (* 
       * FIXME: possibly support embedding optional strings as
       * symbol-names, to handle mangling schemes that aren't
       * Token.IDENT values
       *)
      nat_items: Ast.mod_type_items;
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
            let abi =
              let apos = lexpos ps in
                match peek ps with
                    MOD ->
                      bump ps;
                      let bpos = lexpos ps in
                        CEXP_pexp (span ps apos bpos (Pexp.PEXP_str "cdecl"))
                  | _ ->
                      let cexp = parse_cexp ps in
                        expect ps MOD;
                        cexp
            in
            let name = ctxt "native mod: name" parse_cexp ps in
            let path = ctxt "native mod: path" parse_eq_cexp_opt ps in
            let meta = [| |] in
            let items = Hashtbl.create 0 in
            let get_item ps =
              let (ident, item) = Pexp.parse_mod_ty_item ps in
                htab_put items ident item;
            in
              ignore (bracketed_zero_or_more
                        LBRACE RBRACE None get_item ps);
              let bpos = lexpos ps in
                CEXP_nat_mod
                  (span ps apos bpos { nat_abi = abi;
                                       nat_name = name;
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
              expect ps SEMI;
              CEXP_use_mod
                (span ps apos bpos { use_name = name;
                                     use_path = path;
                                     use_meta = meta })
          end

      | LET ->
          begin
            bump ps;
            let id = parse_cexp ps in
              expect ps EQ;
              let v = parse_cexp ps in
                expect ps IN;
                let body = parse_cexp ps in
                let bpos = lexpos ps in
                  CEXP_let
                    (span ps apos bpos
                       { let_ident = id;
                         let_value = v;
                         let_body = body })
          end

      | ALT ->
          begin
            bump ps;
            expect ps LPAREN;
            let v = parse_cexp ps in
              expect ps RPAREN;
              expect ps LBRACE;
              let rec consume_arms arms =
                match peek ps with
                    CASE ->
                      begin
                        bump ps;
                        expect ps LPAREN;
                        let cond = parse_cexp ps in
                          expect ps RPAREN;
                          expect ps LBRACE;
                          let consequent = parse_cexp ps in
                            expect ps RBRACE;
                            let arm = (cond, consequent) in
                            consume_arms (arm::arms)
                      end
                  | ELSE ->
                      begin
                        bump ps;
                        expect ps LBRACE;
                        let consequent = parse_cexp ps in
                          expect ps RBRACE;
                          expect ps RBRACE;
                          let bpos = lexpos ps in
                            span ps apos bpos
                              { alt_val = v;
                                alt_arms = Array.of_list (List.rev arms);
                                alt_else = consequent }
                      end

                  | _ -> raise (unexpected ps)
              in
                CEXP_alt (consume_arms [])
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
  | CVAL_mod_item of (Ast.ident * Ast.mod_item)
  | CVAL_native_mod_item of (Ast.ident * Ast.native_mod_item)
;;

type env = { env_bindings: (Ast.ident * cval) list;
             env_prefix: filename list;
             env_items: (filename, Ast.mod_items) Hashtbl.t;
             env_files: (node_id,filename) Hashtbl.t;
             env_imported: (node_id, (import_lib * nabi_conv)) Hashtbl.t;
             env_ps: pstate; }

let unexpected_val (expected:string) (v:cval)  =
  let got =
    match v with
        CVAL_str s -> "str \"" ^ (String.escaped s) ^ "\""
      | CVAL_num i -> "num " ^ (Int64.to_string i)
      | CVAL_bool b -> if b then "bool true" else "bool false"
      | CVAL_ident i -> "ident " ^ i
      | CVAL_mod_item (name, _) -> "mod item " ^ name
      | CVAL_native_mod_item (name, _) -> "native mod item " ^ name
  in
    (* FIXME: proper error reporting, please. *)
    failwith ("expected " ^ expected ^ ", got " ^ got)
;;

let rewrap_items id items =
  let item = Ast.MOD_ITEM_mod { Ast.decl_params = [| |];
                                Ast.decl_item = (None, items) } in
    { id = id; node = item }
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

    | CEXP_src_mod {node=s; id=id} ->
        let name = eval_cexp_to_ident env s.src_name in
        let path =
          match s.src_path with
              None -> name ^ ".rs"
            | Some p -> eval_cexp_to_str env p
        in
        let full_path = List.fold_left Filename.concat "" (List.rev (path :: env.env_prefix)) in
        let ps = env.env_ps in
        let p =
          make_parser
            ps.pstate_temp_id
            ps.pstate_node_id
            ps.pstate_opaque_id
            ps.pstate_sess
            ps.pstate_lexfun
            ps.pstate_get_ty_mod
            ps.pstate_infer_lib_name
            env.env_imported
            full_path
        in
        let items = Item.parse_mod_items p EOF in
          htab_put env.env_files id full_path;
          CVAL_mod_item (name, rewrap_items id items)

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
        let add (k, v) = htab_put items k v in
          Array.iter add sub_items;
            CVAL_mod_item (name, rewrap_items id items)

    | CEXP_use_mod {node=u; id=id} ->
        let ps = env.env_ps in
        let name = eval_cexp_to_ident env u.use_name in
        let filename = ps.pstate_infer_lib_name name in
        let ilib = IMPORT_LIB_rust { import_libname = filename;
                                     import_prefix = 1 }
        in
        let tmod = ps.pstate_get_ty_mod filename in
          iflog ps
            begin
              fun _ ->
                log ps "extracted mod type from %s (binding to %s)" filename name;
                log ps "%a" Ast.sprintf_ty (Ast.TY_mod tmod);
            end;
          let mti = Ast.MOD_TYPE_ITEM_mod {Ast.decl_params = [| |];
                                           Ast.decl_item = tmod}
          in
          let span = Hashtbl.find ps.pstate_sess.Session.sess_spans id in
          let item = Item.expand_imported_mod env.env_ps span CONV_rust ilib mti in
            htab_put ps.pstate_imported id (ilib,CONV_rust);
            CVAL_mod_item (name, { id = id; node = item })

    | CEXP_nat_mod {node=cn;id=id} ->
        let conv =
          let v = eval_cexp_to_str env cn.nat_abi in
          match string_to_conv v with
              None -> unexpected_val "calling convention" (CVAL_str v)
            | Some c -> c
        in
        let name = eval_cexp_to_ident env cn.nat_name in
        let filename =
          match cn.nat_path with
              None -> env.env_ps.pstate_infer_lib_name name
            | Some p -> eval_cexp_to_str env p
        in
        let mti = Ast.MOD_TYPE_ITEM_mod {Ast.decl_params = [| |];
                                         Ast.decl_item = (None, cn.nat_items) }
        in
        let ilib = IMPORT_LIB_c { import_libname = filename;
                                  import_prefix = 1 }
        in
        let ps = env.env_ps in
        let span = Hashtbl.find ps.pstate_sess.Session.sess_spans id in
        let item = Item.expand_imported_mod env.env_ps span conv ilib mti in
          htab_put ps.pstate_imported id (ilib,CONV_rust);
          CVAL_mod_item (name, { id = id; node = item })

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

and eval_cexp_to_mod (env:env) (exp:cexp) : (Ast.ident * Ast.mod_item) =
  match eval_cexp env exp with
      CVAL_mod_item i -> i
    | v -> unexpected_val "mod item" v

and eval_cexp_to_native_mod (env:env) (exp:cexp) : (Ast.ident * Ast.native_mod_item) =
  match eval_cexp env exp with
      CVAL_native_mod_item nm -> nm
    | v -> unexpected_val "native mod item" v


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

    | _ -> failwith "unexpected Pexp in Cexp.eval_pexp"


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


let find_main_fn
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
    if Hashtbl.mem ps.pstate_imported item.id
    then ()
    else
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


let with_err_handling sess thunk =
  try
    thunk ()
  with
      Parse_err (ps, str) ->
        Session.fail sess "Parse error: %s\n%!" str;
        List.iter
          (fun (cx,pos) ->
             Session.fail sess "%s:E (parse context): %s\n%!"
               (Session.string_of_pos pos) cx)
          ps.pstate_ctxt;
        let apos = lexpos ps in
          span ps apos apos
            { Ast.crate_items = Hashtbl.create 0;
              Ast.crate_imported = Hashtbl.create 0;
              Ast.crate_native_items = Hashtbl.create 0;
              Ast.crate_main = Ast.NAME_base (Ast.BASE_ident "none");
              Ast.crate_files = Hashtbl.create 0 }
;;


let parse_crate_file
    (sess:Session.sess)
    (tok:Lexing.lexbuf -> Token.token)
    (get_ty_mod:(filename ->  Ast.ty_mod))
    (infer_lib_name:(Ast.ident -> filename))
    : Ast.crate =
  let fname = Session.filename_of sess.Session.sess_in in
  let tref = ref (Temp 0) in
  let nref = ref (Node 0) in
  let oref = ref (Opaque 0) in
  let imported = Hashtbl.create 4 in
  let ps =
    make_parser tref nref oref sess tok get_ty_mod infer_lib_name imported fname
  in

  let files = Hashtbl.create 0 in
  let items = Hashtbl.create 4 in
  let nitems = Hashtbl.create 4 in
  let target_bindings =
    let (os, arch, libc) =
      match sess.Session.sess_targ with
          Linux_x86_elf -> ("linux", "x86", "libc.so.6")
        | Win32_x86_pe -> ("win32", "x86", "msvcrt.dll")
        | MacOS_x86_macho -> ("macos", "x86", "libc.dylib")
    in
      [
        ("target_os", CVAL_str os);
        ("target_arch", CVAL_str arch);
        ("target_libc", CVAL_str libc)
      ]
  in
  let build_bindings =
    [
      ("build_compiler", CVAL_str Sys.executable_name);
      ("build_input", CVAL_str fname);
    ]
  in
  let initial_bindings =
    target_bindings
    @ build_bindings
  in
  let env = { env_bindings = initial_bindings;
              env_prefix = [Filename.dirname fname];
              env_items = Hashtbl.create 0;
              env_files = files;
              env_imported = imported;
              env_ps = ps; }
  in
    with_err_handling sess
      begin
        fun _ ->
          let apos = lexpos ps in
          let _ =
            while (not ((peek ps) = EOF))
            do
              let cexp = parse_cexp ps in
                match eval_cexp env cexp with
                    CVAL_mod_item (name, item) -> htab_put items name item
                  | CVAL_native_mod_item (name, nitem) -> htab_put nitems name nitem
                  | v -> unexpected_val "mod item or native mod item" v
            done
          in
          let bpos = lexpos ps in
          let main = find_main_fn ps items in
          let crate = { Ast.crate_items = items;
                        Ast.crate_imported = imported;
                        Ast.crate_native_items = nitems;
                        Ast.crate_main = main;
                        Ast.crate_files = files }
          in
          let cratei = span ps apos bpos crate in
            htab_put files cratei.id fname;
            cratei
      end
;;

let parse_src_file
    (sess:Session.sess)
    (tok:Lexing.lexbuf -> Token.token)
    (get_ty_mod:(filename ->  Ast.ty_mod))
    (infer_lib_name:(Ast.ident -> filename))
    : Ast.crate =
  let fname = Session.filename_of sess.Session.sess_in in
  let tref = ref (Temp 0) in
  let nref = ref (Node 0) in
  let oref = ref (Opaque 0) in
  let imported = Hashtbl.create 4 in
  let ps =
    make_parser tref nref oref sess tok get_ty_mod infer_lib_name imported fname
  in
    with_err_handling sess
      begin
        fun _ ->
          let apos = lexpos ps in
          let items = Item.parse_mod_items ps EOF in
          let bpos = lexpos ps in
          let files = Hashtbl.create 0 in
          let nitems = Hashtbl.create 4 in
          let main = find_main_fn ps items in
          let crate = { Ast.crate_items = items;
                        Ast.crate_imported = imported;
                        Ast.crate_native_items = nitems;
                        Ast.crate_main = main;
                        Ast.crate_files = files }
          in
          let cratei = span ps apos bpos crate in
            htab_put files cratei.id fname;
            cratei
      end
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
