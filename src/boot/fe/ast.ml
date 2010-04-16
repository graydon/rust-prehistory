(*
 * There are two kinds of rust files:
 *
 * .rc files, containing crates.
 * .rs files, containing source.
 *
 *)

open Common;;

(*
 * Slot names are given by a dot-separated path within the current
 * module namespace.
 *)

type ident = string
;;

type slot_key =
    KEY_ident of ident
  | KEY_temp of temp_id
;;

(* "names" are statically computable references to particular items;
   they never involve dynamic indexing (nor even static tuple-indexing;
   you could add it but there are few contexts that need names that would
   benefit from it).

   Each component of a name may also be type-parametric; you must
   supply type parameters to reference through a type-parametric name
   component. So for example if foo is parametric in 2 types, you can
   write foo[int,int].bar but not foo.bar.
 *)

type proto =
    PROTO_ques  (* fn? foo(...): may yield 1 value or return w/o yielding. Never resumes. *)
  | PROTO_bang  (* fn! foo(...): yields 1 value. Never resumes.                           *)
  | PROTO_star  (* fn* foo(...): may yield N >= 0 values, then returns.                   *)
  | PROTO_plus  (* fn+ foo(...): yields N > 0 values then returns.                        *)
;;

type mutability =
    IMMUTABLE
  | MUTABLE
;;

type purity =
    PURE
  | IMPURE of mutability
;;

type name_base =
    BASE_ident of ident
  | BASE_temp of temp_id
  | BASE_app of (ident * (ty array))

and name_component =
    COMP_ident of ident
  | COMP_app of (ident * (ty array))
  | COMP_idx of int

and name =
    NAME_base of name_base
  | NAME_ext of (name * name_component)

(*
 * Type expressions are transparent to type names, their equality is structural.
 * (after normalization)
 *)
and ty =

    TY_any
  | TY_nil
  | TY_bool
  | TY_mach of ty_mach
  | TY_int
  | TY_char
  | TY_str

  | TY_tup of ty_tup
  | TY_vec of slot
  | TY_rec of ty_rec

  (*
   * Note that ty_idx is only valid inside a slot of a ty_iso group, not
   * in a general type term.
   *)
  | TY_tag of ty_tag
  | TY_iso of ty_iso
  | TY_idx of int

  | TY_fn of ty_fn
  | TY_pred of ty_pred
  | TY_chan of ty
  | TY_port of ty

  | TY_obj of ty_obj
  | TY_task

  | TY_param of (ty_param_idx * opaque_id * mutability)
  | TY_named of name
  | TY_type

  | TY_constrained of (ty * constrs)

and mode =
    MODE_exterior of mutability
  | MODE_interior of mutability
  | MODE_read_alias
  | MODE_write_alias

and slot = { slot_mode: mode;
             slot_ty: ty option; }

and ty_tup = slot array

(* In closed type terms a constraint may refer to components of the
 * term by anchoring off the "formal symbol" '*', which represents "the
 * term this constraint is attached to".
 *
 *
 * For example, if I have a tuple type (int,int), I may wish to enforce
 * the lt predicate on it; I can write this as a constrained type term
 * like:
 *
 * (int,int) : lt( *.{0}, *.{1} )
 *
 * In fact all tuple types are converted to this form for purpose of
 * type-compatibility testing; the argument tuple in a function
 *
 * fn (int x, int y) : lt(x, y) -> int
 *
 * desugars to
 *
 * fn ((int, int) : lt( *.{0}, *.{1} )) -> int
 *
 *)

and carg_base =
    BASE_formal
  | BASE_named of name_base

and carg_path =
    CARG_base of carg_base
  | CARG_ext of (carg_path * name_component)

and carg =
    CARG_path of carg_path
  | CARG_lit of lit

and constr =
    {
      constr_name: name;
      constr_args: carg array;
    }

and constrs = constr array

and ty_rec = (ident * slot) array

(* ty_tag is a sum type.
 *
 * a tag type expression either normalizes to a TY_tag or a TY_iso,
 * which (like in ocaml) is an indexed projection from an iso-recursive
 * group of TY_tags.
 *)

and ty_tag = (name, ty_tup) Hashtbl.t

and ty_iso =
    {
      iso_index: int;
      iso_group: ty_tag array
    }

and ty_sig =
    {
      sig_input_slots: slot array;
      sig_input_constrs: constrs;
      sig_output_slot: slot;
    }

and ty_fn_aux =
    {
      fn_purity: purity;
      fn_proto: proto option;
    }

and ty_fn = (ty_sig * ty_fn_aux)

and ty_pred = (slot array * constrs)

and ty_obj_header = (slot array * constrs)

and ty_obj = (ident,ty_fn) Hashtbl.t

and check_calls = (lval * (atom array)) array

(* put+ f(a,b) means to call f with current put addr and self as ret
 * addr. this is a 'tail yield' that bypasses us during f execution.
 *
 * ret+ f(a,b) means to call f with current put addr and current ret
 * addr. this is a 'tail call' that destroys us.
 *)
and stmt' =

  (* lval-assigning stmts. *)
    STMT_spawn of (lval * domain * lval * (atom array))
  | STMT_init_rec of (lval * ((ident * mode * atom) array) * lval option)
  | STMT_init_tup of (lval * ((mode * atom) array))
  | STMT_init_vec of (lval * slot * (atom array))
  | STMT_init_str of (lval * string)
  | STMT_init_port of lval
  | STMT_init_chan of (lval * (lval option))
  | STMT_copy of (lval * expr)
  | STMT_copy_binop of (lval * binop * atom)
  | STMT_call of (lval * lval * (atom array))
  | STMT_bind of (lval * lval * ((atom option) array))
  | STMT_recv of (lval * lval)
  | STMT_slice of (lval * lval * slice)

  (* control-flow stmts. *)
  | STMT_while of stmt_while
  | STMT_do_while of stmt_while
  | STMT_foreach of stmt_foreach
  | STMT_for of stmt_for
  | STMT_if of stmt_if
  | STMT_put of (proto option * atom option)
  | STMT_ret of (proto option * atom option)
  | STMT_be of (proto option * lval * (atom array))
  | STMT_alt_tag of stmt_alt_tag
  | STMT_alt_type of stmt_alt_type
  | STMT_alt_port of stmt_alt_port

  (* structural and misc stmts. *)
  | STMT_yield
  | STMT_join of lval
  | STMT_send of (lval * lval)
  | STMT_log of atom
  | STMT_note of atom
  | STMT_prove of (constrs)
  | STMT_check of (constrs * check_calls)
  | STMT_check_expr of expr
  | STMT_check_if of (constrs * check_calls * block)
  | STMT_block of block
  | STMT_decl of stmt_decl

and stmt = stmt' identified

and stmt_alt_tag =
    {
      alt_tag_lval: lval;
      alt_tag_arms: pat array;
    }

and stmt_alt_type =
    {
      alt_type_lval: lval;
      alt_type_arms: (ident * slot * stmt) array;
      alt_type_else: stmt option;
    }

and block' = stmt array
and block = block' identified

and stmt_decl =
    DECL_mod_item of (ident * mod_item)
  | DECL_slot of (slot_key * (slot identified))

and stmt_alt_port =
    {
      (* else lval is a timeout value, an f64 count of seconds. *)
      alt_port_arms: (lval * lval) array;
      alt_port_else: (lval * stmt) option;
    }

and stmt_while =
    {
      while_lval: ((stmt array) * expr);
      while_body: block;
    }

and stmt_foreach =
    {
      foreach_proto: proto;
      foreach_slot: (slot identified * ident);
      foreach_call: (lval * atom array);
      foreach_body: block;
    }

and stmt_for =
    {
      for_slot: (slot identified * ident);
      for_seq: ((stmt array) * lval);
      for_body: block;
    }

and stmt_if =
    {
      if_test: expr;
      if_then: block;
      if_else: block option;
    }

and slice =
    { slice_start: atom option;
      slice_len: atom option;
      slice_step: atom option }

and domain =
    DOMAIN_local
  | DOMAIN_thread

and pat' = ident * header_slots * block
and pat = pat' identified

and atom =
    ATOM_literal of (lit identified)
  | ATOM_lval of lval

and expr =
    EXPR_binary of (binop * atom * atom)
  | EXPR_unary of (unop * atom)
  | EXPR_atom of atom

and lit =
  | LIT_nil
  | LIT_bool of bool
  | LIT_mach of (ty_mach * int64 * string)
  | LIT_int of (int64 * string)
  | LIT_char of char
  | LIT_custom of lit_custom


and lit_custom =
    {
      lit_expander: lval;
      lit_arg: lval;
      lit_text: string;
    }

and lval_component =
    COMP_named of name_component
  | COMP_atom of atom


and lval =
    LVAL_base of name_base identified
  | LVAL_ext of (lval * lval_component)

and binop =
    BINOP_or
  | BINOP_and

  | BINOP_eq
  | BINOP_ne

  | BINOP_lt
  | BINOP_le
  | BINOP_ge
  | BINOP_gt

  | BINOP_lsl
  | BINOP_lsr
  | BINOP_asr

  | BINOP_add
  | BINOP_sub
  | BINOP_mul
  | BINOP_div
  | BINOP_mod
  | BINOP_send

and unop =
    UNOP_not
  | UNOP_neg


and header_slots = ((slot identified) * ident) array

and header_tup = (slot identified) array

and fn =
    {
      fn_input_slots: header_slots;
      fn_input_constrs: constrs;
      fn_output_slot: slot identified;
      fn_aux: ty_fn_aux;
      fn_body: block;
    }

and pred =
    {
      pred_input_slots: header_slots;
      pred_input_constrs: constrs;
      pred_body: block;
    }

and obj =
    {
      obj_state: header_slots;
      obj_constrs: constrs;
      obj_fns: (ident,fn identified) Hashtbl.t;
    }

(*
 * An 'a decl is a sort-of-thing that represents a parametric (generative)
 * declaration. Every reference to one of these involves applying 0 or more
 * type arguments, as part of *name resolution*.
 *
 * Slots are *not* parametric declarations. A slot has a specific type
 * even if it's a type that's bound by a quantifier in its environment.
 *)

and ty_param = ident * (ty_param_idx * opaque_id * mutability)

and mod_item' =
    MOD_ITEM_type of ty
  | MOD_ITEM_tag of (header_tup * ty_tag * node_id)
  | MOD_ITEM_pred of pred
  | MOD_ITEM_mod of (mod_view * mod_items)
  | MOD_ITEM_fn of fn
  | MOD_ITEM_obj of obj

and mod_item_decl =
    {
      decl_params: (ty_param identified) array;
      decl_item: mod_item';
    }

and mod_item = mod_item_decl identified
and mod_items = (ident, mod_item) Hashtbl.t

and import =
    {
      import_from: name;
      import_item: ident;
      import_as: ident;
    }

and export =
    EXPORT_all_decls
  | EXPORT_ident of ident

and mod_view =
    {
      view_imports: import list;
      view_exports: export list;
    }

and crate' =
    {
      crate_items: (mod_view * mod_items);
      crate_required: (node_id, (required_lib * nabi_conv)) Hashtbl.t;
      crate_files: (node_id,filename) Hashtbl.t;
      crate_main: name;
    }
and crate = crate' identified
;;

(***********************************************************************)

(* FIXME (bug 541525): finish all parts with ?foo? as their output. *)

let fmt = Format.fprintf;;

let fmt_ident (ff:Format.formatter) (i:ident) : unit =
  fmt ff  "%s" i

let fmt_temp (ff:Format.formatter) (t:temp_id) : unit =
  fmt ff  ".t%d" (int_of_temp t)

let fmt_slot_key ff (s:slot_key) : unit =
  match s with
      KEY_ident i -> fmt_ident ff i
    | KEY_temp t -> fmt_temp ff t

let fmt_proto (ff:Format.formatter) (p:proto) : unit =
  match p with
      PROTO_ques -> fmt ff "?"
    | PROTO_bang -> fmt ff "!"
    | PROTO_star -> fmt ff "*"
    | PROTO_plus -> fmt ff "+"

let rec fmt_app (ff:Format.formatter) (i:ident) (tys:ty array) : unit =
  fmt ff "%s" i;
  fmt_app_args ff tys

and fmt_app_args (ff:Format.formatter) (tys:ty array) : unit =
  fmt ff "[@[";
  for i = 0 to (Array.length tys) - 1;
  do
    if i != 0
    then fmt ff ",@ ";
    fmt_ty ff tys.(i);
  done;
  fmt ff "@]]"

and fmt_name_base (ff:Format.formatter) (nb:name_base) : unit =
  match nb with
      BASE_ident i -> fmt_ident ff i
    | BASE_temp t -> fmt_temp ff t
    | BASE_app (id, tys) -> fmt_app ff id tys

and fmt_name_component (ff:Format.formatter) (nc:name_component) : unit =
  match nc with
      COMP_ident i -> fmt_ident ff i
    | COMP_app (id, tys) -> fmt_app ff id tys
    | COMP_idx i -> fmt ff "_%d" i

and fmt_name (ff:Format.formatter) (n:name) : unit =
  match n with
      NAME_base nb -> fmt_name_base ff nb
    | NAME_ext (n, nc) ->
        fmt_name ff n;
        fmt ff ".";
        fmt_name_component ff nc

and fmt_mutable (ff:Format.formatter) (m:mutability) : unit =
  match m with
      MUTABLE -> fmt ff "mutable "
    | IMMUTABLE -> ()

and fmt_mode (ff:Format.formatter) (m:mode) : unit =
  match m with
      MODE_exterior m -> (fmt_mutable ff m; fmt ff "@@")
    | MODE_interior m -> fmt_mutable ff m
    | MODE_read_alias -> fmt ff "~"
    | MODE_write_alias -> fmt ff "^"

and fmt_slot (ff:Format.formatter) (s:slot) : unit =
  match s.slot_ty with
      None -> fmt ff "auto"
    | Some t ->
        fmt_mode ff s.slot_mode;
        fmt_ty ff t

and fmt_slots (ff:Format.formatter) (slots:slot array) (idents:(ident array) option) : unit =
  fmt ff "(@[";
  for i = 0 to (Array.length slots) - 1
  do
    if i != 0
    then fmt ff ",@ ";
    fmt_slot ff slots.(i);
    begin
      match idents with
          None -> ()
        | Some ids -> (fmt ff " "; fmt_ident ff ids.(i))
    end;
  done;
  fmt ff "@])"

and fmt_ty_fn
    (ff:Format.formatter)
    (ident_and_params:(ident * ty_param array) option)
    (tf:ty_fn)
    : unit =
  let (tsig, ta) = tf in
    begin
      match ta.fn_purity with
          PURE -> fmt ff "pure "
        | IMPURE mut -> fmt_mutable ff mut
    end;
    fmt ff "fn";
    begin
      match ta.fn_proto with
          None -> ()
        | Some p -> fmt_proto ff p
    end;
    begin
      match ident_and_params with
          Some (id, params) ->
            fmt ff " ";
            fmt_ident_and_params ff id params
        | None -> ()
    end;
    fmt_slots ff tsig.sig_input_slots None;
    fmt_decl_constrs ff tsig.sig_input_constrs;
    fmt ff " -> ";
    fmt_slot ff tsig.sig_output_slot;

and fmt_ty_pred
    (ff:Format.formatter)
    (ident_and_params:(ident * ty_param array) option)
    (tp:ty_pred)
    : unit =
  let (in_slots, in_constrs) = tp in
    fmt ff "pred";
    begin
      match ident_and_params with
          Some (id, params) ->
            fmt ff " ";
            fmt_ident_and_params ff id params
        | None -> ()
    end;
    fmt_slots ff in_slots None;
    fmt_decl_constrs ff in_constrs

and fmt_tag (ff:Format.formatter) (ttag:ty_tag) : unit =
  fmt ff "@[tag(@[";
  let first = ref true in
    Hashtbl.iter
      begin
        fun name ttup ->
          (if !first
           then first := false
           else fmt ff ",@ ");
          fmt_name ff name;
          fmt_slots ff ttup None
      end
      ttag;
    fmt ff "@])@]"

and fmt_iso (ff:Format.formatter) (tiso:ty_iso) : unit =
  fmt ff "@[iso [@[";
  for i = 0 to (Array.length tiso.iso_group) - 1
  do
    if i != 0
    then fmt ff ",@ ";
    if i == tiso.iso_index
    then fmt ff "<%d>: " i
    else fmt ff "%d: " i;
    fmt_tag ff tiso.iso_group.(i);
  done;
  fmt ff "@]]@]"

and fmt_ty (ff:Format.formatter) (t:ty) : unit =
  match t with
    TY_any -> fmt ff "any"
  | TY_nil -> fmt ff "()"
  | TY_bool -> fmt ff "bool"
  | TY_mach m -> fmt_mach ff m
  | TY_int -> fmt ff "int"
  | TY_char -> fmt ff "char"
  | TY_str -> fmt ff "str"

  | TY_tup slots -> fmt_slots ff slots None
  | TY_vec s -> (fmt ff "vec["; fmt_slot ff s; fmt ff "]")
  | TY_chan t -> (fmt ff "chan["; fmt_ty ff t; fmt ff "]")
  | TY_port t -> (fmt ff "port["; fmt_ty ff t; fmt ff "]")

  | TY_rec slots ->
      let (idents, slots) =
        let (idents, slots) = List.split (Array.to_list slots) in
          (Array.of_list idents, Array.of_list slots)
      in
        fmt ff "@[rec";
        fmt_slots ff slots (Some idents);
        fmt ff "@]"

  | TY_param (i, oid, m) -> (fmt_mutable ff m;
                             fmt ff "<p#%d/o#%d>" i
                               (int_of_opaque oid))
  | TY_named n -> fmt_name ff n
  | TY_type -> fmt ff "type"

  | TY_fn tfn -> fmt_ty_fn ff None tfn
  | TY_task -> fmt ff "task"
  | TY_tag ttag -> fmt_tag ff ttag
  | TY_iso tiso -> fmt_iso ff tiso
  | TY_idx idx -> fmt ff "<idx#%d>" idx
  | TY_constrained _ -> fmt ff "?constrained?"
  | TY_pred tp -> fmt_ty_pred ff None tp

  | TY_obj fns ->
      fmt_obox ff;
      fmt ff "obj ";
      fmt_obr ff;
      Hashtbl.iter
        begin
          fun id fn ->
            fmt ff "@\n";
            fmt_ty_fn ff (Some (id, [||])) fn;
            fmt ff ";"
        end
        fns;
      fmt_cbb ff


and fmt_constrs (ff:Format.formatter) (cc:constr array) : unit =
  Array.iter (fmt_constr ff) cc

and fmt_decl_constrs (ff:Format.formatter) (cc:constr array) : unit =
  if Array.length cc = 0
  then ()
  else
    begin
      fmt ff " : ";
      fmt_constrs ff cc
    end

and fmt_constr (ff:Format.formatter) (c:constr) : unit =
  fmt_name ff c.constr_name;
  fmt ff "(@[";
  for i = 0 to (Array.length c.constr_args) - 1
  do
    if i != 0
    then fmt ff ",@ ";
    fmt_carg ff c.constr_args.(i);
  done;
  fmt ff "@])"

and fmt_carg_path (ff:Format.formatter) (cp:carg_path) : unit =
  match cp with
      CARG_base BASE_formal -> fmt ff "*"
    | CARG_base (BASE_named nb) -> fmt_name_base ff nb
    | CARG_ext (base, nc) ->
        fmt_carg_path ff base;
        fmt ff ".";
        fmt_name_component ff nc

and fmt_carg (ff:Format.formatter) (ca:carg) : unit =
  match ca with
      CARG_path cp -> fmt_carg_path ff cp
    | CARG_lit lit -> fmt_lit ff lit

and fmt_obox ff = Format.pp_open_box ff 4
and fmt_obox_3 ff = Format.pp_open_box ff 3
and fmt_cbox ff = Format.pp_close_box ff ()
and fmt_obr ff = fmt ff "{"
and fmt_cbr ff = fmt ff "@\n}"
and fmt_cbb ff = (fmt_cbox ff; fmt_cbr ff)

and fmt_stmts (ff:Format.formatter) (ss:stmt array) : unit =
  Array.iter (fmt_stmt ff) ss;

and fmt_block (ff:Format.formatter) (b:stmt array) : unit =
  fmt_obox ff;
  fmt_obr ff;
  fmt_stmts ff b;
  fmt_cbb ff;

and fmt_binop (ff:Format.formatter) (b:binop) : unit =
  fmt ff "%s"
    begin
      match b with
          BINOP_or -> "|"
        | BINOP_and -> "&"

        | BINOP_eq -> "=="
        | BINOP_ne -> "!="

        | BINOP_lt -> "<"
        | BINOP_le -> "<="
        | BINOP_ge -> ">="
        | BINOP_gt -> ">"

        | BINOP_lsl -> "<<"
        | BINOP_lsr -> ">>"
        | BINOP_asr -> ">>>"

        | BINOP_add -> "+"
        | BINOP_sub -> "-"
        | BINOP_mul -> "*"
        | BINOP_div -> "/"
        | BINOP_mod -> "%"
        | BINOP_send -> "<|"
    end


and fmt_unop (ff:Format.formatter) (u:unop) : unit =
  fmt ff "%s"
    begin
      match u with
          UNOP_not -> "!"
        | UNOP_neg -> "-"
    end

and fmt_expr (ff:Format.formatter) (e:expr) : unit =
  match e with
    EXPR_binary (b,a1,a2) ->
      begin
        fmt_atom ff a1;
        fmt ff " ";
        fmt_binop ff b;
        fmt ff " ";
        fmt_atom ff a2
      end
  | EXPR_unary (u,a) ->
      begin
        fmt_unop ff u;
        fmt_atom ff a
      end
  | EXPR_atom a -> fmt_atom ff a

and fmt_mach (ff:Format.formatter) (m:ty_mach) : unit =
  match m with
    TY_u8 -> fmt ff "u8"
  | TY_u16 -> fmt ff "u16"
  | TY_u32 -> fmt ff "u32"
  | TY_u64 -> fmt ff "u64"
  | TY_s8 -> fmt ff "s8"
  | TY_s16 -> fmt ff "s16"
  | TY_s32 -> fmt ff "s32"
  | TY_s64 -> fmt ff "s64"
  | TY_f32 -> fmt ff "f32"
  | TY_f64 -> fmt ff "f64"

and fmt_lit (ff:Format.formatter) (l:lit) : unit =
  match l with
  | LIT_nil -> fmt ff "()"
  | LIT_bool true -> fmt ff "true"
  | LIT_bool false -> fmt ff "false"
  | LIT_mach (m, _, s) ->
      begin
        fmt_mach ff m;
        fmt ff "(%s)" s
      end
  | LIT_int (_,s) -> fmt ff "%s" s
  | LIT_char c -> fmt ff "'%s'" (Char.escaped c)
  | LIT_custom _ -> fmt ff "?lit?"

and fmt_domain (ff:Format.formatter) (d:domain) : unit =
  match d with
      DOMAIN_local -> ()
    | DOMAIN_thread -> fmt ff "thread "

and fmt_atom (ff:Format.formatter) (a:atom) : unit =
  match a with
      ATOM_literal lit -> fmt_lit ff lit.node
    | ATOM_lval lval -> fmt_lval ff lval

and fmt_atoms (ff:Format.formatter) (az:atom array) : unit =
  fmt ff "(";
  Array.iteri
    begin
      fun i a ->
        if i != 0
        then fmt ff ", ";
        fmt_atom ff a;
    end
    az;
  fmt ff ")"

and fmt_lval_component (ff:Format.formatter) (lvc:lval_component) : unit =
  match lvc with
      COMP_named nc -> fmt_name_component ff nc
    | COMP_atom a ->
        begin
          fmt ff "(";
          fmt_atom ff a;
          fmt ff ")"
        end

and fmt_lval (ff:Format.formatter) (l:lval) : unit =
  match l with
      LVAL_base nbi -> fmt_name_base ff nbi.node
    | LVAL_ext (lv, lvc) ->
        begin
          fmt_lval ff lv;
          fmt ff ".";
          fmt_lval_component ff lvc
        end

and fmt_stmt (ff:Format.formatter) (s:stmt) : unit =
  fmt ff "@\n";
  fmt_stmt_body ff s

and fmt_stmt_body (ff:Format.formatter) (s:stmt) : unit =
  begin
    match s.node with
        STMT_log at ->
          begin
            fmt ff "log ";
            fmt_atom ff at;
            fmt ff ";"
          end

      | STMT_spawn (dst, domain, fn, args) ->
          fmt_lval ff dst;
          fmt ff " = spawn ";
          fmt_domain ff domain;
          fmt_lval ff fn;
          fmt_atoms ff args;
          fmt ff ";";

      | STMT_while sw ->
          let (stmts, e) = sw.while_lval in
            begin
              fmt_obox ff;
              fmt ff "while (";
              if Array.length stmts != 0
              then fmt_block ff stmts;
              fmt_expr ff e;
              fmt ff ") ";
              fmt_obr ff;
              fmt_stmts ff sw.while_body.node;
              fmt_cbb ff
            end

      | STMT_do_while sw ->
          let (stmts, e) = sw.while_lval in
            begin
              fmt_obox ff;
              fmt ff "do ";
              fmt_obr ff;
              fmt_stmts ff sw.while_body.node;
              fmt ff "while (";
              if Array.length stmts != 0
              then fmt_block ff stmts;
              fmt_expr ff e;
              fmt ff ");";
              fmt_cbb ff
            end

      | STMT_if sif ->
          fmt_obox ff;
          fmt ff "if (";
          fmt_expr ff sif.if_test;
          fmt ff ") ";
          fmt_obr ff;
          fmt_stmts ff sif.if_then.node;
          begin
            match sif.if_else with
                None -> ()
              | Some e ->
                  begin
                    fmt_cbb ff;
                    fmt_obox_3 ff;
                    fmt ff " else ";
                    fmt_obr ff;
                    fmt_stmts ff e.node
                  end
          end;
          fmt_cbb ff

      | STMT_ret (po, ao) ->
          fmt ff "ret";
          begin
            match po with
                None -> ()
              | Some proto -> fmt_proto ff proto
          end;
          fmt ff " ";
          begin
            match ao with
                None -> ()
              | Some at -> fmt_atom ff at
          end;
          fmt ff ";"

      | STMT_be (po, fn, args) ->
          fmt ff "be";
          begin
            match po with
                None -> ()
              | Some proto -> fmt_proto ff proto
          end;
          fmt ff " ";
          fmt_lval ff fn;
          fmt_atoms ff args;
          fmt ff ";";

      | STMT_block b -> fmt_block ff b.node

      | STMT_copy (lv, ex) ->
          fmt_lval ff lv;
          fmt ff " = ";
          fmt_expr ff ex;
          fmt ff ";"

      | STMT_copy_binop (lv, binop, at) ->
          fmt_lval ff lv;
          fmt ff " ";
          fmt_binop ff binop;
          fmt ff "=";
          fmt_atom ff at;
          fmt ff ";"

      | STMT_call (dst, fn, args) ->
          fmt_lval ff dst;
          fmt ff " = ";
          fmt_lval ff fn;
          fmt_atoms ff args;
          fmt ff ";";

      | STMT_decl (DECL_slot (skey, sloti)) ->
          if sloti.node.slot_ty != None then fmt ff "let ";
          fmt_slot ff sloti.node;
          fmt ff " ";
          fmt_slot_key ff skey;
          fmt ff ";"

      | STMT_decl (DECL_mod_item (ident, item)) ->
          fmt_mod_item ff ident item

      | STMT_init_rec (dst, entries, base) ->
          fmt_lval ff dst;
          fmt ff " = rec(";
          for i = 0 to (Array.length entries) - 1
          do
            if i != 0
            then fmt ff ", ";
            let (ident, mode, atom) = entries.(i) in
              fmt_ident ff ident;
              fmt ff " = ";
              fmt_mode ff mode;
              fmt_atom ff atom;
          done;
          begin
            match base with
                None -> ()
              | Some b ->
                  fmt ff " with ";
                  fmt_lval ff b
          end;
          fmt ff ");"

      | STMT_init_vec (dst, _, atoms) ->
          fmt_lval ff dst;
          fmt ff " = vec(";
          for i = 0 to (Array.length atoms) - 1
          do
            if i != 0
            then fmt ff ", ";
            fmt_atom ff atoms.(i);
          done;
          fmt ff ");"

      | STMT_init_tup (dst, entries) ->
          fmt_lval ff dst;
          fmt ff " = (";
          for i = 0 to (Array.length entries) - 1
          do
            if i != 0
            then fmt ff ", ";
            let (mode, atom) = entries.(i) in
              fmt_mode ff mode;
              fmt_atom ff atom;
          done;
          fmt ff ");";

      | STMT_init_str (dst, s) ->
          fmt_lval ff dst;
          fmt ff " = \"%s\"" (String.escaped s)

      | STMT_check_expr expr ->
          fmt ff "check (";
          fmt_expr ff expr;
          fmt ff ");"

      | STMT_check (constrs, _) ->
          fmt ff "check ";
          fmt_constrs ff constrs;
          fmt ff ";"

      | STMT_for sfor ->
          let (slot, ident) = sfor.for_slot in
          let (stmts, lval) = sfor.for_seq in
            begin
              fmt_obox ff;
              fmt ff "for (";
              fmt_slot ff slot.node;
              fmt ff " ";
              fmt_ident ff ident;
              fmt ff " in ";
              fmt_stmts ff stmts;
              fmt_lval ff lval;
              fmt ff ") ";
              fmt_obr ff;
              fmt_stmts ff sfor.for_body.node;
              fmt_cbb ff
            end

      | STMT_foreach sforeach ->
          let proto = sforeach.foreach_proto in
          let (slot, ident) = sforeach.foreach_slot in
          let (f, az) = sforeach.foreach_call in
            begin
              fmt_obox ff;
              fmt ff "for";
              fmt_proto ff proto;
              fmt ff " (";
              fmt_slot ff slot.node;
              fmt ff " ";
              fmt_ident ff ident;
              fmt ff " = ";
              fmt_lval ff f;
              fmt_atoms ff az;
              fmt ff ") ";
              fmt_obr ff;
              fmt_stmts ff sforeach.foreach_body.node;
              fmt_cbb ff
            end

      | STMT_put (proto, atom) ->
          fmt ff "put";
          begin
            begin
              match proto with
                  Some p -> fmt_proto ff p
                | None -> ()
            end;
            begin
              match atom with
                  Some a -> (fmt ff " "; fmt_atom ff a)
                | None -> ()
            end;
            fmt ff ";"
          end

      | STMT_alt_tag _ -> fmt ff "?stmt_alt_tag?"
      | STMT_alt_type _ -> fmt ff "?stmt_alt_type?"
      | STMT_alt_port _ -> fmt ff "?stmt_alt_port?"
      | STMT_prove _ -> fmt ff "?stmt_prove?"
      | STMT_check_if _ -> fmt ff "?stmt_check_if?"
      | STMT_note _ -> fmt ff "?stmt_note?"
      | STMT_bind _ -> fmt ff "?stmt_bind?"
      | STMT_slice _ -> fmt ff "?stmt_slice?"
      | STMT_init_chan _ -> fmt ff "?stmt_init_chan?"
      | STMT_init_port _ -> fmt ff "?stmt_init_port?"
      | STMT_yield -> fmt ff "?stmt_yield?"
      | STMT_join _ -> fmt ff "?stmt_join?"
      | STMT_send _ -> fmt ff "?stmt_send?"
      | STMT_recv _ -> fmt ff "?stmt_recv?"
  end

and fmt_decl_params (ff:Format.formatter) (params:ty_param array) : unit =
  if Array.length params = 0
  then ()
  else
    begin
      fmt ff "[";
      for i = 0 to (Array.length params) - 1
      do
        if i <> 0
        then fmt ff ", ";
        let (ident, (i, oid, mut)) = params.(i) in
          fmt_mutable ff mut;
          fmt_ident ff ident;
          fmt ff "=<p#%d/o#%d>" i (int_of_opaque oid)
      done;
      fmt ff "]"
    end;

and fmt_header_slots (ff:Format.formatter) (hslots:header_slots) : unit =
  fmt_slots ff
    (Array.map (fun (s,_) -> s.node) hslots)
    (Some (Array.map (fun (_, i) -> i) hslots))

and fmt_ident_and_params (ff:Format.formatter) (id:ident) (params:ty_param array) : unit =
  fmt_ident ff id;
  fmt_decl_params ff params

and fmt_fn (ff:Format.formatter) (id:ident) (params:ty_param array) (f:fn) : unit =
  fmt_obox ff;
  begin
    match f.fn_aux.fn_purity with
        PURE -> fmt ff "pure "
      | IMPURE mut -> fmt_mutable ff mut
  end;
  fmt ff "fn";
  begin
    match f.fn_aux.fn_proto with
        None -> ()
      | Some p -> fmt_proto ff p
  end;
  fmt ff " ";
  fmt_ident_and_params ff id params;
  fmt_header_slots ff f.fn_input_slots;
  fmt_decl_constrs ff f.fn_input_constrs;
  fmt ff " -> ";
  fmt_slot ff f.fn_output_slot.node;
  fmt ff " ";
  fmt_obr ff;
  fmt_stmts ff f.fn_body.node;
  fmt_cbb ff


and fmt_obj (ff:Format.formatter) (id:ident) (params:ty_param array) (obj:obj) : unit =
  fmt_obox ff;
  fmt ff "obj ";
  fmt_ident_and_params ff id params;
  fmt_header_slots ff obj.obj_state;
  fmt_decl_constrs ff obj.obj_constrs;
  fmt ff " ";
  fmt_obr ff;
  Hashtbl.iter
    begin
      fun id fn ->
        fmt ff "@\n";
        fmt_fn ff id [||] fn.node
    end
    obj.obj_fns;
  fmt_cbb ff


and fmt_mod_item (ff:Format.formatter) (id:ident) (item:mod_item) : unit =
  fmt ff "@\n";
  let params = item.node.decl_params in
    begin
      match item.node.decl_item with
          MOD_ITEM_type ty ->
            fmt ff "type ";
            fmt_ident_and_params ff id (Array.map (fun i -> i.node) params);
            fmt ff " = ";
            fmt_ty ff ty;
            fmt ff ";";

        | MOD_ITEM_tag _ ->
            fmt ff "?tagdecl?"

        | MOD_ITEM_pred p ->
            fmt_obox ff;
            fmt ff "pred ";
            fmt_ident_and_params ff id (Array.map (fun i -> i.node) params);
            fmt_header_slots ff p.pred_input_slots;
            fmt_decl_constrs ff p.pred_input_constrs;
            fmt ff " ";
            fmt_obr ff;
            fmt_stmts ff p.pred_body.node;
            fmt_cbb ff

        | MOD_ITEM_mod (view,items) ->
            fmt_obox ff;
            fmt ff "mod ";
            fmt_ident_and_params ff id (Array.map (fun i -> i.node) params);
            fmt ff " ";
            fmt_obr ff;
            fmt_mod_view ff view;
            fmt_mod_items ff items;
            fmt_cbb ff

        | MOD_ITEM_fn f ->
            fmt_fn ff id (Array.map (fun i -> i.node) params) f

        | MOD_ITEM_obj obj ->
            fmt_obj ff id (Array.map (fun i -> i.node) params) obj
    end

and fmt_import (ff:Format.formatter) (import:import) : unit =
  fmt ff "@\n";
  fmt ff "import ";
  if import.import_as <> import.import_item
  then
    fmt ff "%s = " import.import_as;
  fmt_name ff import.import_from;
  fmt ff ".";
  fmt_ident ff import.import_item;

and fmt_export (ff:Format.formatter) (export:export) : unit =
  fmt ff "@\n";
  match export with
      EXPORT_all_decls -> fmt ff "export *;"
    | EXPORT_ident i -> fmt ff "export %s;" i


and fmt_mod_view (ff:Format.formatter) (mv:mod_view) : unit =
  List.iter (fmt_import ff) mv.view_imports;
  List.iter (fmt_export ff) mv.view_exports

and fmt_mod_items (ff:Format.formatter) (mi:mod_items) : unit =
  Hashtbl.iter (fmt_mod_item ff) mi

and fmt_crate (ff:Format.formatter) (c:crate) : unit =
  let (view,items) = c.node.crate_items in
    fmt_mod_view ff view;
    fmt_mod_items ff items


let fmt_to_str (f:Format.formatter -> 'a -> unit) (v:'a) : string =
  let buf = Buffer.create 16 in
  let bf = Format.formatter_of_buffer buf in
    begin
      f bf v;
      Format.pp_print_flush bf ();
      Buffer.contents buf
    end

let sprintf_fmt
    (f:Format.formatter -> 'a -> unit)
    : (unit -> 'a -> string) =
  (fun _ -> fmt_to_str f)


let sprintf_expr = sprintf_fmt fmt_expr;;
let sprintf_name = sprintf_fmt fmt_name;;
let sprintf_lval = sprintf_fmt fmt_lval;;
let sprintf_lval_component = sprintf_fmt fmt_lval_component;;
let sprintf_atom = sprintf_fmt fmt_atom;;
let sprintf_slot = sprintf_fmt fmt_slot;;
let sprintf_slot_key = sprintf_fmt fmt_slot_key;;
let sprintf_mutable = sprintf_fmt fmt_mutable;;
let sprintf_ty = sprintf_fmt fmt_ty;;
let sprintf_tag = sprintf_fmt fmt_tag;;
let sprintf_carg = sprintf_fmt fmt_carg;;
let sprintf_constr = sprintf_fmt fmt_constr;;
let sprintf_stmt = sprintf_fmt fmt_stmt;;
let sprintf_mod_items = sprintf_fmt fmt_mod_items;;
let sprintf_decl_params = sprintf_fmt fmt_decl_params;;
let sprintf_app_args = sprintf_fmt fmt_app_args;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
