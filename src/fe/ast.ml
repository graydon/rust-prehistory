
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

(* "names" are statically computable references to particular slots;
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
  | TY_vec of ty
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

  | TY_mod of (mod_type_items)
  | TY_prog of ty_sig

  | TY_opaque of opaque_id
  | TY_named of name
  | TY_type

  | TY_constrained of (ty * constrs)
  | TY_lim of ty

and mode =
    MODE_exterior
  | MODE_interior
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

and prog =
    {
      prog_init: (init identified) option;
      prog_main: block option;
      prog_fini: block option;
      prog_mod: mod_items;
      prog_slots: (ident, slot identified) Hashtbl.t;
    }

and ty_rec = (ident * slot) array

(* ty_tag is a sum type.
 *
 * a tag type expression either normalizes to a TY_tag or a TY_iso,
 * which (like in ocaml) is an indexed projection from an iso-recursive
 * group of TY_tags.
 *)

and ty_tag = (ident, ty) Hashtbl.t

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
      fn_pure: bool;
      fn_lim: ty_limit;
      fn_proto: proto option;
    }

and ty_fn = (ty_sig * ty_fn_aux)

and ty_pred = (slot array * constrs)

(* put+ f(a,b) means to call f with current put addr and self as ret
 * addr. this is a 'tail yield' that bypasses us during f execution.
 *
 * ret+ f(a,b) means to call f with current pur addr and current ret
 * addr. this is a 'tail call' that destroys us.
 *)
and stmt' =
    STMT_log of atom
  | STMT_spawn of (lval * lval * (atom array))
  | STMT_init_rec of (lval * ((ident * atom) array))
  | STMT_init_vec of (lval * (atom array))
  | STMT_init_tup of (lval * (atom array))
  | STMT_init_port of lval
  | STMT_init_chan of (lval * (lval option))
  | STMT_while of stmt_while
  | STMT_do_while of stmt_while
  | STMT_foreach of stmt_foreach
  | STMT_for of stmt_for
  | STMT_if of stmt_if
  | STMT_try of stmt_try
  | STMT_put of (proto option * atom option)
  | STMT_ret of (proto option * atom option)
  | STMT_be of (proto option * lval * (atom array))
  | STMT_alt_tag of stmt_alt_tag
  | STMT_alt_type of stmt_alt_type
  | STMT_alt_port of stmt_alt_port
  | STMT_prove of (constrs)
  | STMT_check of (constrs)
  | STMT_check_expr of atom
  | STMT_check_if of (constrs * block)
  | STMT_block of block
  | STMT_copy of (lval * expr)
  | STMT_call of (lval * lval * (atom array))
  | STMT_send of (lval * atom)
  | STMT_recv of (lval * lval)
  | STMT_decl of stmt_decl
  | STMT_use of (ty * ident * lval)


and stmt = stmt' identified

and stmt_alt_tag =
    {
      alt_tag_lval: lval;
      alt_tag_arms: (ident, (slot * stmt)) Hashtbl.t;
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
      (* else lval is a timeout value, a b64 count of seconds. *)
      alt_port_arms: (lval * lval) array;
      alt_port_else: (lval * stmt) option;
    }

and stmt_while =
    {
      while_lval: ((stmt array) * atom);
      while_body: block;
    }

and stmt_foreach =
    {
      foreach_proto: proto;
      foreach_call: (lval * lval array);
      foreach_body: block;
    }

and stmt_for =
    {
      for_init: stmt;
      for_test: ((stmt array) * atom);
      for_step: stmt;
      for_body: stmt;
    }

and stmt_if =
    {
      if_test: atom;
      if_then: block;
      if_else: block option;
    }

and stmt_try =
    {
      try_body: block;
      try_fail: block option;
      try_fini: block option;
    }

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
  | LIT_mach of (ty_mach * string)
  | LIT_int of (Big_int.big_int * string)
  | LIT_char of char
  | LIT_str of string
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


and fn =
    {
      fn_input_slots: ((slot identified) * ident) array;
      fn_input_constrs: constrs;
      fn_output_slot: slot identified;
      fn_aux: ty_fn_aux;
      fn_body: block;
    }

and pred =
    {
      pred_input_slots: ((slot identified) * ident) array;
      pred_input_constrs: constrs;
      pred_body: block;
    }

and init =
    {
      init_input_slots: ((slot identified) * ident) array;
      init_input_constrs: constrs;
      init_output_slot: slot identified;
      init_body: block;
    }

(*
 * An 'a decl is a sort-of-thing that represents a parametric (generative)
 * declaration. Every reference to one of these involves applying 0 or more
 * type arguments, as part of *name resolution*.
 *
 * Slots are *not* parametric declarations. A slot has a specific type
 * even if it's a type that's bound by a quantifier in its environment.
 *)


and ty_limit =
    LIMITED
  | UNLIMITED

and 'a decl =
    {
      decl_params: (ty_limit * ident) array;
      decl_item: 'a;
    }

(*
 * We have module types and module declarations. A module declaration is
 * a table of module items. A module type is a table of module-type items.
 *
 * The latter describe the former, despite the fact that modules can
 * contain types: module types are not *equivalent* to module declarations,
 * and every module declaration gives rise to a module value that conforms to
 * a possibly-existential module type.
 *
 * Module values of particular module types are 'opened' by a 'use' statement.
 * This converts a module with opaque existential types into a module with
 * a corresponding set of concrete, disjoint opaque (skolem) types. These can
 * be projected out of the module bound by the 'use' statement in subsequent
 * declarations and statements, without risk of collision with other types.
 *
 * For this reason, the MOD_TYPE_ITEM_opaque_type constructor takes no
 * arguments -- it simply describes the presence of *some* existential type
 * in a module -- but whatever that existential may be, it is converted
 * in the bound module to a MOD_ITEM_type (TY_opaque i) for some fresh i,
 * when 'use'd.
 *
 * This technique is explained in some depth in section 4.2 of the
 * paper "first class modules for haskell", by Mark Shields and Simon
 * Peyton Jones. Hopefully I'm doing it right. It's a little near the
 * limit of tricks I understand.
 *)

and mod_item' =
    MOD_ITEM_opaque_type of ty decl
  | MOD_ITEM_public_type of ty decl
  | MOD_ITEM_pred of pred decl
  | MOD_ITEM_mod of mod_items decl
  | MOD_ITEM_fn of fn decl
  | MOD_ITEM_prog of prog decl

and mod_item = mod_item' identified

and mod_type_item' =
    MOD_TYPE_ITEM_opaque_type of ty_limit decl
  | MOD_TYPE_ITEM_public_type of ty decl
  | MOD_TYPE_ITEM_pred of ty_pred decl
  | MOD_TYPE_ITEM_mod of mod_type_items decl
  | MOD_TYPE_ITEM_fn of ty_fn decl
  | MOD_TYPE_ITEM_prog of ty_sig decl

and mod_type_item = mod_type_item' identified

and mod_type_items = (ident, mod_type_item) Hashtbl.t

and mod_items = (ident, mod_item) Hashtbl.t

and crate =
    {
      crate_items: mod_items;
      crate_files: (node_id,filename) Hashtbl.t;
      crate_main: name;
    }
;;

(***********************************************************************) 

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
  fmt ff "%s[@[" i;
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
    | COMP_idx i -> fmt ff "{%d}" i

and fmt_name (ff:Format.formatter) (n:name) : unit =
  match n with
      NAME_base nb -> fmt_name_base ff nb
    | NAME_ext (n, nc) ->
        fmt_name ff n;
        fmt ff ".";
        fmt_name_component ff nc

and fmt_slot (ff:Format.formatter) (s:slot) : unit =
  match s.slot_ty with
      None -> fmt ff "auto"
    | Some t ->
        begin
          match s.slot_mode with
              MODE_exterior -> fmt ff "@@"
            | MODE_interior -> ()
            | MODE_read_alias -> fmt ff "~"
            | MODE_write_alias -> fmt ff "^"
        end;
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
        | Some ids -> fmt_ident ff ids.(i)
    end;
  done;
  fmt ff "@])"

and fmt_limit (ff:Format.formatter) (lim:ty_limit) : unit =
    if lim = LIMITED
    then fmt ff "lim@;"
    else ()

and fmt_fn_header (ff:Format.formatter) (tf:ty_fn) 
    (id:ident option) (params:((ty_limit * ident) array) option) : unit =
  let (tsig, ta) = tf in
    if ta.fn_pure
    then fmt ff "pure@ ";
    fmt_limit ff ta.fn_lim;
    fmt ff "fn";
    begin
      match ta.fn_proto with
          None -> ()
        | Some p -> fmt_proto ff p
    end;
    begin
      match id with
          None -> ()
        | Some i -> fmt_ident ff i
    end;
    begin
      match params with
          None -> ()
        | Some p -> fmt_decl_params ff p
    end;
    fmt_slots ff tsig.sig_input_slots None;
    fmt ff "@ ->@ ";
    fmt_slot ff tsig.sig_output_slot;

and fmt_ty (ff:Format.formatter) (t:ty) : unit =
  match t with
    TY_any -> fmt ff "any"
  | TY_nil -> fmt ff "()"
  | TY_bool -> fmt ff "bool"
  | TY_mach TY_u8 -> fmt ff "u8"
  | TY_mach TY_u16 -> fmt ff "u16"
  | TY_mach TY_u32 -> fmt ff "u32"
  | TY_mach TY_u64 -> fmt ff "u64"
  | TY_mach TY_s8 -> fmt ff "s8"
  | TY_mach TY_s16 -> fmt ff "s16"
  | TY_mach TY_s32 -> fmt ff "s32"
  | TY_mach TY_s64 -> fmt ff "s64"
  | TY_mach TY_b64 -> fmt ff "b64"
  | TY_int -> fmt ff "int"
  | TY_char -> fmt ff "char"
  | TY_str -> fmt ff "str"

  | TY_tup slots -> fmt_slots ff slots None
  | TY_vec t -> (fmt ff "vec["; fmt_ty ff t; fmt ff "]")
  | TY_chan t -> (fmt ff "chan["; fmt_ty ff t; fmt ff "]")
  | TY_port t -> (fmt ff "port["; fmt_ty ff t; fmt ff "]")

  | TY_rec slots ->
      begin
        fmt ff "@[rec(@[";
        let first = ref true in
          Array.iter
            (fun (id, slot) ->
               if (!first)
               then first := false
               else fmt ff ",@ ";
               fmt_slot ff slot;
               fmt ff "@ ";
               fmt_ident ff id;)
            slots;
        fmt ff "@])@]"
      end

  | TY_opaque id -> fmt ff "o#%d" (int_of_opaque id)
  | TY_named n -> fmt_name ff n
  | TY_type -> fmt ff "type"
  | TY_lim t -> (fmt ff "lim@ "; fmt_ty ff t)

  | TY_fn tfn -> fmt_fn_header ff tfn None None

  (* FIXME: finish these as needed. *)
  | TY_mod mti -> fmt ff "?mod?"
  | TY_prog tp -> fmt ff "?prog?"
  | TY_tag ttag -> fmt ff "?tag?"
  | TY_iso tiso -> fmt ff "?iso?"
  | TY_idx idx -> fmt ff "?idx?"
  | TY_constrained t -> fmt ff "?constrained?"
  | TY_pred p -> fmt ff "?pred?"

and fmt_constrs (ff:Format.formatter) (cc:constr array) : unit =
  Array.iter (fmt_constr ff) cc

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

and fmt_lit (ff:Format.formatter) (l:lit) : unit =
  match l with
  | LIT_nil -> fmt ff "()"
  | LIT_bool true -> fmt ff "true"
  | LIT_bool false -> fmt ff "false"
  | LIT_mach (_, s) -> fmt ff "%s" s
  | LIT_int (_,s) -> fmt ff "%s" s
  | LIT_char c -> fmt ff "'%s'" (Char.escaped c)
  | LIT_str s -> fmt ff "\"%s\"" (String.escaped s)
  | LIT_custom _ -> fmt ff "?lit?"

and fmt_atom (ff:Format.formatter) (a:atom) : unit =
  match a with
      ATOM_literal lit -> fmt_lit ff lit.node
    | ATOM_lval lval -> fmt_lval ff lval

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
  begin
    match s.node with
        STMT_log at ->
          begin
            fmt ff "log ";
            fmt_atom ff at;
            fmt ff ";"
          end

      | STMT_spawn (dst, prog, args) ->
          fmt_lval ff dst;
          fmt ff " = spawn ";
          fmt_lval ff prog;
          fmt ff "(";
          for i = 0 to (Array.length args) - 1
          do
            if i != 0
            then fmt ff ", ";
            fmt_atom ff args.(i);
          done;
          fmt ff ");";

      | STMT_while sw ->
          let (stmts, at) = sw.while_lval in
            begin
              fmt_obox ff;
              fmt ff "while (";
              if Array.length stmts != 0
              then fmt_block ff stmts;
              fmt_atom ff at;
              fmt ff ") ";
              fmt_obr ff;
              fmt_stmts ff sw.while_body.node;
              fmt_cbb ff
            end

      | STMT_do_while sw ->
          let (stmts, at) = sw.while_lval in
            begin
              fmt_obox ff;
              fmt ff "do ";
              fmt_obr ff;
              fmt_stmts ff sw.while_body.node;
              fmt ff "while (";
              if Array.length stmts != 0
              then fmt_block ff stmts;
              fmt_atom ff at;
              fmt ff ");";
              fmt_cbb ff
            end

      | STMT_if sif ->
          fmt_obox ff;
          fmt ff "if (";
          fmt_atom ff sif.if_test;
          fmt ff ") ";
          fmt_obr ff;
          fmt_stmts ff sif.if_then.node;
          begin
            match sif.if_else with
                None -> ()
              | Some e ->
                  begin
                    fmt_cbr ff;
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

      | STMT_block b -> fmt_block ff b.node

      | STMT_copy (lv, ex) ->
          fmt_lval ff lv;
          fmt ff " = ";
          fmt_expr ff ex;
          fmt ff ";"

      | STMT_call (dst, fn, args) ->
          fmt_lval ff dst;
          fmt ff " = ";
          fmt_lval ff fn;
          fmt ff "(";
          for i = 0 to (Array.length args) - 1
          do
            if i != 0
            then fmt ff ", ";
            fmt_atom ff args.(i);
          done;
          fmt ff ");";

      | STMT_decl (DECL_slot (skey, sloti)) ->
          fmt ff "let ";
          fmt_slot ff sloti.node;
          fmt ff " ";
          fmt_slot_key ff skey;
          fmt ff ";"

      | STMT_init_rec (dst, entries) ->
          fmt_lval ff dst;
          fmt ff " = init_rec (...);"

      | _ -> fmt ff "?stmt?;"
  end

(*
  | STMT_for of stmt_for
  | STMT_foreach of stmt_foreach
  | STMT_try of stmt_try
  | STMT_put of (proto option * atom option)
  | STMT_be of (proto option * lval * (atom array))
  | STMT_alt_tag of stmt_alt_tag
  | STMT_alt_type of stmt_alt_type
  | STMT_alt_port of stmt_alt_port
  | STMT_prove of (constrs)
  | STMT_check of (constrs)
  | STMT_checkif of (constrs * stmt)
  | STMT_send of (lval * atom)
  | STMT_recv of (lval * lval)
  | STMT_use of (ty * ident * lval)
*)

and fmt_decl_params (ff:Format.formatter) (params:(ty_limit * ident) array) : unit =
  if Array.length params = 0
  then ()
  else
    begin
      fmt ff "[";
      for i = 0 to (Array.length params) - 1
      do
        if i = 0
        then fmt ff ", ";
        let (lim, id) = params.(i) in
          fmt_limit ff lim;
          fmt ff " ";
          fmt_ident  ff id
      done;
      fmt ff "]"
    end;

and fmt_mod_item (ff:Format.formatter) (id:ident) (item:mod_item) : unit =
  fmt ff "@\n";
  begin
    match item.node with
        MOD_ITEM_opaque_type td -> fmt ff "?tydecl?"
      | MOD_ITEM_public_type td -> fmt ff "?tydecl?"
      | MOD_ITEM_pred pd -> fmt ff "?preddecl?"

      | MOD_ITEM_mod md ->
          begin
            fmt_obox ff;
            fmt ff "mod ";
            fmt_ident ff id;
            fmt_decl_params ff md.decl_params;
            fmt_obr ff;
            fmt_mod_items ff md.decl_item;
            fmt_cbb ff
          end

      | MOD_ITEM_fn fd ->
          begin
            fmt_obox ff;
            fmt ff "fn ";
            fmt_ident ff id;
            fmt ff "(...) -> (...) ";
            fmt_obr ff;
            fmt_stmts ff fd.decl_item.fn_body.node;
            (* let (tfn = ty_fn_of_fn
               fmt_fn_header ff fd.decl_item. (Some id) (Some fd.decl_params);
            *)
            fmt_cbb ff
          end

      | MOD_ITEM_prog pd ->
          begin
            fmt_obox ff;
            fmt ff "prog ";
            fmt_ident ff id;
            fmt_decl_params ff pd.decl_params;
            fmt_obr ff;
            fmt_mod_items ff pd.decl_item.prog_mod;
            begin
              match pd.decl_item.prog_init with
                  None -> ()
                | Some ii -> fmt ff "@\n?init?;"
            end;
            begin
              match pd.decl_item.prog_main with
                  None -> ()
                | Some b ->
                    begin
                      fmt ff "@\n";
                      fmt_obox ff;
                      fmt ff "main ";
                      fmt_obr ff;
                      fmt_stmts ff b.node;
                      fmt_cbb ff
                    end
            end;
            begin
              match pd.decl_item.prog_fini with
                  None -> ()
                | Some b ->
                    begin
                      fmt ff "@\n";
                      fmt_obox ff;
                      fmt ff "fini ";
                      fmt_obr ff;
                      fmt_stmts ff b.node;
                      fmt_cbb ff
                    end
            end;
            fmt_cbb ff
          end
  end

and fmt_mod_items (ff:Format.formatter) (mi:mod_items) : unit =
  Hashtbl.iter (fmt_mod_item ff) mi

and fmt_crate (ff:Format.formatter) (c:crate) : unit =
    fmt_mod_items ff c.crate_items


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


let sprintf_lval = sprintf_fmt fmt_lval;;
let sprintf_atom = sprintf_fmt fmt_atom;;
let sprintf_slot = sprintf_fmt fmt_slot;;
let sprintf_slot_key = sprintf_fmt fmt_slot_key;;
let sprintf_ty = sprintf_fmt fmt_ty;;
let sprintf_carg = sprintf_fmt fmt_carg;;
let sprintf_constr = sprintf_fmt fmt_constr;;
let sprintf_stmt = sprintf_fmt fmt_stmt;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
