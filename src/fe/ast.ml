
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
  | TY_prog of ty_prog

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

and carg =
    CARG_base of carg_base
  | CARG_ext of (carg * name_component)

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
    }

and ty_rec = (ident, slot) Hashtbl.t

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
      sig_output_slot: slot;
    }

and ty_fn_aux =
    {
      fn_pure: bool;
      fn_lim: ty_limit;
      fn_proto: proto option;
    }

and ty_fn = (ty_sig * ty_fn_aux)

and ty_pred = slot array

and ty_prog =
    {
      prog_mod_ty: mod_type_items;
      prog_init_ty: (slot array) option;
    }

(* put+ f(a,b) means to call f with current put addr and self as ret
 * addr. this is a 'tail yield' that bypasses us during f execution.
 *
 * ret+ f(a,b) means to call f with current pur addr and current ret
 * addr. this is a 'tail call' that destroys us.
 *)
and stmt' =
    STMT_log of atom
  | STMT_spawn of atom  (* FIXME: should produce a proc. *)
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
  | STMT_checkif of (constrs * stmt)
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
  | EXPR_rec of ((ident, atom) Hashtbl.t)
  | EXPR_vec of (atom array)
  | EXPR_tup of (atom array)

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
      fn_output_slot: slot identified;
      fn_aux: ty_fn_aux;
      fn_body: block;
    }

and pred =
    {
      pred_input_slots: ((slot identified) * ident) array;
      pred_body: stmt;
    }

and init =
    {
      init_input_slots: ((slot identified) * ident) array;
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
  | MOD_TYPE_ITEM_prog of ty_prog decl

and mod_type_item = mod_type_item' identified

and mod_type_items = (ident, mod_type_item) Hashtbl.t

and mod_items = (ident, mod_item) Hashtbl.t

and crate =
    {
      crate_items: mod_items;
      crate_main: name;
    }
;;


(* Stringification *)


let string_of_key k =
  match k with
      KEY_temp i -> "<temp#" ^ (string_of_int (int_of_temp i)) ^ ">"
    | KEY_ident i -> i
;;

let rec string_of_name_component comp =
  match comp with
      COMP_ident id -> id
    | COMP_app (id, tys) ->
        id ^ "[" ^ (String.concat "," (List.map string_of_ty (Array.to_list tys))) ^ "]"
    | COMP_idx i ->
        "{" ^ (string_of_int i) ^ "}"

and string_of_name name =
  match name with
      NAME_base (BASE_ident id) -> id
    | NAME_base (BASE_temp n) -> "<temp#" ^ (string_of_int (int_of_temp n)) ^ ">"
    | NAME_base (BASE_app (id, tys)) ->
        id ^ "[" ^ (String.concat "," (List.map string_of_ty (Array.to_list tys))) ^ "]"
    | NAME_ext (n, c) ->
        (string_of_name n) ^ "." ^ (string_of_name_component c)

and string_of_ty ty =
  (* FIXME: flesh this out; it's currently only diagnostic. *)
  match ty with
      TY_any -> "any"
    | TY_nil -> "()"
    | TY_bool -> "bool"
    | TY_mach _ -> "mach"
    | TY_int -> "int"
    | TY_char -> "char"
    | TY_str -> "str"

    | TY_tup _ -> "tup"
    | TY_vec _ -> "vec"
    | TY_rec _ -> "rec"

    | TY_tag _ -> "tag"
    | TY_iso _ -> "iso"
    | TY_idx _ -> "idx"

    | TY_fn _ -> "fn"
    | TY_pred _ -> "pred"
    | TY_chan _ -> "chan"
    | TY_port _ -> "port"

    | TY_mod _ -> "mod"
    | TY_prog _ -> "prog"

    | TY_opaque _ -> "opaque"
    | TY_named name -> "<named:" ^ (string_of_name name) ^ ">"
    | TY_type -> "ty"

    | TY_constrained _ -> "constrained"
    | TY_lim _ -> "lim"



(***********************************************************************) 

let fmt = Format.fprintf;;

let fmt_ident (ff:Format.formatter) (i:ident) : unit =
  fmt ff  "%s" i

let fmt_temp (ff:Format.formatter) (t:temp_id) : unit =
  fmt ff  "t#%d" (int_of_temp t)

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
  fmt ff "%s[@[<hv 2>" i;
  for i = 0 to Array.length tys;
  do
    if i != 0
    then fmt ff ",@;";
    fmt_ty ff tys.(i);
  done;
  fmt ff "@;@]]"

and fmt_name_base (ff:Format.formatter) (nb:name_base) : unit =
  match nb with
      BASE_ident i -> fmt_ident ff i
    | BASE_temp t -> fmt_temp ff t
    | BASE_app (id, tys) -> fmt_app ff id tys

and fmt_name_component (ff:Format.formatter) (nc:name_component) : unit =
  match nc with
      COMP_ident i -> fmt_ident ff i
    | COMP_app (id, tys) -> fmt_app ff id tys
    | COMP_idx i -> fmt ff "%d" i

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
  fmt ff "(@[<hv 2>";
  for i = 0 to Array.length slots;
  do
    if i != 0
    then fmt ff ",@;";
    fmt_slot ff slots.(i);
    begin
      match idents with
          None -> ()
        | Some ids -> fmt_ident ff ids.(i)
    end;
  done;
  fmt ff "@;@])"

and fmt_fn_header (ff:Format.formatter) (tf:ty_fn) (id:ident option) : unit =
  let (tsig, ta) = tf in
    if ta.fn_pure
    then fmt ff "pure@;";
    if ta.fn_lim = LIMITED
    then fmt ff "lim@;";
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
    fmt_slots ff tsig.sig_input_slots None;
    fmt ff "@;->@;";
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

  | TY_rec htab ->
      begin
        (* FIXME: sort struct members. *)
        fmt ff "rec {@[<hv 2>";
        Hashtbl.iter (fun id slot -> fmt_slot ff slot; fmt_ident ff id; fmt ff ";@;";) htab;
        fmt ff "@]}@;"
      end

  | TY_opaque id -> fmt ff "o#%d" (int_of_opaque id)
  | TY_named n -> fmt_name ff n
  | TY_type -> fmt ff "type"
  | TY_lim t -> (fmt ff "lim@;"; fmt_ty ff t)

  | TY_fn tfn -> fmt_fn_header ff tfn None

  (* FIXME: finish these as needed. *)
  | TY_mod mti -> fmt ff "?mod?"
  | TY_prog tp -> fmt ff "?prog?"
  | TY_tag ttag -> fmt ff "?tag?"
  | TY_iso tiso -> fmt ff "?iso?"
  | TY_idx idx -> fmt ff "?idx?"
  | TY_constrained t -> fmt ff "?constrained?"
  | TY_pred p -> fmt ff "?pred?"




(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
