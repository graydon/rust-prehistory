
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

type nonce = int
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

type ty_mach = 
    TY_unsigned
  | TY_signed
  | TY_ieee_bfp
  | TY_ieee_dfp
;;

type proto = 
    PROTO_ques  (* fn? foo(...): may yield 1 value or return w/o yielding. Never resumes. *)
  | PROTO_bang  (* fn! foo(...): yields 1 value. Never resumes.                           *)
  | PROTO_star  (* fn* foo(...): may yield N >= 0 values, then returns.                   *)
  | PROTO_plus  (* fn+ foo(...): yields N > 0 values then returns.                        *)
;;

type name_base = 
    BASE_ident of ident
  | BASE_temp of nonce
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
  | TY_mach of (ty_mach * int)
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
  | TY_chan of ty
  | TY_port of ty

  | TY_mod of (mod_type_items)
  | TY_prog of ty_prog

  | TY_opaque of nonce
  | TY_named of name
  | TY_type
      
  | TY_constrained of (ty * constrs)
  | TY_lim of ty
      
and slot = 
    SLOT_exterior of ty
  | SLOT_interior of ty
  | SLOT_read_alias of ty
  | SLOT_write_alias of ty
  | SLOT_auto


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
      prog_init: init option;
      prog_main: stmt option;
      prog_fini: stmt option;
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

      
and ty_tup = slot array
           
and ty_sig = 
    { 
      sig_input_slot: slot;      
      sig_output_slot: slot;
    }

and ty_fn = 
    {
      fn_pure: bool;
      fn_lim: ty_limit;
      fn_sig: ty_sig;
      fn_proto: proto option;
    }


and ty_prog = 
    {
      prog_mod_ty: mod_type_items;
      prog_init_ty: ty_sig option;
    }
      
(* put+ f(a,b) means to call f with current put addr and self as ret
 * addr. this is a 'tail yield' that bypasses us during f execution.
 * 
 * ret+ f(a,b) means to call f with current pur addr and current ret
 * addr. this is a 'tail call' that destroys us.
 *)
and stmt' =
    STMT_while of stmt_while
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
  | STMT_block of stmt_block
  | STMT_copy of (lval * expr)
  | STMT_call of (lval * lval * (atom array))
  | STMT_send of (lval * atom)
  | STMT_recv of (lval * lval)
  | STMT_decl of stmt_decl 
  | STMT_use of (ty * ident * lval)
      
and stmt = stmt' spanned

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

and slot_key = 
    KEY_ident of ident
  | KEY_temp of nonce


and local =
    {
      local_slot: (slot ref) spanned;
      local_vreg: (int option) ref;
      local_aliased: bool ref;
      local_layout: layout;
    }

and frame = 
    {
      frame_heavy: bool;
      frame_layout: layout;
      frame_locals: (slot_key, local) Hashtbl.t;
      frame_items: (ident, (layout * mod_item)) Hashtbl.t;
    }

and stmt_block = 
    {
      block_frame: frame;
      block_stmts: stmt array;
    }

and stmt_decl = 
    DECL_mod_item of (ident * mod_item)
  | DECL_slot of (slot_key * ((slot ref) spanned))

      
and stmt_alt_port = 
    { 
      (* else lval is a timeout value, a b64 count of seconds. *)
      alt_port_arms: (lval * lval) array;
      alt_port_else: (lval * stmt) option;
    }

and stmt_while = 
    {
      while_lval: ((stmt array) * atom);
      while_body: stmt;
    }
      
and stmt_foreach = 
    {
      foreach_proto: proto;
      foreach_frame: frame;
      foreach_call: (lval * lval array);
      foreach_body: stmt;
    }
      
and stmt_for = 
    {
      for_frame: frame;
      for_init: stmt;
      for_test: ((stmt array) * atom);
      for_step: stmt;
      for_body: stmt;
    }

and stmt_if = 
    {
      if_test: atom;
      if_then: stmt;
      if_else: stmt option;
    }

and stmt_try = 
    {
      try_body: stmt;
      try_fail: stmt option;
      try_fini: stmt option;
    }

and atom = 
    ATOM_literal of (lit spanned)
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
  | LIT_unsigned of (int * string)
  | LIT_signed of (int * string)
  | LIT_ieee_bfp of (float * string)
  | LIT_ieee_dfp of ((int * int) * string)
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
    

(* 
 * An lval can resolve to:
 * 
 *   - A local slot that you access through memory operations because it's big, or 
 *     because it is aliased.
 * 
 *   - A module item that you access indirectly through a pointer or some memory structure.
 * 
 *   - A purely local slot that is register sized.
 *)
      
and resolved_path = 
    RES_pr of abi_pseudo_reg
  | RES_member of (layout * resolved_path)
  | RES_deref of resolved_path
  | RES_idx of (resolved_path * resolved_path)
  | RES_vreg of ((int option) ref)

and resolved_target = 
    RES_slot of local
  | RES_item of mod_item

and lval_resolved = 
    {
      res_path: (resolved_path option) ref;
      res_target: (resolved_target option) ref;
    }

and lval' = 
    LVAL_base of name_base
  | LVAL_ext of (lval' * lval_component)

and lval = 
    { 
      lval_src: lval' spanned;
      lval_res: lval_resolved;
    }
      
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
      fn_fixup: fixup;
      fn_ty: ty_fn;
      fn_bind: ident array;
      fn_frame: frame;
      fn_body: stmt;
    }

and pred = 
    {
      pred_ty: ty_sig;
      pred_bind: ident array;
      pred_body: stmt;
    }
      
and init = 
    {
      init_sig: ty_sig;
      init_bind: ident array;
      init_body: stmt;
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

and mod_item = mod_item' spanned   

and mod_type_item' = 
    MOD_TYPE_ITEM_opaque_type of ty_limit decl
  | MOD_TYPE_ITEM_public_type of ty decl
  | MOD_TYPE_ITEM_pred of ty_sig decl
  | MOD_TYPE_ITEM_mod of mod_type_items decl
  | MOD_TYPE_ITEM_fn of ty_fn decl
  | MOD_TYPE_ITEM_prog of ty_prog decl

and mod_type_item = mod_type_item' spanned

and mod_type_items = (ident, mod_type_item) Hashtbl.t


and mod_items = (ident, mod_item) Hashtbl.t
;;


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
