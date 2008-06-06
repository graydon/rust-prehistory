open Array;;
open Hashtbl;;

(* 
 * There are two kinds of rust files:
 * 
 * .rc files, containing crates.
 * .rs files, containing source.
 *
 *)


(* 
 * Slot names are given by a dot-separated path within the current
 * module namespace. 
 *)

type ident = string
;;

type filename = string
;;

type pos = (filename * int * int)
;;

type nonce = int
;;

let nopos : pos = ("no-file", 0, 0)
;;

(* "names" are statically computable references to particular slots;
   they never involve vector indexing. They are formed by a
   dot-separated sequence of identifier and/or index components,
   the latter representing tuple/call/ctor components (foo.#0, foo.#1, etc). 
   
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
    PROTO_ques  (* func? foo(...): may yield 1 value or return w/o yielding. Never resumes. *)
  | PROTO_bang  (* func! foo(...): yields 1 value. Never resumes.                           *)
  | PROTO_star  (* func* foo(...): may yield N >= 0 values, then returns.                   *)
  | PROTO_plus  (* func+ foo(...): yields N > 0 values then returns.                        *)
;;

type name_component =
    COMP_ident of ident
  | COMP_idx of int
  | COMP_app of (ty array)

and name = 
    {
     name_base: ident;
     name_rest: name_component array;
   }

(* 
 * Type expressions are transparent to type names, their equality is structural.
 * (after normalization)
 *)
and ty = 

    TY_any
  | TY_native
  | TY_nil
  | TY_bool
  | TY_char
  | TY_str

      (* 
       * These 2 can probably be redefined as algebraic types.
       *)

  | TY_type  
  | TY_prog

  | TY_int
  | TY_mach of (ty_mach * int)

  | TY_tup of ty_tup
  | TY_vec of ty_vec
  | TY_rec of ty_rec
  | TY_alt of ty_alt

  | TY_func of ty_func
  | TY_chan of ty
  | TY_port of ty
      
  | TY_quote of ty_quote
      
  | TY_named of name
  | TY_opaque of nonce
      
  | TY_constrained of (ty * constrs)
  | TY_mod of (mod_type_items)
      

(* Slots can have an smode qualifier put on them: exterior or alias.
 * If there is no qualifier, the slot is interior. Slots acquire an
 * implicit set of constraints from their type, but can also have an
 * explicit set of constraints stuck on them.  
 *)
      
and slot = 
    { 
      slot_smode: smode;
      slot_ty: ty;
      slot_constrs: constrs;
    }

and smode = 
    SMODE_exterior
  | SMODE_interior
  | SMODE_alias


(* In closed type terms a constraint may refer to components of the
 * term by anchoring off the "formal symbol" '*', which represents "the
 * term this constraint is attached to".
 * 
 * 
 * For example, if I have a tuple type tup[int,int], I may wish to enforce
 * the lt predicate on it; I can write this as a constrained type term
 * like:
 * 
 * tup[int, int] : lt( *.#0, *.#1 )
 * 
 * In fact all tuple types are converted to this form for purpose of
 * type-compatibility testing; the tuple
 * 
 * func (int x, int y) : lt(x, y) -> int
 * 
 * actually has type
 * 
 * func (tup[int, int] : lt( *.#0, *.#1 )) -> int
 * 
 *)

and carg_base = 
    BASE_formal 
  | BASE_named of ident
      
and carg =
    {
      carg_base: carg_base;
      carg_rest: name_component array;
    }

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
      
and ty_rec = 
    { 
      rec_slots: (ident, slot) Hashtbl.t;
      rec_constrs: constrs;
    }
      
(* 
 * An alt type is modeled -- structurally -- as an array of its N
 * variant types. The types do not have to be disjoint.
 * 
 * An alt *declaration* -- as a binding in a module -- is modeled as a
 * triple of a type, an injection function, and a projection function.
 *)
and ty_alt = ty array
    
and ty_tup = 
    {
      tup_types: ty array;
      tup_constrs: constrs;
    }
      
and ty_vec =
    {
      vec_elt_type: ty;
    }
      
and ty_sig = 
    { 
      sig_param_smodes: smode array;
      sig_param_types: ty array;
      
      sig_pre_cond: constrs;
      sig_post_cond: constrs;

      sig_result_smode: smode;
      sig_result_ty: ty;
    }

and ty_func = 
    {
      func_pure: bool;
      func_inline: bool;
      func_sig: ty_sig;
      func_proto: proto option;
    }
      
and ty_quote = 
    TY_quote_expr
  | TY_quote_type
  | TY_quote_decl
  | TY_quote_stmt
      
(* Probably this list should be a lot longer; the canonical rule *)
      (* I've been using is to make a quotation type for every *)
      (* nonterminal *)
      
and stmt =
    STMT_while of stmt_while
  | STMT_foreach of stmt_foreach
  | STMT_for of stmt_for
  | STMT_if of stmt_if
  | STMT_try of stmt_try
  | STMT_put of (expr option * pos)
  | STMT_ret of (expr * pos)
  | STMT_prove of (constrs * pos)
  | STMT_check of (constrs * pos)
  | STMT_checkif of (constrs * stmt * pos)
  | STMT_block of ((stmt array) * pos)
  | STMT_move of (lval * lval * pos)
  | STMT_copy of (lval * expr * pos)
  | STMT_call of (lval * (expr array) * pos)
  | STMT_be of (lval * (expr array) * pos)
  | STMT_send of (lval * expr * pos)
  | STMT_decl of (ident * mod_item * pos)
  | STMT_use of (ty * ident * lval * pos)
      
and stmt_while = 
    {
      while_expr: expr;
      while_body: stmt;
      while_pos: pos;
    }
      
and stmt_foreach = 
    {
      foreach_proto: proto;
      foreach_slot: (ident * slot);
      foreach_body: stmt;
      foreach_pos: pos;
    }
      
and stmt_for = 
    {
      for_init: stmt;
      for_test: expr;
      for_step: stmt;
      for_body: stmt;
      for_pos: pos;
    }

and stmt_if = 
    {
      if_test: expr;
      if_then: stmt;
      if_else: stmt option;
      if_pos: pos;
    }

and stmt_try = 
    {
      try_body: stmt;
      try_fail: stmt option;
      try_fini: stmt option;
      try_pos: pos;
    }

and rec_input = 
    REC_from_copy of (ident * expr)
  | REC_from_move of (ident * lval)

and expr =
    EXPR_literal of (lit * pos)
  | EXPR_binary of (binop * pos * expr * expr)
  | EXPR_unary of (unop * pos * expr)
  | EXPR_lval of (lval * pos)
  | EXPR_call of (lval * pos * (expr array))
  | EXPR_rec of (name * pos * (rec_input array))
  | EXPR_mod of (mod_items * pos)
          
and lit = 
    LIT_str of string
  | LIT_char of char
  | LIT_bool of bool
  | LIT_mach of lit_mach
  | LIT_int of (Big_int.big_int * string)
  | LIT_custom of lit_custom
  | LIT_quote of lit_quote
  | LIT_func of (ty_func * func)
  | LIT_prog of prog

(* 
 * Need to add more quotes as time progresses.
 *)
and lit_quote = 
    QUOTE_expr of expr
  | QUOTE_type of ty
  | QUOTE_stmt of stmt

and lit_mach = 
    LIT_unsigned of (int * string)
  | LIT_signed of (int * string)
  | LIT_ieee_bfp of (float * string)
  | LIT_ieee_dfp of ((int * int) * string)

and lit_custom = 
    {
      lit_expander: lval;
      lit_arg: expr;
      lit_text: string;
    }

and lidx =
    LIDX_named of (name_component * pos)
  | LIDX_index of expr
    
and lval = 
    {
      lval_base: ident;
      lval_rest: lidx array;
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

and unop =
    UNOP_not
  | UNOP_neg


and func = 
    {
      func_ty: ty_func;
      func_bind: ident array;
      func_body: fbody;
    }

and pred = 
    {
      pred_ty: ty_sig;
      pred_bind: ident array;
      pred_body: fbody;
    }

and fbody = 
    FBODY_native of ident
  | FBODY_stmt of stmt
      
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

and 'a decl = 
    {
      decl_params: ident array;
      decl_item: 'a;
    }

(* 
 * We have module types and module expressions. A module expression is 
 * a table of module items. A module type is a table of module-type items.
 * 
 * The latter describe the former, despite the fact that modules can 
 * contain types: module types are not *equivalent* to module expressions,
 * and every module expression gives rise to a module value that conforms to
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
   
and mod_item = 
    MOD_ITEM_type of ty decl
  | MOD_ITEM_pred of pred decl
  | MOD_ITEM_mod of mod_items decl
  | MOD_ITEM_slot of slot
   
      
and mod_type_item = 
    MOD_TYPE_ITEM_opaque_type
  | MOD_TYPE_ITEM_public_type of ty decl
  | MOD_TYPE_ITEM_pred of ty decl
  | MOD_TYPE_ITEM_mod of mod_type_items decl
  | MOD_TYPE_ITEM_slot of ty

and mod_type_items = (ident, mod_type_item) Hashtbl.t


and mod_items = (ident, mod_item) Hashtbl.t
;;
