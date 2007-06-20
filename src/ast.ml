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

type pos = (string * int * int)
;;

let nopos : pos = ("no-file", 0, 0)
;;

(* "names" are statically computable references to particular slots;
   they never involve vector indexing. They are formed by a
   dot-separated sequence of identifier and/or literal-number
   components, the latter representing tuple components (foo.0, foo.1,
   etc). *)

type name_component =
    COMP_string of string
  | COMP_tupidx of int
;;

type name = 
   {
   name_base: string;
   name_rest: name_component array;
   }
;;

type ty_mach = 
    TY_unsigned
  | TY_signed
  | TY_ieee_bfp
  | TY_ieee_dfp
;;

type ty_arith =
    TY_int
  | TY_nat
  | TY_rat
;;

type proto = 
    PROTO_call  (* func  foo(...): returns 1 value. A function.                             *)
  | PROTO_bang  (* func! foo(...): yields 1 value. Never resumes.                           *)
  | PROTO_ques  (* func? foo(...): may yield 1 value or return w/o yielding. Never resumes. *)
  | PROTO_star  (* func* foo(...): may yield N >= 0 values, then returns.                   *)
  | PROTO_plus  (* func+ foo(...): yields N > 0 values then returns.                        *)
;;

type ty = 
    TY_dyn
  | TY_type

  | TY_bool
  | TY_mach of (ty_mach * int)
  | TY_arith of ty_arith
  | TY_str
  | TY_char

  | TY_rec of ty_rec
  | TY_alt of ty_alt
  | TY_tup of ty_tup
  | TY_vec of ty_vec

  | TY_func of ty_func
  | TY_chan of ty_sig

  | TY_prog
  | TY_proc 

  | TY_pred of ty_pred
  | TY_quote of ty_quote

  | TY_named of name
  | TY_abstr of (ty * ty_abstr array)
  | TY_apply of (ty * ty array)

  | TY_lim of ty
	
and ty_abstr = 
    { 
      abstr_name: string;
      abstr_lim: bool
    }
	  
(* Slots can have an "external" qualifier put on them.  If present,
  the external qualifier means that the slot refers to an external
  allocation. If absent, the standard allocation-packing rules apply:
  we try to use immediate or dependent mode, and fall back to internal
  (with transplanting) otherwise. *)

and slot = 
    SLOT_external of ty
  | SLOT_standard of ty

(* 
 * In closed type terms a predicate in the state may refer to
 * components of the term by anchoring off the "formal symbol" '*',
 * which represents "the term this state is attached to". 
 * 
 * 
 * For example, if I have a tuple type (int,int),
 * I may wish to enforce the lt predicate on it;
 * I can write this as a closed type term like:
 * 
 * ( int, int ) : lt( *.0, *.1 )
 * 
 * In fact all tuple types are converted to this
 * form for purpose of type-compatibility testing;
 * the tuple
 * 
 * func ( int x, int y ) : lt( x, y ) -> int
 * 
 * actually has type
 * 
 * func (( int, int ) : lt( *.0, *.1 )) -> int
 * 
 *)      

and parg_base = 
    BASE_formal 
  | BASE_free of string

and parg =
    {
      parg_base: parg_base;
      parg_rest: name_component array;
    }

and pred = 
    { 
      pred_name: name;
      pred_args: parg array;
    }
      
and state = pred array

and ty_rec = 
    { 
      rec_slots: rec_slot array;
      rec_state: state;
    }

and rec_slot = 
    { 
      rec_slot_name: string;
      rec_slot_type: slot;
      rec_slot_state: state;
    }

and ty_alt = alt_case array

and alt_case = 
    { 
      alt_case_name: string;
      alt_case_rec: ty_rec option;
    }

and ty_tup = 
    {
      tup_types: ty array;
      tup_state: state;
    }

and ty_vec =
    {
      vec_elt_type: ty;
      vec_elt_state: state;
    }

and ty_func = 
    { 
      func_inline: bool; 
      func_pure: bool;
      func_sig: ty_sig; 
    }

and pmode = 
    PMODE_copy
  | PMODE_move_in
  | PMODE_move_in_out

and ty_sig = 
    { 
      sig_proto: proto;
      sig_param_types: ty array;
      sig_param_modes: pmode array;
      sig_param_state: state;
      sig_result_ty: ty;
    }

and ty_pred = 
    { 
      ty_pred_auto: bool;
      ty_pred_inline: bool;
      ty_pred_param_ty: ty;
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
  | STMT_yield of (expr option * pos)
  | STMT_return of (expr * pos)
  | STMT_assert of (pred * pos)
  | STMT_block of ((stmt array) * pos)
  | STMT_move of (lval * lval)
  | STMT_copy of (lval * expr)
  | STMT_call of (lval * expr)
  | STMT_send of (lval * expr) (* Async call *)
  | STMT_decl of decl

and stmt_while = 
    {
     while_expr: expr;
     while_body: stmt;
     while_pos: pos;
   }
      
and stmt_foreach = 
    {
     foreach_bindings: decl array;
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

and expr =
    EXPR_literal of (lit * pos)
  | EXPR_binary of (binop * pos * expr * expr)
  | EXPR_unary of (unop * pos * expr)
  | EXPR_tuple of (expr array * pos)
  | EXPR_lval of lval
  | EXPR_call of (lval * expr)

and radix = HEX | DEC | BIN

and lit = 
    LIT_str of string
  | LIT_char of char
  | LIT_bool of bool
  | LIT_mach of lit_mach
  | LIT_arith of (ty_arith * radix * Num.num)
  | LIT_custom of lit_custom
  | LIT_quote of lit_quote

and lit_quote = 
    QUOTE_expr of expr
  | QUOTE_type of ty
  | QUOTE_decl of decl
  | QUOTE_stmt of stmt

and lit_mach = 
    LIT_unsigned of int * radix
  | LIT_signed of int * radix
  | LIT_ieee_bfp of float
  | LIT_ieee_dfp of (int * int)

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
     lval_base: string;
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

and decl = 
    { 
      decl_name: string;
      decl_pos: pos;
      decl_artifact: artifact;
    }

and artifact = 
    ARTIFACT_type of ty
  | ARTIFACT_code of (ty * code)
  | ARTIFACT_slot of (slot * (expr option))

and code = 
    CODE_prog of prog
  | CODE_func of func
  | CODE_port of port

and bind = 
    {
     bind_ty: ty_sig;
     bind_names: string array;
   }

and func = 
    {
     func_proto: proto;
     func_bind: bind;
     func_body: fbody;
   }

and fbody = 
    FBODY_native of string
  | FBODY_stmt of stmt

and port = 
    {
     port_proto: proto;
     port_bind: bind;
     port_auto_body: stmt option;
    }

and init = 
    {
     init_bind: bind;
     init_body: stmt;
   }

and prog = 
    { 
      prog_auto: bool;
      prog_init: init option;
      prog_main: stmt option;
      prog_fini: stmt option;
      prog_decls: decl array;
    }
;;

type visibility = 
    VIS_public
  | VIS_crate
  | VIS_local
;;

type decl_top = (visibility * decl)
;;

