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

type ty_arith =
    TY_int
  | TY_nat
  | TY_rat
;;

type proto = 
    PROTO_call  (* func  foo(...): returns 1 value. A function.                             *)
  | PROTO_ques  (* func? foo(...): may yield 1 value or return w/o yielding. Never resumes. *)
  | PROTO_bang  (* func! foo(...): yields 1 value. Never resumes.                           *)
  | PROTO_star  (* func* foo(...): may yield N >= 0 values, then returns.                   *)
  | PROTO_plus  (* func+ foo(...): yields N > 0 values then returns.                        *)
;;

type name_component =
    COMP_ident of ident
  | COMP_idx of int
;;

type name = 
    {
     name_base: ident;
     name_rest: name_component array;
   }
;;

type ty = 
    TY_dyn
  | TY_type

  | TY_nil
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
  | TY_constrained of (ty * state)

  | TY_lim of ty
	
and ty_abstr = 
    { 
      abstr_name: ident;
      abstr_lim: bool
    }
	  
(* Slots can have an smode qualifier put on them: exterior or alias.
 * If there is no qualifier, the slot is interior. Slots acquire an
 * implicit state from their type, but can also have an explicit state
 * stuck on them. *)

and slot = 
    { 
      slot_const: bool;
      slot_smode: smode;
      slot_ty: ty;
      slot_ident: ident;
      slot_state: state;
    }

and smode = 
    SMODE_exterior
  | SMODE_interior
  | SMODE_alias

(* 
 * In closed type terms a constraint in the state may refer to
 * components of the term by anchoring off the "formal symbol" '*',
 * which represents "the term this state is attached to". 
 * 
 * 
 * For example, if I have a tuple type (int,int),
 * I may wish to enforce the lt predicate on it;
 * I can write this as a constrained type term like:
 * 
 * ( int, int ) : lt( *.#0, *.#1 )
 * 
 * In fact all tuple types are converted to this
 * form for purpose of type-compatibility testing;
 * the tuple
 * 
 * func ( int x, int y ) : lt( x, y ) -> int
 * 
 * actually has type
 * 
 * func (( int, int ) : lt( *.#0, *.#1 )) -> int
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
      
and state = constr array

and ty_rec = 
    { 
      rec_slots: rec_slot array;
      rec_state: state;
    }

and rec_slot = 
    { 
      rec_slot_ident: ident;
      rec_slot_type: slot;
      rec_slot_state: state;
    }

and ty_alt = alt_case array

and alt_case = 
    { 
      alt_case_name: ident;
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
  | PMODE_move_out
  | PMODE_move_in_out

and ty_sig = 
    { 
      sig_proto: proto;

      sig_param_smodes: smode array;
      sig_param_types: ty array;
      sig_param_pmodes: pmode array;

      sig_invoke_state: state;

      sig_result_smode: smode;
      sig_result_ty: ty;
      sig_result_pmode: pmode;

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
  | STMT_assert of (constr * pos)
  | STMT_block of ((stmt array) * pos)
  | STMT_move of (lval * lval)
  | STMT_copy of (lval * expr)
  | STMT_call of (lval * (expr array))
  | STMT_send of (lval * (expr array)) (* Async call *)
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
  | EXPR_lval of (lval * pos)
  | EXPR_call of (lval * pos * (expr array))
  | EXPR_new of (ty * pos * (expr array))

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

and decl = 
    { 
      decl_ident: ident;
      decl_pos: pos;
      decl_artifact: artifact;
    }

and artifact = 
    ARTIFACT_type of ty
  | ARTIFACT_code of code
  | ARTIFACT_slot of (slot * (expr option))

and code = 
    CODE_prog of prog
  | CODE_func of (ty_func * func)
  | CODE_port of port

and bind = 
    {
     bind_ty: ty_sig;
     bind_idents: ident array;
   }

and func = 
    {
     func_proto: proto;
     func_bind: bind;
     func_body: fbody;
   }

and fbody = 
    FBODY_native of ident
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
