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

type rs_pos = (string * int * int)
;;

let nopos : rs_pos = ("no-file", 0, 0)
;;

(* "names" are statically computable references to particular slots;
   they never involve vector indexing. They are formed by a
   dot-separated sequence of identifier and/or literal-number
   components, the latter representing tuple components (foo.0, foo.1,
   etc). *)

type rs_name_component =
    COMP_string of string
  | COMP_tupidx of int
;;

type rs_name = 
   {
   name_base: string;
   name_rest: rs_name_component array;
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

type rs_type = 
    TY_dyn
  | TY_type

  | TY_mach of (ty_mach * int)
  | TY_arith of ty_arith
  | TY_str
  | TY_char

  | TY_rec of ty_rec
  | TY_alt of ty_alt
  | TY_tup of ty_tup
  | TY_vec of ty_vec

  | TY_subr of ty_subr
  | TY_chan of ty_sig

  | TY_port of ty_sig
  | TY_prog
  | TY_proc 

  | TY_pred of ty_pred
  | TY_quote of ty_quote

  | TY_const of rs_type
  | TY_ref of rs_type
  | TY_named of rs_name
  | TY_abstr of (rs_type * string array)
  | TY_apply of (rs_type * rs_type array)

  | TY_lim of rs_type

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

and parg_base_type = 
    PARG_formal 
  | PARG_free of string

and rs_parg =
    {
      parg_base: parg_base_type;
      parg_rest: rs_name_component array;
    }

and rs_pred = 
    { 
      pred_name: rs_name;
      pred_args: rs_parg array;
    }
      
and rs_state = rs_pred array

and ty_rec = 
    { 
      rec_slots: ty_rec_slot array;
      rec_state: rs_state;
    }

and ty_rec_slot = 
    { 
      rec_slot_name: string;
      rec_slot_type: rs_type;
      rec_slot_state: rs_state;
    }

and ty_alt = ty_alt_case array

and ty_alt_case = 
    { 
      alt_case_name: string;
      alt_case_rec: ty_rec option;
    }

and ty_tup = 
    {
      tup_types: rs_type array;
      tup_state: rs_state;
    }

and ty_vec =
    {
      vec_elt_type: rs_type;
    }

and ty_subr = 
    TY_func of ty_qual_sig
  | TY_iter of ty_qual_sig

and ty_qual_sig = 
    { 
      subr_inline: bool; 
      subr_pure: bool;
      subr_sig: ty_sig; 
    }

and ty_sig = 
    { 
      sig_param_tup: ty_tup;
      sig_result_tup: ty_tup; 
    }

and ty_pred = 
    { 
      pred_auto: bool;
      pred_inline: bool; 
      pred_param_tup: ty_tup;
    }

and ty_quote = 
    TY_quote_expr
  | TY_quote_type
  | TY_quote_decl
  | TY_quote_stmt
      (* Probably this list should be a lot longer; the canonical rule
	 I've been using is to make a quotation type for every
	 nonterminal *)

    
(* Values *)

type val_mach = 
    VAL_unsigned of int
  | VAL_signed of int
  | VAL_ieee_bfp of float
  | VAL_ieee_dfp of (int * int)

(*
 * The "value" type is the result of evaluation of an expression. Or
 * seen another way, it is the sort of thing that can be put in a
 * slot.
 *
 * Implementations are required to be able to construct dyns, but 
 * implementations are *not* required to construct anything fancier
 * than dyns. So here our interpreter is defined purely over dyns. 
 * All our values are dyns at runtime. 
 *
 * Our 'dyn' type, therefore, is only relevant to the static reasoning
 * stage; it represents places where the compiler lacks static type
 * information.
 *)

type rs_val = 
    VAL_dyn of (rs_type * rs_val_dyn)

and rs_val_dyn =

    VAL_mach of val_mach
  | VAL_arith of Num.num
  | VAL_str of string
  | VAL_char of char

  | VAL_rec of val_rec
  | VAL_alt of val_alt
  | VAL_vec of val_vec
  | VAL_tup of val_tup

  | VAL_func of val_subr
  | VAL_iter of val_subr
  | VAL_chan of (ty_qual_sig * int)

  | VAL_prog of val_prog
  | VAL_proc of val_proc
  | VAL_port of (val_proc * int)

  | VAL_type of rs_type
  | VAL_quote of val_quote

and val_quote = 

    VAL_quote_expr
  | VAL_quote_type
  | VAL_quote_decl
  | VAL_quote_stmt


and rs_subr_bind = 
    {
     bind_subr: ty_subr;
     bind_names: string array;
    }

and val_subr = 
    {
     subr_bind: rs_subr_bind;
     subr_body: rs_stmt;
    }

and val_rec = (string, rs_val) Hashtbl.t 
      
and val_alt =
    {
     val_alt_case: ty_alt_case;
     val_alt_rec: val_rec;
   }

and val_vec = rs_val array

and val_tup = rs_val array

and val_prog = 
    { 
      prog_auto: bool;
      prog_init: ((ty_sig * string array) * rs_stmt) option;
      prog_main: rs_stmt option;
      prog_fini: rs_stmt option;
      prog_decls: rs_decl array;
    }

and rs_block = 
    {
     mutable block_pc: int;
     block_stmts: rs_stmt array;
     block_names: string Stack.t;
     block_pos: rs_pos;
    }

and rs_frame_flavour = 
    FRAME_iter of rs_subr_bind
  | FRAME_func of rs_subr_bind
  | FRAME_init of (ty_sig * string array)
  | FRAME_main
  | FRAME_fini 

and rs_frame = 
    {
     frame_flavour: rs_frame_flavour;
     frame_blocks: rs_block Stack.t;
    }

and val_proc = 
    {
     proc_prog: val_prog;
     proc_env: (string, rs_val) Hashtbl.t;

   (* 
    * Frames are held in a "push-down list": head is the top
    * frame, but current executing frame may not be top due to iter
    * protocol.  
    *)

     mutable proc_frame: int;
     mutable proc_frames: rs_frame list;

     mutable proc_state: proc_exec_state;
     proc_ports: int array;
   }

and proc_exec_state = 
    PROC_INIT
  | PROC_FINI
  | PROC_MAIN 
  | PROC_SEND 
  | PROC_RECV 

and rs_stmt =

    STMT_while of stmt_while
  | STMT_foreach of stmt_foreach
  | STMT_for of stmt_for
  | STMT_if of stmt_if
  | STMT_try of stmt_try
  | STMT_yield of (rs_expr option * rs_pos)
  | STMT_return of (rs_expr * rs_pos)
  | STMT_assert of (rs_pred * rs_pos)
  | STMT_block of ((rs_stmt array) * rs_pos)
  | STMT_move of (rs_lval * rs_lval)
  | STMT_copy of (rs_lval * rs_expr)
  | STMT_call of (rs_lval * (rs_expr array) )
  | STMT_decl of rs_decl

and stmt_while = 
    {
     while_expr: rs_expr;
     while_body: rs_stmt;
     while_pos: rs_pos;
   }
      
and stmt_foreach = 
    {
     foreach_bindings: rs_decl array;
     foreach_body: rs_stmt;
     foreach_pos: rs_pos;
   }
      
and stmt_for = 
    {
     for_init: rs_stmt;
     for_test: rs_expr;
     for_step: rs_stmt;
     for_body: rs_stmt;
     for_pos: rs_pos;
   }

and stmt_if = 
    {
     if_test: rs_expr;
     if_then: rs_stmt;
     if_else: rs_stmt option;
     if_pos: rs_pos;
   }

and stmt_try = 
    {
     try_body: rs_stmt;
     try_fail: rs_stmt option;
     try_fini: rs_stmt option;
     try_pos: rs_pos;
   }

and rs_expr =

    EXPR_binary of (rs_binop * rs_pos * rs_expr * rs_expr)
  | EXPR_unary of (rs_unop * rs_pos * rs_expr)
  | EXPR_literal of (rs_val * rs_pos)
  | EXPR_lval of rs_lval
  | EXPR_call of (rs_lval * (rs_expr array) )

and rs_lidx =
    LIDX_named of (rs_name_component * rs_pos)
  | LIDX_index of rs_expr

and rs_lval = 
    {
     lval_base: string;
     lval_rest: rs_lidx array;
    }

and rs_binop =    

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

and rs_unop =

    UNOP_not

and rs_decl = 
    { 
      decl_name: string;
      decl_pos: rs_pos;
      decl_type: rs_type;
      decl_value: rs_val;
      decl_state: rs_state;
    }
;;

type rs_visibility = 
    VIS_public
  | VIS_standard
  | VIS_private

type rs_decl_top = (rs_visibility * rs_decl)
