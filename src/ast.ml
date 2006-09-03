open Array;;
open Hashtbl;;

(* 
 * There are two kinds of rust files:
 * 
 * .rc files, containing crates.
 * .rs files, containing source.
 *
 *)

(* Slot names are given by a dot-separated path within the current
   module namespace. *)

type rs_name = string array
;;

type ty_prim =
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
    TY_nil
  | TY_dyn
  | TY_type

  | TY_prim of (ty_prim * int)
  | TY_arith of ty_arith
  | TY_str
  | TY_char

  | TY_rec of ty_rec
  | TY_alt of ty_alt
  | TY_vec 

  | TY_func of ty_subr
  | TY_iter of ty_subr
  | TY_chan of ty_sig

  | TY_port of ty_sig
  | TY_prog
  | TY_proc 

  | TY_pred of ty_pred
  | TY_quote of ty_quote

  | TY_const of rs_type
  | TY_ref of rs_type
  | TY_named of rs_name
  | TY_param of (rs_type * string array)

  | TY_lim of rs_type

(* 
 * An state may include *optional* names in its params.
 * The "formal" name is implied where names are missing,
 * which is for example the return / yield value on a subr
 * or the vector-element type on a vec. This is denoted with "*".
 * 
 * It is a semantic error to omit names in a context where
 * no implied formal parameter exists.
 *)
   
and rs_pred = 
    { 
      pred_name: rs_name;
      pred_args: (rs_name option) array; 
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
    }

and ty_alt = ty_alt_case array

and ty_alt_case = 
    { 
      alt_case_name: string;
      alt_case_rec: ty_rec;
    }

and ty_subr = 
    { 
      subr_inline: bool; 
      subr_pure: bool;
      subr_sig: ty_sig; 
    }

and rs_param_mode = 
    PARAM_move_in 
  | PARAM_move_out 
  | PARAM_move_inout 
  | PARAM_copy 

and rs_param = 
    {
     param_type: rs_type;
     param_mode: rs_param_mode;
   }

and ty_sig = 
    { 
      sig_params: rs_param array;
      sig_result: rs_type; 
      sig_istate: rs_state;
      sig_ostate: rs_state;
    }

and ty_pred = 
    { 
      pred_auto: bool;
      pred_inline: bool; 
      pred_params: rs_param array;
      pred_istate: rs_state;
    }

and ty_quote = 
    TY_quote_expr
  | TY_quote_type
  | TY_quote_decl
  | TY_quote_stmt
      (* Probably this list should be a lot longer; the canonical rule
	 I've been using is to make a quotation type for every
	 nonterminal *)


let init_star_pred : rs_pred = 
    { 
      pred_name = Array.make 1 "init";
      pred_args = Array.make 1 None; 
    }
;;
    
(* Values *)

type val_prim = 
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

    VAL_nil
  | VAL_prim of val_prim
  | VAL_arith of Num.num
  | VAL_str of string
  | VAL_char of char

  | VAL_rec of val_rec
  | VAL_alt of val_rec
  | VAL_vec of val_vec

  | VAL_func of (string array * rs_stmt)
  | VAL_iter of (string array * rs_stmt)
  | VAL_chan of (string array * int)

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

and val_rec = val_rec_slot array

and val_rec_slot = 
    {
     val_rec_slot_name: string;
     val_rec_slot_val: rs_val option;
   }
      
and val_alt =
    {
     val_alt_case: ty_alt_case;
     val_alt_rec: val_rec;
   }

and val_vec = rs_val array

and val_prog = 
    { 
      prog_init: rs_stmt option;
      prog_fini: rs_stmt option;
      prog_main: rs_stmt;
      prog_decls: rs_decl array;
    }

and val_proc = 
    {
     proc_prog: val_prog;
     proc_env: (string, rs_val) Hashtbl.t;
     proc_pc: int;
     proc_state: proc_exec_state;
     proc_ports: int array;
   }

and proc_exec_state = 

    PROC_RUN 
  | PROC_RECV 
  | PROC_SEND 
  | PROC_FINI

and rs_stmt =

    STMT_while of stmt_while
  | STMT_foreach of stmt_foreach
  | STMT_for of stmt_for
  | STMT_if of stmt_if
  | STMT_try of stmt_try
  | STMT_yield of (rs_expr option)
  | STMT_return of rs_expr
  | STMT_assert of rs_pred
  | STMT_block of (rs_stmt array)
  | STMT_move of rs_lval * rs_expr
  | STMT_copy of rs_lval * rs_expr

and stmt_while = 
    {
     while_expr: rs_expr;
     while_body: rs_stmt;
   }
      
and stmt_foreach = 
    {
     foreach_bindings: (string * rs_expr) array;
     foreach_body: rs_stmt;
   }
      
and stmt_for = 
    {
     for_init: rs_stmt;
     for_test: rs_expr;
     for_step: rs_stmt;
     for_body: rs_stmt;
   }

and stmt_if = 
    {
     if_test: rs_expr;
     if_then: rs_stmt;
     if_else: rs_stmt option;
   }

and stmt_try = 
    {
     try_body: rs_stmt;
     try_fail: rs_stmt option;
     try_fini: rs_stmt option;
   }

and rs_expr =

    EXPR_binary of (rs_binop * rs_expr * rs_expr)
  | EXPR_unary of (rs_unop * rs_expr)
  | EXPR_literal of rs_val
  | EXPR_lval of rs_lval

and rs_lval = rs_lidx array

and rs_lidx =
    LIDX_ident of string
  | LIDX_index of rs_expr

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
