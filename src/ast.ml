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
    TY_unsigned of int
  | TY_signed of int
  | TY_ieee_bfp of int
  | TY_ieee_dfp of int
  | TY_ptr of int (* like unsigned, but no arithmetic ops *)
;;

type ty_arith =
    TY_int
  | TY_rat
;;

type rs_type = 
    TY_nil
  | TY_dyn
  | TY_type

  | TY_prim of ty_prim
  | TY_arith of ty_arith
  | TY_str
  | TY_char

  | TY_rec of ty_rec
  | TY_alt of ty_alt
  | TY_vec of ty_vec

  | TY_func of ty_subr
  | TY_iter of ty_subr
  | TY_chan of ty_sig

  | TY_prog
  | TY_proc 
  | TY_port of ty_sig

  | TY_pred of ty_pred
  | TY_quote of ty_quote
  | TY_const of rs_type

(* 
 * An fstate may include *optional* names in its args.
 * The "formal" name is implied where names are missing,
 * which is for example the return / yield value on a subr
 * or the vector-element type on a vec.
 *)
   
and rs_fstate = 
    { 
      fstate_name: rs_name;
      fstate_args: (rs_name option) array; 
    }
   
and rs_state = 
    { 
      state_name: rs_name;
      state_args: rs_name array; 
    }

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

and ty_vec = 
    { 
      vec_elt_type: rs_type;
      vec_state: rs_fstate;
    }

and ty_subr = 
    { 
      subr_inline: bool; 
      subr_pure: bool;
      subr_sig: ty_sig; 
    }

and ty_sig = 
    { 
      sig_params: rs_type array;
      sig_result: rs_type; 
      sig_istate: rs_state;
      sig_ostate: rs_fstate;
    }

and ty_prog = 
    { 
      prog_auto: bool; 
    }

and ty_pred = 
    { 
      pred_auto: bool;
      pred_inline: bool; 
      pred_params: rs_type array;
      pred_state: rs_state;
    }

and ty_quote = 
    TY_quote_expr
  | TY_quote_type
  | TY_quote_stmt
      (* Probably this list should be a lot longer; the canonical rule
	 I've been using is to make a quotation type for every
	 nonterminal *)


let init_star_fstate : rs_fstate = 
    { 
      fstate_name = Array.make 1 "init";
      fstate_args = Array.make 1 None; 
    }
;;
    
(* Values *)

type val_prim = 
    VAL_unsigned of int
  | VAL_signed of int
  | VAL_ieee_bfp of float
  | VAL_ieee_dfp of (int * int)
  | VAL_ptr of int

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

  | VAL_func of rs_stmt
  | VAL_iter of rs_stmt
  | VAL_chan of int

  | VAL_prog of val_prog
  | VAL_proc of val_proc
  | VAL_port of (val_proc * int)

  | VAL_type of rs_type
  | VAL_quote of val_quote

and val_quote = 

    VAL_quote_expr
  | VAL_quote_type
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
  | STMT_block of (rs_stmt array)
  | STMT_assert of rs_fstate
  | STMT_seti of rs_expr * rs_expr * rs_expr
  | STMT_set of rs_expr * rs_expr

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
  | EXPR_name of rs_name

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

  | BINOP_idx

and rs_unop =
  | UNOP_not

and rs_decl = 
    { 
      decl_name: string;
      decl_type: rs_type;
      decl_value: rs_val;
      decl_state: rs_state;
    }
;;

type rs_decl_top = 
    PUBLIC of rs_decl
  | PRIVATE of rs_decl
;;
