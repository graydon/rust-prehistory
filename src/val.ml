(* Values *)

type val_mach = 
    VAL_unsigned of int
  | VAL_signed of int
  | VAL_ieee_bfp of float
  | VAL_ieee_dfp of (int * int)

type result = 
    RVAL of rv
  | LVAL of Ast.lval

and rv = { rv_type: Ast.ty;
	   rv_val: v }

and v =

    VAL_bool of bool
  | VAL_mach of val_mach
  | VAL_arith of Num.num
  | VAL_str of string
  | VAL_char of char

  | VAL_rec of val_rec
  | VAL_alt of val_alt
  | VAL_vec of val_vec
  | VAL_tup of val_tup

  | VAL_func of Ast.func
  | VAL_chan of val_chan

  | VAL_prog of Ast.prog
  | VAL_proc of val_proc

  | VAL_type of Ast.ty
  | VAL_quote of val_quote


and val_quote = 
    VAL_quote_expr of Ast.expr
  | VAL_quote_type of Ast.ty
  | VAL_quote_decl of Ast.decl
  | VAL_quote_stmt of Ast.stmt

and val_chan = 
    {
     chan_proc: int;
     chan_port: int;
    }

and val_rec = (string, v) Hashtbl.t 
      
and val_alt =
    {
     val_alt_case: Ast.alt_case;
     val_alt_rec: val_rec;
    }

and val_vec = v array

and val_tup = v array

and jump_form = 
    JMP_conditional
  | JMP_direct

and op = 
    OP_push of result
  | OP_binop of Ast.binop
  | OP_unop of Ast.unop
  | OP_pop

  | OP_copy
  | OP_move
  | OP_store

  | OP_enter_scope
  | OP_alloc_local of string
  | OP_undef_local of string
  | OP_exit_scope

  | OP_pos of Ast.pos

  | OP_jump of (jump_form * int option)
  | OP_call
  | OP_new
  | OP_send
  | OP_return
  | OP_yield

  | OP_bad

and ops = op array

and frame_flavour = 
    FRAME_func of (Ast.proto * Ast.bind)
  | FRAME_port of (Ast.proto * Ast.bind)
  | FRAME_init of Ast.bind
  | FRAME_main
  | FRAME_fini

and frame = 
    {
     mutable frame_pc: int;
     frame_ops: ops;
     frame_flavour: frame_flavour;
     mutable frame_scope: string list;
     frame_scope_stack: (string list) Stack.t;
     frame_eval_stack: result Stack.t;
     frame_mod: string;
    }

and val_proc = 
    {
     proc_id: int;
     proc_prog: Ast.prog;
     proc_env: (string, (rv option)) Hashtbl.t;
     proc_natives: (string, (val_proc -> ((rv option) array) -> unit)) Hashtbl.t;

     mutable proc_frames: frame list;
     mutable proc_state: proc_exec_state;
     mutable proc_pos: Ast.pos;
     mutable proc_jumped: bool;
     mutable proc_resched: bool;
     mutable proc_trace: bool;
     proc_ports: Ast.port array;
   }

and proc_exec_state = 
    PROC_INIT
  | PROC_FINI
  | PROC_MAIN 
  | PROC_SEND 
  | PROC_RECV 

;;

type val_mod = 
    (string, (Ast.visibility * rv)) Hashtbl.t
;;

type interp = 
    {
     mutable interp_nextproc: int;
     interp_procs: (int, val_proc) Hashtbl.t;
     interp_runq: int Queue.t;
     interp_mods: (string, val_mod) Hashtbl.t
   }
;;
