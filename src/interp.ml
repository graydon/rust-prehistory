open Ast
open Val
;;

exception Interp_err of string
;;

let arr ls = Array.of_list ls
;;


(*****************************************************************)
(*                    Code Generation                            *)
(*****************************************************************)


type rs_emitter = 
  { 
    mutable emit_pc: int;
    mutable emit_ops: ops;
  }
;;

let new_emitter _ = 
  { 
    emit_pc = 0;
    emit_ops = Array.create 4 OP_bad;
  }
;;

let grow_if_necessary e =
  let len = Array.length e.emit_ops in
  if e.emit_pc >= len - 1
  then
    let n = Array.create (2 * len) OP_bad in
    Array.blit e.emit_ops 0 n 0 len;
    e.emit_ops <- n 
;;


let emit_op e op =
  grow_if_necessary e;
  e.emit_ops.(e.emit_pc) <- op;
  e.emit_pc <- e.emit_pc + 1
;;

let backpatch_fwd_jump_to_here e fwd_jump_pc =
  match e.emit_ops.(fwd_jump_pc) with
    OP_jump (form, None) -> 
      e.emit_ops.(fwd_jump_pc) <- OP_jump (form, (Some e.emit_pc))
  | op -> 
      raise (Interp_err "backpatching bad opcode")
;;

let val_mach_of_lit_mach lm = 
  match lm with
    LIT_unsigned (i, _) -> VAL_unsigned i
  | LIT_signed (i, _) -> VAL_signed i
  | LIT_ieee_bfp f -> VAL_ieee_bfp f
  | LIT_ieee_dfp i -> VAL_ieee_dfp i

let ty_mach_of_lit_mach lm = 
  match lm with
   (* 
    * FIXME: figure out a nice lexical strategy for giving
    * the word size in a mach literal. Maybe just use 
    * syntax reader? ~u32{0x1234_ffff} is not so bad?
    *)
    LIT_unsigned (i, _) -> (TY_unsigned, 64)
  | LIT_signed (i, _) -> (TY_signed, 64)
  | LIT_ieee_bfp f -> (TY_ieee_bfp, 64)
  | LIT_ieee_dfp i -> (TY_ieee_dfp, 128)

let val_of_literal lit = 
  match lit with
    LIT_str s -> { rv_type = TY_str; rv_val = VAL_str s }
  | LIT_char c -> { rv_type = TY_char; rv_val = VAL_char c }
  | LIT_bool b -> { rv_type = TY_bool; rv_val = VAL_bool b }
  | LIT_mach m -> { rv_type = TY_mach (ty_mach_of_lit_mach m);
		    rv_val = VAL_mach (val_mach_of_lit_mach m) }
  | LIT_arith (_, _, n) -> 
      { rv_type = (TY_arith (Ll1parser.numty n));
	rv_val = VAL_arith n }
  | LIT_func (fty, f) -> { rv_type = TY_func fty; rv_val = VAL_func f }
  | LIT_prog p -> { rv_type = TY_prog; rv_val = VAL_prog p }
  | _ -> raise (Interp_err "unhandled literal in val_of_literal")


let rec emit_expr_full deref_lvals emit e = 
  match e with 
    EXPR_binary (op, pos, e1, e2) -> 
      emit_expr emit e1;
      emit_expr emit e2;
      emit_op emit (OP_pos pos);
      emit_op emit (OP_binop op)
	
  | EXPR_unary (op, pos, e) -> 
      emit_expr emit e;
      emit_op emit (OP_pos pos);
      emit_op emit (OP_unop op)

  | EXPR_literal (v, pos) -> 
      emit_op emit (OP_pos pos);
      emit_op emit (OP_push (RVAL (val_of_literal v)))

  | EXPR_lval (lv, pos) -> 
      emit_op emit (OP_pos pos);
      emit_op emit (OP_push (LVAL lv));
      if deref_lvals
      then emit_op emit (OP_copy)
      else ()

  | EXPR_call (lv, _, args) -> 
      Array.iter (emit_expr_full false emit) args;
      emit_op emit (OP_push (LVAL lv));
      emit_op emit (OP_copy);
      emit_op emit (OP_call)

  | _ -> raise (Interp_err "unhandled expression form")
	
and emit_expr emit e = emit_expr_full true emit e
;;


let rec emit_stmt emit stmt = 
  match stmt with 
    STMT_while sw -> 
      let restart_pc = emit.emit_pc in
      (emit_op emit (OP_pos sw.while_pos);
       emit_expr emit sw.while_expr;
       emit_op emit (OP_unop UNOP_not);
       let jump_to_exit_pc = emit.emit_pc in
       (emit_op emit (OP_jump (JMP_conditional, None));
	emit_stmt emit sw.while_body;
	emit_op emit (OP_jump (JMP_direct, Some restart_pc));
	backpatch_fwd_jump_to_here emit jump_to_exit_pc))
	
  | STMT_if si -> 
      emit_op emit (OP_pos si.if_pos);
      emit_expr emit si.if_test;
      emit_op emit (OP_unop UNOP_not);
      let jump_to_else_pc = emit.emit_pc in
      (emit_op emit (OP_jump (JMP_conditional, None));
       emit_stmt emit si.if_then;
       let jump_to_end_pc = emit.emit_pc in
       (emit_op emit (OP_jump (JMP_direct, None));
	backpatch_fwd_jump_to_here emit jump_to_else_pc;
        (match si.if_else with 
	  Some el -> emit_stmt emit el
	| None -> ());
	backpatch_fwd_jump_to_here emit jump_to_end_pc))

  | STMT_return (e, pos) -> 
      emit_expr emit e;
      emit_op emit (OP_pos pos);
      emit_op emit OP_return

  | STMT_block (stmts, pos) ->
      emit_op emit (OP_pos pos);
      emit_op emit (OP_enter_scope);
      Array.iter (emit_stmt emit) stmts;
      emit_op emit (OP_exit_scope)

  | STMT_move (dst, src) -> 
      emit_op emit (OP_push (LVAL dst));
      emit_op emit (OP_push (LVAL src));
      emit_op emit (OP_move);
      emit_op emit (OP_store)

  | STMT_copy (lv, e) -> 
      emit_op emit (OP_push (LVAL lv));
      emit_expr emit e;
      emit_op emit (OP_store)
	
  | STMT_call (lv, args) -> 
      Array.iter (emit_expr_full false emit) args;
      emit_op emit (OP_push (LVAL lv));
      emit_op emit (OP_copy);
      emit_op emit (OP_call);
      emit_op emit (OP_pop)

  | STMT_decl (DECL_type (pos, ident, ty)) -> 
      emit_op emit (OP_pos pos);
      emit_op emit (OP_alloc_local ident);
      emit_op emit (OP_push (LVAL { lval_base = ident;
				    lval_rest = arr [] }));
      emit_op emit (OP_push (RVAL { rv_type = TY_type;
				    rv_val = VAL_type ty }));
      emit_op emit (OP_store)

  | STMT_decl (DECL_slot (pos, slot, exprOpt)) -> 
      emit_op emit (OP_pos pos);
      emit_op emit (OP_alloc_local slot.slot_ident);
      emit_op emit (OP_push (LVAL { lval_base = slot.slot_ident;
				    lval_rest = arr [] }));
      (match exprOpt with
	None -> ()
      | Some e -> emit_expr emit e);
      emit_op emit (OP_store)
       
  | STMT_decl _ -> raise (Interp_err "cannot translate other decl types yet");


  | STMT_try _
  | STMT_yield _
  | STMT_assert _
  | STMT_foreach _
  | STMT_send _
  | STMT_for _ -> raise (Interp_err "cannot translate all statements yet")
;;

(*****************************************************************)
(*                     Execution                                 *)
(*****************************************************************)


let types_equal p q =
  p = q
;;

let rec lookup_name_in_dir_modu name dir = 
  let m = Hashtbl.find dir.Ast.dir_ents name.name_base in
  match m with
    MODU_src s -> 
      if Array.length name.name_rest = 0 
      then VAL_prog (s.Ast.src_prog)
      else raise (Interp_err "don't know how to look into prog substructures yet")
  | MODU_dir d -> 
      (match Array.to_list name.Ast.name_rest with 
	[] -> raise (Interp_err "looking up a dir module")	    
      | (Ast.COMP_ident id)::rest -> 
	  lookup_name_in_dir_modu { name_base = id;
				    name_rest = Array.of_list rest; } d
      | _ -> raise (Interp_err "looking up non-ident name component"))
;;

let get_frame ctxt proc = 
  if (proc.proc_frame < 0
    || proc.proc_frame > (List.length proc.proc_frames))
  then raise (Interp_err (ctxt ^ ": bad frame request in frames"))
  else List.nth proc.proc_frames proc.proc_frame 
;;

let load proc lv =
  if Array.length lv.lval_rest != 0 
  then raise (Interp_err "cannot handle complex lvals yet")
  else Hashtbl.find proc.proc_env lv.lval_base
;;

let store proc lv rvo = 
  Hashtbl.replace proc.proc_env lv.lval_base rvo
;;

let exec_load clear_slot proc res = 
  let frame = get_frame "exec_load" proc in 
  let stk = frame.frame_eval_stack in
  match res with 
    RVAL rv -> raise (Interp_err "loading from rval")
  | LVAL lv -> 
      (* FIXME: check for limitation on arg? *)      
      (match load proc lv with 
	Some rv -> Stack.push (RVAL rv) stk
      | None -> raise (Interp_err "loading undefined value"));
      if clear_slot
      then store proc lv None
      else ()
;;

  
(* FIXME: we don't move anything *out* yet *)
let bind_args env args bind =

  let param_types = bind.bind_ty.sig_param_types in
  let n_args = Array.length args in 
  
  for i = 0 to n_args - 1 
  do	
    let arg = args.(i) in
    match arg with 
      None -> ()
    | Some rv -> 
	if (not (types_equal rv.rv_type param_types.(i)))
	then raise (Interp_err "Bad argument type");
    Hashtbl.add env bind.bind_idents.(i) arg
  done
;;


let bind_and_enter_frame proc modname fflav sba block_stmt =
  let emitter = new_emitter () in 
  emit_stmt emitter block_stmt;
  emit_op emitter OP_return;
     Printf.printf "== begin emitted code ==:\n";
     Array.iteri (fun i op -> Printf.printf "%6d: %a\n" i Trace.fmt_op op) emitter.emit_ops;
     Printf.printf "== end emitted code ==:\n";
  let frame = 
    { 
      frame_pc = 0;
      frame_ops = emitter.emit_ops;
      frame_flavour = fflav;		
      frame_scope = [];
      frame_scope_stack = Stack.create ();
      frame_eval_stack = Stack.create ();
      frame_mod = modname;
    }
  in
  proc.proc_frames <- (frame :: proc.proc_frames);
  (match sba with 
    Some (binding,args) -> bind_args proc.proc_env args binding
  | None -> ());
;;


let exec_unop op stk =
  let top = Stack.pop stk in
  match (op, top) with 
    (UNOP_not, 
     RVAL { rv_type = TY_bool;
	    rv_val = VAL_bool b }) -> 
	      
     Stack.push (RVAL { rv_type = TY_bool;
			rv_val = VAL_bool (not b)}) stk
  | _ -> raise (Interp_err "unknown unary operator or operand")
;;


let exec_binop op stk =
  let rhs = Stack.pop stk in
  let lhs = Stack.pop stk in
  let push_bool b = 
    Stack.push (RVAL { rv_type = TY_bool;
		       rv_val = VAL_bool b }) stk in
  let push_num n = 
    let nty = 
      match n with 
	Num.Ratio _ -> TY_rat
      | _           -> TY_int
    in
    Stack.push (RVAL { rv_type = TY_arith nty;
		       rv_val = VAL_arith n }) stk
  in
  match (lhs,rhs) with 
    (RVAL { rv_type=TY_bool; rv_val=VAL_bool lb },
     RVAL { rv_type=TY_bool; rv_val=VAL_bool rb }) -> 
       let res = 
	 (match op with 
	   BINOP_or -> lb || rb
	 | BINOP_and -> lb && rb

	 | BINOP_eq -> lb == rb
	 | BINOP_ne -> lb != rb

	 | BINOP_lt -> lb < rb
	 | BINOP_le -> lb <= rb
	 | BINOP_ge -> lb >= rb
	 | BINOP_gt -> lb > rb
	 | _ -> raise (Interp_err "applying bad operator to boolean operands")
	 )
       in
       push_bool res
  | (RVAL { rv_type=TY_arith _; rv_val=VAL_arith ln },
     RVAL { rv_type=TY_arith _; rv_val=VAL_arith rn }) -> 
       (match op with 
	 BINOP_add -> push_num (Num.add_num ln rn)
       | BINOP_sub -> push_num (Num.sub_num ln rn)
       | BINOP_mul -> push_num (Num.mult_num ln rn)
       | BINOP_div -> push_num (Num.div_num ln rn)
       | BINOP_mod -> push_num (Num.mod_num ln rn)

       | BINOP_eq -> push_bool (Num.eq_num ln rn)
       | BINOP_ne -> push_bool (not (Num.eq_num ln rn))

       | BINOP_lt -> push_bool (Num.lt_num ln rn)
       | BINOP_le -> push_bool (Num.le_num ln rn)
       | BINOP_ge -> push_bool (Num.ge_num ln rn)
       | BINOP_gt -> push_bool (Num.gt_num ln rn)
       | _ -> raise (Interp_err "applying bad operator to arithmetic operands")
       )
	 
  | (_,_) -> raise (Interp_err "unknown operand types")
;;


let exec_jump proc frame addr =
  if addr < frame.frame_pc
  then proc.proc_resched <- true
  else ();
  frame.frame_pc <- addr; 
  proc.proc_jumped <- true  
;;


let exec_op proc op = 
  let frame = get_frame "exec_op" proc in 
  let stk = frame.frame_eval_stack in
  let trueval = { rv_type = TY_bool; rv_val = VAL_bool false } in
  let trueres = (RVAL trueval) in

  match op with 
    OP_push v -> Stack.push v stk
  | OP_binop op -> exec_binop op stk
  | OP_unop op -> exec_unop op stk
  | OP_pop -> let _ = Stack.pop stk in ()
	
  | OP_copy -> exec_load false proc (Stack.pop stk) 
  | OP_move -> exec_load true proc (Stack.pop stk) 
	  
  | OP_store -> 
      let src = Stack.pop stk in
      let dst = Stack.pop stk in
      (match (src, dst) with
	(RVAL rv, LVAL lv) -> 
	  if Array.length lv.lval_rest != 0 
	  then raise (Interp_err "cannot handle complex lvals yet")
	  else Hashtbl.add proc.proc_env lv.lval_base (Some rv)
      | _ -> raise (Interp_err "bad operand combination to store"))
	  
  | OP_enter_scope -> 
      Stack.push frame.frame_scope frame.frame_scope_stack;
      frame.frame_scope <- []
	  
  | OP_alloc_local s -> 
      frame.frame_scope <- s :: frame.frame_scope;
      Hashtbl.add proc.proc_env s None
	
  | OP_undef_local s -> 
      Hashtbl.replace proc.proc_env s None
	
  | OP_exit_scope -> 
      List.iter (fun x -> Hashtbl.remove proc.proc_env x) frame.frame_scope;
      frame.frame_scope <- Stack.pop frame.frame_scope_stack
	  
  | OP_pos p -> proc.proc_pos <- p
	    
  | OP_jump (JMP_conditional, Some addr) -> 
      (match Stack.pop stk with 
	(RVAL { rv_type=TY_bool; rv_val=VAL_bool b }) -> 
	  if b 
	  then exec_jump proc frame addr
	  else ()
      | _ -> raise (Interp_err "conditional jump on non-boolean value"))
	
  | OP_jump (JMP_direct, Some addr) -> exec_jump proc frame addr	  
  | OP_jump (_,None) -> raise (Interp_err "executing unpatched jump")
	
  | OP_call -> 
      proc.proc_resched <- true;
      (match Stack.pop stk with 
	(RVAL { rv_type=_; rv_val=(VAL_func func) }) -> 
	  let isig = func.func_bind.bind_ty in
	  let nargs = Array.length (isig.sig_param_types) in 
	  let args = Array.create nargs None in
	  for i = (nargs - 1) downto 0 
	  do 
	    let rvOpt = 
	      match isig.sig_param_pmodes.(i) with
		PMODE_copy -> 
		  (match Stack.pop stk with
		    RVAL rv -> Some rv
		  | LVAL lv -> load proc lv)
	      | PMODE_move_out -> None
	      | PMODE_move_in
	      | PMODE_move_in_out -> 
		  (match Stack.pop stk with
		    RVAL rv -> raise (Interp_err "moving in from rval")
		  | LVAL lv -> 
		      let rvo = load proc lv in
		      store proc lv None;
		      rvo)
	    in	      
	    args.(i) <- rvOpt
	  done;
	  let sba = Some (func.func_bind, args) in
	  (match func.func_body with 
	    FBODY_stmt s -> 
	      bind_and_enter_frame proc "<some-module>" 
		(FRAME_func (func.func_bind.bind_ty.sig_proto, 
			     func.func_bind)) sba s
	  | FBODY_native n -> 
	      (* check_args args func.func_bind; *)
	      let native_fn = Hashtbl.find proc.proc_natives n in
	      (native_fn proc args);
	      Stack.push trueres stk)
      | _ -> raise (Interp_err "calling non-function"))
	
  | OP_return -> 
      List.iter (Hashtbl.remove proc.proc_env) frame.frame_scope;
      while not (Stack.is_empty frame.frame_scope_stack)
      do
	frame.frame_scope <- Stack.pop frame.frame_scope_stack;
	List.iter (Hashtbl.remove proc.proc_env) frame.frame_scope;
      done;
      proc.proc_frames <- List.tl proc.proc_frames;
      (match proc.proc_frames with 
	x :: _ -> Stack.push trueres x.frame_eval_stack
      | _ -> ())

  | OP_resume -> 
      if proc.proc_frame = 0 
      then raise (Interp_err "resuming at top of stack");
      proc.proc_frame <- proc.proc_frame - 1;
      proc.proc_resched <- true

  | OP_yield -> 
      proc.proc_frame <- proc.proc_frame + 1;
      proc.proc_resched <- true
	  
  | OP_new -> raise (Interp_err "cannot new yet")
  | OP_send -> raise (Interp_err "cannot send yet")
  | OP_bad -> raise (Interp_err "executing bad instruction")
;;


let bind_decl decl env =
  let bv = 
    match decl with
    | DECL_slot (_, _, None) -> None
    | DECL_slot (_, _, Some (EXPR_literal (lit, _))) -> 
	Some (val_of_literal lit)
    | _ ->  raise (Interp_err "bad binding")
  in
  Hashtbl.add env (Ast.decl_id decl) bv
  
;;

let rec bind_std_natives it proc = 
  let reg name fn = Hashtbl.add proc.proc_natives name fn in 
  let getstr v = 
    (match v with 
      Some { rv_type=_; rv_val=VAL_str s } -> s
    | _ -> raise (Interp_err "expected string arg"))
  in
  let getint v = 
    (match v with 
      Some { rv_type=_; rv_val=VAL_arith n } -> n
    | _ -> raise (Interp_err "expected arith arg"))
  in
  let log s =   
    Printf.printf "\nlog: %S\n\n" s
  in

  let spawn parent v = 
    match v with
      Some { rv_type=_; rv_val=(VAL_prog prog) } -> 
	let frame = get_frame "spawn" parent in 
	let stk = frame.frame_eval_stack in
	let nproc = new_proc it prog in
	let rval = RVAL { rv_type = Ast.TY_lim Ast.TY_native;
			  rv_val = (VAL_native (NATIVE_proc nproc)) }
	in
	Stack.push rval stk
    | _ -> raise (Interp_err "expected prog arg")
  in

  reg "putstr" (fun p args -> log (getstr args.(0)));
  reg "putint" (fun p args -> log (Num.string_of_num (getint args.(0))));
  reg "spawn" (fun p args -> spawn p args.(0))


and new_proc it prog = 
  let procid = it.interp_nextproc in  
  it.interp_nextproc <- procid + 1;
  let env = Hashtbl.create (Array.length prog.prog_decls) in 
  Array.iter (fun decl -> bind_decl decl env) prog.prog_decls;
  let proc = { proc_id = procid;
	       proc_prog = prog;
	       proc_env = env;
	       proc_natives = Hashtbl.create 0;
	       proc_frame = 0;
	       proc_frames = [];
	       proc_state = PROC_INIT;
	       proc_ports = Array.of_list [];
	       proc_pos = ("",0,0);
	       proc_jumped = false;
	       proc_resched = false;
	       proc_trace = true }
  in
  bind_std_natives it proc;
  Hashtbl.add it.interp_procs procid proc;
  Queue.add procid it.interp_runq;
  proc
;;
  
let enter_init_frame proc args = 
  match proc.proc_prog.prog_init with 
    Some init -> 
      let bind = init.init_bind in
      let fflav = FRAME_init bind in      
      bind_and_enter_frame proc "<some-module>" fflav (Some (bind, args)) init.init_body
  | _ -> ()
;;


let enter_main_frame proc = 
  match proc.proc_prog.prog_main with 
    Some block_stmt -> 
      bind_and_enter_frame proc "<some-module>" FRAME_main None block_stmt
  | _ -> ()  
;;


let proc_finished p =
  match p.proc_state with
    PROC_FINI when p.proc_frames = [] -> true
  | _ -> false
;;


let step_proc p =
  match p.proc_state with

    PROC_INIT when p.proc_frames = [] -> 
      p.proc_state <- PROC_MAIN;
      enter_main_frame p

  | PROC_MAIN when p.proc_frames = [] -> 
      p.proc_state <- PROC_FINI
	  
  | PROC_INIT
  | PROC_MAIN
  | PROC_FINI -> 
      if p.proc_trace
      then Trace.trace_op p
      else ();
      let f = get_frame "step_proc" p in
      let op = 
	if (f.frame_pc >= Array.length f.frame_ops)
	then OP_return
	else f.frame_ops.(f.frame_pc);
      in
      exec_op p op;
      if p.proc_jumped
      then p.proc_jumped <- false
      else f.frame_pc <- f.frame_pc + 1;
	  
  | _ -> (raise (Interp_err "interpreter wedged"))
;;


(*****************************************************************)
(*                Initialization and Entry                       *)
(*****************************************************************)

let split_str sep s = 
  let tmp = ref [] in
  let accum = Buffer.create 100 in
  let flush _ = (tmp := ((Buffer.contents accum) :: (!tmp)); 
		 Buffer.clear accum)
  in
  let f c = 
    if c = sep 
    then flush ()
    else Buffer.add_char accum c
  in
  String.iter f s;
  if Buffer.length accum > 0
  then flush();
  List.rev (!tmp)
;;


let str_to_name str = 
  let comps = split_str '.' str in
  match comps with 
    [] -> raise (Interp_err "empty name")
  | base :: rest -> 
      { Ast.name_base = base;
	Ast.name_rest = Array.of_list (List.map (fun x -> COMP_ident x) rest); }
;;

let find_entry_prog root entry_name = 
  let name = str_to_name entry_name in
  let v = lookup_name_in_dir_modu name root in
  match v with 
    VAL_prog p -> p
  | _ -> raise (Interp_err ("found non-proc val for " ^ entry_name))
;;


let init_runtime = 
  let t = TY_named { name_base = "sys";
		     name_rest = Array.of_list [COMP_ident "rt"] }
  in
  let v = VAL_rec ( Hashtbl.create 0 )
  in
  Some { rv_type=t; rv_val=v }
;;


let init_argv =
  let t = TY_vec { vec_elt_type = TY_str; 
		   vec_elt_state = Array.of_list []} in
  let v = VAL_vec { vec_storage = Array.of_list [];
		    vec_initsz = 0 } in
  Some { rv_type=t; rv_val=v }
;;


let init_args = 
  Array.of_list [init_runtime; init_argv]
;;


(*****************************************************************)
(*                    Interpreter structure                      *)
(*****************************************************************)

let new_interp root = 
  { interp_nextproc = 0;
    interp_procs = Hashtbl.create 100;
    interp_runq = Queue.create ();
    interp_root = root; }
;;


let interpret root entry_name = 
  Printf.printf "interpreting\n";
  let it = new_interp root in
  let entryproc = new_proc it (find_entry_prog root entry_name) in
  enter_init_frame entryproc init_args;
  let curr = ref entryproc.proc_id in
  while not (Queue.is_empty it.interp_runq)
  do
    let pid = Queue.take it.interp_runq in
    if Hashtbl.mem it.interp_procs pid
    then 
      let p = Hashtbl.find it.interp_procs pid in
      p.proc_resched <- false;
      try
	if not (proc_finished p) 
	then 
	  (
	   if not ((!curr) = pid)
	   then Printf.printf "switching to proc %d\n" p.proc_id;

	   curr := pid;
	   
	   while (not p.proc_resched) && (not (proc_finished p))
	   do 
	     step_proc p
	   done;

	   if (not (proc_finished p))
	   then Queue.add p.proc_id it.interp_runq
	  )
	else
	  ()
      with
	Interp_err s -> 
	  Printf.printf "Interpreter error: %s\n" s	    
    else
      ()
  done;
  Printf.printf "process run queue is empty, exiting\n"
;;
