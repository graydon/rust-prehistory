open Ast
open Val
;;

exception Interp_err of string
;;

let arr ls = Array.of_list ls
;;

(*****************************************************************)
(*                    Output Formatting                          *)
(*****************************************************************)

let fmt_nc out nc =
  match nc with 
    COMP_string s -> Printf.fprintf out "%s" s
  | COMP_tupidx i -> Printf.fprintf out "%d" i
;;

let fmt_val out lit = 
  Printf.fprintf out "val"

let fmt_lit out lit = 
  match lit with 
    LIT_str s -> 
      Printf.fprintf out "\"%s\"" (String.escaped s)
  | LIT_char c -> 
      Printf.fprintf out "'%c'" c
  | LIT_bool b -> 
      Printf.fprintf out "%s" (if b then "true" else "false")
  | LIT_arith (_, _, n) -> 
      Printf.fprintf out "%s" (Num.string_of_num n)
  | _ -> output_string out "**unhandled literal**"
;;

let fmt_binop out op = 
  output_string out 
    (match op with 
      BINOP_or -> "|"
    | BINOP_and -> "&"

    | BINOP_eq -> "=="
    | BINOP_ne -> "!="

    | BINOP_lt -> "<"
    | BINOP_le -> "<="
    | BINOP_ge -> ">="
    | BINOP_gt -> ">"

    | BINOP_lsl -> "<<"
    | BINOP_lsr -> ">>"
    | BINOP_asr -> ">>>"
	
    | BINOP_add -> "+"
    | BINOP_sub -> "-"
    | BINOP_mul -> "*"
    | BINOP_div -> "/"
    | BINOP_mod -> "%")
;;

let fmt_unop out op = 
  output_string out 
    (match op with
      UNOP_not -> "!")
;;

let rec fmt_expr out e = 
  match e with
    EXPR_binary (op, _, lhs, rhs) -> 
      Printf.fprintf out "(%a)%a(%a)" fmt_binop op fmt_expr lhs fmt_expr rhs
  | EXPR_unary (op, _, e2) -> 
      Printf.fprintf out "%a(%a)" fmt_unop op fmt_expr e2	
  | EXPR_literal (lv,_) -> fmt_lit out lv
  | EXPR_lval lv -> fmt_lval out lv
  | EXPR_tuple (es,_) -> 
      (output_string out "(";
       Array.iteri 
	 (fun i x -> 
	   if i != 0 then output_string out ", " else (); 
	   fmt_expr out x) es;
       output_string out ")")
	
  | EXPR_call (lv, args) -> 
      fmt_lval out lv;
      fmt_expr out args

and fmt_lval out lv = 
  output_string out lv.lval_base;
  Array.iter 
    (fun x -> match x with 
      LIDX_named (nc, _) -> (Printf.fprintf out ".%a" fmt_nc nc)
    | LIDX_index e -> (Printf.fprintf out ".(%a)" fmt_expr e))
    lv.lval_rest
;;


let fmt_name out n = 
  output_string out n.name_base;
  Array.iter (fun c -> Printf.fprintf out ".%a" fmt_nc c) n.name_rest
;;


let ty_mach_prefix m = 
  match m with 
    TY_unsigned -> 'u'
  | TY_signed -> 's'
  | TY_ieee_bfp -> 'b'
  | TY_ieee_dfp -> 'd'
;;


let ty_arith_name n =
  match n with 
    TY_int -> "int"
  | TY_nat -> "nat"
  | TY_rat -> "rat"
;;


let rec fmt_type out t = 
  match t with 
    TY_dyn -> output_string out "dyn"
  | TY_type -> output_string out "type"

  | TY_bool -> output_string out "bool"
  | TY_mach (m,n) -> Printf.fprintf out "%c%d" (ty_mach_prefix m) n
  | TY_arith a -> output_string out (ty_arith_name a)
  | TY_str -> output_string out "str"
  | TY_char -> output_string out "char"

  | TY_rec r -> output_string out "(rec ...)"
  | TY_alt a -> output_string out "(alt ...)"
  | TY_tup t -> output_string out "(tup ...)"
  | TY_vec v -> output_string out "(vec ...)"

  | TY_func s -> output_string out "(func ...)"
  | TY_chan c -> output_string out "(chan ...)"

  | TY_prog -> output_string out "prog"
  | TY_proc -> output_string out "proc"

  | TY_pred p -> output_string out "(pred ...)"
  | TY_quote q -> output_string out "(quote ...)"

  | TY_named n -> fmt_name out n 
  | TY_abstr (ty, params) -> output_string out "(abstr ...)"
  | TY_apply (ty, args) -> output_string out "(apply ...)"

  | TY_lim t -> Printf.fprintf out "(lim %a)" fmt_type t
;;


let fmt_jmp out j = 
  match j with 
    JMP_conditional -> output_string out "conditional jump"
  | JMP_direct -> output_string out "direct jump"
;;

let fmt_op out op = 
  match op with
    OP_push v -> Printf.fprintf out "PUSH (%a)" fmt_val v
  | OP_binop op -> Printf.fprintf out "BINOP (%a)" fmt_binop op
  | OP_unop op -> Printf.fprintf out "UNOP (%a)" fmt_unop op
  | OP_pop -> output_string out "POP"

  | OP_copy_lval lv -> Printf.fprintf out "COPY %a" fmt_lval lv
  | OP_move_lval lv -> Printf.fprintf out "MOVE %a" fmt_lval lv
  | OP_store_lval lv -> Printf.fprintf out "STORE %a" fmt_lval lv

  | OP_enter_scope -> output_string out "ENTER_SCOPE"
  | OP_alloc_local s -> Printf.fprintf out "ALLOC_LOCAL %s" s
  | OP_undef_local s -> Printf.fprintf out "UNDEF_LOCAL %s" s
  | OP_exit_scope -> output_string out "EXIT_SCOPE"

  | OP_pos (file,line,col) -> Printf.fprintf out "POS (%s:%d:%d)" file line col

  | OP_jump (JMP_conditional, Some addr) -> Printf.fprintf out "CJUMP %d" addr
  | OP_jump (JMP_direct, Some addr) -> Printf.fprintf out "JUMP %d" addr
  | OP_jump (_,None) -> Printf.fprintf out "<unpatched [C]JUMP>"
  | OP_call -> output_string out "CALL"
  | OP_return -> output_string out "RETURN"
  | OP_yield -> output_string out "YIELD"

  | OP_bad -> output_string out "-"

  | OP_send -> output_string out "SEND"
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
    LIT_str s -> (TY_str, VAL_str s)
  | LIT_char c -> (TY_char, VAL_char c)
  | LIT_bool b -> (TY_bool, VAL_bool b)
  | LIT_mach m -> (TY_mach (ty_mach_of_lit_mach m), VAL_mach (val_mach_of_lit_mach m))
  | LIT_arith (_, _, n) -> (TY_arith (Ll1parser.numty n), VAL_arith n)
  | _ -> raise (Interp_err "unhandled literal in val_of_literal")


let rec emit_expr emit e = 
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
      emit_op emit (OP_push (VAL_dyn (val_of_literal v)))

  | EXPR_tuple (es, pos) -> 
      emit_op emit (OP_pos pos);
      Array.iter (emit_expr emit) es

  | EXPR_lval lv -> 
      emit_op emit (OP_copy_lval lv)

  | EXPR_call (lv, arg) -> 
      emit_expr emit arg;
      emit_op emit (OP_copy_lval lv);
      emit_op emit (OP_call);
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

  | STMT_move (lv2, lv1) -> 
      emit_op emit (OP_move_lval lv1);
      emit_op emit (OP_store_lval lv2)

  | STMT_copy (lv, e) -> 
      emit_expr emit e;
      emit_op emit (OP_store_lval lv)
	
  | STMT_call (lv, arg) -> 
      emit_expr emit arg;
      emit_op emit (OP_copy_lval lv);
      emit_op emit (OP_call);
      emit_op emit (OP_pop)

  | STMT_decl decl -> 
      emit_op emit (OP_alloc_local decl.decl_name)


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
  (* 
     Printf.printf "comparing types: %a == %a ? %B\n" 
     fmt_type p fmt_type q (p = q); 
   *)
  p = q
;;

let check_args args bind = 
  let param_types = bind.bind_ty.sig_param_types in
  let n_args = Array.length args in 
  let n_types = Array.length param_types in
  let n_names = Array.length bind.bind_names in

  (* Printf.printf "Checking %d args against %d types\n" n_args n_types; *)

  if n_names != n_types then
    raise (Interp_err "Inconsistent signature!");
  
  if n_args != n_types then 
    raise (Interp_err "Bad number of args");
  
  for i = 0 to n_args - 1 
  do
    let arg = args.(i) in
    (match arg with 
      VAL_dyn (t,_) -> 
	(* Printf.printf "checking arg type %d\n" i; *)
	if (not (types_equal t param_types.(i)))
	then raise (Interp_err "Bad argument type"));
  done
;;


let bind_args env args bind =
  check_args args bind;
  let n_args = Array.length args in 
  let n_names = Array.length bind.bind_names in

  (* Printf.printf "Binding %d args to %d parameters\n" n_args n_names; *)
  
  for i = 0 to n_args - 1 
  do
    let arg = args.(i) in
    Hashtbl.add env bind.bind_names.(i) (Some arg)
  done
;;


let bind_and_enter_frame proc fflav sba block_stmt =
  let emitter = new_emitter () in 
  emit_stmt emitter block_stmt;
  emit_op emitter OP_return;
     Printf.printf "== begin emitted code ==:\n";
     Array.iteri (fun i op -> Printf.printf "%6d: %a\n" i fmt_op op) emitter.emit_ops;
     Printf.printf "== end emitted code ==:\n";
  let frame = 
    { 
      frame_pc = 0;
      frame_ops = emitter.emit_ops;
      frame_flavour = fflav;		
      frame_scope = [];
      frame_scope_stack = Stack.create ();
      frame_expr_stack = Stack.create ();
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
    (UNOP_not, VAL_dyn (TY_bool, VAL_bool b)) -> 
      Stack.push (VAL_dyn (TY_bool, VAL_bool (not b))) stk
  | _ -> raise (Interp_err "unknown unary operator or operand")
;;


let exec_binop op stk =
  let rhs = Stack.pop stk in
  let lhs = Stack.pop stk in
  let push_bool b = Stack.push (VAL_dyn (TY_bool, VAL_bool b)) stk in
  let push_num n = 
    let nty = 
      match n with 
	Num.Ratio _ -> TY_rat
      | _           -> TY_int
    in
    Stack.push (VAL_dyn (TY_arith nty, VAL_arith n)) stk
  in
  match (lhs,rhs) with 
    (VAL_dyn (TY_bool, VAL_bool lb),
     VAL_dyn (TY_bool, VAL_bool rb)) -> 
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
  | (VAL_dyn (TY_arith _, VAL_arith ln),
     VAL_dyn (TY_arith _, VAL_arith rn)) -> 
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


let exec_op proc op = 
  let frame = List.hd proc.proc_frames in 
  let stk = frame.frame_expr_stack in
  let trueval = (VAL_dyn (TY_bool, VAL_bool false)) in

  match op with 
    OP_push v -> Stack.push v stk
  | OP_binop op -> exec_binop op stk
  | OP_unop op -> exec_unop op stk
  | OP_pop -> let _ = Stack.pop stk in ()
	
  | OP_copy_lval lv 
  | OP_move_lval lv -> 
      if Array.length lv.lval_rest != 0 
      then raise (Interp_err "cannot handle complex lvals yet")
      else	  
	let ov = Hashtbl.find proc.proc_env lv.lval_base in
	(match ov with 
	  Some v -> Stack.push v stk
	| None -> raise (Interp_err "extracting undefined value"));
	(match op with 
	  OP_move_lval _ -> 
	    Hashtbl.replace proc.proc_env lv.lval_base None
	| _ -> ())
	  
  | OP_store_lval lv -> 
      if Array.length lv.lval_rest != 0 
      then raise (Interp_err "cannot handle complex lvals yet")
      else	  
	let v = Stack.pop stk in
	Hashtbl.add proc.proc_env lv.lval_base (Some v)
	  
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
	  
  | OP_pos p -> 
      (match p with 
	(file,line,col) -> 
	  (* Printf.printf "exec pos (%s:%d:%d)\n" file line col; *)
	  proc.proc_pos <- p)
	
  | OP_jump (JMP_conditional, Some addr) -> 
      (match (Stack.pop frame.frame_expr_stack) with 
	(VAL_dyn (_, VAL_bool b)) -> 
	  if b 
	  then (frame.frame_pc <- addr; proc.proc_jumped <- true)
      | _ -> raise (Interp_err "conditional jump on non-boolean value"))
	
  | OP_jump (JMP_direct, Some addr) -> 
      (frame.frame_pc <- addr; proc.proc_jumped <- true)
	  
  | OP_jump (_,None) -> 
      raise (Interp_err "executing unpatched jump")
	
  | OP_call -> 
      (match (Stack.pop frame.frame_expr_stack) with 
	(VAL_dyn (_, VAL_func func)) -> 
	  let isig = func.func_bind.bind_ty in
	  let nargs = Array.length (isig.sig_param_types) in 
	  let args = Array.create nargs trueval in
	  for i = (nargs - 1) downto 0 
	  do 
	    args.(i) <- Stack.pop frame.frame_expr_stack
	  done;
	  let sba = Some (func.func_bind, args) in
	  (match func.func_body with 
	    FBODY_stmt s -> 
	      bind_and_enter_frame proc (FRAME_func (func.func_bind.bind_ty.sig_proto, 
						     func.func_bind)) sba s
	  | FBODY_native n -> 
	      check_args args func.func_bind;
	      (* Printf.printf "resolving native %s\n" n; *)
	      let native_fn = Hashtbl.find proc.proc_natives n in
	      (native_fn proc args);
	      Stack.push trueval stk)
      | _ -> raise (Interp_err "calling non-function"))
	
  | OP_return -> 
      List.iter (fun x -> Hashtbl.remove proc.proc_env x) frame.frame_scope;
      while not (Stack.is_empty frame.frame_scope_stack)
      do
	frame.frame_scope <- Stack.pop frame.frame_scope_stack;
	List.iter (fun x -> Hashtbl.remove proc.proc_env x) frame.frame_scope;
      done;
      proc.proc_frames <- List.tl proc.proc_frames;
      (match proc.proc_frames with 
	x :: _ -> Stack.push trueval x.frame_expr_stack
      | _ -> ())
	  
  | OP_yield -> raise (Interp_err "cannot yield yet")
  | OP_send -> raise (Interp_err "cannot send yet")
  | OP_bad -> raise (Interp_err "executing bad instruction")
;;


let bind_decl decl env =
  (* Printf.printf "binding decl of '%s'\n" decl.decl_name; *)
  let bv = 
    match decl.decl_artifact with
      ARTIFACT_code (ty, (CODE_func f)) -> Some (VAL_dyn (ty, VAL_func f))
    | ARTIFACT_slot (_, None) -> None
    | ARTIFACT_slot (_, Some (EXPR_literal (lit, _))) -> 
	Some (VAL_dyn (val_of_literal lit))
    | _ ->  raise (Interp_err "bad binding")
  in
  Hashtbl.add env decl.decl_name bv
  
;;

let procid = ref 1
;;

let new_proc prog = 
  let env = Hashtbl.create (Array.length prog.prog_decls) in 
  Array.iter (fun decl -> bind_decl decl env) prog.prog_decls;
  { proc_id = (incr procid; !procid);
    proc_prog = prog;
    proc_env = env;
    proc_natives = Hashtbl.create 0;
    proc_frames = [];
    proc_state = PROC_INIT;
    proc_ports = Array.of_list [];
    proc_pos = ("",0,0);
    proc_jumped = false }
;;

  
let enter_init_frame proc args = 
  (* Printf.printf "entering init frame\n"; *)
  match proc.proc_prog.prog_init with 
    Some init -> 
      let bind = init.init_bind in
      let fflav = FRAME_init bind in      
      bind_and_enter_frame proc fflav (Some (bind, args)) init.init_body
  | _ -> ()
;;


let enter_main_frame proc = 
  (* Printf.printf "entering main frame\n"; *)
  match proc.proc_prog.prog_main with 
    Some block_stmt -> 
      bind_and_enter_frame proc FRAME_main None block_stmt
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
      (* Printf.printf "completed init, beginning main\n"; *)
      p.proc_state <- PROC_MAIN;
      enter_main_frame p

  | PROC_MAIN when p.proc_frames = [] -> 
      (* Printf.printf "completed main, finishing\n"; *)
      p.proc_state <- PROC_FINI

  | PROC_MAIN 
  | PROC_INIT 
  | PROC_FINI -> 
      let f = List.hd p.proc_frames in
      (* Printf.printf "(pc=%d)\n" f.frame_pc; *)
      if (f.frame_pc >= Array.length f.frame_ops)
      then exec_op p OP_return
      else exec_op p f.frame_ops.(f.frame_pc);
      if p.proc_jumped
      then p.proc_jumped <- false
      else f.frame_pc <- f.frame_pc + 1
	  
  | _ -> (raise (Interp_err "interpreter wedged"))
;;


(*****************************************************************)
(*                Initialization and Entry                       *)
(*****************************************************************)


let find_entry_prog sf entry_name = 
  let binding = Hashtbl.find sf entry_name in 
  match binding with 
    (VIS_public, d) -> 
      (match d.decl_artifact with
	ARTIFACT_code (_, (CODE_prog prog)) -> prog
      | _ -> raise (Interp_err ("cannot find 'pub prog " ^ entry_name ^ "'")))
  | _ -> raise (Interp_err ("cannot find 'pub " ^ entry_name ^ "'"))
;;


let init_runtime = 
  let t = TY_named { name_base = "sys";
		     name_rest = Array.of_list [COMP_string "rt"] }
  in
  let v = VAL_rec ( Hashtbl.create 0 )
  in
  VAL_dyn (t,v)
;;


let init_argv =
  let t = TY_vec { vec_elt_type = TY_str; 
		   vec_elt_state = Array.of_list []} in
  let v = VAL_vec (Array.of_list []) in
  VAL_dyn (t,v)
;;


let init_args = 
  Array.of_list [init_runtime; init_argv]
;;

let bind_std_natives proc = 
  let reg name fn = Hashtbl.add proc.proc_natives name fn in 
  let getstr v = 
    (match v with 
      VAL_dyn (_,VAL_str s) -> s
    | _ -> raise (Interp_err "expected string arg"))
  in
  let getint v = 
    (match v with 
      VAL_dyn (_,VAL_arith n) -> n
    | _ -> raise (Interp_err "expected string arg"))
  in
  reg "putstr" (fun p args -> print_string (getstr args.(0)));
  reg "putint" (fun p args -> print_string (Num.string_of_num (getint args.(0))))
;;


let interpret sf entry_name = 
  let p = new_proc (find_entry_prog sf entry_name) in
  try
    bind_std_natives p;
    Printf.printf "interpreting\n";
    enter_init_frame p init_args;
    while not (proc_finished p) do
      step_proc p;
    done;
    Printf.printf "yay!\n"
  with
    Interp_err s -> 
      Printf.printf "Interpreter error: %s\n" s
;;
