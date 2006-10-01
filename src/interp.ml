open Ast
;;

exception Interp_err of string
;;


(*****************************************************************)
(*                    Output Formatting                          *)
(*****************************************************************)


let fmt_nc out nc =
  match nc with 
    COMP_string s -> Printf.fprintf out "%s" s
  | COMP_tupidx i -> Printf.fprintf out "%d" i
;;

let fmt_lit out lit = 
  match lit with 
    VAL_dyn (TY_str, VAL_str s) -> 
      Printf.fprintf out "\"%s\"" (String.escaped s)
  | VAL_dyn (TY_char, VAL_char c) -> 
      Printf.fprintf out "'%c'" c
  | VAL_dyn (_, VAL_arith n) -> 
      Printf.fprintf out "%s" (Num.string_of_num n)
  | _ -> output_string out "**error:non-literal**"
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
  | EXPR_literal (v,_) -> fmt_lit out v
  | EXPR_lval lv -> fmt_lval out lv
  | EXPR_call (lv, args) -> 
      fmt_lval out lv;
      output_string out "(";
      Array.iteri 
	(fun i arg -> 
	  (if i = 0 then () else output_string out ", ");
	  fmt_expr out arg) 
	args;
      output_string out ")";

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

  | TY_mach (m,n) -> Printf.fprintf out "%c%d" (ty_mach_prefix m) n
  | TY_arith a -> output_string out (ty_arith_name a)
  | TY_str -> output_string out "str"
  | TY_char -> output_string out "char"

  | TY_rec r -> output_string out "(rec ...)"
  | TY_alt a -> output_string out "(alt ...)"
  | TY_tup t -> output_string out "(tup ...)"
  | TY_vec v -> output_string out "(vec ...)"

  | TY_subr s -> output_string out "(subr ...)"
  | TY_chan c -> output_string out "(chan ...)"

  | TY_port c -> output_string out "(port ...)"
  | TY_prog -> output_string out "prog"
  | TY_proc -> output_string out "proc"

  | TY_pred p -> output_string out "(pred ...)"
  | TY_quote q -> output_string out "(quote ...)"

  | TY_const t -> Printf.fprintf out "(const %a)" fmt_type t
  | TY_ref t -> Printf.fprintf out "(ref %a)" fmt_type t
  | TY_named n -> fmt_name out n 
  | TY_abstr (ty, params) -> output_string out "(abstr ...)"
  | TY_apply (ty, args) -> output_string out "(apply ...)"

  | TY_lim t -> Printf.fprintf out "(lim %a)" fmt_type t


(*****************************************************************)
(*                     Execution                                 *)
(*****************************************************************)

let bind_decl decl env =
  Printf.printf "binding decl of '%s'\n" decl.decl_name;
  Hashtbl.add env decl.decl_name decl.decl_value
;;


let types_equal p q =
  Printf.printf "comparing types: %a == %a ? %B\n" 
    fmt_type p fmt_type q (p = q); 
  p = q
;;

let bind_args env args bind =
  let param_types = bind.bind_sig.subr_sig.sig_param_tup.tup_types in
  let n_args = Array.length args in 
  let n_types = Array.length param_types in
  let n_names = Array.length bind.bind_names in

  Printf.printf "Checking %d args against %d types\n" n_args n_types;
  Printf.printf "Binding %d args to %d parameters\n" n_args n_names;

  if n_names != n_types then
    raise (Interp_err "Inconsistent signature!");
  
  if n_args != n_names then 
    raise (Interp_err "Bad number of args");
  
  for i = 0 to n_args - 1 
  do
    let arg = args.(i) in
    (match arg with 
      VAL_dyn (t,_) -> 
	Printf.printf "checking arg type %d\n" i;
	if (not (types_equal t param_types.(i)))
	then raise (Interp_err "Bad argument type"));
    Hashtbl.add env bind.bind_names.(i) (Some arg)
  done
;;

let new_proc prog = 
  let env = Hashtbl.create (Array.length prog.prog_decls) in 
  Array.iter (fun decl -> bind_decl decl env) prog.prog_decls;
  { proc_prog = prog;
    proc_env = env;
    proc_frame = 0;
    proc_frames = [];
    proc_state = PROC_INIT;
    proc_ports = Array.of_list [] }
;;

let enter_block proc block_stmt =
  if proc.proc_frames = [] 
  then 
    raise (Interp_err "entering block with no frame")
  else
    match block_stmt with 
      (STMT_block (stmts, pos)) -> 
	Printf.printf "entering block\n";
	let frame = List.nth proc.proc_frames proc.proc_frame in
	let block = { block_pc = 0;
		      block_stmts = stmts;
		      block_names = Stack.create();
		      block_pos = pos } in
	Stack.push block frame.frame_blocks
    | _ -> raise (Interp_err "\"entering\" non-block statement")
;;


let bind_and_enter_frame proc fflav sba block_stmt =
  proc.proc_frames <- ({ frame_flavour = fflav;
			 frame_blocks = Stack.create () }
		       :: proc.proc_frames);
  (match sba with 
    Some (binding,args) -> bind_args proc.proc_env args binding
  | None -> ());
  enter_block proc block_stmt
;;

  
let enter_init_frame proc args = 
  Printf.printf "entering init frame\n";
  match proc.proc_prog.prog_init with 
    Some ((sigt,names), block_stmt) -> 
      let bind = 
	{ 
	  bind_sig = { subr_inline = false;
		       subr_pure = true;
		       subr_sig = sigt; };
	  bind_names = names
	} 
      in
      let fflav = FRAME_init bind in      
      bind_and_enter_frame proc fflav (Some (bind,args)) block_stmt
  | _ -> ()
;;


let enter_main_frame proc = 
  Printf.printf "entering main frame\n";
  match proc.proc_prog.prog_main with 
    Some block_stmt -> 
      bind_and_enter_frame proc FRAME_main None block_stmt
  | _ -> ()  
;;


let pluck full_val = 
  match full_val with 
    VAL_dyn (_,v) -> v
;;
	

let enter_frame_val proc frame_val args =
  match (pluck frame_val) with 
    VAL_subr (SUBR_func, s) -> 
      (bind_and_enter_frame proc (FRAME_func s.subr_bind)
	 (Some (s.subr_bind,args)) s.subr_body)
  | VAL_subr (SUBR_iter, s) -> 
      (bind_and_enter_frame proc (FRAME_iter s.subr_bind)
	 (Some (s.subr_bind,args)) s.subr_body)
  | _ -> raise (Interp_err "Entering non-subroutine value")
;;
  

let proc_finished p =
  match p.proc_state with
    PROC_FINI when p.proc_frames = [] -> true
  | _ -> false
;;


let lookup_lval lval env =
  if Array.length lval.lval_rest != 0 
  then raise (Interp_err "can't handle multi-component lvals yet!")
  else 
    match Hashtbl.find env lval.lval_base with 
      Some v -> (Printf.printf "resolved lval %s\n" lval.lval_base); v
    | None -> 
	raise 
	  (Interp_err 
	     (Printf.sprintf 
		"resolved lval %s in uninitialized state" 
		lval.lval_base))
;;



let rec eval_expr proc expr =
  match expr with 
    EXPR_binary (binop, pos, e1, e2) -> raise (Interp_err "can't handle binary exprs yet")
  | EXPR_unary (unop, pos, e) -> raise (Interp_err "can't handle unary exprs yet")
  | EXPR_literal (v, pos) -> v
  | EXPR_lval lval -> lookup_lval lval proc.proc_env
  | EXPR_call (lval, args) -> raise (Interp_err "can't handle call exprs yet")

and eval_args_and_call proc lval args =
  let subr_val = lookup_lval lval proc.proc_env in
  let arg_vals = Array.map (eval_expr proc) args in
  enter_frame_val proc subr_val arg_vals   
;;

let exec_stmt proc stmt = 
  match stmt with 
    STMT_while w -> ()
  | STMT_foreach f -> ()
  | STMT_for f -> ()
  | STMT_if i -> ()
  | STMT_try t -> ()
  | STMT_yield y -> ()
  | STMT_return r -> ()
  | STMT_assert a -> ()
  | STMT_block b -> ()
  | STMT_move (dst,src) -> (Printf.printf "moving %a to %a\n" fmt_lval src fmt_lval dst)
  | STMT_copy (dst,src) -> (Printf.printf "copying %a to %a\n" fmt_expr src fmt_lval dst)
  | STMT_call (lval,args) -> 
      let e = EXPR_call (lval,args) in 
      (Printf.printf "calling: %a\n" fmt_expr e);
      eval_args_and_call proc lval args      
  | STMT_decl d -> ()
;;

let step_proc p =
  match p.proc_state with

    PROC_INIT when p.proc_frames = [] -> 
      Printf.printf "completed init, beginning main\n";
      p.proc_state <- PROC_MAIN;
      p.proc_frame <- 0;
      enter_main_frame p

  | PROC_MAIN when p.proc_frames = [] -> 
      Printf.printf "completed main, finishing\n";
      p.proc_state <- PROC_FINI

  | PROC_MAIN 
  | PROC_INIT 
  | PROC_FINI -> 
      let f = List.nth p.proc_frames p.proc_frame in
      let b = Stack.top f.frame_blocks in
      if (b.block_pc >= Array.length b.block_stmts)
      then 
	(let _ = Stack.pop f.frame_blocks in
	Printf.printf "leaving frame\n";
	if Stack.is_empty f.frame_blocks
	then 
	  p.proc_frames <- List.tl p.proc_frames)
      else
	((match b.block_pos with 
	  (file,line,_) -> Printf.printf "(block %s:%d, pc=%d)\n" file line b.block_pc);
	 exec_stmt p b.block_stmts.(b.block_pc);
	 b.block_pc <- b.block_pc + 1)
	  
  | _ -> (raise (Interp_err "interpreter wedged"))
;;


(*****************************************************************)
(*                Initialization and Entry                       *)
(*****************************************************************)


let get_const_prog_val v = 
  match v with 
    Some (VAL_dyn (TY_const (TY_prog), VAL_prog (vp))) -> vp
  | _ -> raise (Interp_err "non-value")
;;


let find_entry_prog sf entry_name = 
  let binding = Hashtbl.find sf entry_name in 
  match binding with 
    (VIS_public, d) when (d.decl_type = TY_const (TY_prog)) 
    -> get_const_prog_val d.decl_value
  | _ -> raise (Interp_err ("cannot find 'pub prog " ^ entry_name ^ "'"))
;;


let init_runtime = 
  let t = TY_ref (TY_named { name_base = "sys";
			     name_rest = Array.of_list [COMP_string "rt"] })
  in
  let v = VAL_rec ( Hashtbl.create 0 )
  in
  VAL_dyn (t,v)
;;


let init_argv =
  let t = TY_ref (TY_vec { vec_elt_type = TY_str }) in
  let v = VAL_vec (Array.of_list []) in
  VAL_dyn (t,v)
;;


let init_args = 
  Array.of_list [init_runtime; init_argv]
;;


let interpret sf entry_name = 
  let p = new_proc (find_entry_prog sf entry_name) in
  try
    Printf.printf "interpreting\n";
    enter_init_frame p init_args;
    while not (proc_finished p) do
      Printf.printf "stepping...\n";
      step_proc p;
      Printf.printf "... stepped\n"
    done;
    Printf.printf "yay!\n"
  with
    Interp_err s -> 
      Printf.printf "Interpreter error: %s\n" s
;;
