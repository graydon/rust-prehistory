open Ast
;;

exception Interp_err of string
;;


let new_proc prog = { proc_prog = prog;
		      proc_env = Hashtbl.create 100;
		      proc_frame = 0;
		      proc_frames = [];
		      proc_state = PROC_INIT;
		      proc_ports = Array.of_list [] }
;;



let enter_block proc stmts pos  =
  if proc.proc_frames = [] 
  then 
    raise (Interp_err "entering block with no frame")
  else
    let frame = List.nth proc.proc_frames proc.proc_frame in
    let block = { block_pc = 0;
		  block_stmts = stmts;
		  block_names = Stack.create();
		  block_pos = pos } in
    Stack.push block frame.frame_blocks
;;


let enter_init_frame proc = 
  match proc.proc_prog.prog_init with 
    Some (signature, STMT_block (stmts,pos)) -> 
      proc.proc_frames <- ({ frame_flavour = FRAME_init signature;
			     frame_blocks = Stack.create () }
			     :: proc.proc_frames);
      enter_block proc stmts pos
  | _ -> ()
;;


let enter_main_frame proc = 
  match proc.proc_prog.prog_main with 
    Some (STMT_block (stmts,pos)) -> 
      proc.proc_frames <- ({ frame_flavour = FRAME_main;
			     frame_blocks = Stack.create () } 
			   :: proc.proc_frames);
      enter_block proc stmts pos
  | _ -> ()  
;;


let proc_finished p =
  match p.proc_state with
    PROC_FINI when p.proc_frames = [] -> true
  | _ -> false
;;

let fmt_nc out nc =
  match nc with 
    COMP_string s -> Printf.fprintf out "%s" s
  | COMP_tupidx i -> Printf.fprintf out "%d" i
;;

let fmt_lit out lit = 
  match lit with 
    VAL_dyn (TY_str, VAL_str s) -> Printf.fprintf out "\"%s\"" (String.escaped s)
  | VAL_dyn (TY_char, VAL_char c) -> Printf.fprintf out "'%c'" c
  | VAL_dyn (_, VAL_arith n) -> Printf.fprintf out "%s" (Num.string_of_num n)
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

let exec_stmt s = 
  match s with 
    STMT_while w -> ()
  | STMT_foreach f -> ()
  | STMT_for f -> ()
  | STMT_if i -> ()
  | STMT_try t -> ()
  | STMT_yield y -> ()
  | STMT_return r -> ()
  | STMT_assert a -> ()
  | STMT_block b -> ()
  | STMT_move (dst,src) -> (Printf.printf "moving %a to %a" fmt_lval src fmt_lval dst)
  | STMT_copy (dst,src) -> (Printf.printf "copying %a to %a" fmt_expr src fmt_lval dst)
  | STMT_call (fn,args) -> (Printf.printf "calling: %a" fmt_expr (EXPR_call (fn,args)))
  | STMT_decl d -> ()
;;

let step_proc p =
  match p.proc_state with

    PROC_INIT when p.proc_frames = [] -> 
      p.proc_state <- PROC_MAIN;
      p.proc_frame <- 0;
      enter_main_frame p

  | PROC_MAIN when p.proc_frames = [] -> 
      p.proc_state <- PROC_FINI

  | PROC_MAIN 
  | PROC_INIT 
  | PROC_FINI -> 
      let f = List.nth p.proc_frames p.proc_frame in
      let b = Stack.top f.frame_blocks in
      if (b.block_pc >= Array.length b.block_stmts)
      then 
	(let _ = Stack.pop f.frame_blocks in
	if Stack.is_empty f.frame_blocks
	then 
	  p.proc_frames <- List.tl p.proc_frames)
      else
	((match b.block_pos with 
	  (file,line,_) -> Printf.printf "(block %s:%d, pc=%d)" file line b.block_pc);
	 exec_stmt b.block_stmts.(b.block_pc);
	 b.block_pc <- b.block_pc + 1)
	  
  | _ -> (raise (Interp_err "interpreter wedged"))
;;

let get_prog_val v = 
  match v with 
    VAL_dyn (TY_prog, VAL_prog (vp)) -> vp
  | _ -> raise (Interp_err "non-value")
;;

let find_entry_prog sf entry_name = 
  let binding = Hashtbl.find sf entry_name in 
  match binding with 
    (VIS_public, d) when (d.decl_type = TY_prog) -> get_prog_val d.decl_value
  | _ -> raise (Interp_err ("cannot find 'pub prog " ^ entry_name ^ "'"))
;;

let interpret sf entry_name = 
  let p = new_proc (find_entry_prog sf entry_name) in
  try
    Printf.printf "interpreting...\n";
    enter_init_frame p;
    while not (proc_finished p) do
      Printf.printf "stepping... ";
      step_proc p;
      Printf.printf " ... ok\n"
    done;
    Printf.printf "yay!"
  with
    Interp_err s -> 
      Printf.printf "Interpreter error: %s" s
;;
