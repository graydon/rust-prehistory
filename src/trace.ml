open Ast
open Val
;;

(*****************************************************************)
(*                    Output Formatting                          *)
(*****************************************************************)

let fmt_nc out nc =
  match nc with 
    COMP_ident s -> Printf.fprintf out "%s" s
  | COMP_idx i -> Printf.fprintf out "%d" i
  | COMP_app _ -> output_string out "[...]"
;;

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
  | LIT_func f -> 
      output_string out "func(...){...}"
  | LIT_prog f -> 
      output_string out "prog(...){...}"

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


let fmt_name out n = 
  output_string out n.name_base;
  Array.iter (fun c -> Printf.fprintf out ".%a" fmt_nc c) n.name_rest
;;

let rec fmt_type out t = 
  match t with 
    TY_dyn -> output_string out "dyn"
  | TY_type -> output_string out "type"

  | TY_nil -> output_string out "nil"
  | TY_bool -> output_string out "bool"
  | TY_mach (m,n) -> Printf.fprintf out "%c%d" (ty_mach_prefix m) n
  | TY_arith a -> output_string out (ty_arith_name a)
  | TY_str -> output_string out "str"
  | TY_char -> output_string out "char"

  | TY_tup t -> output_string out "(tup ...)"
  | TY_vec v -> output_string out "(vec ...)"

  | TY_func s -> output_string out "(func ...)"
  | TY_chan c -> output_string out "(chan ...)"
  | TY_port s -> output_string out "(port ...)"

  | TY_prog -> output_string out "prog"

  | TY_pred p -> output_string out "(pred ...)"
  | TY_quote q -> output_string out "(quote ...)"

  | TY_named n -> fmt_name out n 
	
  | TY_constrained (ty, _) -> (output_string out "(";
			       fmt_type out ty;
			       output_string out ": ...state)" )

  | TY_lim t -> Printf.fprintf out "(lim %a)" fmt_type t
;;


let fmt_res out r = 
  match r with
    LVAL lv -> Printf.fprintf out "lval:%s" lv.lval_base
  | RVAL rv -> (Printf.fprintf out "rval:";
		fmt_type out rv.rv_type)
;;

let rec fmt_expr out e = 
  match e with
    EXPR_binary (op, _, lhs, rhs) -> 
      Printf.fprintf out "(%a)%a(%a)" fmt_binop op fmt_expr lhs fmt_expr rhs
  | EXPR_unary (op, _, e2) -> 
      Printf.fprintf out "%a(%a)" fmt_unop op fmt_expr e2	
  | EXPR_literal (lit, _) -> 
      fmt_lit out lit
  | EXPR_lval (lv, _) -> 
      fmt_lval out lv	
  | EXPR_call (lv, _, args) -> 
      fmt_lval out lv;
      fmt_exprs out args
  | EXPR_rec (name,_,_) -> 
      fmt_name out name;
      output_string out "{...}"


and fmt_exprs out exprs = 
  output_string out "(";
  Array.iteri 
    (fun i x -> 
      if i != 0 then output_string out ", " else (); 
      fmt_expr out x) exprs;
  output_string out ")"
  

and fmt_lval out lv = 
  output_string out lv.lval_base;
  Array.iter 
    (fun x -> match x with 
      LIDX_named (nc, _) -> (Printf.fprintf out ".%a" fmt_nc nc)
    | LIDX_index e -> (Printf.fprintf out ".(%a)" fmt_expr e))
    lv.lval_rest
;;


let fmt_jmp out j = 
  match j with 
    JMP_conditional -> output_string out "conditional jump"
  | JMP_direct -> output_string out "direct jump"
;;

let fmt_op out op = 
  match op with
    OP_push v -> Printf.fprintf out "PUSH (%a)" fmt_res v
  | OP_binop op -> Printf.fprintf out "BINOP (%a)" fmt_binop op
  | OP_unop op -> Printf.fprintf out "UNOP (%a)" fmt_unop op
  | OP_pop -> output_string out "POP"

  | OP_copy -> output_string out "COPY"
  | OP_move -> output_string out "MOVE"
  | OP_store -> output_string out "STORE"

  | OP_enter_scope -> output_string out "ENTER_SCOPE"
  | OP_alloc_local s -> Printf.fprintf out "ALLOC_LOCAL %s" s
  | OP_undef_local s -> Printf.fprintf out "UNDEF_LOCAL %s" s
  | OP_exit_scope -> output_string out "EXIT_SCOPE"

  | OP_pos (file,line,col) -> Printf.fprintf out "POS (%s:%d:%d)" file line col

  | OP_jump (JMP_conditional, Some addr) -> Printf.fprintf out "CJUMP %d" addr
  | OP_jump (JMP_direct, Some addr) -> Printf.fprintf out "JUMP %d" addr
  | OP_jump (_,None) -> Printf.fprintf out "<unpatched [C]JUMP>"
  | OP_call -> output_string out "CALL"
  | OP_new -> output_string out "NEW"
  | OP_return -> output_string out "RETURN"
  | OP_yield -> output_string out "YIELD"

  | OP_bad -> output_string out "-"

  | OP_send -> output_string out "SEND"
;;
  

let fmt_stack out stk = 
  output_string out "[";
  let i = ref 0 in
  let f x = 
    Printf.fprintf out "%s%a" 
      (if !i == 0 then "" else ", ")
      fmt_res x;
    i := !i + 1
  in
  Stack.iter f stk;
  output_string out "]";
;;

let trace_op proc =
  match proc.proc_frames with
    [] -> ()
  | frame::_ -> 
      let stk = frame.frame_eval_stack in
      let op = 
	if (frame.frame_pc >= Array.length frame.frame_ops)
	then OP_return
	else frame.frame_ops.(frame.frame_pc);
      in
      (match op with 
	OP_pos _ -> ()
      | _ ->
	  if Stack.is_empty stk
	  then ()
	  else Printf.printf 
	      "       |  %a\n" 
	      fmt_stack stk);
      Printf.printf
	"%6d | %a \n"
	frame.frame_pc
	fmt_op op
;;
