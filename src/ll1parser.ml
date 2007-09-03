
type token = 

  (* Expression operator symbols *)
    PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | EQ
  | PLUS_EQ
  | MINUS_EQ
  | STAR_EQ
  | SLASH_EQ
  | PERCENT_EQ
  | LT
  | LE
  | EQEQ
  | NE
  | GE
  | GT
  | NOT
  | AND
  | OR
  | LSL
  | LSR
  | ASR

  (* Structural symbols *)
  | AT
  | CARET
  | DOT
  | COMMA
  | SEMI
  | COLON
  | RARROW
  | LARROW
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE

  (* Module and crate keywords *)
  | CRATE
  | MOD
  | USE

  (* Metaprogramming keywords *)
  | SYNTAX
  | META
  | TILDE

  (* Statement keywords *)
  | IF
  | LET
  | CONST
  | ELSE
  | WHILE
  | FOR
  | TRY
  | FAIL
  | INIT
  | MAIN
  | FINI
  | YIELD
  | RETURN

  (* Type and type-state keywords *)
  | TYPE
  | PRED
  | ASSERT

  (* Type qualifiers *)
  | LIM
  | PURE

  (* Declarator qualifiers *)
  | PUBLIC
  | PRIVATE
  | AUTO
  | INLINE
  | NATIVE

  (* Magic runtime services *)
  | NEW
  | LOG
  | REFLECT
  | EVAL

  (* Literals *)
  | LIT_BIN       of (Num.num)
  | LIT_HEX       of (Num.num)
  | LIT_DEC       of (Num.num)
  | LIT_STR       of (string)
  | LIT_CHAR      of (char)

  (* Name components *)
  | IDENT         of (string)
  | IDX           of (int)

  (* Reserved type names *)
  | NIL
  | BOOL
  | INT
  | NAT
  | RAT
  | CHAR
  | STR
  | BFP           of int
  | DFP           of int
  | SIGNED        of int
  | UNSIGNED      of int

  (* Algebraic type constructors *)
  | REC
  | ALT
  | VEC
  | DYN

  (* Callable type constructors *)
  | FUNC
  | FUNC_BANG
  | FUNC_QUES
  | FUNC_STAR
  | FUNC_PLUS

  | CHAN
  | CHAN_BANG
  | CHAN_QUES
  | CHAN_STAR
  | CHAN_PLUS

  | PORT
  | PORT_BANG
  | PORT_QUES
  | PORT_STAR
  | PORT_PLUS

  (* Process types *)
  | PROC
  | PROG

  | EOF
      
;;

let string_of_tok t = 
  match t with 
    (* Operator symbols (mostly) *)
    PLUS       -> "+"
  | MINUS      -> "-"
  | STAR       -> "*"
  | SLASH      -> "/"
  | PERCENT    -> "%"
  | EQ         -> "="
  | PLUS_EQ    -> "+="
  | MINUS_EQ   -> "-="
  | STAR_EQ    -> "*="
  | SLASH_EQ   -> "/="
  | PERCENT_EQ -> "%="
  | LT         -> "<"
  | LE         -> "<="
  | EQEQ       -> "=="
  | NE         -> "!="
  | GE         -> ">="
  | GT         -> ">"
  | NOT        -> "!"
  | AND        -> "&"
  | OR         -> "|"
  | LSL        -> "<<"
  | LSR        -> ">>"
  | ASR        -> ">>>"

  (* Structural symbols *)
  | AT         -> "@"
  | CARET      -> "^"
  | DOT        -> "."
  | COMMA      -> ","
  | SEMI       -> ";"
  | COLON      -> ":"
  | RARROW     -> "->"
  | LARROW     -> "<-"
  | LPAREN     -> "("
  | RPAREN     -> ")"
  | LBRACKET   -> "["
  | RBRACKET   -> "]"
  | LBRACE     -> "{"
  | RBRACE     -> "}"

  (* Module and crate keywords *)
  | CRATE      -> "crate"
  | MOD        -> "mod"
  | USE        -> "use"

  (* Metaprogramming keywords *)
  | SYNTAX     -> "syntax"
  | META       -> "meta"
  | TILDE      -> "~"

  (* Control-flow keywords *)
  | IF         -> "if"
  | LET        -> "let"
  | CONST      -> "const"
  | ELSE       -> "else"
  | WHILE      -> "while"
  | FOR        -> "for"
  | TRY        -> "try"
  | FAIL       -> "fail"
  | INIT       -> "init"
  | MAIN       -> "main"
  | FINI       -> "fini"
  | YIELD      -> "yield"
  | RETURN     -> "return"

  (* Type and type-state keywords *)
  | TYPE       -> "type"
  | PRED       -> "pred"
  | ASSERT     -> "assert"

  (* Type qualifiers *)
  | LIM        -> "lim"
  | PURE       -> "pure"

  (* Declarator qualifiers *)
  | PUBLIC     -> "pub"
  | PRIVATE    -> "priv"
  | AUTO       -> "auto"
  | INLINE     -> "inline"
  | NATIVE     -> "native"

  (* Magic runtime services *)
  | NEW        -> "new"
  | LOG        -> "log"
  | REFLECT    -> "reflect"
  | EVAL       -> "eval"

  (* Literals *)
  | LIT_HEX n  -> (Num.string_of_num n)
  | LIT_DEC n  -> (Num.string_of_num n)
  | LIT_BIN n  -> (Num.string_of_num n)
  | LIT_STR s  -> ("\"" ^ (String.escaped s) ^ "\"")
  | LIT_CHAR c -> ("'" ^ (Char.escaped c) ^ "'")

  (* Name components *)
  | IDENT s    -> s
  | IDX i   -> ("#" ^ (string_of_int i))

  (* Reserved type names *)
  | NIL        -> "()"
  | BOOL       -> "bool"
  | INT        -> "int"
  | NAT        -> "nat"
  | RAT        -> "rat"
  | CHAR       -> "char"
  | STR        -> "str"
  | BFP i      -> ("b" ^ (string_of_int i))
  | DFP i      -> ("d" ^ (string_of_int i))
  | SIGNED i   -> ("s" ^ (string_of_int i))
  | UNSIGNED i -> ("u" ^ (string_of_int i))

  (* Algebraic type constructors *)
  | REC        -> "rec"
  | ALT        -> "alt"
  | VEC        -> "vec"
  | DYN        -> "dyn"

  (* Callable type constructors *)
  | FUNC            -> "func"
  | FUNC_QUES       -> "func?"
  | FUNC_BANG       -> "func!"       
  | FUNC_STAR       -> "func*"
  | FUNC_PLUS       -> "func+"

  | CHAN            -> "chan"
  | CHAN_QUES       -> "chan?"
  | CHAN_BANG       -> "chan!"
  | CHAN_STAR       -> "chan*"
  | CHAN_PLUS       -> "chan+"

  | PORT            -> "port"
  | PORT_QUES       -> "port?"
  | PORT_BANG       -> "port!"
  | PORT_STAR       -> "port*"
  | PORT_PLUS       -> "port+"

  (* Process/program types *)
  | PROC       -> "proc"
  | PROG       -> "prog"

  | EOF        -> "<EOF>"
;;

(* Fundamental parser types and actions *)


type pstate = 
    { mutable pstate_peek : token;
      mutable pstate_ctxt : (string * Ast.pos) list;
      pstate_lexfun       : Lexing.lexbuf -> token;
      pstate_lexbuf       : Lexing.lexbuf }
;;


exception Parse_err of (pstate * string)
;;


let lexpos ps = 
  let p = ps.pstate_lexbuf.Lexing.lex_start_p in
  (p.Lexing.pos_fname,
   p.Lexing.pos_lnum ,
   (p.Lexing.pos_cnum) - (p.Lexing.pos_bol))
;;


let ctxt (n:string) (f:pstate -> 'a) (ps:pstate) : 'a =
  (ps.pstate_ctxt <- (n, lexpos ps) :: ps.pstate_ctxt;
   let res = f ps in
   ps.pstate_ctxt <- List.tl ps.pstate_ctxt;
   res)
;;


let peek ps = 
  (Printf.printf "peeking at: %s\n" (string_of_tok ps.pstate_peek);
   ps.pstate_peek)

;;


let bump ps = 
  (Printf.printf "bumping past: %s\n" (string_of_tok ps.pstate_peek);
   ps.pstate_peek <- ps.pstate_lexfun ps.pstate_lexbuf)
;;


let expect ps t = 
  let p = peek ps in
  if p == t 
  then bump ps
  else 
    let msg = ("Expected '" ^ (string_of_tok t) ^ 
	       "', found '" ^ (string_of_tok p ) ^ "'") in
    raise (Parse_err (ps, msg))
;;


let err str ps = 
  (Parse_err (ps, (str)))
;;

    
let unexpected ps = 
  err ("Unexpected token '" ^ (string_of_tok (peek ps)) ^ "'") ps
;;

(* Simple helpers *)

let numty n =
  match n with 
    Num.Ratio _ -> Ast.TY_rat
  | _           -> Ast.TY_int
;;


let arr ls = Array.of_list ls
;;


let arl ls = Array.of_list (List.rev ls)
;;


(* Parser combinators *)

let separated_by needOne sep rule ps =
  let accum = 
    if needOne
    then 
      let init = rule ps
      in ref [init] 
    else 
      ref []
  in
  while peek ps == sep
  do
    bump ps;
    accum := (rule ps) :: !accum
  done;
  arl !accum
;;

let bracketed_seq_nosep needOne bra ket rule ps =
  expect ps bra;
  let accum = 
    if needOne
    then 
      let init = rule ps
      in ref [init]
    else
      ref []
  in
  while peek ps != ket
  do
    accum := (rule ps) :: !accum
  done;
  expect ps ket;
  arl !accum
;;


let bracketed_zero_or_more_nosep bra ket rule ps =
  bracketed_seq_nosep false bra ket rule ps
;;


let bracketed_one_or_more_nosep bra ket rule ps =
  bracketed_seq_nosep true bra ket rule ps
;;


let zero_or_more sep rule ps =
  separated_by false sep rule ps
;;


let one_or_more sep rule ps = 
  separated_by true sep rule ps
;;


let bracketed bra ket rule ps =
  expect ps bra;
  let res = rule ps in
  expect ps ket;
  res

(* Small parse rules *)


let parse_pmode ps = 
  match peek ps with
    MINUS -> (bump ps; Ast.PMODE_move_in)
  | PLUS -> (bump ps; Ast.PMODE_move_out)
  | EQ -> (bump ps; Ast.PMODE_move_in_out)
  | _ -> Ast.PMODE_copy
;;


let parse_smode ps =
  match peek ps with
    AT -> (bump ps; Ast.SMODE_alias)
  | CARET -> (bump ps; Ast.SMODE_exterior)
  | _ -> Ast.SMODE_interior
;;
  
 
let parse_ident ps = 
  match peek ps with
    IDENT id -> (bump ps; id)
  | _ -> raise (unexpected ps)
;;

	
let parse_name_component ps =
  match peek ps with
    IDENT str -> bump ps; (Ast.COMP_ident str)
  | IDX i -> bump ps; (Ast.COMP_idx i)
  | _ -> raise (unexpected ps)
;;


let parse_name ps = 
  let base = ctxt "name: base" parse_ident ps in
  let rest = 
    match peek ps with 
      DOT -> (bump ps; 
	      ctxt "name: rest" 
		(zero_or_more DOT parse_name_component) ps)
    | _ -> arr []
  in
  { Ast.name_base = base;
    Ast.name_rest = rest }
;;


let parse_constraint_arg ps =
  match peek ps with
    IDENT n -> bump ps; Ast.BASE_named n
  | STAR -> bump ps; Ast.BASE_formal
  | _ -> raise (unexpected ps)
;;


let parse_carg ps = 
  let base = 
    match peek ps with
      STAR -> Ast.BASE_formal
    | IDENT str -> Ast.BASE_named str
    | _ -> raise (unexpected ps)
  in
  let rest = ctxt "carg: rest" 
      (zero_or_more DOT parse_name_component) ps in
  { 
    Ast.carg_base = base;
    Ast.carg_rest = rest;
  }
    

let parse_constraint ps = 
  match peek ps with 
    (* NB: A constraint *looks* a lot like an EXPR_call, but is restricted *)
    (* syntactically: the constraint name needs to be a name (not an lval) *)
    (* and the constraint args all need to be cargs, which are similar to  *)
    (* names but can begin with the 'formal' base anchor '*'.              *)
    IDENT _ -> 
      let n = ctxt "constraint: name" parse_name ps in
      let 
	  args = ctxt "constraint: args" 
	  (bracketed LPAREN RPAREN
	     (zero_or_more COMMA parse_carg)) ps
      in
      { Ast.constr_name = n;
	Ast.constr_args = args }
  | _ -> raise (unexpected ps)
;;
    

let parse_state ps = 
  match peek ps with
    COLON -> 
      ctxt "state: constraints" (one_or_more COMMA parse_constraint) ps
  | _ -> arr []
;;
  
  
let rec parse_base_ty ps = 
  match peek ps with 
    TYPE -> 
      bump ps; 
      Ast.TY_type
	
  | BOOL -> 
      bump ps; 
      Ast.TY_bool
	
  | INT -> 
      bump ps; 
      Ast.TY_arith (Ast.TY_int)
	
  | NAT -> 
      bump ps; 
      Ast.TY_arith (Ast.TY_nat)
	
  | RAT -> 
      bump ps; 
      Ast.TY_arith (Ast.TY_rat)
	
  | STR -> 
      bump ps; 
      Ast.TY_str

  | CHAR -> 
      bump ps; 
      Ast.TY_char

  | _ -> raise (unexpected ps)


and parse_ty_rest base ps =
  match peek ps with
    COLON -> 
      bump ps;
      let 
	  state = ctxt "ty_rest: state" parse_state ps 
      in
      parse_ty_rest (Ast.TY_constrained (base, state)) ps
	
  | LBRACKET -> 
      let 
	  rest = ctxt "ty_rest: apply" 
	  (bracketed LBRACKET RBRACKET 
	     (one_or_more COMMA parse_ty))
	  ps
      in
      parse_ty_rest (Ast.TY_apply (base, rest)) ps
	
  | _ -> base

and parse_ty ps = 
  let base = ctxt "ty: base" parse_base_ty ps in
  parse_ty_rest base ps
;;


(* The Giant Mutually-Recursive AST Parse Functions *)


let rec parse_lidx ps = 
  let pos = lexpos ps in
  match peek ps with
    IDENT _ | IDX _ -> 
      let ncomp = ctxt "lidx: name component" parse_name_component ps in
      Ast.LIDX_named (ncomp, pos)
  | LPAREN -> 
      bump ps;
      let e = ctxt "lidx: expr" parse_expr ps in
      expect ps RPAREN;
      Ast.LIDX_index e
  | _ -> raise (unexpected ps)


and parse_lval ps base pos =
  let rest = 
    match peek ps with 
      DOT -> (bump ps; 
	      ctxt "lval: rest" 
		(zero_or_more DOT parse_lidx) ps)
    | _ -> arr []
  in
  { Ast.lval_base = base;
    Ast.lval_rest = rest }


and parse_expr_list ps = 
  (bracketed LPAREN RPAREN
     (zero_or_more COMMA parse_expr)) ps 


and parse_ATOMIC_expr ps =
  let pos = lexpos ps in
  match peek ps with
    LPAREN -> 
      bump ps;
      let e = parse_expr ps in
      (expect ps RPAREN; e)
  
  | LIT_BIN n -> 
      bump ps;
      Ast.EXPR_literal
	(Ast.LIT_arith (numty n, Ast.BIN, n), pos)

  | LIT_HEX n -> 
      bump ps;
      Ast.EXPR_literal
	(Ast.LIT_arith (numty n, Ast.HEX, n), pos)

  | LIT_DEC n -> 
      bump ps;
      Ast.EXPR_literal
	(Ast.LIT_arith (numty n, Ast.DEC, n), pos)

  | LIT_STR str ->
      bump ps;
      Ast.EXPR_literal 
	(Ast.LIT_str str, pos)

  | LIT_CHAR ch ->
      bump ps;
      Ast.EXPR_literal 
	(Ast.LIT_char ch, pos)

  | IDENT str -> 
      bump ps;
      let pos = lexpos ps in
      let lval = parse_lval ps str pos in
      (match peek ps with 
	LPAREN -> 
	  let args = ctxt "call: args" parse_expr_list ps in
	  Ast.EXPR_call (lval, pos, args)	    
      | _ -> Ast.EXPR_lval (lval, pos))

  | NEW -> 
      bump ps;
      let pos = lexpos ps in
      let ty = ctxt "new: ty" parse_ty ps in
      let args = ctxt "new: args" parse_expr_list ps in
      Ast.EXPR_new (ty, pos, args)
  
  | _ -> raise (unexpected ps)


and parse_NEGATION_expr ps =
  let _ = Printf.printf ">>> NEGATION expr\n" in
  match peek ps with
    NOT ->
      let pos = lexpos ps in
      bump ps;
      Ast.EXPR_unary (Ast.UNOP_not, pos, (parse_NEGATION_expr ps))
  | _ -> parse_ATOMIC_expr ps


(* Binops are all left-associative,                *)
(* so we factor out some of the parsing code here. *)
and binop_rhs ps lhs rhs_parse_fn op =
  let pos = lexpos ps in
  bump ps; 
  Ast.EXPR_binary (op, pos, lhs, (rhs_parse_fn ps))

and parse_FACTOR_expr ps =
  let lhs = ctxt "FACTOR" parse_NEGATION_expr ps in
  match peek ps with 
    STAR    -> binop_rhs ps lhs parse_FACTOR_expr Ast.BINOP_mul
  | SLASH   -> binop_rhs ps lhs parse_FACTOR_expr Ast.BINOP_div
  | PERCENT -> binop_rhs ps lhs parse_FACTOR_expr Ast.BINOP_mod
  | _       -> lhs

and parse_TERM_expr ps =
  let lhs = ctxt "TERM" parse_FACTOR_expr ps in
  match peek ps with 
    PLUS  -> binop_rhs ps lhs parse_TERM_expr Ast.BINOP_add
  | MINUS -> binop_rhs ps lhs parse_TERM_expr Ast.BINOP_sub
  | _     -> lhs

and parse_SHIFT_expr ps =
  let lhs = ctxt "SHIFT" parse_TERM_expr ps in
  match peek ps with 
    LSL -> binop_rhs ps lhs parse_SHIFT_expr Ast.BINOP_lsl
  | LSR -> binop_rhs ps lhs parse_SHIFT_expr Ast.BINOP_lsr
  | ASR -> binop_rhs ps lhs parse_SHIFT_expr Ast.BINOP_asr
  | _   -> lhs

and parse_RELATIONAL_expr ps =
  let lhs = ctxt "RELATIONAL" parse_SHIFT_expr ps in
  match peek ps with 
    LT -> binop_rhs ps lhs parse_RELATIONAL_expr Ast.BINOP_lt
  | LE -> binop_rhs ps lhs parse_RELATIONAL_expr Ast.BINOP_le
  | GE -> binop_rhs ps lhs parse_RELATIONAL_expr Ast.BINOP_ge
  | GT -> binop_rhs ps lhs parse_RELATIONAL_expr Ast.BINOP_gt
  | _  -> lhs

and parse_EQUALITY_expr ps =
  let lhs = ctxt "EQUALITY" parse_RELATIONAL_expr ps in
  match peek ps with 
    EQEQ -> binop_rhs ps lhs parse_EQUALITY_expr Ast.BINOP_eq
  | NE   -> binop_rhs ps lhs parse_EQUALITY_expr Ast.BINOP_ne
  | _    -> lhs

and parse_AND_expr ps =
  let lhs = ctxt "AND" parse_EQUALITY_expr ps in
  match peek ps with 
    AND -> binop_rhs ps lhs parse_AND_expr Ast.BINOP_and
  | _   -> lhs

and parse_OR_expr ps =
  let lhs = ctxt "OR" parse_AND_expr ps in
  match peek ps with 
    OR -> binop_rhs ps lhs parse_OR_expr Ast.BINOP_or
  | _  -> lhs

and parse_expr ps =
  ctxt "expr" parse_OR_expr ps

and parse_slot const ps = 
  let mode = ctxt "slot: mode" parse_smode ps in
  let ty = ctxt "slot: ty" parse_ty ps in 
  let ident = ctxt "slot: ident" parse_ident ps in
  let state = ctxt "slot: state" parse_state ps in 
  {
   Ast.slot_const = const;
   Ast.slot_smode = mode;
   Ast.slot_ty = ty;
   Ast.slot_ident = ident;
   Ast.slot_state = state;
 }

	
and parse_block ps = 
  let pos = lexpos ps in
  let 
      stmts = ctxt "block: stmts" 
      (bracketed_zero_or_more_nosep LBRACE RBRACE parse_stmt) ps
  in
  Ast.STMT_block (stmts, pos)

and parse_stmt ps =
  let pos = lexpos ps in
  match peek ps with 
    IF -> 
      bump ps;
      let e = ctxt "stmt: if cond" (bracketed LPAREN RPAREN parse_expr) ps in
      let then_stmt = ctxt "stmt: if-then" parse_block ps in
      let else_stmt = 
	(match peek ps with 
	  ELSE -> 
	    bump ps;
	    Some (ctxt "stmt: if-else" parse_block ps)
	| _ -> None)
      in
      Ast.STMT_if 
	{ Ast.if_test = e;
	  Ast.if_then = then_stmt;
	  Ast.if_else = else_stmt;
	  Ast.if_pos = pos }

  | WHILE -> 
      bump ps;
      let e = ctxt "stmt: while cond" (bracketed LPAREN RPAREN parse_expr) ps in
      let s = ctxt "stmt: while body" parse_stmt ps in
      Ast.STMT_while 
	{ Ast.while_expr = e;
	  Ast.while_body = s;
	  Ast.while_pos = pos }
	
  | YIELD -> 
      bump ps;
      let e = 
	match peek ps with
	  SEMI -> None
	| _ -> 
	    let expr = ctxt "stmt: yield expr" parse_expr ps in 
	    expect ps SEMI;
	    Some expr
      in
      Ast.STMT_yield (e, pos)

  | RETURN -> 
      bump ps;
      let e = ctxt "stmt: return expr" parse_expr ps in 
      expect ps SEMI;
      Ast.STMT_return (e, pos)
	
  | LBRACE -> ctxt "stmt: block" parse_block ps

  | LET | CONST
  | FUNC | FUNC_QUES | FUNC_BANG | FUNC_STAR | FUNC_PLUS
  | PORT | PORT_QUES | PORT_BANG | PORT_STAR | PORT_PLUS
  | PROG | AUTO | NATIVE
    -> 
      let decl = ctxt "stmt: decl" parse_decl ps in
      Ast.STMT_decl decl


  | IDENT str ->       
      let pos = lexpos ps in      
      let lval = ctxt "stmt: lval" parse_lval ps str pos in
      (match peek ps with 
	
	LPAREN -> 
	  let args = ctxt "stmt: call args" parse_expr_list ps in 
	  expect ps SEMI;
	  Ast.STMT_call (lval, args)
	    
      | EQ -> 
	  bump ps;
	  let e = ctxt "stmt: copy rval" parse_expr ps in 
	  expect ps SEMI;
	  Ast.STMT_copy (lval, e)

      | LARROW -> 
	  bump ps;
	  let pos = lexpos ps in      
	  let base = ctxt "stmt: move rhs base" parse_ident ps in
	  let rhs = ctxt "stmt: move rhs rest" parse_lval ps base pos in 
	  expect ps SEMI;
	  Ast.STMT_move (lval, rhs)

      | _ -> raise (unexpected ps))

  | _ -> raise (unexpected ps)

and parse_prog_items p declist ps =
  let pos = lexpos ps in 
  match peek ps with 
    MAIN -> 
      bump ps; 
      let main = ctxt "prog_item: main" parse_stmt ps in 
      (match p.Ast.prog_main with
	None -> parse_prog_items { p with Ast.prog_main = Some main } declist ps
      | _ -> raise (err "duplicate main declaration" ps))

  | RBRACE -> 
      bump ps; 
      {p with Ast.prog_decls = arl declist }

  |_ -> 
      let decl = ctxt "prog_item: decl" parse_decl ps in 
      parse_prog_items p (decl :: declist) ps

	
and parse_prog ps = 
  let pos = lexpos ps in
  let prog = { Ast.prog_auto = false; 
	       Ast.prog_init = None;
	       Ast.prog_main = None;
	       Ast.prog_fini = None;
	       Ast.prog_decls = arr []; }
  in
  match peek ps with 
    LBRACE -> 
      bump ps; 
      parse_prog_items prog [] ps
  | _ -> raise (unexpected ps)

and parse_bind_param ps =
  let smode = ctxt "bind_param: smode" parse_smode ps in
  let ty = ctxt "bind_param: ty" parse_ty ps in
  let pmode = ctxt "bind_param: pmode" parse_pmode ps in
  let ident = ctxt "bind_param: ident" parse_ident ps in
  let state = ctxt "bind_param: extra state" parse_state ps in 
  let ty' = 
    if Array.length state == 0
    then ty 
    else Ast.TY_constrained (ty, state)
  in
  (smode, ty', pmode, ident)
	   

and parse_bind proto ps = 
  expect ps LPAREN;  
  let (smodes, tys, pmodes, idents) = 
    match peek ps with
      RPAREN -> (bump ps; (arr [], arr [], arr [], arr []))
    | _ -> 
	let (s,t,p,i) = ctxt "bind: param 0" parse_bind_param ps in
	let smodes = ref [s] in
	let tys = ref [t] in
	let pmodes = ref [p] in
	let idents = ref [i] in
	while peek ps == COMMA
	do
	  bump ps;
	  let (s,t,p,i) = ctxt "bind: param n" parse_bind_param ps in
	  smodes := s :: !smodes;
	  tys := t :: !tys;
	  pmodes := p :: !pmodes;
	  idents := i :: !idents
	done;
	expect ps RPAREN;
	(arl !smodes, arl !tys, arl !pmodes, arl !idents)
  in
  let istate = ctxt "bind: param state" parse_state ps in

  let result_smode = ctxt "bind: smode" parse_smode ps in
  let result_ty = ctxt "bind: param ty" parse_ty ps in
  let result_pmode = ctxt "bind: param ty" parse_pmode ps in
    
  { Ast.bind_ty = 
    { Ast.sig_proto = proto;

      Ast.sig_param_smodes = smodes;
      Ast.sig_param_types = tys;
      Ast.sig_param_pmodes = pmodes;

      Ast.sig_invoke_state = istate;

      Ast.sig_result_smode = result_smode;
      Ast.sig_result_ty = result_ty;
      Ast.sig_result_pmode = result_pmode;
    };
    Ast.bind_idents = idents; }


(* parse_func starts at the first lparen of the sig. *)
and parse_func native_id_opt proto ps =
  let bind = ctxt "func: bindings" (parse_bind proto) ps in
  let body = 
    match native_id_opt with
      None -> Ast.FBODY_stmt (ctxt "func: body" parse_block ps)
    | Some id -> Ast.FBODY_native id
  in
  { Ast.func_proto = proto;
    Ast.func_bind = bind;
    Ast.func_body = body; }
    
and parse_port auto proto ps =
  let bind = ctxt "port: bindings" (parse_bind proto) ps in
  let body = 
    if auto 
    then Some (ctxt "port: body" parse_block ps) 
    else None 
  in
  { Ast.port_proto = proto;
    Ast.port_bind = bind;
    Ast.port_auto_body = body }


and parse_decl ps = 
  let pos = lexpos ps in  

  let auto = 
    match peek ps with 
      AUTO -> bump ps; true
    | _ -> false
  in

  let native = 
    match peek ps with 
      NATIVE -> bump ps; true
    | _ -> false
  in    

  let inline = 
    match peek ps with 
      NATIVE -> bump ps; true
    | _ -> false
  in    

  let pure = 
    match peek ps with 
      PURE -> bump ps; true
    | _ -> false
  in    

  let do_func proto ps = 
    bump ps;
    if auto 
    then raise (Parse_err (ps, "meaningless 'auto' function"))
    else ();
    let id = parse_ident ps in
    (* FIXME: assignment of native names is broken / ad-hoc *)
    let native_id_opt = if native then Some id else None in
    let func = parse_func native_id_opt proto ps in 
    let fty = { Ast.func_inline = inline;
		Ast.func_pure = pure;
		Ast.func_sig = func.Ast.func_bind.Ast.bind_ty } 
    in    
    { Ast.decl_ident = id; 
      Ast.decl_pos = pos;
      Ast.decl_artifact = Ast.ARTIFACT_code (Ast.CODE_func (fty, func)) }
  in

  let do_port proto ps = 
    bump ps;
    if native 
    then raise (Parse_err (ps, "meaningless 'native' port"))
    else ();
    let id = parse_ident ps in
    let port = parse_port auto proto ps in 
    { Ast.decl_ident = id; 
      Ast.decl_pos = pos;
      Ast.decl_artifact = Ast.ARTIFACT_code (Ast.CODE_port port) }
  in

  let do_slot const ps =
      bump ps;
      let pos = lexpos ps in      
      let slot = ctxt "decl: slot" (parse_slot const) ps in
      let init = 
	match peek ps with
	  EQ -> 
	    bump ps;
	    Some (ctxt "decl: slot init" parse_expr ps)
	| _ -> None
      in
      { Ast.decl_ident = slot.Ast.slot_ident;
	Ast.decl_pos = pos;
	Ast.decl_artifact = Ast.ARTIFACT_slot (slot, init) }
  in
    
  match peek ps with 
    PROG -> 
      bump ps;
      let n = ctxt "decl: prog ident" parse_ident ps in
      let prog = ctxt "decl: prog body" parse_prog ps in
      let prog = { prog with Ast.prog_auto = auto } in
      { Ast.decl_ident = n;
	Ast.decl_pos = pos;
	Ast.decl_artifact = Ast.ARTIFACT_code (Ast.CODE_prog prog) }
	
  | FUNC -> do_func Ast.PROTO_call ps
  | FUNC_QUES -> do_func Ast.PROTO_ques ps
  | FUNC_BANG -> do_func Ast.PROTO_bang ps
  | FUNC_STAR -> do_func Ast.PROTO_star ps
  | FUNC_PLUS -> do_func Ast.PROTO_plus ps

  | PORT -> do_port Ast.PROTO_call ps
  | PORT_QUES -> do_port Ast.PROTO_ques ps
  | PORT_BANG -> do_port Ast.PROTO_bang ps
  | PORT_STAR -> do_port Ast.PROTO_star ps
  | PORT_PLUS -> do_port Ast.PROTO_plus ps

  | LET -> do_slot false ps
  | CONST -> do_slot true ps

  | _ -> raise (unexpected ps)
  

and parse_decl_top ps =
  let pos = lexpos ps in  
  match peek ps with 
    PUBLIC -> 
      bump ps;
      let d = ctxt "decl_top: public" parse_decl ps in
      (Ast.VIS_public, d)

  | PRIVATE -> 
      bump ps;
      let d = ctxt "decl_top: private" parse_decl ps in
      (Ast.VIS_local, d)

  | _ -> 
      bump ps;
      let d = ctxt "decl_top: crate" parse_decl ps in
      (Ast.VIS_crate, d)

and parse_topdecls ps decls =
  match peek ps with
    EOF -> List.rev decls
  | _ -> 
      let d = ctxt "topdecls" parse_decl_top ps in
      parse_topdecls ps (d::decls)

and sourcefile tok lbuf = 
  let first = tok lbuf in
  let ps = { pstate_peek = first;
	     pstate_ctxt = [];
	     pstate_lexfun = tok;
	     pstate_lexbuf = lbuf }
  in  
  let bindings = Hashtbl.create 100 in
  let decls =       
    try 
      parse_topdecls ps []
    with 
      Parse_err (ps, str) -> 
	Printf.printf "Parser error: %s\n" str;
	List.iter 
	  (fun (cx,(file,line,col)) -> 
	    Printf.printf "%s:%d:%d:E [PARSE CONTEXT] %s\n" file line col cx) 
	  ps.pstate_ctxt;
	[]
  in

  List.iter 
    (fun (vis,decl) -> 
      Hashtbl.add bindings decl.Ast.decl_ident (vis,decl)) decls;
  bindings


;;
