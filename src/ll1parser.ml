
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

  (* Control-flow keywords *)
  | IF
  | LET
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
  | TUPIDX        of (int)

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
  | FUNC_Q
  | FUNC_STAR
  | FUNC_PLUS

  | CHAN
  | CHAN_Q
  | CHAN_STAR
  | CHAN_PLUS

  | PORT
  | PORT_Q
  | PORT_STAR
  | PORT_PLUS

  (* Process types *)
  | PROC
  | PROG

  | EOF
      
;;

let string_of_tok t = 
  match t with 
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
  | TUPIDX i   -> ("#" ^ (string_of_int i))

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
  | FUNC_Q          -> "func?"
  | FUNC_STAR       -> "func*"
  | FUNC_PLUS       -> "func+"

  | CHAN            -> "chan"
  | CHAN_Q          -> "chan?"
  | CHAN_STAR       -> "chan*"
  | CHAN_PLUS       -> "chan+"

  | PORT            -> "port"
  | PORT_Q          -> "port?"
  | PORT_STAR       -> "port*"
  | PORT_PLUS       -> "port+"

  (* Process/program types *)
  | PROC       -> "proc"
  | PROG       -> "prog"

  | EOF        -> "<EOF>"
;;

let arr ls = Array.of_list ls
;;

let arl ls = Array.of_list (List.rev ls)
;;

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

let peek ps = 
  (Printf.printf "peeking at: %s\n" (string_of_tok ps.pstate_peek);
   ps.pstate_peek)

;;

let bump ps = 
  (Printf.printf "bumping past: %s\n" (string_of_tok ps.pstate_peek);
   ps.pstate_peek <- ps.pstate_lexfun ps.pstate_lexbuf)
;;


let numty n =
  match n with 
    Num.Ratio _ -> Ast.TY_rat
  | _           -> Ast.TY_int
;;

let rec expect ps t = 
  let p = peek ps in
  if p == t 
  then bump ps
  else 
    let msg = ("Expected '" ^ (string_of_tok t) ^ 
	       "', found '" ^ (string_of_tok p ) ^ "'") in
    raise (Parse_err (ps, msg))

and err str ps = 
  (Parse_err (ps, (str)))
    
and unexpected ps = 
  err ("Unexpected token '" ^ (string_of_tok (peek ps)) ^ "'") ps

and parse_ident ps = 
  match peek ps with
    IDENT id -> (bump ps; id)
  | _ -> raise (unexpected ps)
	
and parse_lval ps base pos =
  let components = ref [] in 
  while peek ps == DOT 
  do
    bump ps;
    let pos = lexpos ps in
    match peek ps with
      IDENT str -> 
	bump ps;
	components := ((Ast.LIDX_named 
			  ((Ast.COMP_string str), pos)) :: !components)
    | TUPIDX i -> 
	bump ps;
	components := ((Ast.LIDX_named 
			  ((Ast.COMP_tupidx i), pos)) :: !components)
    | LPAREN -> 
	bump ps;
	let e = parse_expr ps in
	expect ps RPAREN;
	components := ((Ast.LIDX_index e) :: !components)
    | _ -> 
	raise (unexpected ps)

  done;
  { Ast.lval_base = base;
    Ast.lval_rest = arl !components }


and parse_ATOMIC_expr ps =
  let pos = lexpos ps in
  match peek ps with
    LPAREN -> 
      bump ps;
      (match peek ps with
	RPAREN -> Ast.EXPR_tuple (arr [], pos)
      | _ -> 
	  let e = parse_expr ps in
	  expect ps RPAREN;
	  e)
	
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
      let lval = parse_lval ps str pos in
      (match peek ps with 
	LPAREN -> 
	  let arg = parse_expr ps in
	  Ast.EXPR_call (lval, arg)
      | _ -> Ast.EXPR_lval lval)

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


(* Tuples are *not* paren-enclosed expressions. That would make
   1-element tuples ambiguous with paren-enclosed expressions for
   order-of-operation overriding and function args. Instead, N-ary
   tuples are only defined for N >= 2, and comma is the tuple
   constructor: the lowest-precedence binary operator. *)

(* Hopefully the user will never care enough to notice this. The only
   place it feels weird is in handling functions. But function
   signatures are carefully arranged syntactic sugar in the first
   place: they combine an optional naming of the function, a naming of
   parameters, a typing of parameters, declaration of parameter
   passing modes, and declaration of a cross-parameter typestate. An
   "N-ary" function for N >=2 can only be applied to a compatible
   N-tuple. A 0-ary function can only be applied to (), and a 1-ary
   function can only be applied to a compatible value. This is how
   users expect to interpret 0-ary and 1-ary "tuples" anyways. The
   parse context for a call-argument list is not the same parse
   context as a tuple expression. *)

and parse_tuple_expr ps =
  let lhs = ctxt "tuple" parse_OR_expr ps in 
  match peek ps with 
    COMMA -> 
      let pos = lexpos ps in
      let exprs = ref [lhs] in
      while peek ps == COMMA
      do
	bump ps;
	let next = parse_OR_expr ps in 
	exprs := next :: !exprs
      done;
      Ast.EXPR_tuple (arl !exprs, pos)
  | _ -> lhs


and parse_expr ps =
  parse_tuple_expr ps


and parse_slot ps = 
  match peek ps with
    CARET -> 
      bump ps;
      let t = ctxt "slot" parse_ty ps in 
      Ast.SLOT_external t
  | _ -> 
    let t = ctxt "slot" parse_ty ps in 
    Ast.SLOT_standard t


and parse_ty ps = 
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

  | _ -> failwith "unimplemented parse rules"
  
and parse_stmt ps =
  let pos = lexpos ps in
  match peek ps with 
    IF -> 
      bump ps;
      expect ps LPAREN;
      let e = ctxt "stmt: if cond" parse_expr ps in
      expect ps RPAREN;
      let then_stmt = ctxt "stmt: if-then stmt" parse_stmt ps in
      let else_stmt = 
	match then_stmt with 
	  Ast.STMT_block _ -> 
	    (match peek ps with 
	      ELSE -> 
		bump ps;
		Some (ctxt "stmt: if-else stmt" parse_stmt ps)
	    | _ -> None)
	| _  -> None
      in
      Ast.STMT_if 
	{ Ast.if_test = e;
	  Ast.if_then = then_stmt;
	  Ast.if_else = else_stmt;
	  Ast.if_pos = pos }

  | WHILE -> 
      bump ps;
      expect ps LPAREN;
      let e = ctxt "stmt: while cond" parse_expr ps in
      expect ps RPAREN;
      let s = ctxt "stmt: while body" parse_stmt ps in
      Ast.STMT_while 
	{ Ast.while_expr = e;
	  Ast.while_body = s;
	  Ast.while_pos = pos }
	
  | RETURN -> 
      bump ps;
      let e = ctxt "stmt: return expr" parse_expr ps in 
      Ast.STMT_return (e, pos)
	
  | LBRACE -> 
      bump ps;
      let rec parse_stmts stmts = 
	match peek ps with 
	  RBRACE -> (bump ps; stmts)
	| _ -> 
	    let h = ctxt "stmt: block member" parse_stmt ps in 
	    parse_stmts (h::stmts)
      in
      Ast.STMT_block (arl (parse_stmts []), pos)

  | IDENT str -> 
      bump ps;
      let lval = ctxt "stmt: lval" parse_lval ps str pos in
      (match peek ps with 
	LPAREN -> 
	  let e = ctxt "stmt: call" parse_expr ps in 
	  expect ps SEMI;
	  Ast.STMT_call (lval, e)
      | EQ -> 
	  bump ps;
	  let e = ctxt "stmt: copy" parse_expr ps in 
	  expect ps SEMI;
	  Ast.STMT_copy (lval, e)
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
  let ty = ctxt "bind_param: type" parse_ty ps in
  let pmode = 
    match peek ps with
      MINUS -> (bump ps; Ast.PMODE_move_in)
    | EQ -> (bump ps; Ast.PMODE_move_in_out)
    | _ -> Ast.PMODE_copy
  in
  let name = ctxt "bind_param: ident" parse_ident ps in
  (ty, pmode, name)

and parse_bind ps = 
  expect ps LPAREN;  
  match peek ps with
    RPAREN -> (bump ps; [])
  | _ -> 
      let p0 = ctxt "bind: param 0" parse_bind_param ps in
      let params = ref [p0] in
      while peek ps == COMMA
      do
	bump ps;
	let p = ctxt "bind: param n" parse_bind_param ps in
	params := p :: !params
      done;
      expect ps RPAREN;
      List.rev !params

(* parse_func starts at the first lparen of the sig. *)
and parse_func ps =
  let bindings = ctxt "func: bindings" parse_bind ps in
  let (tys, pmodes, names) = 
    List.fold_left
      (fun 
	(tys, pmodes, names)
	  (ty,pmode,name) ->
	    (ty::tys, pmode::pmodes, name::names))
      ([],[],[])
      bindings
  in
  (tys, pmodes, names)

and parse_decl ps = 
  let pos = lexpos ps in  
  let auto = 
    match peek ps with 
      AUTO -> bump ps; true
    | _ -> false
  in
  match peek ps with 
    PROG -> 
      bump ps;
      let n = ctxt "decl: prog ident" parse_ident ps in
      let prog = ctxt "decl: prog body" parse_prog ps in
      let prog = { prog with Ast.prog_auto = auto } in
      { Ast.decl_name = n;
	Ast.decl_pos = pos;
	Ast.decl_artifact = Ast.ARTIFACT_code (Ast.TY_prog, Ast.CODE_prog prog) }
	
(*
   FIXME: waiting on implementation of parse_func.
  | FUNC -> 
      bump ps;
      let n = parse_ident ps in
      let (ty, func) = parse_func ps in 
      { Ast.decl_name = n; 
	Ast.decl_pos = pos;
	Ast.decl_artifact = Ast.ARTIFACT_code (ty, Ast.CODE_func func) }
*)
      
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
      Hashtbl.add bindings decl.Ast.decl_name (vis,decl)) decls;
  bindings


;;
