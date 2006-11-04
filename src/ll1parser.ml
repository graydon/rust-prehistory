
exception Parse_err of (Ast.rs_pos * string)
;;

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
  | LIT_NUM       of (Num.num)
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
  | CHAN
  | QUERY
  | QUERY_Q
  | QUERY_PLUS
  | QUERY_STAR

  (* Process types *)
  | PROC
  | PROG
  | PORT

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
  | LIT_NUM n  -> (Num.string_of_num n)
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
  | FUNC       -> "func"
  | CHAN       -> "chan"
  | QUERY      -> "query"
  | QUERY_Q    -> "query?"
  | QUERY_PLUS -> "query+"
  | QUERY_STAR -> "query*"

  (* Process types *)
  | PROC       -> "proc"
  | PROG       -> "prog"
  | PORT       -> "port"

  | EOF        -> "<EOF>"
;;

type pstate = 
    { mutable pstate_peek : token;
      pstate_lexfun       : Lexing.lexbuf -> token;
      pstate_lexbuf       : Lexing.lexbuf }
;;

let lexpos ps = 
  let p = ps.pstate_lexbuf.Lexing.lex_start_p in
  (p.Lexing.pos_fname,
   p.Lexing.pos_lnum ,
   (p.Lexing.pos_cnum) - (p.Lexing.pos_bol))
;;

let peek ps = ps.pstate_peek
;;

let bump ps = 
  ps.pstate_peek <- ps.pstate_lexfun ps.pstate_lexbuf
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
    let pos = lexpos ps in
    let msg = ("Expected '" ^ (string_of_tok t) ^ 
	       "', found '" ^ (string_of_tok p ) ^ "'") in
    raise (Parse_err (pos, msg))
    
and unexpected ps = 
  (Parse_err (lexpos ps, ("Unexpected token '" ^ 
			  (string_of_tok (peek ps)) ^ "'")))

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
    Ast.lval_rest = Array.of_list (List.rev !components) }


and parse_ATOMIC_expr ps =
  let pos = lexpos ps in
  match peek ps with
    LPAREN -> 
      bump ps;
      let e = parse_expr ps in
      expect ps RPAREN;
      e
	
  | LIT_NUM n -> 
      bump ps;
      Ast.EXPR_literal 
	(Ast.VAL_dyn (Ast.TY_arith (numty n), 
		      Ast.VAL_arith n), pos)

  | LIT_STR str ->
      bump ps;
      Ast.EXPR_literal 
	(Ast.VAL_dyn (Ast.TY_str, 
		      Ast.VAL_str str), pos)

  | LIT_CHAR ch ->
      bump ps;
      Ast.EXPR_literal 
	(Ast.VAL_dyn (Ast.TY_char, 
		      Ast.VAL_char ch), pos)

  | IDENT str -> 
      bump ps;
      let lval = parse_lval ps str pos in
      (match peek ps with 
	LPAREN -> 
	  bump ps;
	  let arg = parse_expr ps in
	  expect ps RPAREN;
	  Ast.EXPR_call (lval, arg)
      | _ -> Ast.EXPR_lval lval)

  | _ -> raise (unexpected ps)


and parse_NEGATION_expr ps =
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
  let lhs = parse_NEGATION_expr ps in
  match peek ps with 
    STAR    -> binop_rhs ps lhs parse_FACTOR_expr Ast.BINOP_mul
  | SLASH   -> binop_rhs ps lhs parse_FACTOR_expr Ast.BINOP_div
  | PERCENT -> binop_rhs ps lhs parse_FACTOR_expr Ast.BINOP_mod
  | _       -> lhs

and parse_TERM_expr ps =
  let lhs = parse_FACTOR_expr ps in
  match peek ps with 
    PLUS  -> binop_rhs ps lhs parse_TERM_expr Ast.BINOP_add
  | MINUS -> binop_rhs ps lhs parse_TERM_expr Ast.BINOP_sub
  | _     -> lhs

and parse_SHIFT_expr ps =
  let lhs = parse_TERM_expr ps in
  match peek ps with 
    LSL -> binop_rhs ps lhs parse_SHIFT_expr Ast.BINOP_lsl
  | LSR -> binop_rhs ps lhs parse_SHIFT_expr Ast.BINOP_lsr
  | ASR -> binop_rhs ps lhs parse_SHIFT_expr Ast.BINOP_asr
  | _   -> lhs

and parse_RELATIONAL_expr ps =
  let lhs = parse_SHIFT_expr ps in
  match peek ps with 
    LT -> binop_rhs ps lhs parse_RELATIONAL_expr Ast.BINOP_lt
  | LE -> binop_rhs ps lhs parse_RELATIONAL_expr Ast.BINOP_le
  | GE -> binop_rhs ps lhs parse_RELATIONAL_expr Ast.BINOP_ge
  | GT -> binop_rhs ps lhs parse_RELATIONAL_expr Ast.BINOP_gt
  | _  -> lhs

and parse_EQUALITY_expr ps =
  let lhs = parse_RELATIONAL_expr ps in
  match peek ps with 
    EQEQ -> binop_rhs ps lhs parse_EQUALITY_expr Ast.BINOP_eq
  | NE   -> binop_rhs ps lhs parse_EQUALITY_expr Ast.BINOP_ne
  | _    -> lhs

and parse_AND_expr ps =
  let lhs = parse_EQUALITY_expr ps in
  match peek ps with 
    AND -> binop_rhs ps lhs parse_AND_expr Ast.BINOP_and
  | _   -> lhs

and parse_OR_expr ps =
  let lhs = parse_AND_expr ps in
  match peek ps with 
    OR -> binop_rhs ps lhs parse_OR_expr Ast.BINOP_or
  | _  -> lhs

and parse_tuple_expr ps =
  let lhs = parse_OR_expr ps in 
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
      Ast.EXPR_tuple (Array.of_list (List.rev !exprs), pos)
  | _ -> lhs

and parse_expr ps =
  parse_tuple_expr ps

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

  | CARET -> 
      bump ps;
      let t = parse_ty ps in 
      Ast.TY_ref t

  | _ -> failwith "unimplemented parse rules"
  
and parse_stmt ps =
  let pos = lexpos ps in
  match peek ps with 
    IF -> 
      bump ps;
      expect ps LPAREN;
      let e = parse_expr ps in
      expect ps RPAREN;
      let then_stmt = parse_stmt ps in
      let else_stmt = 
	match then_stmt with 
	  Ast.STMT_block _ -> 
	    (match peek ps with 
	      ELSE -> 
		bump ps;
		Some (parse_stmt ps)
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
      let e = parse_expr ps in
      expect ps RPAREN;
      let s = parse_stmt ps in
      Ast.STMT_while 
	{ Ast.while_expr = e;
	  Ast.while_body = s;
	  Ast.while_pos = pos }
	
  | RETURN -> 
      bump ps;
      let e = parse_expr ps in 
      Ast.STMT_return (e, pos)
	
  | LBRACE -> 
      bump ps;
      let rec parse_stmts _ = 
	match peek ps with 
	  RBRACE -> (bump ps; [])
	| _ -> let tl = parse_stmts () in 
	  (parse_stmt ps) :: tl
      in
      Ast.STMT_block (Array.of_list (List.rev (parse_stmts ())), pos)

  | IDENT str -> 
      bump ps;
      let lval = parse_lval ps str pos in
      (match peek ps with 
	LPAREN -> 
	  let e = parse_expr ps in 
	  expect ps RPAREN;
	  expect ps SEMI;
	  Ast.STMT_call (lval, e)
      | EQ -> 
	  bump ps;
	  let e = parse_expr ps in 
	  expect ps SEMI;
	  Ast.STMT_copy (lval, e)
      | _ -> raise (unexpected ps))

  | _ -> raise (unexpected ps)

let sourcefile tok lbuf = 
  failwith "unimplemented parser"


;;
