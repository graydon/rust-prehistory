
type token = 

    (* Expression operator symbols *)
    PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | EQ
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
  | DOTDOT
  | COMMA
  | SEMI
  | COLON
  | RARROW
  | SEND
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
  | ELSE
  | DO
  | WHILE
  | ALT

  | TRY
  | FAIL
  | INIT
  | MAIN
  | FINI

  | FOR of Ast.proto option
  | PUT of Ast.proto option
  | RET of Ast.proto option

  (* Type and type-state keywords *)
  | TYPE
  | PRED
  | CHECK
  | PROVE

  (* Type qualifiers *)
  | PURE
  | AUTO

  (* Module-item qualifiers *)
  | PUB

  (* Module value / stmt declarators. *) 
  | VAL
  | DYN

  (* Magic runtime services *)
  | LOG

  (* Literals *)
  | LIT_INT       of (Big_int.big_int * string)
  | LIT_STR       of (string)
  | LIT_CHAR      of (char)
  | LIT_BOOL      of (bool)

  (* Name components *)
  | IDENT         of (string)
  | IDX           of (int)

  (* Reserved type names *)
  | NIL
  | BOOL
  | INT
  | CHAR
  | STR
  | BFP           of int
  | DFP           of int
  | SIGNED        of int
  | UNSIGNED      of int

  (* Algebraic type constructors *)
  | REC
  | TAG
  | VEC
  | ANY
  | LIM

  (* Callable type constructors *)
  | FN of Ast.proto option

  | CHAN
  | PORT

  (* Process types *)
  | PROG
  | PROC

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
    | DOTDOT     -> ".."
    | COMMA      -> ","
    | SEMI       -> ";"
    | COLON      -> ":"
    | RARROW     -> "->"
    | SEND       -> "<|"
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
    | ELSE       -> "else"
    | DO         -> "do"
    | WHILE      -> "while"
    | ALT        -> "alt"

    | TRY        -> "try"
    | FAIL       -> "fail"
    | INIT       -> "init"
    | MAIN       -> "main"
    | FINI       -> "fini"

    | FOR None   -> "for"
    | FOR (Some Ast.PROTO_ques)   -> "for?"
    | FOR (Some Ast.PROTO_bang)   -> "for!"
    | FOR (Some Ast.PROTO_star)   -> "for*"
    | FOR (Some Ast.PROTO_plus)   -> "for+"

    | PUT None   -> "put"
    | PUT (Some Ast.PROTO_ques)   -> "put?"
    | PUT (Some Ast.PROTO_bang)   -> "put!"
    | PUT (Some Ast.PROTO_star)   -> "put*"
    | PUT (Some Ast.PROTO_plus)   -> "put+"

    | RET None   -> "ret"
    | RET (Some Ast.PROTO_ques)   -> "ret?"
    | RET (Some Ast.PROTO_bang)   -> "ret!"
    | RET (Some Ast.PROTO_star)   -> "ret*"
    | RET (Some Ast.PROTO_plus)   -> "ret+"


    (* Type and type-state keywords *)
    | TYPE       -> "type"
    | PRED       -> "pred"
    | CHECK      -> "check"
    | PROVE      -> "prove"

    (* Type qualifiers *)
    | PURE       -> "pure"
    | AUTO       -> "auto"

    (* Declarator qualifiers *)
    | PUB        -> "pub"

    (* Module value / stmt declarators. *) 
    | VAL        -> "val"
    | DYN        -> "dyn"

    (* Magic runtime services *)
    | LOG        -> "log"

    (* Literals *)
    | LIT_INT (n,s)  -> s
    | LIT_STR s  -> ("\"" ^ (String.escaped s) ^ "\"")
    | LIT_CHAR c -> ("'" ^ (Char.escaped c) ^ "'")
    | LIT_BOOL b -> if b then "true" else "false"

    (* Name components *)
    | IDENT s    -> s
    | IDX i   -> ("#" ^ (string_of_int i))

    (* Reserved type names *)
    | NIL        -> "nil"
    | BOOL       -> "bool"
    | INT        -> "int"
    | CHAR       -> "char"
    | STR        -> "str"
    | BFP i      -> ("b" ^ (string_of_int i))
    | DFP i      -> ("d" ^ (string_of_int i))
    | SIGNED i   -> ("s" ^ (string_of_int i))
    | UNSIGNED i -> ("u" ^ (string_of_int i))

    (* Algebraic type constructors *)
    | REC        -> "rec"
    | TAG        -> "tag"
    | VEC        -> "vec"
    | ANY        -> "any"
    | LIM        -> "lim"

    (* Function type constructors *)
    | FN None   -> "fn"
    | FN (Some Ast.PROTO_ques)   -> "fn?"
    | FN (Some Ast.PROTO_bang)   -> "fn!"
    | FN (Some Ast.PROTO_star)   -> "fn*"
    | FN (Some Ast.PROTO_plus)   -> "fn+"

    (* Ports and channels *)
    | CHAN          -> "chan"
    | PORT          -> "port"

    (* Process/program declarator types *)
    | PROG         -> "prog"
    | PROC       -> "proc"

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

let span apos bpos x = 
  { Ast.node = x; Ast.span = { Ast.lo = apos; Ast.hi = bpos } }

let ctxt (n:string) (f:pstate -> 'a) (ps:pstate) : 'a =
  (ps.pstate_ctxt <- (n, lexpos ps) :: ps.pstate_ctxt;
   let res = f ps in
     ps.pstate_ctxt <- List.tl ps.pstate_ctxt;
     res)
;;


let peek ps = 
  (Printf.printf "peeking at: %s     // %s\n" 
     (string_of_tok ps.pstate_peek)
     (match ps.pstate_ctxt with
          (s, _) :: _ -> s
        | _ -> "<empty>");
   ps.pstate_peek)

;;


let bump ps = 
  (Printf.printf "bumping past: %s\n" (string_of_tok ps.pstate_peek);
   ps.pstate_peek <- ps.pstate_lexfun ps.pstate_lexbuf)
;;


let span_bump ps x =
  let apos = lexpos ps in
  let _ = bump ps in
  let bpos = lexpos ps in
    span apos bpos x
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


let arr ls = Array.of_list ls
;;


let arl ls = Array.of_list (List.rev ls)
;;


(* Parser combinators *)

let one_or_more sep rule ps = 
  let accum = ref [rule ps] in
    while peek ps == sep
    do 
      bump ps;
      accum := (rule ps) :: !accum
    done;
    arl !accum
;;

let bracketed_seq mandatory bra ket sepOpt rule ps =
  expect ps bra;
  let accum = ref [] in
  let dosep _ = 
    (match sepOpt with 
         None -> ()
       | Some tok -> 
           if (!accum = [])
           then () 
           else expect ps tok)
  in
    while mandatory > List.length (!accum) do
      dosep ();
      accum := (rule ps) :: (!accum)
    done;
    while peek ps != ket
    do
      dosep ();    
      accum := (rule ps) :: !accum
    done;
    expect ps ket;
    arl !accum
;;


let path sep rule ps = 
  let accum = ref [] in
    while peek ps == sep
    do
      expect ps sep;
      accum := (ctxt "path" rule ps) :: !accum
    done;
    arl !accum
;;


let bracketed_zero_or_more bra ket sepOpt prule ps =
  bracketed_seq 0 bra ket sepOpt (ctxt "bracketed_seq" prule) ps
;;


let bracketed_one_or_more bra ket sepOpt prule ps =
  bracketed_seq 1 bra ket sepOpt (ctxt "bracketed_seq" prule) ps
;;

let bracketed_two_or_more bra ket sepOpt prule ps =
  bracketed_seq 2 bra ket sepOpt (ctxt "bracketed_seq" prule) ps
;;


let bracketed bra ket prule ps =
  expect ps bra;
  let res = ctxt "bracketed" prule ps in
    expect ps ket;
    res

(* Small parse rules *)

let parse_ident ps = 
  match peek ps with
      IDENT id -> (bump ps; id)
    | _ -> raise (unexpected ps)
;;


let rec parse_name_component ps =
  match peek ps with
      IDENT str -> bump ps; (Ast.COMP_ident str)
    | IDX i -> bump ps; (Ast.COMP_idx i)
    | LBRACKET -> 
        let 
            tys = 
          ctxt "name_component: apply" 
            (bracketed_one_or_more LBRACKET RBRACKET (Some COMMA) parse_ty) ps
        in
          Ast.COMP_app tys
            
    | _ -> raise (unexpected ps)


and parse_name ps = 
  let base = ctxt "name: base" parse_ident ps in
  let rest = ctxt "name: rest" (path DOT parse_name_component) ps in
    { Ast.name_base = base;
      Ast.name_rest = rest }


and parse_constraint_arg ps =
  match peek ps with
      IDENT n -> bump ps; Ast.BASE_named n
    | STAR -> bump ps; Ast.BASE_formal
    | _ -> raise (unexpected ps)


and parse_carg ps = 
  let base = 
    match peek ps with
        STAR -> Ast.BASE_formal
      | IDENT str -> Ast.BASE_named str
      | _ -> raise (unexpected ps)
  in
  let rest = ctxt "carg: rest" (path DOT parse_name_component) ps in
    { 
      Ast.carg_base = base;
      Ast.carg_rest = rest;
    }
      

and parse_constraint ps = 
  match peek ps with 
      (* NB: A constraint *looks* a lot like an EXPR_call, but is restricted *)
      (* syntactically: the constraint name needs to be a name (not an lval) *)
      (* and the constraint args all need to be cargs, which are similar to  *)
      (* names but can begin with the 'formal' base anchor '*'.              *)
      IDENT _ -> 
        let n = ctxt "constraint: name" parse_name ps in
        let 
            args = ctxt "constraint: args" 
          (bracketed_zero_or_more 
             LPAREN RPAREN (Some COMMA) 
             parse_carg) ps
        in
          { Ast.constr_name = n;
            Ast.constr_args = args }
    | _ -> raise (unexpected ps)


and parse_constrs ps = 
  ctxt "state: constraints" (one_or_more COMMA parse_constraint) ps            
        
and parse_base_ty ps = 
  match peek ps with 
          
      BOOL -> 
        bump ps; 
        Ast.TY_bool
          
    | INT -> 
        bump ps; 
        Ast.TY_int

    | STR -> 
        bump ps; 
        Ast.TY_str

    | CHAR -> 
        bump ps; 
        Ast.TY_char

    | NIL -> 
        bump ps;
        Ast.TY_nil

    | _ -> raise (unexpected ps)


and parse_slot ps =
  match peek ps with
      AT -> 
        bump ps;
        let ty = parse_ty ps in
          Ast.SLOT_alias ty

    | CARET -> 
        bump ps; 
        let ty = parse_ty ps in 
          Ast.SLOT_exterior ty
            
    | _ -> 
        let ty = parse_ty ps in 
          Ast.SLOT_interior ty
        
and parse_ty ps = 
  match peek ps with 
      LPAREN -> bracketed LPAREN RPAREN parse_ty ps
    | _ -> 
        let base = ctxt "ty: base" parse_base_ty ps in
          match peek ps with
              COLON -> 
                bump ps;
                let constrs = ctxt "ty: constrs" parse_constrs ps in
                  Ast.TY_constrained (base, constrs)
                    
            | _ -> base                
;;


(* The Giant Mutually-Recursive AST Parse Functions *)


let rec parse_lidx ps = 
  match peek ps with
      IDENT _ | IDX _ -> 
        let ncomp = ctxt "lidx: name component" parse_name_component ps in
          Ast.LIDX_named (ncomp)
    | LPAREN -> 
        bump ps;
        let e = ctxt "lidx: expr" parse_expr ps in
          expect ps RPAREN;
          Ast.LIDX_index e
    | _ -> raise (unexpected ps)
        

and parse_lval ps =
  let apos = lexpos ps in
  let base = ctxt "lval: base" parse_ident ps in
  let rest = ctxt "lval: rest" (path DOT parse_lidx) ps in
  let bpos = lexpos ps in
    span apos bpos 
      { Ast.lval_base = base;
        Ast.lval_rest = rest }
      

and parse_rec_input ps = 
  let lab = (ctxt "rec input: label" parse_ident ps) in
    match peek ps with
        EQ -> 
          bump ps;
          let expr = (ctxt "rec input: expr" parse_expr ps) in
            Ast.REC_from_copy (lab, expr)
      | LARROW -> 
          bump ps; 
          let lval = (ctxt "rec input: lval" parse_lval ps) in
            Ast.REC_from_move (lab, lval)
      | _ -> raise (unexpected ps)
          

and parse_rec_inputs ps = 
  bracketed_zero_or_more LBRACE RBRACE (Some COMMA) 
    (ctxt "rec inputs" parse_rec_input) ps


and parse_expr_list ps = 
  bracketed_zero_or_more LPAREN RPAREN (Some COMMA) 
    (ctxt "expr list" parse_expr) ps


and parse_atomic_expr ps =
  match peek ps with
      LPAREN -> 
        let apos = lexpos ps in
        let _ = bump ps in
        let e = ctxt "paren expr" parse_expr ps in
        let _ = expect ps RPAREN in
        let bpos = lexpos ps in 
          span apos bpos e.Ast.node
            
    | LIT_INT (n,s) -> 
        span_bump ps 
          (Ast.EXPR_literal
             (Ast.LIT_int (n, s)))
          
    | LIT_STR str ->
        span_bump ps 
          (Ast.EXPR_literal 
             (Ast.LIT_str str))
                    
    | LIT_CHAR ch ->
        span_bump ps
          (Ast.EXPR_literal 
             (Ast.LIT_char ch))
          
    | IDENT _ -> 
        let apos = lexpos ps in
        let lval = parse_lval ps in
          (match peek ps with 
               LPAREN -> 
                 let args = ctxt "call: args" parse_expr_list ps in
                 let bpos = lexpos ps in
                   span apos bpos (Ast.EXPR_call (lval, args))
             | LBRACE -> 
                 let ty = ctxt "rec expr: ty" parse_ty ps in
                 let inputs = ctxt "rec expr: rec inputs" parse_rec_inputs ps in
                 let bpos = lexpos ps in
                   span apos bpos (Ast.EXPR_rec (ty, inputs))
             | _ -> 
                 let bpos = lexpos ps in 
                   span apos bpos (Ast.EXPR_lval lval))
            
    | _ -> raise (unexpected ps)
        

and name_of_lval ps lval = 
  let extract_nc lidx = 
    match lidx with 
        Ast.LIDX_named nc -> nc
      | Ast.LIDX_index _ ->
          raise (Parse_err (ps, "expression-based lval found " ^ 
                              "where static name required"))
  in
    { Ast.name_base = lval.Ast.node.Ast.lval_base;
      Ast.name_rest = Array.map extract_nc lval.Ast.node.Ast.lval_rest }


and parse_negation_expr ps =
  match peek ps with
      NOT ->
        let apos = lexpos ps in
        bump ps;
        let e = ctxt "negation expr" parse_negation_expr ps in
        let bpos = lexpos ps in
          span apos bpos (Ast.EXPR_unary (Ast.UNOP_not, e))
    | _ -> parse_atomic_expr ps
        

(* Binops are all left-associative,                *)
(* so we factor out some of the parsing code here. *)
and binop_rhs ps name lhs rhs_parse_fn op =
  bump ps; 
  let apos = lexpos ps in
  let e = Ast.EXPR_binary (op, lhs, (ctxt (name ^ " rhs") rhs_parse_fn ps)) in
  let bpos = lexpos ps in 
    span apos bpos e
    
and parse_factor_expr ps =
  let name = "factor expr" in
  let lhs = ctxt (name ^ " lhs") parse_negation_expr ps in
    match peek ps with 
        STAR    -> binop_rhs ps name lhs parse_factor_expr Ast.BINOP_mul
      | SLASH   -> binop_rhs ps name lhs parse_factor_expr Ast.BINOP_div
      | PERCENT -> binop_rhs ps name lhs parse_factor_expr Ast.BINOP_mod
      | _       -> lhs

and parse_term_expr ps =
  let name = "term expr" in
  let lhs = ctxt (name ^ " lhs") parse_factor_expr ps in
    match peek ps with 
        PLUS  -> binop_rhs ps name lhs parse_term_expr Ast.BINOP_add
      | MINUS -> binop_rhs ps name lhs parse_term_expr Ast.BINOP_sub
      | _     -> lhs

and parse_shift_expr ps =
  let name = "shift expr" in
  let lhs = ctxt (name ^ " lhs") parse_term_expr ps in
    match peek ps with 
        LSL -> binop_rhs ps name lhs parse_shift_expr Ast.BINOP_lsl
      | LSR -> binop_rhs ps name lhs parse_shift_expr Ast.BINOP_lsr
      | ASR -> binop_rhs ps name lhs parse_shift_expr Ast.BINOP_asr
      | _   -> lhs

and parse_relational_expr ps =
  let name = "relational expr" in
  let lhs = ctxt (name ^ " lhs") parse_shift_expr ps in
    match peek ps with 
        LT -> binop_rhs ps name lhs parse_relational_expr Ast.BINOP_lt
      | LE -> binop_rhs ps name lhs parse_relational_expr Ast.BINOP_le
      | GE -> binop_rhs ps name lhs parse_relational_expr Ast.BINOP_ge
      | GT -> binop_rhs ps name lhs parse_relational_expr Ast.BINOP_gt
      | _  -> lhs

and parse_equality_expr ps =
  let name = "equality expr" in
  let lhs = ctxt (name ^ " lhs") parse_relational_expr ps in
    match peek ps with 
        EQEQ -> binop_rhs ps name lhs parse_equality_expr Ast.BINOP_eq
      | NE   -> binop_rhs ps name lhs parse_equality_expr Ast.BINOP_ne
      | _    -> lhs

and parse_and_expr ps =
  let name = "and expr" in
  let lhs = ctxt (name ^ " lhs") parse_equality_expr ps in
    match peek ps with 
        AND -> binop_rhs ps name lhs parse_and_expr Ast.BINOP_and
      | _   -> lhs

and parse_or_expr ps =
  let name = "or expr" in
  let lhs = ctxt (name ^ " lhs") parse_and_expr ps in
    match peek ps with 
        OR -> binop_rhs ps name lhs parse_or_expr Ast.BINOP_or
      | _  -> lhs

and parse_expr (ps:pstate) : Ast.expr  =
  ctxt "expr" parse_or_expr ps

and parse_slot_and_ident ps = 
  let slot = ctxt "slot and ident: slot" parse_slot ps in
  let ident = ctxt "slot and ident: ident" parse_ident ps in
    (slot, ident)
      
 and parse_two_or_more_tup_slots_and_idents ps = 
  let both = 
    ctxt "tup slots and idents" 
      (bracketed_two_or_more LPAREN RPAREN (Some COMMA) parse_slot_and_ident) ps
  in
  let (slots, idents) = List.split (Array.to_list both) in
    (arl slots, arl idents)

 and parse_one_or_more_tup_slots_and_idents ps = 
  let both = 
    ctxt "tup slots and idents" 
      (bracketed_one_or_more LPAREN RPAREN (Some COMMA) parse_slot_and_ident) ps
  in
  let (slots, idents) = List.split (Array.to_list both) in
    (arl slots, arl idents)
      
and parse_block ps = 
  let apos = lexpos ps in
  let stmts = ctxt "block: stmts" 
    (bracketed_zero_or_more LBRACE RBRACE None parse_stmt) ps
  in
  let bpos = lexpos ps in 
    span apos bpos (Ast.STMT_block stmts)

and parse_init ps = 
  let init = 
    match peek ps with
        EQ -> 
          bump ps;
          Some (ctxt "init: expr" parse_expr ps)
      | _ -> None
  in
  let _ = expect ps SEMI in 
    init

and parse_slot_and_ident_and_init ps = 
  let (slot, ident) = 
    ctxt "slot and init: ident and slot" 
      parse_slot_and_ident ps 
  in
  let init = ctxt "stmt slot decl: init" parse_init ps in
    (slot, ident, init)

and parse_stmt ps =
  let apos = lexpos ps in
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
          let bpos = lexpos ps in
            span apos bpos 
              (Ast.STMT_if 
                 { Ast.if_test = e;
                   Ast.if_then = then_stmt;
                   Ast.if_else = else_stmt; })

      | WHILE -> 
          bump ps;
          let e = ctxt "stmt: while cond" (bracketed LPAREN RPAREN parse_expr) ps in
          let s = ctxt "stmt: while body" parse_stmt ps in
          let bpos = lexpos ps in
            span apos bpos 
              (Ast.STMT_while 
                 { Ast.while_expr = e;
                   Ast.while_body = s; })
              
      | PUT proto -> 
          bump ps;
          let e = 
            match peek ps with
                SEMI -> None
              | _ -> 
                  let expr = ctxt "stmt: put expr" parse_expr ps in 
                    expect ps SEMI;
                    Some expr
          in
          let bpos = lexpos ps in
            span apos bpos (Ast.STMT_put (proto, e))

      | RET proto -> 
          bump ps;
          let e = 
            match peek ps with
                SEMI -> None
              | _ -> 
                  let expr = ctxt "stmt: ret expr" parse_expr ps in 
                    expect ps SEMI;
                    Some expr
          in
          let bpos = lexpos ps in
            span apos bpos (Ast.STMT_ret (proto, e))
              
      | LBRACE -> ctxt "stmt: block" parse_block ps

      | VAL -> 
          bump ps;
          (match peek ps with 
               LPAREN -> 
                 let (slots, idents) = 
                   ctxt "stmt tup decl: slots and idents" 
                     parse_two_or_more_tup_slots_and_idents ps in
                 let init = ctxt "stmt tup decl: init" parse_init ps in
                 let bpos = lexpos ps in 
                   span apos bpos 
                     (Ast.STMT_decl 
                        (Ast.DECL_slot_tup 
                           (slots, idents, init)))
             | _ -> 
                 let (slot, ident, init) = ctxt "stmt slot" parse_slot_and_ident_and_init ps in
                 let bpos = lexpos ps in 
                   span apos bpos 
                     (Ast.STMT_decl 
                        (Ast.DECL_mod_item 
                           (ident, (span apos bpos 
                                      (Ast.MOD_ITEM_slot (slot, init)))))))
                     
                      
                      
      | LIM | PORT | PROG | MOD | (FN _) -> 
          let decl = ctxt "stmt: decl" parse_mod_item ps in
          let bpos = lexpos ps in 
            span apos bpos (Ast.STMT_decl (Ast.DECL_mod_item decl))

      | LPAREN -> 
          let lvals = 
            ctxt "stmt: paren_copy_to_tup tup" 
              (bracketed_one_or_more LPAREN RPAREN (Some COMMA) parse_lval) ps 
          in
          let _ = expect ps EQ in 
          let expr = ctxt "stmt: paren_copy_to_tup rval" parse_expr ps in 
          let _ = expect ps SEMI in
          let bpos = lexpos ps in 
            span apos bpos (Ast.STMT_copy (Ast.COPY_to_tup (lvals, expr)))
              
      | IDENT _ ->
          let lval = ctxt "stmt: lval" parse_lval ps in
            (match peek ps with 
                 
                 LPAREN -> 
                   let args = ctxt "stmt: call args" parse_expr_list ps in 
                   let _ = expect ps SEMI in
                   let bpos = lexpos ps in
                     span apos bpos (Ast.STMT_call (lval, args))

               | EQ -> 
                   bump ps;
                   let e = ctxt "stmt: copy rval" parse_expr ps in 
                   let _ = expect ps SEMI in
                   let bpos = lexpos ps in
                     span apos bpos (Ast.STMT_copy (Ast.COPY_to_lval (lval, e)))

               | LARROW -> 
                   let rhs = ctxt "stmt: recv rhs" parse_lval ps in 
                   let _ = expect ps SEMI in
                   let bpos = lexpos ps in 
                     span apos bpos (Ast.STMT_recv (lval, rhs))

               | SEND -> 
                   let rhs = ctxt "stmt: send rhs" parse_expr ps in 
                   let _ = expect ps SEMI in 
                   let bpos = lexpos ps in 
                     span apos bpos (Ast.STMT_send (lval, rhs))
                       
               | _ -> raise (unexpected ps))
              
      | _ -> raise (unexpected ps)
          
and parse_prog_items term p ps =
  match peek ps with 
      MAIN -> 
        bump ps; 
        let main = ctxt "prog_item: main" parse_stmt ps in 
          (match p.Ast.prog_main with
               None -> parse_prog_items term { p with Ast.prog_main = Some main } ps
             | _ -> raise (err "duplicate main declaration" ps))

    | x -> 
        if x = term
        then (bump ps; p)
        else
          let (ident, item) = ctxt "prog_item: mod item" parse_mod_item ps in 
            ( Hashtbl.add p.Ast.prog_mod ident item;
              parse_prog_items term p ps)
              

              
and parse_prog ps = 
  let prog = { Ast.prog_init = None;
               Ast.prog_main = None;
               Ast.prog_fini = None;
               Ast.prog_mod = Hashtbl.create 0; }
  in
    match peek ps with 
        LBRACE -> 
          bump ps; 
          parse_prog_items RBRACE prog ps
      | _ -> raise (unexpected ps)
        

and parse_sig_and_bind ps = 
  let (input_slot, idents) = 
    match peek ps with 
        NIL -> (Ast.SLOT_interior (Ast.TY_nil), arl [])
      | LPAREN -> 
          let (slots, idents) = 
            ctxt "sig and bind: idents and slots" 
              parse_one_or_more_tup_slots_and_idents ps in
            if Array.length slots = 1
            then (slots.(0), idents)
            else (Ast.SLOT_interior (Ast.TY_tup slots), idents)
      | _ -> raise (unexpected ps)
  in
  let _ = expect ps RARROW in
  let output_slot = ctxt "sig and bind: output" parse_slot ps in
    ({ Ast.sig_input_slot = input_slot;
       Ast.sig_output_slot = output_slot }, idents)


(* parse_fn starts at the first lparen of the sig. *)
and parse_fn proto_opt lim pure ps =
  let (si, bind) = ctxt "fn: sig and bind" parse_sig_and_bind ps in
  let body = ctxt "fn: body" parse_block ps in
    { Ast.fn_ty = { Ast.fn_pure = pure;
                    Ast.fn_proto = proto_opt;
                    Ast.fn_lim = lim; 
                    Ast.fn_sig = si; };
      Ast.fn_bind = bind;
      Ast.fn_body = body; }

and flag ps tok = 
  if peek ps = tok
  then (bump ps; true)
  else false

and parse_lim ps = 
  if flag ps LIM 
  then Ast.LIMITED
  else Ast.UNLIMITED

and parse_ty_param ps =  
  let lim = parse_lim ps in
  let id = parse_ident ps in
    (lim, id)
      
and parse_ty_params ps = 
  match peek ps with 
      LBRACKET -> 
        bracketed_zero_or_more LBRACKET RBRACKET (Some COMMA) parse_ty_param ps
    | _ -> arl []


and parse_mod_item ps = 
  let apos = lexpos ps in  
  let public = flag ps PUB in
  let pure = flag ps PURE in
  let lim = parse_lim ps in 
    
    match peek ps with 
        PROG -> 
          bump ps;
          let ident = ctxt "mod prog item: ident" parse_ident ps in
          let params = ctxt "mod prog item: type params" parse_ty_params ps in
          let prog = ctxt "mod prog item: prog body" parse_prog ps in
          let bpos = lexpos ps in
          let 
              decl = { Ast.decl_params = params;
                       Ast.decl_item =  prog }
          in
            (ident, span apos bpos (Ast.MOD_ITEM_prog decl))
                       
      | FN proto_opt ->
          bump ps;
          let ident = ctxt "mod fn item: ident" parse_ident ps in
          let params = ctxt "mod fn item: type params" parse_ty_params ps in
          let fn = ctxt "mod fn item: fn" (parse_fn proto_opt lim pure) ps in 
          let 
              decl = { Ast.decl_params = params;
                       Ast.decl_item = fn }
          in
          let bpos = lexpos ps in
            (ident, 
             span apos bpos 
               (Ast.MOD_ITEM_fn decl))
              
      | VAL ->     
          bump ps;
          let (slot, ident, init) = ctxt "mod slot" parse_slot_and_ident_and_init ps in
          let bpos = lexpos ps in
            (ident, span apos bpos (Ast.MOD_ITEM_slot (slot, init)))

      | TYPE -> 
          bump ps;
          let ident = ctxt "mod ty item: ident" parse_ident ps in
          let params = ctxt "mod ty item: type params" parse_ty_params ps in
          let _ = expect ps EQ in
          let ty = ctxt "mod type item: ty" parse_ty ps in
          let bpos = lexpos ps in
          let decl = { Ast.decl_params = params;
                       Ast.decl_item = ty; } 
          in
          let item = 
            if public 
            then (Ast.MOD_ITEM_public_type decl)
            else (Ast.MOD_ITEM_opaque_type decl)
          in
            (ident, span apos bpos item)
              
      | _ -> raise (unexpected ps)


let make_parser tok fname = 
  Printf.printf "making parser for: %s\n" fname;
  let lexbuf = Lexing.from_channel (open_in fname) in
  let spos = { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = fname } in
  let cpos = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname } in
    lexbuf.Lexing.lex_start_p <- spos;
    lexbuf.Lexing.lex_curr_p <- cpos;
    let first = tok lexbuf in
      { pstate_peek = first;
        pstate_ctxt = [];
        pstate_lexfun = tok;
        pstate_lexbuf = lexbuf }
;;



let rec parse_crate_entry tok prefix htab ps = 
  expect ps MOD;
  let apos = lexpos ps in
  let name = ctxt "modu: name" parse_ident ps in
  let fname = 
    match peek ps with
        EQ -> 
          bump ps;
          (match peek ps with
               LIT_STR s -> bump ps; s
             | _ -> raise (unexpected ps))
      | _ -> name
  in
  let items = 
    match peek ps with
        SEMI -> 
          bump ps;
          let p = make_parser tok (Filename.concat prefix fname) in
            parse_raw_mod_items p
              
      | RBRACE -> 
          bump ps;
          let subprefix = Filename.concat prefix fname in
          let items = parse_crate_entries tok fname subprefix ps in            
            expect ps LBRACE;
            items
              
      | _ -> raise (unexpected ps)
  in
  let bpos = lexpos ps in
  let item_mod = span apos bpos (Ast.MOD_ITEM_mod { Ast.decl_params = arl [];
                                                    Ast.decl_item = items })
  in
    Hashtbl.add htab name item_mod
      
and parse_raw_mod_items ps = 
  let htab = Hashtbl.create 0 in
    while peek ps != EOF
    do
      let (ident, item) = parse_mod_item ps in
        Hashtbl.add htab ident item
    done;
    expect ps EOF;
    htab
      
      
and parse_crate_entries tok fname prefix ps = 
    let htab = Hashtbl.create 0 in
      while peek ps != EOF
      do 
        parse_crate_entry tok prefix htab ps
      done;
      expect ps EOF;
      htab
;;

let report_error (ps, str) = 
  Printf.printf "Parser error: %s\n" str;
  List.iter 
    (fun (cx,(file,line,col)) -> 
       Printf.printf "%s:%d:%d:E [PARSE CONTEXT] %s\n" file line col cx) 
    ps.pstate_ctxt
;;

let parse_crate tok fname = 
  let ps = make_parser tok fname in
  let htab =  
    try 
      parse_crate_entries tok fname (Filename.dirname fname) ps
    with 
        Parse_err perr -> 
          report_error perr; 
          failwith "parse error"
  in
    htab
;;
