
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
  | COMMA
  | SEMI
  | COLON
  | COLONEQ
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
  | ELSE
  | WHILE
  | FOR
  | FOR_QUES
  | FOR_BANG
  | FOR_STAR
  | FOR_PLUS
  | TRY
  | FAIL
  | INIT
  | MAIN
  | FINI
  | PUT
  | RET
  | BE

  (* Type and type-state keywords *)
  | TYPE
  | PRED
  | CHECK
  | PROVE

  (* Type qualifiers *)
  | PURE

  (* Declarator qualifiers *)
  | PUB
  | AUTO
  | INLINE
  | NATIVE

  (* Magic runtime services *)
  | LOG
  | REFLECT
  | EVAL

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
  | ALT
  | VEC
  | ANY

  (* Callable type constructors *)
  | FUNC
  | FUNC_BANG
  | FUNC_QUES
  | FUNC_STAR
  | FUNC_PLUS

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
    | COMMA      -> ","
    | SEMI       -> ";"
    | COLON      -> ":"
    | COLONEQ    -> ":="
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
    | FOR_QUES   -> "for?"
    | FOR_BANG   -> "for!"
    | FOR_STAR   -> "for*"
    | FOR_PLUS   -> "for+"
    | TRY        -> "try"
    | FAIL       -> "fail"
    | INIT       -> "init"
    | MAIN       -> "main"
    | FINI       -> "fini"
    | PUT        -> "put"
    | RET        -> "ret"
    | BE         -> "be"

    (* Type and type-state keywords *)
    | TYPE       -> "type"
    | PRED       -> "pred"
    | CHECK      -> "check"
    | PROVE      -> "prove"

    (* Type qualifiers *)
    | PURE       -> "pure"

    (* Declarator qualifiers *)
    | PUB        -> "pub"
    | AUTO       -> "auto"
    | INLINE     -> "inline"
    | NATIVE     -> "native"

    (* Magic runtime services *)
    | LOG        -> "log"
    | REFLECT    -> "reflect"
    | EVAL       -> "eval"

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
    | ALT        -> "alt"
    | VEC        -> "vec"
    | ANY        -> "any"

    (* Callable type constructors *)
    | FUNC            -> "func"
    | FUNC_QUES       -> "func?"
    | FUNC_BANG       -> "func!"       
    | FUNC_STAR       -> "func*"
    | FUNC_PLUS       -> "func+"

    | CHAN            -> "chan"
    | PORT            -> "port"

    (* Process/program declarator types *)
    | PROG       -> "prog"
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

let bracketed_seq needOne bra ket sepOpt rule ps =
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
      (match sepOpt with 
           None -> ()
         | Some tok -> 
             if !accum = []
             then () 
             else expect ps tok);    
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


let bracketed_zero_or_more bra ket sepOpt rule ps =
  bracketed_seq false bra ket sepOpt (ctxt "bracketed_seq_nosep" rule) ps
;;


let bracketed_one_or_more bra ket sepOpt rule ps =
  bracketed_seq true bra ket sepOpt (ctxt "bracketed_one_or_more_nosep" rule) ps
;;


let bracketed bra ket rule ps =
  expect ps bra;
  let res = ctxt "bracketed" rule ps in
    expect ps ket;
    res

(* Small parse rules *)


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


let rec parse_name_component ps =
  match peek ps with
      IDENT str -> bump ps; (Ast.COMP_ident str)
    | IDX i -> bump ps; (Ast.COMP_idx i)
    | LBRACKET -> 
        let 
            tys = ctxt "name_component: apply" 
          (bracketed LBRACKET RBRACKET 
             (one_or_more COMMA parse_ty))
          ps
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
    
and parse_colon_constrs ps = 
  match peek ps with
      COLON -> 
        bump ps;
        parse_constrs ps
    | _ -> arr []
        
        
and parse_base_ty ps = 
  match peek ps with 
      TYPE -> 
        bump ps; 
        Ast.TY_type
          
    | BOOL -> 
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

    | PROG -> 
        bump ps;
        Ast.TY_prog

    | NATIVE -> 
        bump ps;
        Ast.TY_native

    | _ -> raise (unexpected ps)


and parse_ty_rest base ps =
  match peek ps with
      COLON -> 
        bump ps;
        let 
            state = ctxt "ty_rest: constrs" parse_constrs ps 
        in
          parse_ty_rest (Ast.TY_constrained (base, state)) ps
            
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


and parse_lval ps =
  let base = (ctxt "lval: base" parse_ident ps) in
  let rest = ctxt "lval: rest" (path DOT parse_lidx) ps in
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


and parse_ATOMIC_expr ps =
  let pos = lexpos ps in
    match peek ps with
        LPAREN -> 
          bump ps;
          let e = parse_expr ps in
            (expect ps RPAREN; e)
              
      | LIT_INT (n,s) -> 
          bump ps;
          Ast.EXPR_literal
            (Ast.LIT_int (n, s), pos)

      | LIT_STR str ->
          bump ps;
          Ast.EXPR_literal 
            (Ast.LIT_str str, pos)

      | LIT_CHAR ch ->
          bump ps;
          Ast.EXPR_literal 
            (Ast.LIT_char ch, pos)
            
      | IDENT _ -> 
          let pos = lexpos ps in
          let lval = parse_lval ps in
            (match peek ps with 
                 LPAREN -> 
                   let args = ctxt "call: args" parse_expr_list ps in
                     Ast.EXPR_call (lval, pos, args)
               | LBRACE -> 
                   let name = name_of_lval ps lval in
                   let inputs = ctxt "rec expr: rec inputs" parse_rec_inputs ps in
                     Ast.EXPR_rec (name, pos, inputs)
               | _ -> Ast.EXPR_lval (lval, pos))
              
      | _ -> raise (unexpected ps)


and name_of_lval ps lval = 
  let extract_nc lidx = 
    match lidx with 
        Ast.LIDX_named (nc, pos) -> nc
      | Ast.LIDX_index _ ->
          raise (Parse_err (ps, "expression-based lval found " ^ 
                              "where static name required"))
  in
    { Ast.name_base = lval.Ast.lval_base;
      Ast.name_rest = Array.map extract_nc lval.Ast.lval_rest }


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

and parse_slot ps = 
  let mode = ctxt "slot: mode" parse_smode ps in
  let ty = ctxt "slot: ty" parse_ty ps in 
  let ident = ctxt "slot: ident" parse_ident ps in
  let constrs = ctxt "slot: constrs" parse_colon_constrs ps in 
    {
      Ast.slot_smode = mode;
      Ast.slot_ty = ty;
      Ast.slot_ident = ident;
      Ast.slot_constrs = constrs;
    }

      
and parse_block ps = 
  let pos = lexpos ps in
  let 
      stmts = ctxt "block: stmts" 
    (bracketed_zero_or_more LBRACE RBRACE None (ctxt "block: stmt" parse_stmt)) ps
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
              
      | PUT -> 
          bump ps;
          let e = 
            match peek ps with
                SEMI -> None
              | _ -> 
                  let expr = ctxt "stmt: put expr" parse_expr ps in 
                    expect ps SEMI;
                    Some expr
          in
            Ast.STMT_put (e, pos)

      | RET -> 
          bump ps;
          let e = ctxt "stmt: return expr" parse_expr ps in 
            expect ps SEMI;
            Ast.STMT_ret (e, pos)
              
      | LBRACE -> ctxt "stmt: block" parse_block ps
          
      | LET 
      | FUNC | FUNC_QUES | FUNC_BANG | FUNC_STAR | FUNC_PLUS
      | PORT | PROG | AUTO | NATIVE | MOD
          -> 
          let decl = ctxt "stmt: decl" parse_decl ps in
            Ast.STMT_decl decl


      | IDENT _ ->
          let lval = ctxt "stmt: lval" parse_lval ps in
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
                       
               | COLONEQ -> 
                   let rhs = ctxt "stmt: move rhs rest" parse_lval ps in 
                     expect ps SEMI;
                     Ast.STMT_move (lval, rhs)
                       
               | _ -> raise (unexpected ps))
              
      | _ -> raise (unexpected ps)
          
and parse_prog_items term p declist ps =
  match peek ps with 
      MAIN -> 
        bump ps; 
        let main = ctxt "prog_item: main" parse_stmt ps in 
          (match p.Ast.prog_main with
               None -> parse_prog_items term { p with Ast.prog_main = Some main } declist ps
             | _ -> raise (err "duplicate main declaration" ps))

    | x -> 
        if x = term
        then 
          (bump ps; {p with Ast.prog_decls = arl declist })
        else 
          let decl = ctxt "prog_item: decl" parse_decl ps in 
            parse_prog_items term p (decl :: declist) ps
              

              
and parse_prog ps = 
  let prog = { Ast.prog_init = None;
               Ast.prog_main = None;
               Ast.prog_fini = None;
               Ast.prog_decls = arr []; }
  in
    match peek ps with 
        LBRACE -> 
          bump ps; 
          parse_prog_items RBRACE prog [] ps
      | _ -> raise (unexpected ps)

and parse_param ps =
  let smode = ctxt "param: smode" parse_smode ps in
  let ty = ctxt "param: ty" parse_ty ps in
  let ident = ctxt "param: ident" parse_ident ps in
    (smode, ty, ident)
      

and parse_sig_and_bind ps = 
  expect ps LPAREN;  
  let (smodes, tys, idents) = 
    match peek ps with
        RPAREN -> (bump ps; (arr [], arr [], arr []))
      | _ -> 
          let (s,t,i) = ctxt "bind: param 0" parse_param ps in
          let smodes = ref [s] in
          let tys = ref [t] in
          let idents = ref [i] in
            while peek ps == COMMA
            do
              bump ps;
              let (s,t,i) = ctxt "bind: param n" parse_param ps in
                smodes := s :: !smodes;
                tys := t :: !tys;
                idents := i :: !idents
            done;
            expect ps RPAREN;
            (arl !smodes, arl !tys, arl !idents)
  in
  let precond = ctxt "bind: precond" parse_colon_constrs ps in

    expect ps RARROW;

    let result_smode = ctxt "bind: result smode" parse_smode ps in
    let result_ty = ctxt "bind: result ty" parse_ty ps in
    let (result_ty, postcond) = 
      match result_ty with 
          Ast.TY_constrained (ty, constrs) -> (ty, constrs)
        | t -> (t, arr [])
    in
      
      ({ Ast.sig_param_smodes = smodes;
         Ast.sig_param_types = tys;
         
         Ast.sig_pre_cond = precond;
         Ast.sig_post_cond = postcond;
         
         Ast.sig_result_smode = result_smode;
         Ast.sig_result_ty = result_ty;
       },
       idents)


(* parse_func starts at the first lparen of the sig. *)
and parse_func native_id_opt proto_opt pure inline ps =
  let (si, bind) = ctxt "func: sig and bind" parse_sig_and_bind ps in
  let body = 
    match native_id_opt with
        None -> Ast.FBODY_stmt (ctxt "func: body" parse_block ps)
      | Some id -> (expect ps SEMI; Ast.FBODY_native id)
  in
    { Ast.func_ty = { Ast.func_pure = pure;
                      Ast.func_inline = inline;
                      Ast.func_proto = proto_opt;
                      Ast.func_sig = si; };
      Ast.func_bind = bind;
      Ast.func_body = body; }

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
        INLINE -> bump ps; true
      | _ -> false
  in    

  let pure = 
    match peek ps with 
        PURE -> bump ps; true
      | _ -> false
  in    

  let do_func proto_opt ps = 
    bump ps;
    if auto 
    then raise (Parse_err (ps, "meaningless 'auto' function"))
    else ();
    let id = parse_ident ps in
      (* FIXME: assignment of native names is broken / ad-hoc *)
    let native_id_opt = if native then Some id else None in
    let func = parse_func native_id_opt proto_opt pure inline ps in 
    let fty = func.Ast.func_ty in
    let fexpr = Ast.EXPR_literal (Ast.LIT_func (fty, func), pos) in
    let slot = { Ast.slot_smode = Ast.SMODE_exterior;
                 Ast.slot_ty = Ast.TY_func fty;
                 Ast.slot_ident = id;
                 Ast.slot_constrs = arl []; }
    in
      Ast.DECL_slot (pos, slot, Some fexpr) 
  in

  let do_slot ps =
    bump ps;
    let pos = lexpos ps in      
    let slot = ctxt "decl: slot" parse_slot ps in
    let init = 
      match peek ps with
          EQ -> 
            bump ps;
            Some (ctxt "decl: slot init" parse_expr ps)
        | _ -> None
    in
      expect ps SEMI;
      Ast.DECL_slot (pos, slot, init)
  in
    
    match peek ps with 
        PROG -> 
          bump ps;
          let id = ctxt "decl: prog ident" parse_ident ps in
          let prog = ctxt "decl: prog body" parse_prog ps in
          let pexpr = Ast.EXPR_literal (Ast.LIT_prog prog, pos) in
          let slot = { Ast.slot_smode = Ast.SMODE_exterior;
                       Ast.slot_ty = Ast.TY_prog;
                       Ast.slot_ident = id;
                       Ast.slot_constrs = arl []; }
          in
            Ast.DECL_slot (pos, slot, Some pexpr) 
              
      | FUNC -> do_func None ps
      | FUNC_QUES -> do_func (Some Ast.PROTO_ques) ps
      | FUNC_BANG -> do_func (Some Ast.PROTO_bang) ps
      | FUNC_STAR -> do_func (Some Ast.PROTO_star) ps
      | FUNC_PLUS -> do_func (Some Ast.PROTO_plus) ps
      | LET -> do_slot ps

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


let rec parse_modu_ents tok prefix ents ps = 
  expect ps MOD;
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
  let m = 
    match peek ps with
        SEMI -> 
          bump ps;
          let p = make_parser tok (Filename.concat prefix fname) in
          let prog = { Ast.prog_init = None;
                       Ast.prog_main = None;
                       Ast.prog_fini = None;
                       Ast.prog_decls = arr []; }
          in
          let prog = parse_prog_items EOF prog [] p in
            Ast.MODU_src { Ast.src_fname = fname;
                           Ast.src_prog = prog; }
      | RBRACE -> 
          bump ps;
          let subprefix = Filename.concat prefix fname in
          let dir = Ast.MODU_dir (parse_modu_dir tok fname subprefix ps) in
            expect ps LBRACE;
            dir
      | _ -> raise (unexpected ps)
  in
    ((name, m) :: ents)
      
and parse_modu_dir tok fname prefix ps = 
  let entlist = parse_modu_ents tok prefix [] ps in
  let ents = Hashtbl.create (List.length entlist) in
    List.iter 
      (fun (name,m) -> Hashtbl.add ents name m) 
      entlist;
    { Ast.dir_fname = fname;
      Ast.dir_ents = ents; }
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
  let dir =  
    try 
      parse_modu_dir tok fname (Filename.dirname fname) ps
    with 
        Parse_err perr -> 
          report_error perr; 
          failwith "parse error"
  in
    dir
;;
