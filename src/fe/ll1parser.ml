
open Common;;

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
  | TILDE
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
  | POUND

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
  | BE of Ast.proto option

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

  (* Value / stmt declarators. *)
  | LET
  | DYN

  (* Magic runtime services *)
  | LOG

  (* Literals *)
  | LIT_INT       of (Big_int.big_int * string)
  | LIT_FLO       of (string)
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
    | TILDE      -> "~"
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
    | POUND      -> "#"

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

    | BE None   -> "be"
    | BE (Some Ast.PROTO_ques)   -> "be?"
    | BE (Some Ast.PROTO_bang)   -> "be!"
    | BE (Some Ast.PROTO_star)   -> "be*"
    | BE (Some Ast.PROTO_plus)   -> "be+"


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

    (* Value / stmt declarators. *)
    | LET        -> "val"
    | DYN        -> "dyn"

    (* Magic runtime services *)
    | LOG        -> "log"

    (* Literals *)
    | LIT_INT (n,s)  -> s
    | LIT_FLO n -> n
    | LIT_STR s  -> ("\"" ^ (String.escaped s) ^ "\"")
    | LIT_CHAR c -> ("'" ^ (Char.escaped c) ^ "'")
    | LIT_BOOL b -> if b then "true" else "false"

    (* Name components *)
    | IDENT s    -> s
    | IDX i      -> ("{" ^ (string_of_int i) ^ "}")

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
    { mutable pstate_peek         : token;
      mutable pstate_ctxt         : (string * pos) list;
      pstate_lexfun       : Lexing.lexbuf -> token;
      pstate_lexbuf       : Lexing.lexbuf;
      pstate_sess         : Session.sess;
      pstate_temp_id   : temp_id ref;
      pstate_node_id   : node_id ref }
;;

let log (ps:pstate) = Session.log "parse"
  ps.pstate_sess.Session.sess_log_parse
  ps.pstate_sess.Session.sess_log_out
;;

exception Parse_err of (pstate * string)
;;


let lexpos ps =
  let p = ps.pstate_lexbuf.Lexing.lex_start_p in
    (p.Lexing.pos_fname,
     p.Lexing.pos_lnum ,
     (p.Lexing.pos_cnum) - (p.Lexing.pos_bol))
;;

let span ps apos bpos x =
  let span = { lo = apos; hi = bpos } in
  let id = !(ps.pstate_node_id) in
    log ps "span for node #%d: %s" (int_of_node id) (Session.string_of_span span);
    ps.pstate_node_id := Node ((int_of_node id)+1);
    htab_put ps.pstate_sess.Session.sess_spans id span;
    { node = x; id = id }
;;

let rec respan ps lval =
  match lval with
      Ast.LVAL_base nb ->
        let s = Hashtbl.find ps.pstate_sess.Session.sess_spans nb.id in
          Ast.LVAL_base (span ps s.lo s.hi nb.node)
    | Ast.LVAL_ext (base, ext) ->
        Ast.LVAL_ext ((respan ps base), ext)
;;

let ctxt (n:string) (f:pstate -> 'a) (ps:pstate) : 'a =
  (ps.pstate_ctxt <- (n, lexpos ps) :: ps.pstate_ctxt;
   let res = f ps in
     ps.pstate_ctxt <- List.tl ps.pstate_ctxt;
     res)
;;


let peek ps =
  begin
    log ps "peeking at: %s     // %s\n%!"
      (string_of_tok ps.pstate_peek)
      (match ps.pstate_ctxt with
           (s, _) :: _ -> s
         | _ -> "<empty>");
    ps.pstate_peek
  end
;;


let bump ps =
  begin
    log ps "bumping past: %s\n%!" (string_of_tok ps.pstate_peek);
    ps.pstate_peek <- ps.pstate_lexfun ps.pstate_lexbuf
  end
;;


let span_bump ps x =
  let apos = lexpos ps in
  let _ = bump ps in
  let bpos = lexpos ps in
    span ps apos bpos x
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

let arj ar = Array.concat (Array.to_list ar)

let arj1st (pairs:(('a array) * 'b) array) : (('a array) * 'b array) =
  let (az, bz) = List.split (Array.to_list pairs) in
    (Array.concat az, Array.of_list bz)

(* Parser combinators *)
let path sep prule ps =
  let accum = ref [] in
    while peek ps == sep
    do
      expect ps sep;
      accum := (ctxt "path" prule ps) :: !accum
    done;
    arl !accum
;;

let one_or_more sep prule ps =
  let accum = ref [prule ps] in
    while peek ps == sep
    do
      bump ps;
      accum := (prule ps) :: !accum
    done;
    arl !accum
;;

let bracketed_seq mandatory bra ket sepOpt prule ps =
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
      accum := (prule ps) :: (!accum)
    done;
    while peek ps != ket
    do
      dosep ();
      accum := (prule ps) :: !accum
    done;
    expect ps ket;
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
      IDENT id ->
        (bump ps;
         match peek ps with
             LBRACKET ->
               let tys =
                 ctxt "name_component: apply"
                   (bracketed_one_or_more LBRACKET RBRACKET (Some COMMA) parse_ty) ps
               in
                 Ast.COMP_app (id, tys)
           | _ -> Ast.COMP_ident id)

    | IDX i ->
        bump ps;
        Ast.COMP_idx i
    | _ -> raise (unexpected ps)

and parse_name_base ps =
  match peek ps with
      IDENT i ->
        (bump ps;
         match peek ps with
             LBRACKET ->
               let tys =
                 ctxt "name_base: apply"
                   (bracketed_one_or_more LBRACKET RBRACKET (Some COMMA) parse_ty) ps
               in
                 Ast.BASE_app (i, tys)
           | _ -> Ast.BASE_ident i)
    | _ -> raise (unexpected ps)

and parse_name ps =
  let base = Ast.NAME_base (parse_name_base ps) in
    match peek ps with
        DOT ->
          bump ps;
          let comps = one_or_more DOT parse_name_component ps in
            Array.fold_left (fun x y -> Ast.NAME_ext (x, y)) base comps
      | _ -> base

and parse_carg_base ps =
  match peek ps with
      STAR -> bump ps; Ast.BASE_formal
    | _ -> Ast.BASE_named (parse_name_base ps)

and parse_carg ps =
  let base = Ast.CARG_base (parse_carg_base ps) in
    match peek ps with
        DOT ->
          bump ps;
          let comps = one_or_more DOT parse_name_component ps in
            Array.fold_left (fun x y -> Ast.CARG_ext (x, y)) base comps
      | _ -> base

and parse_lval_component (ps:pstate)
    : (Ast.stmt array * Ast.lval_component) =
  match peek ps with
      LPAREN ->
        bump ps;
        let (stmts, atom) = ctxt "lval_component: expr" parse_expr ps in
          expect ps RPAREN;
          (stmts, Ast.COMP_atom atom)
    | _ -> ([||], Ast.COMP_named (parse_name_component ps))

and parse_lval (ps:pstate) : (Ast.stmt array * Ast.lval) =
  let apos = lexpos ps in
  let base' = parse_name_base ps in
  let bpos = lexpos ps in
  let base = Ast.LVAL_base (span ps apos bpos base') in
    match peek ps with
        DOT ->
          bump ps;
          let (stmts', comps) = arj1st (one_or_more DOT parse_lval_component ps) in
          let lval = Array.fold_left (fun x y -> Ast.LVAL_ext (x, y)) base comps in
            (stmts', lval)
      | _ ->
          ([||], base)


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

and parse_atomic_ty ps =
  match peek ps with

      BOOL ->
        bump ps;
        Ast.TY_bool

    | INT ->
        bump ps;
        Ast.TY_int

    | CHAR ->
        bump ps;
        Ast.TY_char

    | STR ->
        bump ps;
        Ast.TY_str

    | NIL ->
        bump ps;
        Ast.TY_nil

    | ANY ->
        bump ps;
        Ast.TY_any

    | CHAN ->
        Ast.TY_chan (bracketed LBRACE RBRACE parse_ty ps)

    | PORT ->
        Ast.TY_port (bracketed LBRACE RBRACE parse_ty ps)

    | VEC ->
        Ast.TY_vec (bracketed LBRACE RBRACE parse_ty ps)

    | LIM ->
        bump ps;
        Ast.TY_lim (parse_atomic_ty ps)

    | IDENT _ -> Ast.TY_named (parse_name ps)


    | TAG ->
        bump ps;
        let htab = Hashtbl.create 4 in
        let parse_tag_entry ps =
          let ident = parse_ident ps in
          let ty =
            match peek ps with
                LPAREN -> bracketed LPAREN RPAREN parse_ty ps
              | NIL -> Ast.TY_nil
              | _ -> Ast.TY_nil
          in
            htab_put htab ident ty
        in
        let _ = one_or_more OR parse_tag_entry ps in
          Ast.TY_tag htab

    | LBRACE ->
        let htab = Hashtbl.create 4 in
        let parse_rec_entry ps =
          let (slot, ident) = parse_slot_and_ident false ps in
            htab_put htab ident slot
        in
        let _ = bracketed_zero_or_more LBRACE RBRACE None parse_rec_entry ps in
          Ast.TY_rec htab

    | LPAREN -> bracketed LPAREN RPAREN parse_ty ps


    | _ -> raise (unexpected ps)


and parse_slot param_slot ps =
  match (peek ps, param_slot) with
      (AT, _) ->
        bump ps;
        let ty = parse_ty ps in
          { Ast.slot_mode = Ast.MODE_exterior;
            Ast.slot_ty = Some ty }

    | (CARET, true) ->
        bump ps;
        let ty = parse_ty ps in
          { Ast.slot_mode = Ast.MODE_write_alias;
            Ast.slot_ty = Some ty }

    | (TILDE, true) ->
        bump ps;
        let ty = parse_ty ps in
          { Ast.slot_mode = Ast.MODE_read_alias;
            Ast.slot_ty = Some ty }

    | _ ->
        let ty = parse_ty ps in
          { Ast.slot_mode = Ast.MODE_interior;
            Ast.slot_ty = Some ty }


and parse_identified_slot param_slot ps =
  let apos = lexpos ps in
  let slot = parse_slot param_slot ps in
  let bpos = lexpos ps in
    span ps apos bpos slot

and parse_tuple_ty ps =
  one_or_more COMMA parse_ty ps

and parse_constrained_ty ps =
  let base = ctxt "ty: base" parse_atomic_ty ps in
    match peek ps with
        COLON ->
          bump ps;
          let constrs = ctxt "ty: constrs" parse_constrs ps in
            Ast.TY_constrained (base, constrs)

      | _ -> base

and parse_ty ps =
  parse_constrained_ty ps


and parse_rec_input htab ps =
  let lab = (ctxt "rec input: label" parse_ident ps) in
    match peek ps with
        EQ ->
          bump ps;
          let (stmts, expr) = (ctxt "rec input: expr" parse_expr ps) in
            htab_put htab lab expr;
            stmts
      | _ -> raise (unexpected ps)


and parse_rec_inputs ps =
  let htab = Hashtbl.create 4 in
  let stmts_s = bracketed_zero_or_more LBRACE RBRACE (Some COMMA)
    (ctxt "rec inputs" (parse_rec_input htab)) ps
  in
    (arj stmts_s, htab)


and parse_expr_list bra ket ps =
  arj1st (bracketed_zero_or_more bra ket (Some COMMA)
            (ctxt "expr list" parse_expr) ps)

and slot_auto = { Ast.slot_mode = Ast.MODE_interior;
                  Ast.slot_ty = None }

and build_tmp ps slot apos bpos =
  let nonce = !(ps.pstate_temp_id) in
    ps.pstate_temp_id := Temp ((int_of_temp nonce)+1);
    if ps.pstate_sess.Session.sess_log_parse
    then
      Printf.fprintf ps.pstate_sess.Session.sess_log_out "building temporary %d\n%!" (int_of_temp nonce)
    else
      ();
    let decl = Ast.DECL_slot (Ast.KEY_temp nonce, (span ps apos bpos slot)) in
    let declstmt = span ps apos bpos (Ast.STMT_decl decl) in
    let tmp = Ast.LVAL_base (span ps apos bpos (Ast.BASE_temp nonce)) in
      (nonce, tmp, declstmt)


and span_bump_lit ps ty lit =
  let apos = lexpos ps in
  let _ = bump ps in
  let bpos = lexpos ps in
    Ast.ATOM_literal (span ps apos bpos lit)


and parse_atom ps =
  match peek ps with
      LIT_INT (n,s) -> span_bump_lit ps Ast.TY_int (Ast.LIT_int (n, s))
    | LIT_STR str -> span_bump_lit ps Ast.TY_str (Ast.LIT_str str)
    | LIT_CHAR ch -> span_bump_lit ps Ast.TY_char (Ast.LIT_char ch)
    | _ -> raise (unexpected ps)


and parse_bottom_expr ps =
  match peek ps with
      LPAREN ->
        let _ = bump ps in
        let (stmts, atom) = ctxt "paren expr" parse_expr ps in
        let _ = expect ps RPAREN in
          (stmts, atom)

    | LBRACE ->
        let apos = lexpos ps in
        let (stmts, htab) = ctxt "rec expr: rec inputs" parse_rec_inputs ps in
        let bpos = lexpos ps in
        let (_, tmp, decl) = build_tmp ps slot_auto apos bpos in
        let stmt = span ps apos bpos (Ast.STMT_copy (tmp, Ast.EXPR_rec htab)) in
          (Array.append stmts [| decl; stmt |], Ast.ATOM_lval (respan ps tmp))

    | LBRACKET ->
        let apos = lexpos ps in
        let (stmts, lvals) = ctxt "vec expr: exprs" (parse_expr_list LBRACKET RBRACKET) ps in
        let bpos = lexpos ps in
        let (_, tmp, decl) = build_tmp ps slot_auto apos bpos in
        let stmt = span ps apos bpos (Ast.STMT_copy (tmp, Ast.EXPR_vec lvals)) in
          (Array.append stmts [| decl; stmt |], Ast.ATOM_lval (respan ps tmp))

    | IDENT _ ->
        let apos = lexpos ps in
        let (lstmts, lval) = parse_lval ps in
          (match peek ps with

               LPAREN ->
                 let (astmts, args) = ctxt "call: args" (parse_expr_list LPAREN RPAREN) ps in
                 let bpos = lexpos ps in
                 let (nonce, tmp, tempdecl) = build_tmp ps slot_auto apos bpos in
                 let call = span ps apos bpos (Ast.STMT_call (tmp, lval, args)) in
                 let cstmts = [| tempdecl; call |] in
                 let stmts = Array.concat [lstmts; astmts; cstmts] in
                   (stmts, Ast.ATOM_lval (respan ps tmp))

             | _ ->
                 (lstmts, Ast.ATOM_lval lval))


    | _ -> ([| |], parse_atom ps)


and parse_negation_expr ps =
  match peek ps with
      NOT ->
        let apos = lexpos ps in
          bump ps;
          let (stmts, atom) = ctxt "negation expr" parse_negation_expr ps in
          let bpos = lexpos ps in
          let (_, tmp, decl) = build_tmp ps slot_auto apos bpos in
          let copy = span ps apos bpos (Ast.STMT_copy (tmp, Ast.EXPR_unary (Ast.UNOP_not, atom))) in
          let stmts = Array.append stmts [| decl; copy |] in
            (stmts, Ast.ATOM_lval (respan ps tmp))
    | _ -> parse_bottom_expr ps


(* Binops are all left-associative,                *)
(* so we factor out some of the parsing code here. *)
and binop_rhs ps name lhs rhs_parse_fn op =
  bump ps;
  let (lstmts, l_atom) = lhs in
  let apos = lexpos ps in
  let (rstmts, r_atom) = (ctxt (name ^ " rhs") rhs_parse_fn ps) in
  let bpos = lexpos ps in
  let (_, tmp, decl) = build_tmp ps slot_auto apos bpos in
  let copy = span ps apos bpos (Ast.STMT_copy (tmp, Ast.EXPR_binary (op, l_atom, r_atom))) in
  let stmts = Array.concat [lstmts; rstmts; [| decl; copy |] ] in
    (stmts, Ast.ATOM_lval (respan ps tmp))

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

and parse_expr (ps:pstate) : (Ast.stmt array * Ast.atom)  =
  ctxt "expr" parse_or_expr ps

and parse_slot_and_ident param_slot ps =
  let slot = ctxt "slot and ident: slot" (parse_slot param_slot) ps in
  let ident = ctxt "slot and ident: ident" parse_ident ps in
    (slot, ident)

and parse_identified_slot_and_ident param_slot ps =
  let slot = ctxt "identified slot and ident: slot" (parse_identified_slot param_slot) ps in
  let ident = ctxt "identified slot and ident: ident" parse_ident ps in
    (slot, ident)

and parse_two_or_more_identified_tup_slots_and_idents param_slot ps =
  let both =
    ctxt "two+ tup slots and idents"
      (bracketed_two_or_more LPAREN RPAREN (Some COMMA) (parse_identified_slot_and_ident param_slot)) ps
  in
  let (slots, idents) = List.split (Array.to_list both) in
    (arr slots, arr idents)

and parse_one_or_more_identified_slot_ident_pairs param_slot ps =
  ctxt "one+ tup slots and idents"
    (bracketed_one_or_more LPAREN RPAREN (Some COMMA) (parse_identified_slot_and_ident param_slot)) ps

and parse_block ps =
  let apos = lexpos ps in
  let stmts = arj (ctxt "block: stmts"
                     (bracketed_zero_or_more LBRACE RBRACE None parse_stmts) ps)
  in
  let bpos = lexpos ps in
    span ps apos bpos stmts

and parse_block_stmt ps =
  let apos = lexpos ps in
  let block = parse_block ps in
  let bpos = lexpos ps in
    span ps apos bpos (Ast.STMT_block block)

and parse_init ps =
  let init =
    match peek ps with
        EQ ->
          bump ps;
          let (stmts, atom) = (ctxt "init: expr" parse_expr ps) in
            (stmts, Some atom)
      | _ -> (arr [], None)
  in
  let _ = expect ps SEMI in
    init

and parse_slot_and_ident_and_init ps =
  let (slot, ident) =
    ctxt "slot, ident and init: slot and ident"
      (parse_slot_and_ident false) ps
  in
  let (stmts, init) = ctxt "slot, ident and init: init" parse_init ps in
    (stmts, slot, ident, init)

(*
 * We have no way to parse a single Ast.stmt; any incoming syntactic statement
 * may desugar to N>1 real Ast.stmts
 *)
and parse_stmts ps =
  let spans stmts apos bpos stmt =
    Array.append stmts [| (span ps apos bpos stmt) |]
  in
  let apos = lexpos ps in
    match peek ps with

        LOG ->
          bump ps;
          let (stmts, atom) = ctxt "stmts: log value" parse_expr ps in
            expect ps SEMI;
            let bpos = lexpos ps in
              spans stmts apos bpos (Ast.STMT_log atom)

      | IF ->
          bump ps;
          let (stmts, atom) = ctxt "stmts: if cond" (bracketed LPAREN RPAREN parse_expr) ps in
          let then_block = ctxt "stmts: if-then" parse_block ps in
          let else_block =
            (match peek ps with
                 ELSE ->
                   bump ps;
                   Some (ctxt "stmts: if-else" parse_block ps)
               | _ -> None)
          in
          let bpos = lexpos ps in
            spans stmts apos bpos
              (Ast.STMT_if
                 { Ast.if_test = atom;
                   Ast.if_then = then_block;
                   Ast.if_else = else_block; })

      | WHILE ->
          bump ps;
          let (stmts, test) = ctxt "stmts: while cond" (bracketed LPAREN RPAREN parse_expr) ps in
          let body_block = ctxt "stmts: while body" parse_block ps in
          let bpos = lexpos ps in
            spans stmts apos bpos
              (Ast.STMT_while
                 { Ast.while_lval = (stmts, test);
                   Ast.while_body = body_block; })

      | PUT proto ->
          bump ps;
          let (stmts, e) =
            match peek ps with
                SEMI -> (arr [], None)
              | _ ->
                  let (stmts, expr) = ctxt "stmts: put expr" parse_expr ps in
                    expect ps SEMI;
                    (stmts, Some expr)
          in
          let bpos = lexpos ps in
            spans stmts apos bpos (Ast.STMT_put (proto, e))

      | RET proto ->
          bump ps;
          let (stmts, e) =
            match peek ps with
                SEMI -> (bump ps; (arr [], None))
              | _ ->
                  let (stmts, expr) = ctxt "stmts: ret expr" parse_expr ps in
                    expect ps SEMI;
                    (stmts, Some expr)
          in
          let bpos = lexpos ps in
            spans stmts apos bpos (Ast.STMT_ret (proto, e))

      | BE proto ->
          bump ps;
          let (lstmts, lval) = ctxt "be: lval" parse_lval ps in
          let (astmts, args) = ctxt "be: args" (parse_expr_list LPAREN RPAREN) ps in
          let bpos = lexpos ps in
          let be = span ps apos bpos (Ast.STMT_be (proto, lval, args)) in
          Array.concat [ lstmts; astmts; [| be |] ]

      | LBRACE -> [| ctxt "stmts: block" parse_block_stmt ps |]

      | LET ->
          bump ps;
          (match peek ps with
               LPAREN ->
                 let (slots, idents) =
                   ctxt "stmt tup decl: slots and idents"
                     (bracketed LPAREN RPAREN (parse_two_or_more_identified_tup_slots_and_idents false)) ps in
                 let (stmts, init) = ctxt "stmt tup decl: init" parse_init ps in
                 let bpos = lexpos ps in
                   (*
                    * A little destructuring assignment sugar:
                    *
                    *   let (int a, int b) = foo();
                    *
                    * desugars to:
                    *
                    *   temp (int, int) t_n = foo();
                    *   let int a = t_n.{0};
                    *   let int b = t_n.{1};
                    *
                    *)
                 let (nonce, tmp, tempdecl) =
                   build_tmp ps
                     { Ast.slot_mode = Ast.MODE_interior;
                       Ast.slot_ty = Some (Ast.TY_tup (Array.map (fun x -> x.node) slots)) }
                     apos bpos
                 in

                 let copies = ref [] in

                 let makedecl i slot =
                   (match init with
                        None -> ()
                      | Some _ ->
                          let ext = Ast.COMP_named (Ast.COMP_idx i) in
                          let src_lval = Ast.LVAL_ext ((respan ps tmp), ext) in
                          let src_atom = Ast.ATOM_lval src_lval in
                          let dst_lval = Ast.LVAL_base (span ps apos bpos (Ast.BASE_ident idents.(i))) in
                          let copy = span ps apos bpos (Ast.STMT_copy (dst_lval, Ast.EXPR_atom src_atom)) in
                            copies := copy :: (!copies));
                   let decl = Ast.DECL_slot (Ast.KEY_ident idents.(i), slot) in
                     span ps apos bpos (Ast.STMT_decl decl)
                 in
                 let letdecls = Array.mapi makedecl slots in
                   Array.concat [stmts; [| tempdecl |]; letdecls; arl (!copies)]

             | _ ->
                 let (stmts, slot, ident, init) =
                   ctxt "stmt slot" parse_slot_and_ident_and_init ps in
                 let bpos = lexpos ps in
                 let decl = Ast.DECL_slot (Ast.KEY_ident ident,
                                           (span ps apos bpos slot))
                 in
                 let copy = match init with
                     None -> [| |]
                   | Some atom -> [| span ps apos bpos
                                       (Ast.STMT_copy
                                          (Ast.LVAL_base
                                             (span ps apos bpos (Ast.BASE_ident ident)),
                                           Ast.EXPR_atom atom)) |]
                 in
                   Array.concat [stmts; [| span ps apos bpos (Ast.STMT_decl decl) |]; copy])



      | LIM | PORT | PROG | MOD | (FN _) ->
          let (ident, stmts, item) = ctxt "stmt: decl" parse_mod_item ps in
          let bpos = lexpos ps in
          let decl = Ast.DECL_mod_item (ident, item) in
            spans stmts apos bpos (Ast.STMT_decl decl)

      | LPAREN ->
          let (lstmts, lvals) =
            arj1st (ctxt "stmt: paren_copy_to_tup tup"
                      (bracketed_one_or_more LPAREN RPAREN (Some COMMA) parse_lval) ps)
          in
          let _ = expect ps EQ in
          let (estmts, atom) = ctxt "stmt: paren_copy_to_tup rval" parse_expr ps in
          let _ = expect ps SEMI in
          let stmts = Array.append lstmts estmts in
          let bpos = lexpos ps in
            (*
             * A little destructuring assignment sugar:
             *
             *   (a, b) = foo();
             *
             * desugars to:
             *
             *   temp auto t_n = foo();
             *   a = t_n.{0};
             *   b = t_n.{1};
             *
             *)

          let (nonce, tmp, tempdecl) =
            build_tmp ps slot_auto apos bpos in
          let copy = span ps apos bpos (Ast.STMT_copy (tmp, Ast.EXPR_atom atom)) in
          let make_copy i dst =
            let ext = Ast.COMP_named (Ast.COMP_idx i) in
            let lval = Ast.LVAL_ext ((respan ps tmp), ext) in
            let e = Ast.EXPR_atom (Ast.ATOM_lval lval) in
              span ps apos bpos (Ast.STMT_copy (dst, e))
          in
            let copies = Array.mapi make_copy lvals in
              Array.concat [ stmts; [| tempdecl; copy |]; copies ]

      | IDENT _ ->
          let (lstmts, lval) = ctxt "stmt: lval" parse_lval ps in
            (match peek ps with

                 LPAREN ->
                   let (astmts, args) = ctxt "stmt: call args" (parse_expr_list LPAREN RPAREN) ps in
                   let _ = expect ps SEMI in
                   let stmts = Array.append lstmts astmts in
                   let bpos = lexpos ps in
                   let (nonce, tmp, tempdecl) = build_tmp ps slot_auto apos bpos in
                   let call = span ps apos bpos (Ast.STMT_call ((respan ps tmp), lval, args)) in
                     Array.append stmts [| tempdecl; call |]

               | EQ ->
                   bump ps;
                   let (stmts, atom) = ctxt "stmt: copy rval" parse_expr ps in
                   let _ = expect ps SEMI in
                   let bpos = lexpos ps in
                     spans stmts apos bpos (Ast.STMT_copy (lval, Ast.EXPR_atom atom))

               | LARROW ->
                   let (stmts, rhs) = ctxt "stmt: recv rhs" parse_lval ps in
                   let _ = expect ps SEMI in
                   let bpos = lexpos ps in
                     spans stmts apos bpos (Ast.STMT_recv (lval, rhs))

               | SEND ->
                   let (stmts, rhs) = ctxt "stmt: send rhs" parse_expr ps in
                   let _ = expect ps SEMI in
                   let bpos = lexpos ps in
                     spans stmts apos bpos (Ast.STMT_send (lval, rhs))

               | _ -> raise (unexpected ps))

      | _ -> raise (unexpected ps)

and parse_prog_item prog_cell stmts_cell ps =
  match peek ps with
      MAIN ->
        bump ps;
        let main = ctxt "prog_item: main" parse_block ps in
          prog_cell := { (!prog_cell) with Ast.prog_main = Some main }

    | FINI ->
        bump ps;
        let fini = ctxt "prog_item: fini" parse_block ps in
          prog_cell := { (!prog_cell) with Ast.prog_fini = Some fini }

    | _ ->
        let (ident, stmts, item) = ctxt "prog_item: mod item" parse_mod_item ps in
          htab_put (!prog_cell).Ast.prog_mod ident item;
          stmts_cell := stmts :: (!stmts_cell)

and parse_prog ps =
  let prog = { Ast.prog_init = None;
               Ast.prog_main = None;
               Ast.prog_fini = None;
               Ast.prog_mod = Hashtbl.create 4; }
  in
  let prog_cell = ref prog in
  let stmts_cell = ref [] in
  let _ = ctxt "prog" (bracketed_zero_or_more LBRACE RBRACE None
                         (parse_prog_item prog_cell stmts_cell)) ps
  in
    (Array.concat (List.rev !stmts_cell), !prog_cell)


and parse_fn_in_and_out ps =
  let inputs =
    match peek ps with
        NIL -> (bump ps; [| |])
      | LPAREN ->
          ctxt "fn in and out: input idents and slots"
            (parse_one_or_more_identified_slot_ident_pairs true) ps
      | _ -> raise (unexpected ps)
  in
  let _ = expect ps RARROW in
  let output = ctxt "fn in and out: output slot" (parse_identified_slot true) ps in
    (inputs, output)


(* parse_fn starts at the first lparen of the sig. *)
and parse_fn proto_opt lim pure ps =
    let (inputs, output) = ctxt "fn: fn_in_and_out" parse_fn_in_and_out ps in
    let body = ctxt "fn: body" parse_block ps in
      { Ast.fn_input_slots = inputs;
        Ast.fn_output_slot = output;
        Ast.fn_aux = { Ast.fn_pure = pure;
                       Ast.fn_proto = proto_opt;
                       Ast.fn_lim = lim; };
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
    | _ -> arr []


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
          let (stmts, prog) = ctxt "mod prog item: prog body" parse_prog ps in
          let bpos = lexpos ps in
          let
              decl = { Ast.decl_params = params;
                       Ast.decl_item =  prog }
          in
            (ident, stmts, span ps apos bpos (Ast.MOD_ITEM_prog decl))

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
            (ident, arr [],
             span ps apos bpos
               (Ast.MOD_ITEM_fn decl))

      | TYPE ->
          bump ps;
          let ident = ctxt "mod ty item: ident" parse_ident ps in
          let params = ctxt "mod ty item: type params" parse_ty_params ps in
          let _ = expect ps EQ in
          let ty = ctxt "mod type item: ty" parse_ty ps in
          let _ = expect ps SEMI in
          let bpos = lexpos ps in
          let decl = { Ast.decl_params = params;
                       Ast.decl_item = ty; }
          in
          let item =
            if public
            then (Ast.MOD_ITEM_public_type decl)
            else (Ast.MOD_ITEM_opaque_type decl)
          in
            (ident, arr [], span ps apos bpos item)

      | _ -> raise (unexpected ps)


and make_parser tref nref sess tok fname =
  let lexbuf = Lexing.from_channel (open_in fname) in
  let spos = { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = fname } in
  let cpos = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname } in
    lexbuf.Lexing.lex_start_p <- spos;
    lexbuf.Lexing.lex_curr_p <- cpos;
    let first = tok lexbuf in
    let ps =
      { pstate_peek = first;
        pstate_ctxt = [];
        pstate_lexfun = tok;
        pstate_lexbuf = lexbuf;
        pstate_sess = sess;
        pstate_temp_id = tref;
        pstate_node_id = nref }
    in
      log ps "made parser for: %s\n%!" fname;
      ps

and parse_crate_mod_entry
    tok
    (prefix:string)
    (mod_items:Ast.mod_items)
    (ps:pstate)
    : unit =
  expect ps MOD;
  let apos = lexpos ps in
  let name = ctxt "mod: name" parse_ident ps in
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
          let fname = Filename.concat prefix fname in
          let p =
            make_parser
              ps.pstate_temp_id
              ps.pstate_node_id
              ps.pstate_sess tok
              fname
          in
            parse_raw_mod_items p

      | RBRACE ->
          bump ps;
          let subprefix = Filename.concat prefix fname in
          let items = parse_crate_mod_entries tok fname subprefix ps LBRACE in
            items
      | _ -> raise (unexpected ps)
  in
  let bpos = lexpos ps in
  let item_mod =
    (* FIXME: permit type-parametric top-level modules. *)
    span ps apos bpos (Ast.MOD_ITEM_mod { Ast.decl_params = arr [];
                                          Ast.decl_item = items })
  in
    htab_put mod_items name item_mod

and parse_raw_mod_items
    (ps:pstate)
    : Ast.mod_items =
  let items = Hashtbl.create 4 in
    while peek ps != EOF
    do
      let (ident, stmts, item) = parse_mod_item ps in
        if Array.length stmts != 0
        then raise (Parse_err (ps, "top-level module cannot contain implicit statements"));
        htab_put items ident item
    done;
    expect ps EOF;
    items

and parse_crate_mod_entries
    tok
    (fname:string)
    (prefix:string)
    (ps:pstate)
    (endtok:token)
    : Ast.mod_items =
  let items = Hashtbl.create 4 in
    while peek ps != endtok
    do
      parse_crate_mod_entry tok prefix items ps
    done;
    expect ps endtok;
    items

and parse_root_crate_entries
    tok
    (fname:string)
    (prefix:string)
    (ps:pstate)
    : Ast.crate =
  let items = Hashtbl.create 4 in
  let explicit_main = ref None in
    while peek ps != EOF
    do
      match peek ps with
          MAIN ->
            begin
              bump ps;
              expect ps EQ;
              let name = parse_name ps in
                expect ps SEMI;
                explicit_main := Some name
            end
        | MOD ->
            parse_crate_mod_entry tok prefix items ps
        | _ -> raise (unexpected ps)
    done;
    expect ps EOF;
    let main =
      match !explicit_main with
          None -> infer_main_prog ps items
        | Some m -> m
    in
      { Ast.crate_items = items;
        Ast.crate_main = main }

and infer_main_prog
    (ps:pstate)
    (crate_items:Ast.mod_items)
    : Ast.name =
  let progs = ref [] in
  let rec dig prefix_name items =
    Hashtbl.iter (extract_prog prefix_name) items
  and extract_prog prefix_name ident item =
    match item.node with
        Ast.MOD_ITEM_mod md ->
          if Array.length md.Ast.decl_params = 0
          then dig prefix_name md.Ast.decl_item
          else ()
      | Ast.MOD_ITEM_prog pd ->
          if Array.length pd.Ast.decl_params = 0
          then
            begin
              let name =
                match prefix_name with
                    None -> Ast.NAME_base (Ast.BASE_ident ident)
                  | Some n -> Ast.NAME_ext (n, Ast.COMP_ident ident)
              in
                progs := name :: (!progs)
            end
          else ()
      | _ -> ()
  in
    dig None crate_items;
    match !progs with
        [] -> raise (err "cannot infer main program: no programs found" ps)
      | [x] -> x
      | _ -> raise (err "cannot infer main program: multiple programs found" ps)
;;

let parse_crate
    (sess:Session.sess)
    tok
    : Ast.crate =
  try
    let fname = sess.Session.sess_crate in
    let tref = ref (Temp 0) in
    let nref = ref (Node 0) in
    let ps = make_parser tref nref sess tok fname in
      parse_root_crate_entries tok fname (Filename.dirname fname) ps
  with
      Parse_err (ps, str) ->
        Session.fail sess "Parse error: %s\n%!" str;
        List.iter
          (fun (cx,pos) ->
             Session.fail sess "%s:E (parse context): %s\n%!"
               (Session.string_of_pos pos) cx)
          ps.pstate_ctxt;
        { Ast.crate_items = Hashtbl.create 0;
          Ast.crate_main = Ast.NAME_base (Ast.BASE_ident "none") }
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
