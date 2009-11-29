
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
  | NATIVE

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
  | MUTABLE

  (* Module-item qualifiers *)
  | PUB

  (* Value / stmt declarators. *)
  | LET

  (* Magic runtime services *)
  | LOG
  | SPAWN

  (* Literals *)
  | LIT_INT       of (Big_int.big_int * string)
  | LIT_FLO       of string
  | LIT_STR       of string
  | LIT_CHAR      of char
  | LIT_BOOL      of bool

  (* Name components *)
  | IDENT         of string
  | IDX           of int

  (* Reserved type names *)
  | NIL
  | BOOL
  | INT
  | CHAR
  | STR
  | MACH          of ty_mach

  (* Algebraic type constructors *)
  | REC
  | TAG
  | VEC
  | ANY

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
    | NATIVE     -> "native"

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
    | MUTABLE    -> "mutable"

    (* Declarator qualifiers *)
    | PUB        -> "pub"

    (* Value / stmt declarators. *)
    | LET        -> "let"

    (* Magic runtime services *)
    | LOG        -> "log"
    | SPAWN      -> "spawn"

    (* Literals *)
    | LIT_INT (n,s)  -> s
    | LIT_FLO n -> n
    | LIT_STR s  -> ("\"" ^ (String.escaped s) ^ "\"")
    | LIT_CHAR c -> ("'" ^ (Char.escaped c) ^ "'")
    | LIT_BOOL b -> if b then "true" else "false"

    (* Name components *)
    | IDENT s    -> s
    | IDX i      -> ("_" ^ (string_of_int i))

    (* Reserved type names *)
    | NIL        -> "nil"
    | BOOL       -> "bool"
    | INT        -> "int"
    | CHAR       -> "char"
    | STR        -> "str"
    | MACH m     -> string_of_ty_mach m

    (* Algebraic type constructors *)
    | REC        -> "rec"
    | TAG        -> "tag"
    | VEC        -> "vec"
    | ANY        -> "any"

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
    | PROC         -> "proc"

    | EOF        -> "<EOF>"
;;

(* 
 * NB: pexps (parser-expressions) are only used transiently during
 * parsing and syntax-expansion.  They're desugared into the general
 * AST. Expressions that can show up in source correspond to this loose
 * grammar and have a wide-ish flexibility in *theoretical* composition;
 * only subsets of those compositions are legal in various AST contexts.
 * 
 * Desugaring on the fly is unfortunately complicated enough to require
 * -- or at least "make much more convenient" -- this two-pass routine. 
 *)

type pexp' =
    PEXP_call of (pexp * pexp array)
  | PEXP_spawn of pexp
  | PEXP_rec of ((Ast.ident * pexp) array)
  | PEXP_tup of (pexp array)
  | PEXP_vec of (Ast.slot * (pexp array))
  | PEXP_port
  | PEXP_chan of (pexp option)
  | PEXP_binop of (Ast.binop * pexp * pexp)
  | PEXP_unop of (Ast.unop * pexp)
  | PEXP_ident of Ast.ident
  | PEXP_app of (Ast.ident * (Ast.ty array))
  | PEXP_ext_name of (pexp * Ast.name_component)
  | PEXP_ext_pexp of (pexp * pexp)
  | PEXP_lit of Ast.lit
  | PEXP_str of string
  | PEXP_mutable of pexp
  | PEXP_exterior of pexp

and pexp = pexp' identified
;;

(* 
 * Desugarings depend on context:
 * 
 *   - If a pexp is used on the RHS of an assignment, it's turned into
 *     an initialization statement such as STMT_init_rec or such. This
 *     removes the possibility of initializing into a temp only to
 *     copy out. If the topmost pexp in such a desugaring is an atom,
 *     unop or binop, of course, it will still just emit a STMT_copy
 *     on a primitive expression.
 * 
 *   - If a pexp is used in the context where an atom is required, a 
 *     statement declaring a temporary and initializing it with the 
 *     result of the pexp is prepended, and the temporary atom is used.
 *)

(* Fundamental parser types and actions *)


type pstate =
    { mutable pstate_peek         : token;
      mutable pstate_ctxt         : (string * pos) list;
      pstate_lexfun       : Lexing.lexbuf -> token;
      pstate_lexbuf       : Lexing.lexbuf;
      pstate_file         : filename;
      pstate_sess         : Session.sess;
      pstate_temp_id   : temp_id ref;
      pstate_node_id   : node_id ref }
;;

let log (ps:pstate) = Session.log "parse"
  ps.pstate_sess.Session.sess_log_parse
  ps.pstate_sess.Session.sess_log_out
;;

let iflog ps thunk =
  if ps.pstate_sess.Session.sess_log_parse
  then thunk ()
  else ()
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
    iflog ps (fun _ -> log ps "span for node #%d: %s"
                (int_of_node id) (Session.string_of_span span));
    ps.pstate_node_id := Node ((int_of_node id)+1);
    htab_put ps.pstate_sess.Session.sess_spans id span;
    { node = x; id = id }
;;

(* The point of this is to make a new node_id entry for a node that is a "copy" of
   an lval returned from somewhere else. For example if you create a temp, the lval
   it returns can only be used in *one* place, for the node_id denotes the place that
   lval is first used; subsequent uses of 'the same' reference must clone_lval it 
   into a new node_id. Otherwise there is trouble. *)

let clone_span ps oldnode newthing =
  let s = Hashtbl.find ps.pstate_sess.Session.sess_spans oldnode.id in
    span ps s.lo s.hi newthing
;;


let rec clone_lval ps lval =
  match lval with
      Ast.LVAL_base nb ->
        let nnb = clone_span ps nb nb.node in
          Ast.LVAL_base nnb
    | Ast.LVAL_ext (base, ext) ->
        Ast.LVAL_ext ((clone_lval ps base), ext)
;;

let clone_atom ps atom =
  match atom with
      Ast.ATOM_literal _ -> atom
    | Ast.ATOM_lval lv -> Ast.ATOM_lval (clone_lval ps lv)
;;

let ctxt (n:string) (f:pstate -> 'a) (ps:pstate) : 'a =
  (ps.pstate_ctxt <- (n, lexpos ps) :: ps.pstate_ctxt;
   let res = f ps in
     ps.pstate_ctxt <- List.tl ps.pstate_ctxt;
     res)
;;


let peek ps =
  begin
    iflog ps (fun _ -> log ps "peeking at: %s     // %s"
                (string_of_tok ps.pstate_peek)
                (match ps.pstate_ctxt with
                     (s, _) :: _ -> s
                   | _ -> "<empty>"));
    ps.pstate_peek
  end
;;


let bump ps =
  begin
    iflog ps (fun _ -> log ps "bumping past: %s" (string_of_tok ps.pstate_peek));
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
    (* Decay IDX tokens to identifiers if they occur ousdide name paths. *)
    | IDX i -> (bump ps; string_of_tok (IDX i))
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
  match peek ps with
      IDENT _ ->
        begin
          let base = Ast.CARG_base (parse_carg_base ps) in
          let path =
            match peek ps with
                DOT ->
                  bump ps;
                  let comps = one_or_more DOT parse_name_component ps in
                    Array.fold_left (fun x y -> Ast.CARG_ext (x, y)) base comps
              | _ -> base
          in
            Ast.CARG_path path
        end
    | _ ->
        Ast.CARG_lit (parse_lit ps)

and parse_lval (ps:pstate) : (Ast.stmt array * Ast.lval) =
  let pexp = parse_pexp ps in
    desugar_lval ps pexp

and parse_constraint ps =
  match peek ps with
      (* NB: A constraint *looks* a lot like an EXPR_call, but is restricted *)
      (* syntactically: the constraint name needs to be a name (not an lval) *)
      (* and the constraint args all need to be cargs, which are similar to  *)
      (* names but can begin with the 'formal' base anchor '*'.              *)
      IDENT _ ->
        let n = ctxt "constraint: name" parse_name ps in
        let args = ctxt "constraint: args"
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

    | PROC ->
        bump ps;
        Ast.TY_proc

    | CHAN ->
        bump ps;
        Ast.TY_chan (bracketed LBRACKET RBRACKET parse_ty ps)

    | PORT ->
        bump ps;
        Ast.TY_port (bracketed LBRACKET RBRACKET parse_ty ps)

    | VEC ->
        bump ps;
        Ast.TY_vec (bracketed LBRACKET RBRACKET (parse_slot false) ps)

    | IDENT _ -> Ast.TY_named (parse_name ps)


    | TAG ->
        bump ps;
        let htab = Hashtbl.create 4 in
        let parse_tag_entry ps =
          let ident = parse_ident ps in
          let tup =
            match peek ps with
                LPAREN -> bracketed_zero_or_more LPAREN RPAREN (Some COMMA) (parse_slot false) ps
              | NIL -> (bump ps; [| |])
              | _ -> raise (err "tag variant missing argument list" ps)
          in
            htab_put htab ident tup
        in
        let _ = bracketed_one_or_more LPAREN RPAREN (Some COMMA) (ctxt "tag: variant" parse_tag_entry) ps in
          Ast.TY_tag htab

    | REC ->
        bump ps;
        let ltab = ref [] in
        let parse_rec_entry ps =
          let mut = parse_mutability ps in
          let (slot, ident) = parse_slot_and_ident false ps in
            ltab := ltab_put (!ltab) ident (apply_mutability ps slot mut)
        in
        let _ = bracketed_zero_or_more LPAREN RPAREN (Some COMMA) parse_rec_entry ps in
          Ast.TY_rec (arl (!ltab))

    | LPAREN ->
        let slots = bracketed_zero_or_more LPAREN RPAREN (Some COMMA) (parse_slot false) ps in
          if Array.length slots = 1 && (match slots.(0).Ast.slot_mode with
                                            Ast.MODE_interior _ -> true
                                          | _ -> false)
          then match slots.(0).Ast.slot_ty with
              None -> raise (err "slot without type" ps )
            | Some t -> t
          else Ast.TY_tup slots

    | MACH m -> Ast.TY_mach m

    (* FIXME: parse mod types. *)

    | _ -> raise (unexpected ps)

and parse_mutability ps =
  if flag ps MUTABLE
  then Ast.MUTABLE
  else Ast.IMMUTABLE

and apply_mutability ps slot mut =
  let mode =
    match (slot.Ast.slot_mode, mut) with
        (Ast.MODE_exterior _, mut) -> Ast.MODE_exterior mut
      | (Ast.MODE_interior _, mut) -> Ast.MODE_interior mut
      | (Ast.MODE_read_alias, Ast.MUTABLE)
      | (Ast.MODE_write_alias, Ast.MUTABLE) ->
          raise (err "mutable alias slot" ps)
      | (m, _) -> m
  in
    { slot with Ast.slot_mode = mode }

and parse_slot param_slot ps =
  let mut = parse_mutability ps in
  match (peek ps, param_slot) with
      (AT, _) ->
        bump ps;
        let ty = parse_ty ps in
          { Ast.slot_mode = Ast.MODE_exterior mut;
            Ast.slot_ty = Some ty }

    | (CARET, true) ->
        bump ps;
        if mut = Ast.MUTABLE
        then raise (err "mutable qualifier on write-alias" ps);
        let ty = parse_ty ps in
          { Ast.slot_mode = Ast.MODE_write_alias;
            Ast.slot_ty = Some ty }

    | (TILDE, true) ->
        bump ps;
        if mut = Ast.MUTABLE
        then raise (err "mutable qualifier on read-alias" ps);
        let ty = parse_ty ps in
          { Ast.slot_mode = Ast.MODE_read_alias;
            Ast.slot_ty = Some ty }

    | _ ->
        let ty = parse_ty ps in
          { Ast.slot_mode = Ast.MODE_interior mut;
            Ast.slot_ty = Some ty }


and parse_identified_slot param_slot ps =
  let apos = lexpos ps in
  let slot = parse_slot param_slot ps in
  let bpos = lexpos ps in
    span ps apos bpos slot

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


and parse_rec_input ltab ps =
  let lab = (ctxt "rec input: label" parse_ident ps) in
    match peek ps with
        EQ ->
          bump ps;
          let pexp = (ctxt "rec input: expr" parse_pexp ps) in
            ltab := ltab_put (!ltab) lab pexp
      | _ -> raise (unexpected ps)


and parse_rec_inputs ps =
  let ltab = ref [] in
  let _ = bracketed_zero_or_more LPAREN RPAREN (Some COMMA)
    (ctxt "rec inputs" (parse_rec_input ltab)) ps
  in
    arl (!ltab)


and parse_expr_atom_list bra ket ps =
  arj1st (bracketed_zero_or_more bra ket (Some COMMA)
            (ctxt "expr-atom list" parse_expr_atom) ps)

and slot_auto = { Ast.slot_mode = Ast.MODE_interior Ast.MUTABLE;
                  Ast.slot_ty = None }

and parse_auto_slot_and_init ps =
  let apos = lexpos ps in
  let ident = parse_ident ps in
  let bpos = lexpos ps in
  let lval = Ast.LVAL_base (span ps apos bpos (Ast.BASE_ident ident)) in
  let stmts = ctxt "slot, ident and init: init" (parse_init lval) ps in
    (stmts, slot_auto, ident)


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


and parse_lit ps =
  match peek ps with
      LIT_INT (n,s) -> (bump ps; Ast.LIT_int (n,s))
    | LIT_CHAR c -> (bump ps; Ast.LIT_char c)
    | LIT_BOOL b -> (bump ps; Ast.LIT_bool b)
    | NIL -> (bump ps; Ast.LIT_nil)
    | _ -> raise (unexpected ps)


and parse_bottom_pexp ps : pexp =
  let apos = lexpos ps in
  match peek ps with
      LPAREN ->
        let pexps = ctxt "paren pexps(s)" parse_pexp_list ps in
        let bpos = lexpos ps in
          if Array.length pexps = 1
          then
            pexps.(0)
          else
            span ps apos bpos (PEXP_tup pexps)

    | MUTABLE ->
        bump ps;
        let inner = parse_pexp ps in
        let bpos = lexpos ps in
          span ps apos bpos (PEXP_mutable inner)

    | AT ->
        bump ps;
        let inner = parse_pexp ps in
        let bpos = lexpos ps in
          span ps apos bpos (PEXP_exterior inner)

    | REC ->
          bump ps;
          let inputs = ctxt "rec pexp: rec inputs" parse_rec_inputs ps in
          let bpos = lexpos ps in
            span ps apos bpos (PEXP_rec inputs)

    | VEC ->
        bump ps;
        begin
          let slot =
            match peek ps with
                RPAREN -> bracketed LBRACKET RBRACKET (parse_slot false) ps
              | _ -> { Ast.slot_mode = Ast.MODE_interior Ast.IMMUTABLE;
                       Ast.slot_ty = None }
          in
          let pexps = ctxt "vec pexp: exprs" parse_pexp_list ps in
          let bpos = lexpos ps in
            span ps apos bpos (PEXP_vec (slot, pexps))
        end


    | LIT_STR s ->
        bump ps;
        let bpos = lexpos ps in
          span ps apos bpos (PEXP_str s)

    | PORT ->
        begin
            bump ps;
            expect ps NIL;
            let bpos = lexpos ps in
              span ps apos bpos (PEXP_port)
        end

    | CHAN ->
        begin
            bump ps;
            let port =
              match peek ps with
                  NIL -> (bump ps; None)
                | LPAREN ->
                    begin
                      bump ps;
                      let lv = parse_pexp ps in
                        expect ps RPAREN;
                        Some lv
                    end
                | _ -> raise (unexpected ps)
            in
            let bpos = lexpos ps in
              span ps apos bpos (PEXP_chan port)
        end

    | SPAWN ->
          bump ps;
          let pexp = ctxt "spawn pexp: init call" parse_pexp ps in
          let bpos = lexpos ps in
              span ps apos bpos (PEXP_spawn pexp)

    | IDENT i ->
        begin
          bump ps;
          match peek ps with
              LBRACKET ->
                begin
                  let tys =
                    ctxt "apply-type expr"
                      (bracketed_one_or_more LBRACKET RBRACKET (Some COMMA) parse_ty) ps
                  in
                  let bpos = lexpos ps in
                    span ps apos bpos (PEXP_app (i, tys))
                end

            | _ ->
                begin
                  let bpos = lexpos ps in
                    span ps apos bpos (PEXP_ident i)
                end
        end

    | _ ->
        let lit = parse_lit ps in
        let bpos = lexpos ps in
          span ps apos bpos (PEXP_lit lit)


and parse_ext_pexp ps pexp =
  let apos = lexpos ps in
    match peek ps with
        NIL | LPAREN ->
          let args = parse_pexp_list ps in
          let bpos = lexpos ps in
          let ext = span ps apos bpos (PEXP_call (pexp, args)) in
            parse_ext_pexp ps ext

      | DOT ->
          begin
            bump ps;
            let ext =
              match peek ps with
                  LPAREN ->
                    bump ps;
                    let rhs = parse_pexp ps in
                      expect ps RPAREN;
                      let bpos = lexpos ps in
                        span ps apos bpos (PEXP_ext_pexp (pexp, rhs))
                | _ ->
                    let rhs = parse_name_component ps in
                    let bpos = lexpos ps in
                      span ps apos bpos (PEXP_ext_name (pexp, rhs))
            in
              parse_ext_pexp ps ext
          end

      | _ -> pexp


and parse_negation_pexp ps =
    let apos = lexpos ps in
      match peek ps with
          NOT ->
            let rhs = ctxt "negation pexp" parse_negation_pexp ps in
            let bpos = lexpos ps in
              span ps apos bpos (PEXP_unop (Ast.UNOP_not, rhs))

        | _ ->
            let lhs = parse_bottom_pexp ps in
              parse_ext_pexp ps lhs


(* Binops are all left-associative,                *)
(* so we factor out some of the parsing code here. *)
and binop_rhs ps name apos lhs rhs_parse_fn op =
  bump ps;
  let rhs = (ctxt (name ^ " rhs") rhs_parse_fn ps) in
  let bpos = lexpos ps in
    span ps apos bpos (PEXP_binop (op, lhs, rhs))


and parse_factor_pexp ps =
  let name = "factor pexp" in
  let apos = lexpos ps in
  let lhs = ctxt (name ^ " lhs") parse_negation_pexp ps in
    match peek ps with
        STAR    -> binop_rhs ps name apos lhs parse_factor_pexp Ast.BINOP_mul
      | SLASH   -> binop_rhs ps name apos lhs parse_factor_pexp Ast.BINOP_div
      | PERCENT -> binop_rhs ps name apos lhs parse_factor_pexp Ast.BINOP_mod
      | _       -> lhs


and parse_term_pexp ps =
  let name = "term pexp" in
  let apos = lexpos ps in
  let lhs = ctxt (name ^ " lhs") parse_factor_pexp ps in
    match peek ps with
        PLUS  -> binop_rhs ps name apos lhs parse_term_pexp Ast.BINOP_add
      | MINUS -> binop_rhs ps name apos lhs parse_term_pexp Ast.BINOP_sub
      | _     -> lhs


and parse_shift_pexp ps =
  let name = "shift pexp" in
  let apos = lexpos ps in
  let lhs = ctxt (name ^ " lhs") parse_term_pexp ps in
    match peek ps with
        LSL -> binop_rhs ps name apos lhs parse_shift_pexp Ast.BINOP_lsl
      | LSR -> binop_rhs ps name apos lhs parse_shift_pexp Ast.BINOP_lsr
      | ASR -> binop_rhs ps name apos lhs parse_shift_pexp Ast.BINOP_asr
      | _   -> lhs


and parse_relational_pexp ps =
  let name = "relational pexp" in
  let apos = lexpos ps in
  let lhs = ctxt (name ^ " lhs") parse_shift_pexp ps in
    match peek ps with
        LT -> binop_rhs ps name apos lhs parse_relational_pexp Ast.BINOP_lt
      | LE -> binop_rhs ps name apos lhs parse_relational_pexp Ast.BINOP_le
      | GE -> binop_rhs ps name apos lhs parse_relational_pexp Ast.BINOP_ge
      | GT -> binop_rhs ps name apos lhs parse_relational_pexp Ast.BINOP_gt
      | _  -> lhs


and parse_equality_pexp ps =
  let name = "equality pexp" in
  let apos = lexpos ps in
  let lhs = ctxt (name ^ " lhs") parse_relational_pexp ps in
    match peek ps with
        EQEQ -> binop_rhs ps name apos lhs parse_equality_pexp Ast.BINOP_eq
      | NE   -> binop_rhs ps name apos lhs parse_equality_pexp Ast.BINOP_ne
      | _    -> lhs


and parse_and_pexp ps =
  let name = "and pexp" in
  let apos = lexpos ps in
  let lhs = ctxt (name ^ " lhs") parse_equality_pexp ps in
    match peek ps with
        AND -> binop_rhs ps name apos lhs parse_and_pexp Ast.BINOP_and
      | _   -> lhs


and parse_or_pexp ps =
  let name = "or pexp" in
  let apos = lexpos ps in
  let lhs = ctxt (name ^ " lhs") parse_and_pexp ps in
    match peek ps with
        OR -> binop_rhs ps name apos lhs parse_or_pexp Ast.BINOP_or
      | _  -> lhs


and parse_pexp ps =
  parse_or_pexp ps


and parse_pexp_list ps =
  match peek ps with
      LPAREN ->
        bracketed_zero_or_more LPAREN RPAREN (Some COMMA)
          (ctxt "pexp list" parse_pexp) ps
    | NIL -> (bump ps; [| |])
    | _ -> raise (unexpected ps)


and atom_lval ps at =
  match at with
      Ast.ATOM_lval lv -> lv
    | Ast.ATOM_literal _ -> raise (err "literal where lval expected" ps)


and desugar_lval ps pexp =
  let s = Hashtbl.find ps.pstate_sess.Session.sess_spans pexp.id in
  let (apos, bpos) = (s.lo, s.hi) in
    match pexp.node with

        PEXP_ident ident ->
          let nb = span ps apos bpos (Ast.BASE_ident ident) in
            ([||], Ast.LVAL_base nb)

      | PEXP_app (ident, tys) ->
          let nb = span ps apos bpos (Ast.BASE_app (ident, tys)) in
            ([||], Ast.LVAL_base nb)

      | PEXP_ext_name (base_pexp, comp) ->
          let (base_stmts, base_atom) = desugar_expr_atom ps base_pexp in
          let base_lval = atom_lval ps base_atom in
            (base_stmts, Ast.LVAL_ext (base_lval, Ast.COMP_named comp))

      | PEXP_ext_pexp (base_pexp, ext_pexp) ->
          let (base_stmts, base_atom) = desugar_expr_atom ps base_pexp in
          let (ext_stmts, ext_atom) = desugar_expr_atom ps ext_pexp in
          let base_lval = atom_lval ps base_atom in
            (Array.append base_stmts ext_stmts,
             Ast.LVAL_ext (base_lval, Ast.COMP_atom (clone_atom ps ext_atom)))

      | _ ->
          let (stmts, atom) = desugar_expr_atom ps pexp in
            (stmts, atom_lval ps atom)


and desugar_expr ps pexp : (Ast.stmt array * Ast.expr) =
  match pexp.node with

      PEXP_unop (op, pe) ->
        let (stmts, at) = desugar_expr_atom ps pe in
          (stmts, Ast.EXPR_unary (op, at))

    | PEXP_binop (op, lhs, rhs) ->
          let (lhs_stmts, lhs_atom) = desugar_expr_atom ps lhs in
          let (rhs_stmts, rhs_atom) = desugar_expr_atom ps rhs in
            (Array.append lhs_stmts rhs_stmts,
             Ast.EXPR_binary (op, lhs_atom, rhs_atom))

    | _ ->
        let (stmts, at) = desugar_expr_atom ps pexp in
          (stmts, Ast.EXPR_atom at)


and desugar_expr_atom
    (ps:pstate)
    (pexp:pexp)
    : (Ast.stmt array * Ast.atom) =
  let s = Hashtbl.find ps.pstate_sess.Session.sess_spans pexp.id in
  let (apos, bpos) = (s.lo, s.hi) in
    match pexp.node with

        PEXP_unop _
      | PEXP_binop _
      | PEXP_rec _
      | PEXP_tup _
      | PEXP_str _
      | PEXP_vec _
      | PEXP_port
      | PEXP_chan _
      | PEXP_call _
      | PEXP_spawn _ ->
          let (_, tmp, decl_stmt) = build_tmp ps slot_auto apos bpos in
          let stmts = desugar_expr_init ps tmp pexp in
            (Array.append [| decl_stmt |] stmts,
             Ast.ATOM_lval (clone_lval ps tmp))

      | PEXP_lit lit ->
          ([||], Ast.ATOM_literal (span ps apos bpos lit))

      | PEXP_ident _
      | PEXP_app _
      | PEXP_ext_name _
      | PEXP_ext_pexp _ ->
          let (stmts, lval) = desugar_lval ps pexp in
            (stmts, Ast.ATOM_lval lval)

      | PEXP_exterior inner ->
          raise (err "exterior symbol in atom context" ps)

      | PEXP_mutable _ ->
          raise (err "mutable keyword in atom context" ps)


and desugar_expr_mode_atom ps pexp : (Ast.stmt array * (Ast.mode * Ast.atom)) =
  let desugar_inner mut e =
    let (stmts, atom) = desugar_expr_atom ps e in
      (stmts, (mut, atom))
  in
    match pexp.node with
        PEXP_mutable {node=(PEXP_exterior e); id=_} ->
          desugar_inner (Ast.MODE_exterior Ast.MUTABLE) e
      | PEXP_mutable e ->
          desugar_inner (Ast.MODE_interior Ast.MUTABLE) e
      | _ ->
          desugar_inner (Ast.MODE_interior Ast.IMMUTABLE) pexp

and desugar_expr_atoms ps pexps : (Ast.stmt array * Ast.atom array) =
  arj1st (Array.map (desugar_expr_atom ps) pexps)

and desugar_expr_mode_atoms ps pexps : (Ast.stmt array * (Ast.mode * Ast.atom) array) =
  arj1st (Array.map (desugar_expr_mode_atom ps) pexps)

and desugar_expr_init ps dst_lval pexp : (Ast.stmt array) =
  let s = Hashtbl.find ps.pstate_sess.Session.sess_spans pexp.id in
  let (apos, bpos) = (s.lo, s.hi) in

    match pexp.node with

        PEXP_lit _
      | PEXP_ident _
      | PEXP_app _
      | PEXP_ext_name _
      | PEXP_ext_pexp _ ->
          let (stmts, atom) = desugar_expr_atom ps pexp in
          let expr = Ast.EXPR_atom atom in
            [| span ps apos bpos (Ast.STMT_copy (dst_lval, expr)) |]

      | PEXP_binop (op, lhs, rhs) ->
          let (lhs_stmts, lhs_atom) = desugar_expr_atom ps lhs in
          let (rhs_stmts, rhs_atom) = desugar_expr_atom ps rhs in
          let expr = Ast.EXPR_binary (op, lhs_atom, rhs_atom) in
          let copy_stmt = span ps apos bpos (Ast.STMT_copy (dst_lval, expr)) in
            Array.concat [ lhs_stmts; rhs_stmts; [| copy_stmt |] ]

      | PEXP_unop (op, rhs) ->
          let (rhs_stmts, rhs_atom) = desugar_expr_atom ps rhs in
          let expr = Ast.EXPR_unary (op, rhs_atom) in
          let copy_stmt = span ps apos bpos (Ast.STMT_copy (dst_lval, expr)) in
            Array.append rhs_stmts [| copy_stmt |]

      | PEXP_call (fn, args) ->
          let (fn_stmts, fn_atom) = desugar_expr_atom ps fn in
          let (arg_stmts, arg_atoms) = desugar_expr_atoms ps args in
          let fn_lval = atom_lval ps fn_atom in
          let call_stmt = span ps apos bpos (Ast.STMT_call (dst_lval, fn_lval, arg_atoms)) in
            Array.concat [ fn_stmts; arg_stmts; [| call_stmt |] ]

      | PEXP_spawn sub ->
          begin
            match sub.node with
                PEXP_call (prog, args) ->
                  let (prog_stmts, prog_atom) = desugar_expr_atom ps prog in
                  let (arg_stmts, arg_atoms) = desugar_expr_atoms ps args in
                  let prog_lval = atom_lval ps prog_atom in
                  let spawn_stmt = span ps apos bpos (Ast.STMT_spawn (dst_lval, prog_lval, arg_atoms)) in
                    Array.concat [ prog_stmts; arg_stmts; [| spawn_stmt |] ]
              | _ -> raise (err "non-call spawn" ps)
          end

          | PEXP_rec args ->
          let (arg_stmts, entries) =
            arj1st
              begin
                Array.map
                  begin
                    fun (ident, pexp) ->
                      let (stmts, (mut, atom)) = desugar_expr_mode_atom ps pexp in
                        (stmts, (ident, mut, atom))
                  end
                  args
              end
          in
          let rec_stmt = span ps apos bpos (Ast.STMT_init_rec (dst_lval, entries)) in
            Array.append arg_stmts [| rec_stmt |]

      | PEXP_tup args ->
          let (arg_stmts, arg_mode_atoms) = desugar_expr_mode_atoms ps args in
          let stmt = span ps apos bpos (Ast.STMT_init_tup (dst_lval, arg_mode_atoms)) in
            Array.append arg_stmts [| stmt |]

      | PEXP_str s ->
          let stmt = span ps apos bpos (Ast.STMT_init_str (dst_lval, s)) in
            [| stmt |]

      | PEXP_vec (slot, args) ->
          let (arg_stmts, arg_atoms) = desugar_expr_atoms ps args in
          let stmt = span ps apos bpos (Ast.STMT_init_vec (dst_lval, slot, arg_atoms)) in
            Array.append arg_stmts [| stmt |]

      | PEXP_port ->
          [| span ps apos bpos (Ast.STMT_init_port dst_lval) |]

      | PEXP_chan pexp_opt ->
          let (port_stmts, port_opt) =
            match pexp_opt with
                None -> ([||], None)
              | Some port_pexp ->
                  begin
                    let (port_stmts, port_atom) = desugar_expr_atom ps port_pexp in
                    let port_lval = atom_lval ps port_atom in
                      (port_stmts, Some port_lval)
                  end
          in
          let chan_stmt =
            span ps apos bpos
              (Ast.STMT_init_chan (dst_lval, port_opt))
          in
            Array.append port_stmts [| chan_stmt |]

      | PEXP_exterior inner ->
          raise (err "exterior symbol in initialiser context" ps)

      | PEXP_mutable _ ->
          raise (err "mutable keyword in initialiser context" ps)



and parse_expr (ps:pstate) : (Ast.stmt array * Ast.expr) =
  let pexp = ctxt "expr" parse_pexp ps in
    desugar_expr ps pexp

and parse_expr_atom (ps:pstate) : (Ast.stmt array * Ast.atom) =
  let pexp = ctxt "expr" parse_pexp ps in
    desugar_expr_atom ps pexp

and parse_expr_init (lv:Ast.lval) (ps:pstate) : (Ast.stmt array) =
  let pexp = ctxt "expr" parse_pexp ps in
    desugar_expr_init ps lv pexp

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

and parse_init
    (lval:Ast.lval)
    (ps:pstate)
    : Ast.stmt array =
  let apos = lexpos ps in
  let stmts =
    match peek ps with
        EQ ->
          bump ps;
          parse_expr_init lval ps
      | LARROW ->
          bump ps;
          let (stmts, rhs) = ctxt "init: port" parse_lval ps in
          let bpos = lexpos ps in
          let stmt = Ast.STMT_recv (lval, rhs) in
            Array.append stmts [| (span ps apos bpos stmt) |]
      | _ -> arr []
  in
  let _ = expect ps SEMI in
    stmts

and parse_slot_and_ident_and_init ps =
  let apos = lexpos ps in
  let (slot, ident) =
    ctxt "slot, ident and init: slot and ident"
      (parse_slot_and_ident false) ps
  in
  let bpos = lexpos ps in
  let lval = Ast.LVAL_base (span ps apos bpos (Ast.BASE_ident ident)) in
  let stmts = ctxt "slot, ident and init: init" (parse_init lval) ps in
    (stmts, slot, ident)

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
          let (stmts, atom) = ctxt "stmts: log value" parse_expr_atom ps in
            expect ps SEMI;
            let bpos = lexpos ps in
              spans stmts apos bpos (Ast.STMT_log atom)

      | CHECK ->
          bump ps;
          begin
            match peek ps with
                LPAREN ->
                  bump ps;
                  let (stmts, expr) = ctxt "stmts: check value" parse_expr ps in
                    expect ps RPAREN;
                    expect ps SEMI;
                    let bpos = lexpos ps in
                      spans stmts apos bpos (Ast.STMT_check_expr expr)
              | IF ->
                  bump ps;
                  expect ps LPAREN;
                  let constrs = parse_constrs ps in
                  expect ps RPAREN;
                  let block = parse_block ps in
                  let bpos = lexpos ps in
                    [| span ps apos bpos (Ast.STMT_check_if (constrs, block)) |]

              | _ ->
                  let constrs = parse_constrs ps in
                    expect ps SEMI;
                    let bpos = lexpos ps in
                      [| span ps apos bpos (Ast.STMT_check constrs) |]
          end

      | IF ->
          bump ps;
          let (stmts, expr) = ctxt "stmts: if cond" (bracketed LPAREN RPAREN parse_expr) ps in
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
                 { Ast.if_test = expr;
                   Ast.if_then = then_block;
                   Ast.if_else = else_block; })

      | WHILE ->
          bump ps;
          let (stmts, test) = ctxt "stmts: while cond" (bracketed LPAREN RPAREN parse_expr) ps in
          let body_block = ctxt "stmts: while body" parse_block ps in
          let bpos = lexpos ps in
            [| span ps apos bpos
                 (Ast.STMT_while
                    { Ast.while_lval = (stmts, test);
                      Ast.while_body = body_block; }) |]

      | PUT proto ->
          bump ps;
          let (stmts, e) =
            match peek ps with
                SEMI -> (arr [], None)
              | _ ->
                  let (stmts, expr) = ctxt "stmts: put expr" parse_expr_atom ps in
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
                  let (stmts, expr) = ctxt "stmts: ret expr" parse_expr_atom ps in
                    expect ps SEMI;
                    (stmts, Some expr)
          in
          let bpos = lexpos ps in
            spans stmts apos bpos (Ast.STMT_ret (proto, e))

      | BE proto ->
          bump ps;
          let (lstmts, lval) = ctxt "be: lval" parse_lval ps in
          let (astmts, args) = ctxt "be: args" (parse_expr_atom_list LPAREN RPAREN) ps in
          let bpos = lexpos ps in
          let be = span ps apos bpos (Ast.STMT_be (proto, lval, args)) in
          Array.concat [ lstmts; astmts; [| be |] ]

      | LBRACE -> [| ctxt "stmts: block" parse_block_stmt ps |]

      | LET ->
          bump ps;
          begin
            match peek ps with
               LPAREN ->
                 let (slots, idents) =
                   ctxt "stmt tup decl: slots and idents"
                     (bracketed LPAREN RPAREN (parse_two_or_more_identified_tup_slots_and_idents false)) ps in
                 let bpos = lexpos ps in
                 let (nonce, tmp, tempdecl) =
                   build_tmp ps
                     { Ast.slot_mode = Ast.MODE_interior Ast.IMMUTABLE;
                       Ast.slot_ty = Some (Ast.TY_tup (Array.map (fun x -> x.node) slots)) }
                     apos bpos
                 in
                 let stmts = ctxt "stmt tup decl: init" (parse_init tmp) ps in
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

                 let copies = ref [] in

                 let makedecl i slot =
                   begin
                     let ext = Ast.COMP_named (Ast.COMP_idx i) in
                     let src_lval = Ast.LVAL_ext ((clone_lval ps tmp), ext) in
                     let src_atom = Ast.ATOM_lval src_lval in
                     let dst_lval = Ast.LVAL_base (span ps apos bpos (Ast.BASE_ident idents.(i))) in
                     let copy = span ps apos bpos (Ast.STMT_copy (dst_lval, Ast.EXPR_atom src_atom)) in
                       copies := copy :: (!copies)
                   end;
                   let slot = {slot with node = apply_mutability ps slot.node Ast.MUTABLE} in
                   let decl = Ast.DECL_slot (Ast.KEY_ident idents.(i), slot) in
                     span ps apos bpos (Ast.STMT_decl decl)
                 in
                 let letdecls = Array.mapi makedecl slots in
                   Array.concat [stmts; [| tempdecl |]; letdecls; arl (!copies)]

             | _ ->
                 let (stmts, slot, ident) =
                   ctxt "stmt slot" parse_slot_and_ident_and_init ps in
                 let slot = apply_mutability ps slot Ast.MUTABLE in
                 let bpos = lexpos ps in
                 let decl = Ast.DECL_slot (Ast.KEY_ident ident,
                                           (span ps apos bpos slot))
                 in
                   Array.concat [[| span ps apos bpos (Ast.STMT_decl decl) |]; stmts]
          end

      | AUTO ->
          bump ps;
          let (stmts, slot, ident) =
            ctxt "stmt slot" parse_auto_slot_and_init ps in
          let slot = apply_mutability ps slot Ast.MUTABLE in
          let bpos = lexpos ps in
          let decl = Ast.DECL_slot (Ast.KEY_ident ident,
                                    (span ps apos bpos slot))
          in
            Array.concat [[| span ps apos bpos (Ast.STMT_decl decl) |]; stmts]


      | PROG | MOD | TYPE | (FN _) | PRED ->
          let (ident, stmts, item) = ctxt "stmt: decl" parse_mod_item ps in
          let bpos = lexpos ps in
          let decl = Ast.DECL_mod_item (ident, item) in
          let stmts = expand_tags_to_stmts ps item stmts in
            spans stmts apos bpos (Ast.STMT_decl decl)

      | LPAREN ->
          let (lstmts, lvals) =
            arj1st (ctxt "stmt: paren_copy_to_tup tup"
                      (bracketed_one_or_more LPAREN RPAREN (Some COMMA) parse_lval) ps)
          in
          let _ = expect ps EQ in
          let (estmts, atom) = ctxt "stmt: paren_copy_to_tup rval" parse_expr_atom ps in
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
             *   auto t_n = foo();
             *   a = t_n.{0};
             *   b = t_n.{1};
             *
             *)

          let (nonce, tmp, tempdecl) =
            build_tmp ps slot_auto apos bpos in
          let copy = span ps apos bpos (Ast.STMT_copy (tmp, Ast.EXPR_atom atom)) in
          let make_copy i dst =
            let ext = Ast.COMP_named (Ast.COMP_idx i) in
            let lval = Ast.LVAL_ext ((clone_lval ps tmp), ext) in
            let e = Ast.EXPR_atom (Ast.ATOM_lval lval) in
              span ps apos bpos (Ast.STMT_copy (dst, e))
          in
            let copies = Array.mapi make_copy lvals in
              Array.concat [ stmts; [| tempdecl; copy |]; copies ]

      | _ ->
          let (lstmts, lval) = ctxt "stmt: lval" parse_lval ps in
            begin
              match peek ps with

                  SEMI -> (bump ps; lstmts)

                | EQ -> parse_init lval ps

                | LARROW ->
                    bump ps;
                    let (stmts, rhs) = ctxt "stmt: recv rhs" parse_lval ps in
                    let _ = expect ps SEMI in
                    let bpos = lexpos ps in
                      spans stmts apos bpos (Ast.STMT_recv (lval, rhs))

                | SEND ->
                    bump ps;
                    let (stmts, rhs) = ctxt "stmt: send rhs" parse_expr_atom ps in
                    let _ = expect ps SEMI in
                    let bpos = lexpos ps in
                    let (src, copy) = match rhs with
                        Ast.ATOM_lval lv -> (lv, [| |])
                      | _ ->
                          let (nonce, tmp, tempdecl) = build_tmp ps slot_auto apos bpos in
                          let copy = span ps apos bpos (Ast.STMT_copy (tmp, Ast.EXPR_atom rhs)) in
                            ((clone_lval ps tmp), [| tempdecl; copy |])
                    in
                    let send = span ps apos bpos (Ast.STMT_send (lval, src)) in
                      Array.concat [ stmts; copy; [| send |] ]

                | _ -> raise (unexpected ps)
            end

and parse_prog_item prog_cell stmts_cell ps =
  match peek ps with
      MAIN ->
        bump ps;
        let main = ctxt "prog_item: main" parse_block ps in
          prog_cell := { (!prog_cell) with Ast.prog_main = Some main }

    | INIT ->
          bump ps;
          let apos = lexpos ps in
          let (inputs, constrs, output) = ctxt "prog_item: init in_and_out" parse_in_and_out ps in
          let body = ctxt "prog_item: init body" parse_block ps in
          let bpos = lexpos ps in
          let proc_input_slot = { Ast.slot_mode = Ast.MODE_interior Ast.IMMUTABLE;
                                  Ast.slot_ty = Some Ast.TY_proc }
          in
          let init = span ps apos bpos
            { Ast.init_proc_input = span ps apos bpos proc_input_slot;
              Ast.init_input_slots = inputs;
              Ast.init_input_constrs = constrs;
              Ast.init_output_slot = output;
              Ast.init_body = body }
          in
          prog_cell := { (!prog_cell) with Ast.prog_init = Some init }

    | FINI ->
        bump ps;
        let fini = ctxt "prog_item: fini" parse_block ps in
          prog_cell := { (!prog_cell) with Ast.prog_fini = Some fini }

    | PUB | PURE
    | PROG | MOD | TYPE | (FN _) | PRED ->
        let (ident, stmts, item) = ctxt "prog_item: mod item" parse_mod_item ps in
        let mod_items = (!prog_cell).Ast.prog_mod in
          htab_put mod_items ident item;
          expand_tags_to_items ps item mod_items;
          stmts_cell := stmts :: (!stmts_cell)

    | LET ->
        bump ps;
        let apos = lexpos ps in
        let (slot, ident) =
          ctxt "prog_item: slot and ident"
            (parse_slot_and_ident false) ps
        in
        let bpos = lexpos ps in
          expect ps SEMI;
          htab_put (!prog_cell).Ast.prog_slots ident (span ps apos bpos slot)

    | _ -> raise (unexpected ps)


and parse_prog ps =
  let prog = { Ast.prog_init = None;
               Ast.prog_main = None;
               Ast.prog_fini = None;
               Ast.prog_mod = Hashtbl.create 4;
               Ast.prog_slots = Hashtbl.create 4; }
  in
  let prog_cell = ref prog in
  let stmts_cell = ref [] in
  let _ = ctxt "prog" (bracketed_zero_or_more LBRACE RBRACE None
                         (parse_prog_item prog_cell stmts_cell)) ps
  in
    (Array.concat (List.rev !stmts_cell), !prog_cell)

and parse_inputs ps =
  let slots =
    match peek ps with
        NIL -> (bump ps; [| |])
      | LPAREN -> ctxt "inputs: input idents and slots"
          (parse_one_or_more_identified_slot_ident_pairs true) ps
      | _ -> raise (unexpected ps)
  in
  let constrs =
    match peek ps with
        COLON -> (bump ps; ctxt "inputs: constrs" parse_constrs ps)
      | _ -> [| |]
  in
  let rec rewrite_carg_path cp =
    match cp with
        Ast.CARG_base (Ast.BASE_named (Ast.BASE_ident ident)) ->
          begin
            let res = ref cp in
              for i = 0 to (Array.length slots) - 1
              do
                let (_, ident') = slots.(i) in
                  if ident' = ident
                  then res := Ast.CARG_ext (Ast.CARG_base Ast.BASE_formal,
                                            Ast.COMP_idx i)
                  else ()
              done;
              !res
          end
      | Ast.CARG_base _ -> cp
      | Ast.CARG_ext (cp, ext) ->
          Ast.CARG_ext (rewrite_carg_path cp, ext)
  in
    (* Rewrite constrs with input tuple as BASE_formal. *)
    Array.iter
      begin
        fun constr ->
          let args = constr.Ast.constr_args in
            Array.iteri
              begin
                fun i carg ->
                  match carg with
                      Ast.CARG_path cp ->
                        args.(i) <- Ast.CARG_path (rewrite_carg_path cp)
                    | _ -> ()
              end
              args
      end
      constrs;
    (slots, constrs)


and parse_in_and_out ps =
  let (inputs, constrs) = parse_inputs ps in
  let _ = expect ps RARROW in
  let output = ctxt "fn in and out: output slot" (parse_identified_slot true) ps in
    (inputs, constrs, output)


(* parse_fn starts at the first lparen of the sig. *)
and parse_fn proto_opt pure ps =
    let (inputs, constrs, output) = ctxt "fn: in_and_out" parse_in_and_out ps in
    let body = ctxt "fn: body" parse_block ps in
      { Ast.fn_input_slots = inputs;
        Ast.fn_input_constrs = constrs;
        Ast.fn_output_slot = output;
        Ast.fn_aux = { Ast.fn_purity = pure;
                       Ast.fn_proto = proto_opt; };
        Ast.fn_body = body; }

and parse_pred ps =
  let (inputs, constrs) = ctxt "pred: inputs" parse_inputs ps in
  let body = ctxt "pred: body" parse_block ps in
    { Ast.pred_input_slots = inputs;
      Ast.pred_input_constrs = constrs;
      Ast.pred_body = body }

and flag ps tok =
  if peek ps = tok
  then (bump ps; true)
  else false

and parse_ty_param ps = parse_ident ps

and parse_ty_params ps =
  match peek ps with
      LBRACKET ->
        bracketed_zero_or_more LBRACKET RBRACKET (Some COMMA) parse_ty_param ps
    | _ -> arr []

and parse_native_mod_item ps =
  let apos = lexpos ps in
  let (ident, item) =
    match peek ps with
        FN None ->
          begin
            bump ps;
            let ident = ctxt "native fn: ident" parse_ident ps in
            let (inputs, constrs, output) = ctxt "native fn: in_and_out" parse_in_and_out ps in
              expect ps SEMI;
              let nfn =
                {
                  Ast.native_fn_input_slots = inputs;
                  Ast.native_fn_input_constrs = constrs;
                  Ast.native_fn_output_slot = output;
                }
              in
                (ident, Ast.NATIVE_fn nfn)
          end

      | TYPE ->
          begin
            bump ps;
            let ident = ctxt "native ty: ident" parse_ident ps in
            let tymach = match peek ps with
                MACH m -> m
              | _ -> raise (unexpected ps)
            in
              expect ps SEMI;
                (ident, Ast.NATIVE_type tymach)
          end

      | MOD ->
          begin
            bump ps;
            let ident = ctxt "native mod: ident" parse_ident ps in
            let items = (bracketed_zero_or_more LBRACE RBRACE None
                           parse_native_mod_item ps)
            in
            let htab = Hashtbl.create 4 in
              Array.iter
                begin
                  fun (ident, item) ->
                    htab_put htab ident item
                end
                items;
              (ident, Ast.NATIVE_mod htab)
          end

      | _ -> raise (unexpected ps)
  in
  let bpos = lexpos ps in
    (ident, (span ps apos bpos item))


and parse_mod_item ps =
  let apos = lexpos ps in
  let public = flag ps PUB in
  let pure =
    match peek ps with
        PURE -> Ast.PURE
      | MUTABLE -> Ast.IMPURE Ast.MUTABLE
      | _ -> Ast.IMPURE Ast.IMMUTABLE
  in

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
          let fn = ctxt "mod fn item: fn" (parse_fn proto_opt pure) ps in
          let
              decl = { Ast.decl_params = params;
                       Ast.decl_item = fn }
          in
          let bpos = lexpos ps in
            (ident, arr [],
             span ps apos bpos
               (Ast.MOD_ITEM_fn decl))

      | PRED ->
          bump ps;
          let ident = ctxt "mod pred item: ident" parse_ident ps in
          let params = ctxt "mod pred item: type params" parse_ty_params ps in
          let pred = ctxt "mod pred item: pred" parse_pred ps in
          let
              decl = { Ast.decl_params = params;
                       Ast.decl_item = pred }
          in
          let bpos = lexpos ps in
            (ident, arr [],
             span ps apos bpos
               (Ast.MOD_ITEM_pred decl))

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
            (ident, [| |], span ps apos bpos item)

      | _ -> raise (unexpected ps)


and expand_tags ps item : (Ast.ident * Ast.mod_item) array =
  let handle_ty_tag params ttag =
    let tags = ref [] in
      Hashtbl.iter
        begin
          fun ident tup ->
            let header = Array.map (fun slot -> (clone_span ps item slot)) tup in
            let decl = { Ast.decl_params = params;
                         Ast.decl_item = (header, ttag) }
            in
            let tag_item' = Ast.MOD_ITEM_tag decl in
            let tag_item = clone_span ps item tag_item' in
              tags := (ident, tag_item) :: (!tags)
        end
        ttag;
      arr (!tags)
  in
  let handle_ty_decl tyd =
    match tyd.Ast.decl_item with
        Ast.TY_tag ttag -> handle_ty_tag tyd.Ast.decl_params ttag
      | _ -> [| |]
  in
    match item.node with
        Ast.MOD_ITEM_public_type tyd -> handle_ty_decl tyd
      | Ast.MOD_ITEM_opaque_type tyd -> handle_ty_decl tyd
      | _ -> [| |]


and expand_tags_to_stmts ps item stmts : Ast.stmt array =
  let id_items = expand_tags ps item in
  let stmts' =
    Array.map
      (fun (ident, tag_item) ->
         clone_span ps item
           (Ast.STMT_decl
              (Ast.DECL_mod_item (ident, tag_item))))
      id_items
  in
    Array.append stmts stmts'


and expand_tags_to_items ps item (items:Ast.mod_items) : unit =
  let id_items = expand_tags ps item in
    Array.iter
      (fun (ident, item) -> htab_put items ident item)
      id_items


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
        pstate_file = fname;
        pstate_sess = sess;
        pstate_temp_id = tref;
        pstate_node_id = nref }
    in
      iflog ps (fun _ -> log ps "made parser for: %s\n%!" fname);
      ps

and parse_crate_mod_entry
    (prefix:string)
    (files:(node_id,filename) Hashtbl.t)
    (mod_items:Ast.mod_items)
    (native_mod_items:Ast.native_mod_items)
    (ps:pstate)
    : unit =
  match peek ps with
      NATIVE ->
        begin
            bump ps;
            let (ident, item) = parse_native_mod_item ps in
              htab_put native_mod_items ident item;
              htab_put files item.id ps.pstate_file
        end
    | _ ->
        begin
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
              | _ ->
                  begin
                    match peek ps with 
                        LBRACE -> name
                      | SEMI -> name ^ ".rs"
                      | _ -> raise (unexpected ps)
                  end
          in
          let full_fname = Filename.concat prefix fname in
          let (items,is_cu) =
            match peek ps with
                SEMI ->
                  bump ps;
                  let p =
                    make_parser
                      ps.pstate_temp_id
                      ps.pstate_node_id
                      ps.pstate_sess
                      ps.pstate_lexfun
                      full_fname
                  in
                    (parse_file_mod_items p, true)
              | LBRACE ->
                  bump ps;
                  let items =
                    parse_crate_mod_entries full_fname files ps
                  in
                    (items, false)

              | _ -> raise (unexpected ps)
          in
          let bpos = lexpos ps in
          let item_mod =
            (* FIXME: permit type-parametric top-level modules. *)
            span ps apos bpos (Ast.MOD_ITEM_mod { Ast.decl_params = arr [];
                                                  Ast.decl_item = items })
          in
            if is_cu
            then htab_put files item_mod.id full_fname;
            htab_put mod_items name item_mod
        end

and parse_file_mod_items
    (ps:pstate)
    : Ast.mod_items =
  let items = Hashtbl.create 4 in
    while peek ps != EOF
    do
      let (ident, stmts, item) = parse_mod_item ps in
        if Array.length stmts != 0
        then raise (Parse_err (ps, "top-level module cannot contain implicit statements"));
        htab_put items ident item;
        expand_tags_to_items ps item items;
    done;
    expect ps EOF;
    items

and parse_crate_mod_entries
    (prefix:string)
    (files:(node_id,filename) Hashtbl.t)
    (ps:pstate)
    : Ast.mod_items =
  let items = Hashtbl.create 4 in
  let nitems = Hashtbl.create 4 in
    while peek ps != RBRACE
    do
      parse_crate_mod_entry prefix files items nitems ps
    done;
    expect ps RBRACE;
    items

and parse_root_crate_entries
    (fname:string)
    (prefix:string)
    (files:(node_id,filename) Hashtbl.t)
    (ps:pstate)
    : Ast.crate =
  let items = Hashtbl.create 4 in
  let nitems = Hashtbl.create 4 in
  let explicit_main = ref None in
  let apos = lexpos ps in
    while peek ps != EOF
    do
      match peek ps with
          MAIN ->
            begin
              bump ps;
              expect ps EQ;
              let name = parse_name ps in
                expect ps SEMI;
                match !explicit_main with
                    None -> explicit_main := Some name
                  | Some _ -> raise (err "multiple explicit main programs given in crate" ps )
            end
        | NATIVE | MOD ->
            parse_crate_mod_entry prefix files items nitems ps
        | _ -> raise (unexpected ps)
    done;
    expect ps EOF;
    let main =
      match !explicit_main with
          None -> infer_main_prog ps items
        | Some m -> m
    in
    let bpos = lexpos ps in
      span ps apos bpos
        { Ast.crate_items = items;
          Ast.crate_native_items = nitems;
          Ast.crate_main = main;
          Ast.crate_files = files }

and infer_main_prog
    (ps:pstate)
    (crate_items:Ast.mod_items)
    : Ast.name =
  let progs = ref [] in
  let extend prefix_name ident =
    match prefix_name with
        None -> Ast.NAME_base (Ast.BASE_ident ident)
      | Some n -> Ast.NAME_ext (n, Ast.COMP_ident ident)
  in
  let rec dig prefix_name items =
    Hashtbl.iter (extract_prog prefix_name) items
  and extract_prog prefix_name ident item =
    match item.node with
        Ast.MOD_ITEM_mod md ->
          if Array.length md.Ast.decl_params = 0
          then dig (Some (extend prefix_name ident)) md.Ast.decl_item
          else ()
      | Ast.MOD_ITEM_prog pd ->
          if Array.length pd.Ast.decl_params = 0
          then progs := (extend prefix_name ident) :: (!progs)
          else ()
      | _ -> ()
  in
    dig None crate_items;
    match !progs with
        [] -> raise (err "cannot infer main program: no programs found" ps)
      | [x] -> x
      | _ -> raise (err "cannot infer main program: multiple programs found" ps)
;;

let parse_root_with_parse_fn
    (suffix:string)
    fn
    (sess:Session.sess)
    tok
    : Ast.crate =
  let files = Hashtbl.create 0 in
  let fname = sess.Session.sess_in in
  let tref = ref (Temp 0) in
  let nref = ref (Node 0) in
  let ps = make_parser tref nref sess tok fname in
  let apos = lexpos ps in
    try
      if Filename.check_suffix fname suffix
      then fn fname (Filename.dirname fname) files ps
      else raise (err "parsing wrong kind of file" ps)
    with
        Parse_err (ps, str) ->
          Session.fail sess "Parse error: %s\n%!" str;
          List.iter
            (fun (cx,pos) ->
               Session.fail sess "%s:E (parse context): %s\n%!"
                 (Session.string_of_pos pos) cx)
            ps.pstate_ctxt;
          span ps apos apos
            { Ast.crate_items = Hashtbl.create 0;
              Ast.crate_native_items = Hashtbl.create 0;
              Ast.crate_main = Ast.NAME_base (Ast.BASE_ident "none");
              Ast.crate_files = files }

let parse_root_srcfile_entries
    (fname:string)
    (prefix:string)
    (files:(node_id,filename) Hashtbl.t)
    (ps:pstate)
    : Ast.crate =
  let stem = Filename.chop_suffix (Filename.basename fname) ".rs" in
  let apos = lexpos ps in
  let items = parse_file_mod_items ps in
  let bpos = lexpos ps in
  let modi = span ps apos bpos (Ast.MOD_ITEM_mod { Ast.decl_params = arr [];
                                                   Ast.decl_item = items })
  in
  let mitems = Hashtbl.create 0 in
    htab_put files modi.id fname;
    htab_put mitems stem modi;
    span ps apos bpos { Ast.crate_items = mitems;
                        Ast.crate_native_items = Hashtbl.create 0;
                        Ast.crate_main = infer_main_prog ps mitems;
                        Ast.crate_files = files }
;;

let parse_crate = parse_root_with_parse_fn ".rc" parse_root_crate_entries;;
let parse_srcfile = parse_root_with_parse_fn ".rs" parse_root_srcfile_entries;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
