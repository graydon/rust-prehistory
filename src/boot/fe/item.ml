
open Common;;
open Token;;
open Parser;;

(* Item grammar. *)

let default_exports =
  let e = Hashtbl.create 0 in
    Hashtbl.add e Ast.EXPORT_all_decls ();
    e
;;

let empty_view = { Ast.view_imports = Hashtbl.create 0;
                   Ast.view_exports = default_exports }
;;

let rec parse_expr (ps:pstate) : (Ast.stmt array * Ast.expr) =
  let pexp = ctxt "expr" Pexp.parse_pexp ps in
    Pexp.desugar_expr ps pexp

and parse_expr_atom (ps:pstate) : (Ast.stmt array * Ast.atom) =
  let pexp = ctxt "expr" Pexp.parse_pexp ps in
    Pexp.desugar_expr_atom ps pexp

and parse_expr_atom_list
    (bra:token)
    (ket:token)
    (ps:pstate)
    : (Ast.stmt array * Ast.atom array) =
  arj1st (bracketed_zero_or_more bra ket (Some COMMA)
            (ctxt "expr-atom list" parse_expr_atom) ps)

and parse_expr_init (lv:Ast.lval) (ps:pstate) : (Ast.stmt array) =
  let pexp = ctxt "expr" Pexp.parse_pexp ps in
    Pexp.desugar_expr_init ps lv pexp

and parse_lval (ps:pstate) : (Ast.stmt array * Ast.lval) =
  let pexp = Pexp.parse_pexp ps in
    Pexp.desugar_lval ps pexp

and parse_identified_slot_and_ident
    (aliases_ok:bool)
    (ps:pstate)
    : (Ast.slot identified * Ast.ident) =
  let slot = ctxt "identified slot and ident: slot" (Pexp.parse_identified_slot aliases_ok) ps in
  let ident = ctxt "identified slot and ident: ident" Pexp.parse_ident ps in
    (slot, ident)

and parse_two_or_more_identified_tup_slots_and_idents
    (aliases_ok:bool)
    (ps:pstate)
    : ((Ast.slot identified) array * Ast.ident array) =
  let both =
    ctxt "two+ tup slots and idents"
      (bracketed_two_or_more LPAREN RPAREN (Some COMMA) (parse_identified_slot_and_ident aliases_ok)) ps
  in
  let (slots, idents) = List.split (Array.to_list both) in
    (arr slots, arr idents)

and parse_zero_or_more_identified_slot_ident_pairs
    (aliases_ok:bool)
    (ps:pstate)
    : (((Ast.slot identified) * Ast.ident) array) =
  ctxt "zero+ tup slots and idents"
    (bracketed_zero_or_more LPAREN RPAREN (Some COMMA) (parse_identified_slot_and_ident aliases_ok)) ps

and parse_block (ps:pstate) : Ast.block =
  let apos = lexpos ps in
  let stmts = arj (ctxt "block: stmts"
                     (bracketed_zero_or_more LBRACE RBRACE None parse_stmts) ps)
  in
  let bpos = lexpos ps in
    span ps apos bpos stmts

and parse_block_stmt (ps:pstate) : Ast.stmt =
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

and parse_slot_and_ident_and_init
    (ps:pstate)
    : (Ast.stmt array * Ast.slot * Ast.ident) =
  let apos = lexpos ps in
  let (slot, ident) =
    ctxt "slot, ident and init: slot and ident"
      (Pexp.parse_slot_and_ident false) ps
  in
  let bpos = lexpos ps in
  let lval = Ast.LVAL_base (span ps apos bpos (Ast.BASE_ident ident)) in
  let stmts = ctxt "slot, ident and init: init" (parse_init lval) ps in
    (stmts, slot, ident)

and parse_auto_slot_and_init
    (ps:pstate)
    : (Ast.stmt array * Ast.slot * Ast.ident) =
  let apos = lexpos ps in
  let ident = Pexp.parse_ident ps in
  let bpos = lexpos ps in
  let lval = Ast.LVAL_base (span ps apos bpos (Ast.BASE_ident ident)) in
  let stmts = ctxt "slot, ident and init: init" (parse_init lval) ps in
    (stmts, slot_auto, ident)

(*
 * We have no way to parse a single Ast.stmt; any incoming syntactic statement
 * may desugar to N>1 real Ast.stmts
 *)

and parse_stmts (ps:pstate) : Ast.stmt array =
  let apos = lexpos ps in
    match peek ps with

        LOG ->
          bump ps;
          let (stmts, atom) = ctxt "stmts: log value" parse_expr_atom ps in
            expect ps SEMI;
            spans ps stmts apos (Ast.STMT_log atom)

      | CHECK ->
          bump ps;
          begin

            let rec name_to_lval (bpos:pos) (name:Ast.name)
                : Ast.lval =
              match name with
                  Ast.NAME_base nb ->
                    Ast.LVAL_base (span ps apos bpos nb)
                | Ast.NAME_ext (n, nc) ->
                    Ast.LVAL_ext (name_to_lval bpos n, Ast.COMP_named nc)
            in

            let rec carg_path_to_lval (bpos:pos) (path:Ast.carg_path)
                : Ast.lval =
              match path with
                  Ast.CARG_base Ast.BASE_formal ->
                    raise (err "converting formal constraint-arg to atom" ps)
                | Ast.CARG_base (Ast.BASE_named nb) ->
                    Ast.LVAL_base (span ps apos bpos nb)
                | Ast.CARG_ext (pth, nc) ->
                    Ast.LVAL_ext (carg_path_to_lval bpos pth, Ast.COMP_named nc)
            in

            let carg_to_atom (bpos:pos) (carg:Ast.carg)
                : Ast.atom =
              match carg with
                  Ast.CARG_lit lit ->
                    Ast.ATOM_literal (span ps apos bpos lit)
                | Ast.CARG_path pth ->
                    Ast.ATOM_lval (carg_path_to_lval bpos pth)
            in

            let synthesise_check_call (bpos:pos) (constr:Ast.constr)
                : (Ast.lval * (Ast.atom array)) =
              let lval = name_to_lval bpos constr.Ast.constr_name in
              let args = Array.map (carg_to_atom bpos) constr.Ast.constr_args in
                (lval, args)
            in

            let synthesise_check_calls (bpos:pos) (constrs:Ast.constrs)
                : Ast.check_calls =
              Array.map (synthesise_check_call bpos) constrs
            in

              match peek ps with
                  LPAREN ->
                    bump ps;
                    let (stmts, expr) = ctxt "stmts: check value" parse_expr ps in
                      expect ps RPAREN;
                      expect ps SEMI;
                      spans ps stmts apos (Ast.STMT_check_expr expr)

                | IF ->
                    bump ps;
                    expect ps LPAREN;
                    let constrs = Pexp.parse_constrs ps in
                      expect ps RPAREN;
                      let block = parse_block ps in
                      let bpos = lexpos ps in
                      let calls = synthesise_check_calls bpos constrs in
                        [| span ps apos bpos (Ast.STMT_check_if (constrs, calls, block)) |]

                | _ ->
                    let constrs = Pexp.parse_constrs ps in
                      expect ps SEMI;
                      let bpos = lexpos ps in
                      let calls = synthesise_check_calls bpos constrs in
                        [| span ps apos bpos (Ast.STMT_check (constrs, calls)) |]
          end

      | ALT ->
          bump ps;
          begin
            match peek ps with
                TYPE -> [| |]
              | LPAREN ->
                  let (stmts, lval) = bracketed LPAREN RPAREN parse_lval ps in
                  let parse_tag_pat ps =
                    match peek ps with
                        IDENT ident ->
                          bump ps;
                          ident,
                          begin
                            match peek ps with
                                LPAREN ->
                                (parse_zero_or_more_identified_slot_ident_pairs
                                  false ps)
                              | _ -> [| |]
                          end
                      | tok -> raise (Parse_err (ps,
                          "Expected tag constructor but found '" ^
                          (string_of_tok tok) ^ "'"))
                  in
                  let rec parse_arms ps =
                    match peek ps with
                        CASE ->
                          bump ps;
                          begin
                            match bracketed LPAREN RPAREN parse_tag_pat ps with
                              (tag_cons, tag_vars) ->
                                let block = parse_block ps in
                                let arm = (tag_cons, tag_vars, block) in
                                (span ps apos (lexpos ps) arm)::(parse_arms ps)
                          end
                      | _ -> []
                  in
                  let parse_alt_block ps =
                    let arms = ctxt "alt tag arms" parse_arms ps in
                    spans ps stmts apos begin
                      Ast.STMT_alt_tag {
                        Ast.alt_tag_lval = lval;
                        Ast.alt_tag_arms = Array.of_list arms
                      }
                    end
                  in
                  bracketed LBRACE RBRACE parse_alt_block ps
              | _ -> [| |]
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
            spans ps stmts apos
              (Ast.STMT_if
                 { Ast.if_test = expr;
                   Ast.if_then = then_block;
                   Ast.if_else = else_block; })

      | FOR ->
          bump ps;
          begin
            match peek ps with
                EACH ->
                  bump ps;
                  let inner ps : ((Ast.slot identified * Ast.ident)
                                  * Ast.stmt array
                                  * (Ast.lval * Ast.atom array)) =
                    let slot = (parse_identified_slot_and_ident true ps) in
                    let _    = (expect ps EQ) in
                    let (stmts1, iter) = (rstr true parse_lval) ps in
                    let (stmts2, args) = parse_expr_atom_list LPAREN RPAREN ps in
                      (slot, Array.append stmts1 stmts2, (iter, args))
                  in
                  let (slot, stmts, call) = ctxt "stmts: foreach head"
                    (bracketed LPAREN RPAREN inner) ps
                  in
                  let body_block = ctxt "stmts: foreach body" parse_block ps in
                  let bpos = lexpos ps in
                    Array.append stmts [| span ps apos bpos
                                            (Ast.STMT_for_each
                                               { Ast.for_each_slot = slot;
                                                 Ast.for_each_call = call;
                                                 Ast.for_each_body = body_block; }) |]
              | _ ->
                  let inner ps =
                    let slot = (parse_identified_slot_and_ident false ps) in
                    let _    = (expect ps IN) in
                    let lval = (parse_lval ps) in
                      (slot, lval) in
                  let (slot, seq) = ctxt "stmts: for head" (bracketed LPAREN RPAREN inner) ps in
                  let body_block = ctxt "stmts: for body" parse_block ps in
                  let bpos = lexpos ps in
                    [| span ps apos bpos
                         (Ast.STMT_for
                            { Ast.for_slot = slot;
                              Ast.for_seq = seq;
                              Ast.for_body = body_block; }) |]
          end

      | WHILE ->
          bump ps;
          let (stmts, test) = ctxt "stmts: while cond" (bracketed LPAREN RPAREN parse_expr) ps in
          let body_block = ctxt "stmts: while body" parse_block ps in
          let bpos = lexpos ps in
            [| span ps apos bpos
                 (Ast.STMT_while
                    { Ast.while_lval = (stmts, test);
                      Ast.while_body = body_block; }) |]

      | PUT ->
          begin
            bump ps;
            match peek ps with
                EACH ->
                  bump ps;
                  let (lstmts, lval) = ctxt "put each: lval" (rstr true parse_lval) ps in
                  let (astmts, args) = ctxt "put each: args" (parse_expr_atom_list LPAREN RPAREN) ps in
                  let bpos = lexpos ps in
                  let be = span ps apos bpos (Ast.STMT_put_each (lval, args)) in
                    expect ps SEMI;
                    Array.concat [ lstmts; astmts; [| be |] ]

              | _ ->
                  begin
                    let (stmts, e) =
                      match peek ps with
                          SEMI -> (arr [], None)
                        | _ ->
                            let (stmts, expr) = ctxt "stmts: put expr" parse_expr_atom ps in
                              expect ps SEMI;
                              (stmts, Some expr)
                    in
                      spans ps stmts apos (Ast.STMT_put e)
                  end
          end

      | RET ->
          bump ps;
          let (stmts, e) =
            match peek ps with
                SEMI -> (bump ps; (arr [], None))
              | _ ->
                  let (stmts, expr) = ctxt "stmts: ret expr" parse_expr_atom ps in
                    expect ps SEMI;
                    (stmts, Some expr)
          in
            spans ps stmts apos (Ast.STMT_ret e)

      | BE ->
          bump ps;
          let (lstmts, lval) = ctxt "be: lval" (rstr true parse_lval) ps in
          let (astmts, args) = ctxt "be: args" (parse_expr_atom_list LPAREN RPAREN) ps in
          let bpos = lexpos ps in
          let be = span ps apos bpos (Ast.STMT_be (lval, args)) in
            expect ps SEMI;
            Array.concat [ lstmts; astmts; [| be |] ]

      | LBRACE -> [| ctxt "stmts: block" parse_block_stmt ps |]

      | LET ->
          bump ps;
          begin
            match peek ps with
               LPAREN ->
                 begin
                   bump ps;
                   match peek ps with
                       RPAREN ->
                         bump ps;
                         let slot = slot_nil in
                         let ident = Pexp.parse_ident ps in
                         let bpos = lexpos ps in
                         let lval = Ast.LVAL_base (span ps apos bpos (Ast.BASE_ident ident)) in
                         let stmts = ctxt "nil-slot init: init" (parse_init lval) ps in
                         let slot = Pexp.apply_mutability ps slot Ast.MUTABLE in
                         let bpos = lexpos ps in
                         let decl = Ast.DECL_slot (Ast.KEY_ident ident,
                                                   (span ps apos bpos slot))
                         in
                           Array.concat [[| span ps apos bpos (Ast.STMT_decl decl) |]; stmts]
                     | _ ->
                         begin

                           let (slots, idents) =
                             ctxt "stmt tup decl: slots and idents"
                               (bracketed LPAREN RPAREN (parse_two_or_more_identified_tup_slots_and_idents false)) ps
                           in
                           let bpos = lexpos ps in
                           let (_, tmp, tempdecl) =
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
                               let copy = span ps apos bpos
                                 (Ast.STMT_copy (dst_lval, Ast.EXPR_atom src_atom)) in
                                 copies := copy :: (!copies)
                             end;
                             let slot = {slot with node = Pexp.apply_mutability ps slot.node Ast.MUTABLE} in
                             let decl = Ast.DECL_slot (Ast.KEY_ident idents.(i), slot) in
                               span ps apos bpos (Ast.STMT_decl decl)
                           in
                           let letdecls = Array.mapi makedecl slots in
                             Array.concat [stmts; [| tempdecl |]; letdecls; arl (!copies)]
                         end
                 end

             | _ ->
                 let (stmts, slot, ident) =
                   ctxt "stmt slot" parse_slot_and_ident_and_init ps in
                 let slot = Pexp.apply_mutability ps slot Ast.MUTABLE in
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
          let slot = Pexp.apply_mutability ps slot Ast.MUTABLE in
          let bpos = lexpos ps in
          let decl = Ast.DECL_slot (Ast.KEY_ident ident,
                                    (span ps apos bpos slot))
          in
            Array.concat [[| span ps apos bpos (Ast.STMT_decl decl) |]; stmts]

      | YIELD ->
          bump ps;
          expect ps SEMI;
          let bpos = lexpos ps in
            [| span ps apos bpos Ast.STMT_yield |]

      | FAIL ->
          bump ps;
          expect ps SEMI;
          let bpos = lexpos ps in
            [| span ps apos bpos Ast.STMT_fail |]

      | JOIN ->
          bump ps;
          let (stmts, lval) = ctxt "stmts: task expr" parse_lval ps in
            expect ps SEMI;
            spans ps stmts apos (Ast.STMT_join lval)

      | MOD | OBJ | TYPE | (FN _) | PRED | USE | NATIVE ->
          let (ident, item) = ctxt "stmt: decl" parse_mod_item ps in
          let decl = Ast.DECL_mod_item (ident, item) in
          let stmts = expand_tags_to_stmts ps item in
            spans ps stmts apos (Ast.STMT_decl decl)

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

          let (_, tmp, tempdecl) =
            build_tmp ps slot_auto apos bpos in
          let copy = span ps apos bpos
            (Ast.STMT_copy (tmp, Ast.EXPR_atom atom)) in
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

                | OPEQ binop_token ->
                    bump ps;
                    let (stmts, rhs) = ctxt "stmt: opeq rhs" parse_expr_atom ps in
                    let binop =
                      match binop_token with
                          PLUS    -> Ast.BINOP_add
                        | MINUS   -> Ast.BINOP_sub
                        | STAR    -> Ast.BINOP_mul
                        | SLASH   -> Ast.BINOP_div
                        | PERCENT -> Ast.BINOP_mod
                        | AND     -> Ast.BINOP_and
                        | OR      -> Ast.BINOP_or
                        | LSL     -> Ast.BINOP_lsl
                        | LSR     -> Ast.BINOP_lsr
                        | ASR     -> Ast.BINOP_asr
                        | _       -> raise (err "unknown opeq token" ps)
                    in
                      expect ps SEMI;
                      spans ps stmts apos (Ast.STMT_copy_binop (lval, binop, rhs))

                | LARROW ->
                    bump ps;
                    let (stmts, rhs) = ctxt "stmt: recv rhs" parse_lval ps in
                    let _ = expect ps SEMI in
                      spans ps stmts apos (Ast.STMT_recv (lval, rhs))

                | SEND ->
                    bump ps;
                    let (stmts, rhs) = ctxt "stmt: send rhs" parse_expr_atom ps in
                    let _ = expect ps SEMI in
                    let bpos = lexpos ps in
                    let (src, copy) = match rhs with
                        Ast.ATOM_lval lv -> (lv, [| |])
                      | _ ->
                          let (_, tmp, tempdecl) = build_tmp ps slot_auto apos bpos in
                          let copy = span ps apos bpos
                            (Ast.STMT_copy (tmp, Ast.EXPR_atom rhs)) in
                              ((clone_lval ps tmp), [| tempdecl; copy |])
                    in
                    let send = span ps apos bpos (Ast.STMT_send (lval, src)) in
                      Array.concat [ stmts; copy; [| send |] ]

                | _ -> raise (unexpected ps)
            end


and parse_ty_param (iref:int ref) (ps:pstate) : Ast.ty_param identified =
  let apos = lexpos ps in
  let mut = Pexp.parse_mutability ps in
  let ident = Pexp.parse_ident ps in
  let i = !iref in
  let bpos = lexpos ps in
    incr iref;
    span ps apos bpos (ident, (i, mut))

and parse_ty_params (ps:pstate)
    : (Ast.ty_param identified) array =
  match peek ps with
      LBRACKET ->
        bracketed_zero_or_more LBRACKET RBRACKET (Some COMMA) (parse_ty_param (ref 0)) ps
    | _ -> arr []

and parse_ident_and_params (ps:pstate) (cstr:string)
    : (Ast.ident * (Ast.ty_param identified) array) =
  let ident = ctxt ("mod " ^ cstr ^ " item: ident") Pexp.parse_ident ps in
  let params = ctxt ("mod " ^ cstr ^ " item: type params") parse_ty_params ps in
    (ident, params)

and parse_inputs
    (ps:pstate)
    : ((Ast.slot identified * Ast.ident) array * Ast.constrs)  =
  let slots =
    match peek ps with
        LPAREN -> ctxt "inputs: input idents and slots"
          (parse_zero_or_more_identified_slot_ident_pairs true) ps
      | _ -> raise (unexpected ps)
  in
  let constrs =
    match peek ps with
        COLON -> (bump ps; ctxt "inputs: constrs" Pexp.parse_constrs ps)
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


and parse_in_and_out
    (ps:pstate)
    : ((Ast.slot identified * Ast.ident) array * Ast.constrs * Ast.slot identified) =
  let (inputs, constrs) = parse_inputs ps in
  let output =
    match peek ps with
        RARROW ->
          bump ps;
          ctxt "fn in and out: output slot" (Pexp.parse_identified_slot true) ps
      | _ ->
          let apos = lexpos ps in
            span ps apos apos slot_nil
  in
    (inputs, constrs, output)


(* parse_fn starts at the first lparen of the sig. *)
and parse_fn
    (is_iter:bool)
    (pure:Ast.purity)
    (ps:pstate)
    : Ast.fn =
    let (inputs, constrs, output) = ctxt "fn: in_and_out" parse_in_and_out ps in
    let body = ctxt "fn: body" parse_block ps in
      { Ast.fn_input_slots = inputs;
        Ast.fn_input_constrs = constrs;
        Ast.fn_output_slot = output;
        Ast.fn_aux = { Ast.fn_purity = pure;
                       Ast.fn_is_iter = is_iter; };
        Ast.fn_body = body; }

and parse_pred (ps:pstate) : Ast.pred =
  let (inputs, constrs) = ctxt "pred: inputs" parse_inputs ps in
  let body = ctxt "pred: body" parse_block ps in
    { Ast.pred_input_slots = inputs;
      Ast.pred_input_constrs = constrs;
      Ast.pred_body = body }

and parse_meta_input (ps:pstate) : (Ast.ident * string option) =
  let lab = (ctxt "meta input: label" Pexp.parse_ident ps) in
    match peek ps with
        EQ ->
          bump ps;
          let v =
            match peek ps with
                UNDERSCORE -> bump ps; None
              | LIT_STR s -> bump ps; Some s
              | _ -> raise (unexpected ps)
          in
            (lab, v)
      | _ -> raise (unexpected ps)

and parse_meta_pat (ps:pstate) : Ast.meta_pat =
  bracketed_zero_or_more LPAREN RPAREN
    (Some COMMA) parse_meta_input ps

and parse_meta (ps:pstate) : Ast.meta =
  Array.map
    begin
      fun (id,v) ->
        match v with
            None ->
              raise (err "wildcard found in meta pattern where value expected" ps)
          | Some v -> (id,v)
    end
    (parse_meta_pat ps)

and parse_optional_meta_pat (ps:pstate) (ident:Ast.ident) : Ast.meta_pat =
  match peek ps with
      LPAREN -> parse_meta_pat ps
    | _ -> [| ("name", Some ident) |]

and parse_mod_item (ps:pstate) : (Ast.ident * Ast.mod_item) =
  let apos = lexpos ps in
  let pure =
    match peek ps with
        PURE -> (bump ps; Ast.PURE)
      | MUTABLE -> (bump ps; Ast.IMPURE Ast.MUTABLE)
      | _ -> Ast.IMPURE Ast.IMMUTABLE
  in
  let parse_lib_name ident =
    match peek ps with
        EQ ->
          begin
            bump ps;
            match peek ps with
                LIT_STR s -> (bump ps; s)
              | _ -> raise (unexpected ps)
          end
      | _ -> ps.pstate_infer_lib_name ident
  in

    match peek ps with

        FN | ITER ->
          let is_iter = (peek ps) = ITER in
            bump ps;
            let (ident, params) = parse_ident_and_params ps "fn" in
            let fn = ctxt "mod fn item: fn" (parse_fn is_iter pure) ps in
            let bpos = lexpos ps in
              (ident,
               span ps apos bpos
                 (decl params (Ast.MOD_ITEM_fn fn)))

      | PRED ->
          bump ps;
          let (ident, params) = parse_ident_and_params ps "pred" in
          let pred = ctxt "mod pred item: pred" parse_pred ps in
          let bpos = lexpos ps in
            (ident,
             span ps apos bpos
               (decl params (Ast.MOD_ITEM_pred pred)))

      | TYPE ->
          bump ps;
          let (ident, params) = parse_ident_and_params ps "type" in
          let _ = expect ps EQ in
          let ty = ctxt "mod type item: ty" Pexp.parse_ty ps in
          let _ = expect ps SEMI in
          let bpos = lexpos ps in
          let item = Ast.MOD_ITEM_type ty in
            (ident, span ps apos bpos (decl params item))

      | MOD ->
          bump ps;
          let (ident, params) = parse_ident_and_params ps "mod" in
            expect ps LBRACE;
            let items = parse_mod_items ps RBRACE in
            let bpos = lexpos ps in
              (ident,
               span ps apos bpos
                 (decl params (Ast.MOD_ITEM_mod items)))

      | NATIVE ->
          begin
            bump ps;
            let conv =
              match peek ps with
                  LIT_STR s ->
                    bump ps;
                    begin
                      match string_to_conv s with
                          None -> raise (unexpected ps)
                        | Some c -> c
                    end
                | _ -> CONV_cdecl
            in
              expect ps MOD;
              let (ident, params) = parse_ident_and_params ps "native mod" in
              let path = parse_lib_name ident in
              let items = parse_mod_items_from_signature ps in
              let bpos = lexpos ps in
              let rlib = REQUIRED_LIB_c { required_libname = path;
                                          required_prefix = ps.pstate_depth }
              in
              let item = decl params (Ast.MOD_ITEM_mod items) in
              let item = span ps apos bpos item in
                note_required_mod ps {lo=apos; hi=bpos} conv rlib item;
                (ident, item)
          end

      | USE ->
          begin
            bump ps;
            let ident = ctxt "use mod: ident" Pexp.parse_ident ps in
            let meta = ctxt "use mod: meta" parse_optional_meta_pat ps ident in
            let bpos = lexpos ps in
            let id = (span ps apos bpos ()).id in
            let (path, items) =
              ps.pstate_get_mod meta id ps.pstate_node_id ps.pstate_opaque_id
            in
            let bpos = lexpos ps in
              expect ps SEMI;
              let rlib = REQUIRED_LIB_rust { required_libname = path;
                                             required_prefix = ps.pstate_depth }
              in
                iflog ps
                  begin
                    fun _ ->
                      log ps "extracted mod from %s (binding to %s)" path ident;
                      log ps "%a" Ast.sprintf_mod_items items;
                  end;
                let item = decl [||] (Ast.MOD_ITEM_mod (empty_view, items)) in
                let item = span ps apos bpos item in
                  note_required_mod ps {lo=apos; hi=bpos} CONV_rust rlib item;
                  (ident, item)
          end

      | OBJ ->
          begin
            bump ps;
            let (ident, params) = parse_ident_and_params ps "obj" in
            let (state, constrs) = (ctxt "obj state" parse_inputs ps) in
              expect ps LBRACE;
              let fns = Hashtbl.create 0 in
                while (not (peek ps = RBRACE))
                do
                  let apos = lexpos ps in
                  let pure =
                    match peek ps with
                        PURE -> (bump ps; Ast.PURE)
                      | MUTABLE -> (bump ps; Ast.IMPURE Ast.MUTABLE)
                      | _ -> Ast.IMPURE Ast.IMMUTABLE
                  in
                    match peek ps with
                        FN | ITER ->
                          let is_iter = (peek ps) = ITER in
                            bump ps;
                            let ident = ctxt "obj fn: ident" Pexp.parse_ident ps in
                            let fn = ctxt "obj fn: fn" (parse_fn is_iter pure) ps in
                            let bpos = lexpos ps in
                              htab_put fns ident (span ps apos bpos fn)
                      | RBRACE -> ()
                      | _ -> raise (unexpected ps)
                done;
                expect ps RBRACE;
                let bpos = lexpos ps in
                let obj = { Ast.obj_state = state;
                            Ast.obj_constrs = constrs;
                            Ast.obj_fns = fns }
                in
                  (ident,
                   span ps apos bpos
                     (decl params (Ast.MOD_ITEM_obj obj)))
          end


      | _ -> raise (unexpected ps)


and parse_mod_items_from_signature (ps:pstate) : (Ast.mod_view * Ast.mod_items) =
    let mis = Hashtbl.create 0 in
      expect ps LBRACE;
      while not (peek ps = RBRACE)
      do
        let (ident, mti) = ctxt "mod items from sig: mod item"
          parse_mod_item_from_signature ps
        in
          Hashtbl.add mis ident mti;
      done;
      expect ps RBRACE;
      (empty_view, mis)


and parse_mod_item_from_signature (ps:pstate)
    : (Ast.ident * Ast.mod_item) =
  let apos = lexpos ps in
    match peek ps with
        MOD ->
          bump ps;
          let (ident, params) = parse_ident_and_params ps "mod signature" in
          let items = parse_mod_items_from_signature ps in
          let bpos = lexpos ps in
          (ident, span ps apos bpos (decl params (Ast.MOD_ITEM_mod items)))

    | FN | ITER ->
        let is_iter = (peek ps) = ITER in
          bump ps;
          let (ident, params) = parse_ident_and_params ps "fn signature" in
          let (inputs, constrs, output) = parse_in_and_out ps in
            let bpos = lexpos ps in
            let body = span ps apos bpos [| |] in
            let fn =
              Ast.MOD_ITEM_fn
                { Ast.fn_input_slots = inputs;
                  Ast.fn_input_constrs = constrs;
                  Ast.fn_output_slot = output;
                  Ast.fn_aux = { Ast.fn_purity = Ast.IMPURE Ast.IMMUTABLE;
                                 Ast.fn_is_iter = is_iter; };
                  Ast.fn_body = body; }
            in
            let node = span ps apos bpos (decl params fn) in
              begin
                match peek ps with
                    EQ ->
                      bump ps;
                      begin
                        match peek ps with
                            LIT_STR s ->
                              bump ps;
                              htab_put ps.pstate_required_syms node.id s
                          | _ -> raise (unexpected ps)
                      end;
                  | _ -> ()
              end;
              expect ps SEMI;
              (ident, node)

    | TYPE ->
        bump ps;
        let (ident, params) = parse_ident_and_params ps "type type" in
        let t =
          match peek ps with
              SEMI -> Ast.TY_native (next_opaque_id ps)
            | _ -> Pexp.parse_ty ps
        in
          expect ps SEMI;
          let bpos = lexpos ps in
            (ident, span ps apos bpos (decl params (Ast.MOD_ITEM_type t)))

    (* FIXME: parse pred, obj. *)
    | _ -> raise (unexpected ps)


and expand_tags
    (ps:pstate)
    (item:Ast.mod_item)
    : (Ast.ident * Ast.mod_item) array =
  let handle_ty_tag id ttag =
    let tags = ref [] in
      Hashtbl.iter
        begin
          fun name tup ->
            let ident = match name with
                Ast.NAME_base (Ast.BASE_ident ident) -> ident
              | _ -> raise (Parse_err (ps, "unexpected name type while expanding tag"))
            in
            let header = Array.map (fun slot -> (clone_span ps item slot)) tup in
            let tag_item' = Ast.MOD_ITEM_tag (header, ttag, id) in
            let tag_item = clone_span ps item (decl item.node.Ast.decl_params tag_item') in
              tags := (ident, tag_item) :: (!tags)
        end
        ttag;
      arr (!tags)
  in
  let handle_ty_decl id tyd =
    match tyd with
        Ast.TY_tag ttag -> handle_ty_tag id ttag
      | _ -> [| |]
  in
    match item.node.Ast.decl_item with
        Ast.MOD_ITEM_type tyd -> handle_ty_decl item.id tyd
      | _ -> [| |]


and expand_tags_to_stmts
    (ps:pstate)
    (item:Ast.mod_item)
    : Ast.stmt array =
  let id_items = expand_tags ps item in
    Array.map
      (fun (ident, tag_item) ->
         clone_span ps item
           (Ast.STMT_decl
              (Ast.DECL_mod_item (ident, tag_item))))
      id_items


and expand_tags_to_items
    (ps:pstate)
    (item:Ast.mod_item)
    (items:Ast.mod_items)
    : unit =
  let id_items = expand_tags ps item in
    Array.iter
      (fun (ident, item) -> htab_put items ident item)
      id_items


and note_required_mod
    (ps:pstate)
    (sp:span)
    (conv:nabi_conv)
    (rlib:required_lib)
    (item:Ast.mod_item)
    : unit =
  iflog ps (fun _ -> log ps "marking item #%d as required" (int_of_node item.id));
  htab_put ps.pstate_required item.id (rlib, conv);
  if not (Hashtbl.mem ps.pstate_sess.Session.sess_spans item.id)
  then Hashtbl.add ps.pstate_sess.Session.sess_spans item.id sp;
  match item.node.Ast.decl_item with
      Ast.MOD_ITEM_mod (_, items) ->
        Hashtbl.iter
          begin
            fun _ sub ->
              note_required_mod ps sp conv rlib sub
          end
          items
    | _ -> ()


and parse_import
    (ps:pstate)
    (imports:(Ast.ident, Ast.name) Hashtbl.t)
    : unit =
  let import a n =
    let a = match a with
        None ->
          begin
            match n with
                Ast.NAME_ext (_, Ast.COMP_ident i)
              | Ast.NAME_ext (_, Ast.COMP_app (i, _))
              | Ast.NAME_base (Ast.BASE_ident i)
              | Ast.NAME_base (Ast.BASE_app (i, _)) -> i
              | _ -> raise (Parse_err (ps, "bad import specification"))
          end
      | Some i -> i
    in
      Hashtbl.add imports a n
  in
    match peek ps with
        IDENT i ->
          begin
            bump ps;
            match peek ps with
                EQ ->
                  (* 
                   * import x = ...
                   *)
                  bump ps;
                  import (Some i) (Pexp.parse_name ps)
              | _ ->
                  (*
                   * import x...
                   *)
                  import None (Pexp.parse_name_ext ps
                                 (Ast.NAME_base
                                    (Ast.BASE_ident i)))
          end
      | _ ->
          import None (Pexp.parse_name ps)


and parse_export
    (ps:pstate)
    (exports:(Ast.export, unit) Hashtbl.t)
    : unit =
  let e =
    match peek ps with
        STAR -> bump ps; Ast.EXPORT_all_decls
      | IDENT i -> bump ps; Ast.EXPORT_ident i
      | _ -> raise (unexpected ps)
  in
    Hashtbl.add exports e ()


and parse_mod_items
    (ps:pstate)
    (terminal:token)
    : (Ast.mod_view * Ast.mod_items) =
  ps.pstate_depth <- ps.pstate_depth + 1;
  let imports = Hashtbl.create 0 in
  let exports = Hashtbl.create 0 in
  let in_view = ref true in
  let items = Hashtbl.create 4 in
    while (not (peek ps = terminal))
    do
      if !in_view
      then
        match peek ps with
            IMPORT ->
              bump ps;
              parse_import ps imports;
              expect ps SEMI;
          | EXPORT ->
              bump ps;
              parse_export ps exports;
              expect ps SEMI;
          | _ ->
              in_view := false
      else
        let (ident, item) = parse_mod_item ps in
          htab_put items ident item;
          expand_tags_to_items ps item items;
    done;
    if (Hashtbl.length exports) = 0
    then Hashtbl.add exports Ast.EXPORT_all_decls ();
    expect ps terminal;
    ps.pstate_depth <- ps.pstate_depth - 1;
    let view = { Ast.view_imports = imports;
                 Ast.view_exports = exports }
    in
      (view, items)
;;



(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
