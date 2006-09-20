%{

(* Header *)

open Ast;;

let ty_NIL:ty_tup = { tup_types = Array.of_list [];
		      tup_state = Array.of_list [] }
;;

let val_NIL:val_tup = Array.of_list []
;;

let numty n =
  match n with 
    Num.Ratio _ -> TY_rat
  | _           -> TY_int
;;

let smallnum n =
  match n with
    Num.Int i -> i
  | _         -> raise Parse_error
;;

let anonymize_tuple (tupty, names) =
  let dict = Hashtbl.create 100 in
  let name_to_tupidx name = 
    try
      COMP_tupidx (Hashtbl.find dict name)
    with      
      Not_found -> raise Parse_error
  in
  let bind_parg parg =
    match parg.parg_base with 
      PARG_formal -> parg
    | PARG_free name -> 
	let head = name_to_tupidx name in
	let tail = Array.to_list parg.parg_rest in
	{ parg with 
	  parg_rest = Array.of_list (head :: tail) }
  in
  let bind_pred pred =
    { pred with 
      pred_args = Array.map bind_parg pred.pred_args }
  in
  let n = ref 0 in
  List.iter (fun name -> Hashtbl.add dict name !n; incr n) names;  
  { tupty with 
    tup_state = Array.map bind_pred tupty.tup_state }
;;

%}

/* Declarations */

%token EOF

/* Expression nodes that reduce to overridable 2 or 3-operand calls. */
%token <Ast.rs_pos> PLUS MINUS STAR SLASH PERCENT
%token <Ast.rs_pos> EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
%token <Ast.rs_pos> LT LE EQEQ NE GE GT 
%token <Ast.rs_pos> NOT AND OR LSL LSR ASR

/**************************************************************/
/* No user-overriding operators or symbols beyond this line.  */
/**************************************************************/

/* Structural symbols, erased in AST so no position. */
%token CARET DOT COMMA SEMI COLON RARROW LARROW
%token LPAREN RPAREN LBRACKET RBRACKET 
%token <Ast.rs_pos> LBRACE RBRACE 

/* Keywords for the crate and module system. */
%token <Ast.rs_pos> CRATE MOD USE PUB

/* Metaprogramming keywords. */
%token <Ast.rs_pos> SYNTAX META TILDE

/* Control-flow keywords. */
%token <Ast.rs_pos> IF ELSE WHILE FOR
%token <Ast.rs_pos> TRY FAIL INIT MAIN FINI
%token <Ast.rs_pos> YIELD RETURN

/* Type and type-state keywords. */
%token <Ast.rs_pos> TYPE PRED ASSERT

/* Type qualifiers. */
%token <Ast.rs_pos> LIM PURE

/* Declarator qualifiers. */
%token <Ast.rs_pos> PUBLIC PRIVATE AUTO INLINE


/* Magic runtime services. */
%token <Ast.rs_pos> SPAWN LOG REFLECT EVAL

/* Literals. */
%token <Num.num * Ast.rs_pos> LIT_NUM
%token <string * Ast.rs_pos> LIT_STR 
%token <char * Ast.rs_pos> LIT_CHAR 

/* Identifiers. */
%token <string * Ast.rs_pos> IDENT


/* 
 * Type constructors are copied around so much that
 * we have decided not to give them positions. It's 
 * just as likely to be wrong as right. 
 */
      
/* Basic types. */
%token NIL 
%token INT NAT RAT
%token CHAR STR
%token <int> BFP DFP SIGNED UNSIGNED

/* Algebraic type constructors. */
%token REC ALT VEC DYN

/* Callable type constructors. */
%token FUNC ITER CHAN

/* Process types. */
%token PROC PROG PORT

/* Precedences (mostly borrowed from C). */
%left OR
%left AND
%left EQ NE
%left LT LE GE GT
%left LSL LSR ASR
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT

/* Entries. */
%start sourcefile

%type <rs_expr>          literal
%type <rs_expr>          expr

%type <rs_name>          name

%type <string list>      ident_list
%type <string array>     idents

%type <rs_type>          simple_ty_expr

%type <rs_pred>                 pred
%type <rs_pred list>            pred_list
%type <rs_pred array>           preds

%type <rs_decl>              decl
%type <rs_decl_top>          qual_decl_top
%type <rs_decl_top list>     qual_decl_top_list
%type <(string, Ast.rs_decl_top) Hashtbl.t>    sourcefile

%%

/* Rules */

call:
    lval 
      LPAREN 
      exprs 
      RPAREN            { ($1, $3)                            }

  | lval 
      LPAREN 
      RPAREN            { ($1, Array.of_list [])              }
      

expr: 
    expr OR expr        { EXPR_binary (BINOP_or, $2, $1, $3)  }
  | expr AND expr       { EXPR_binary (BINOP_and, $2, $1, $3) }
      
  | expr LT expr        { EXPR_binary (BINOP_lt, $2, $1, $3)  }
  | expr LE expr        { EXPR_binary (BINOP_le, $2, $1, $3)  }
  | expr GE expr        { EXPR_binary (BINOP_ge, $2, $1, $3)  }
  | expr GT expr        { EXPR_binary (BINOP_gt, $2, $1, $3)  }

  | expr LSL expr       { EXPR_binary (BINOP_lsl, $2, $1, $3) }
  | expr LSR expr       { EXPR_binary (BINOP_lsr, $2, $1, $3) }
  | expr ASR expr       { EXPR_binary (BINOP_asr, $2, $1, $3) }

  | expr PLUS expr      { EXPR_binary (BINOP_add, $2, $1, $3) }
  | expr MINUS expr     { EXPR_binary (BINOP_sub, $2, $1, $3) }

  | expr STAR expr      { EXPR_binary (BINOP_mul, $2, $1, $3) }
  | expr SLASH expr     { EXPR_binary (BINOP_div, $2, $1, $3) }
  | expr PERCENT expr   { EXPR_binary (BINOP_mod, $2, $1, $3) }

  | NOT expr            { EXPR_unary  (UNOP_not, $1, $2)      }
      
  | call                { EXPR_call $1                        }
  | literal             { $1                                  }
  | lval                { EXPR_lval $1                        }

expr_list:
    expr COMMA expr_list      { $1 :: $3 }
  | expr                      { [$1]     }

exprs:
    expr_list                 { Array.of_list $1 }

literal:
    LIT_STR             { EXPR_literal (VAL_dyn (TY_str, VAL_str (fst $1)), 
					(snd $1))                             }

  | LIT_CHAR            { EXPR_literal (VAL_dyn (TY_char, VAL_char (fst $1)), 
					(snd $1))                             }

  | LIT_NUM             { EXPR_literal (VAL_dyn (TY_arith (numty (fst $1)),
						 VAL_arith (fst $1)), 
					(snd $1))                             }

lidx: 
    IDENT                     { LIDX_named (COMP_string (fst $1), (snd $1))       }
  | LIT_NUM                   { LIDX_named (COMP_tupidx (smallnum (fst $1)), (snd $1)) }
  | LPAREN expr RPAREN        { LIDX_index $2 }

lidx_list:
    lidx DOT lidx_list        { $1 :: $3 }
  | lidx                      { [$1]     }

lval:
    IDENT                     { { lval_base = (fst $1);
				  lval_rest = Array.of_list [] } }
  | IDENT DOT lidx_list       { { lval_base = (fst $1); 
				  lval_rest = Array.of_list $3 } }
name: 
      lval               { { name_base = $1.lval_base;
			     name_rest = 
			     Array.map (fun x -> 
                               match x with 
				 LIDX_named i -> (fst i)
			       | LIDX_index _ -> raise Parse_error) 
			     $1.lval_rest } }
      
ident_list: 
    IDENT COMMA ident_list { (fst $1) :: $3 }
  | IDENT                  { [fst $1]       }  

idents: 
  ident_list             { Array.of_list $1 }



name_comp:
    IDENT                     { COMP_string (fst $1)             }
  | LIT_NUM                   { COMP_tupidx (smallnum (fst $1))  }

name_comp_list:
    name_comp DOT name_comp_list   { $1 :: $3 }
  | name_comp                      { [$1]     }

name_comps:
    name_comp_list            { Array.of_list $1 }

pred_arg_base:
    IDENT                        { PARG_free (fst $1) }
  | STAR                         { PARG_formal  }

pred_arg:
    pred_arg_base DOT name_comps { { parg_base = $1;
				     parg_rest = $3; } }
  | pred_arg_base                { { parg_base = $1;
				     parg_rest = Array.of_list []; } }

pred_arg_list:
    pred_arg COMMA pred_arg_list { $1 :: $3 }
  | pred_arg                     { [$1]     }  

pred_args:
    pred_arg_list                { Array.of_list $1 }

pred:
    name LPAREN pred_args RPAREN { { pred_name = $1; 
				     pred_args = $3 } }

pred_list:
    pred COMMA pred_list         { $1 :: $3 }
  | pred                         { [$1]     }  
    
preds:
    pred_list                    { Array.of_list $1 }

state:
    COLON preds                  { $2 }

anonymous_tuple_type:
    simple_ty_expr_list 
                                 { { tup_types = Array.of_list $1;
				     tup_state = Array.of_list [] } }
  | NIL                          { ty_NIL }

binding:
      simple_ty_expr IDENT       { ($1,$2)  }

binding_list:
    binding COMMA binding_list   { $1 :: $3 }
  | binding                      { [$1]     }

binding_tuple_type_maybe_state:

    LPAREN binding_list RPAREN state  
      { let (types,npos) = List.split $2 in
        let (names,poss) = List.split npos in
        ({ tup_types = Array.of_list types; 
	   tup_state = $4; }, names) }

  | LPAREN binding_list RPAREN        
      { let (types,npos) = List.split $2 in
        let (names,poss) = List.split npos in
      ({ tup_types = Array.of_list types;
	 tup_state = Array.of_list []; }, names) }

anonymous_tuple_type_maybe_state:
    anonymous_tuple_type state        { {$1 with tup_state = $2} }
  | anonymous_tuple_type              { $1                       }
   
tuple_ty:
    anonymous_tuple_type_maybe_state { $1                   } 
  | binding_tuple_type_maybe_state   { (anonymize_tuple $1) }


mach_ty_expr:
    UNSIGNED             { TY_mach (TY_unsigned, $1)       }
  | SIGNED               { TY_mach (TY_signed, $1)         }
  | BFP                  { TY_mach (TY_ieee_bfp, $1)       }
  | DFP                  { TY_mach (TY_ieee_dfp, $1)       }
      
arith_ty_expr:
    INT                  { TY_arith (TY_int)               }
  | NAT                  { TY_arith (TY_nat)               }
  | RAT                  { TY_arith (TY_rat)               }

prim_ty_expr:

    mach_ty_expr         { $1                              }
  | arith_ty_expr        { $1                              }

  | STR                  { TY_str                          }
  | CHAR                 { TY_char                         }

  | PROC                 { TY_proc                         }
  | PROG                 { TY_proc                         }
  | VEC
      LBRACKET
      simple_ty_expr
      RBRACKET           { TY_vec { vec_elt_type = $3 }    }
  | name                 { TY_named $1                     }
  | prim_ty_expr 
            LBRACKET
            simple_ty_exprs
            RBRACKET     { TY_apply ($1, $3)               }
  | LPAREN 
    complex_ty_expr
    RPAREN               { $2                              }


simple_ty_expr:
          prim_ty_expr        { $1                              }
  |       prim_ty_expr CARET  { TY_ref $1                       }
  | MINUS prim_ty_expr        { TY_const $2                     }
  | MINUS prim_ty_expr CARET  { TY_const (TY_ref $2)            }

simple_ty_expr_list:
    simple_ty_expr COMMA simple_ty_expr_list  { $1 :: $3 }
  | simple_ty_expr                            { [$1]     }

simple_ty_exprs:
  simple_ty_expr_list                         { Array.of_list $1 }

subr_variety:
    FUNC  { fun r -> TY_func r }
  | ITER  { fun r -> TY_iter r }

subr_qual:

    INLINE PURE subr_variety    { fun s -> $3 { subr_inline = true; 
						subr_pure = true; 
						subr_sig = s; } }
      
  | INLINE subr_variety         { fun s -> $2 { subr_inline = true; 
						subr_pure = false; 
						subr_sig = s; } }
      
  | PURE subr_variety           { fun s -> $2 { subr_inline = false; 
						subr_pure = true; 
						subr_sig = s; } }
      
  | subr_variety                { fun s -> $1 { subr_inline = false; 
						subr_pure = false; 
						subr_sig = s; } }

rec_slot:
    simple_ty_expr IDENT SEMI   
      { { rec_slot_name = (fst $2);
	  rec_slot_type = $1;
	  rec_slot_state = Array.of_list []} }
      
  | simple_ty_expr IDENT state SEMI  
      { { rec_slot_name = (fst $2);
	  rec_slot_type = $1;
	  rec_slot_state = $3} }
				    

rec_slot_list:
    rec_slot rec_slot_list      { $1 :: $2 }
  | rec_slot                    { [$1]     }

rec_slots:
    rec_slot_list               { Array.of_list $1 }

rec_body:
    rec_slots                   { { rec_slots = $1; 
				    rec_state = Array.of_list [] } }

  | rec_slots state             { { rec_slots = $1;
				    rec_state = $2 } }

alt_case:
    IDENT                         { { alt_case_name = (fst $1); 
				      alt_case_rec = None } } 

  | IDENT LBRACE rec_body RBRACE  { { alt_case_name = (fst $1);
				      alt_case_rec = Some $3 } }

alt_case_list:
    alt_case alt_case_list        { $1 :: $2 }
  | alt_case                      { [$1]     }

alt_cases:
  alt_case_list                   { Array.of_list $1 }

complex_ty_expr:
    subr_ty                           { TY_subr $1                  }
  | tuple_ty                          { TY_tup $1                   }
  | REC LBRACE rec_body RBRACE        { TY_rec $3                   }
  | ALT LBRACE alt_cases RBRACE       { TY_alt $3                   }

stmt: 
    WHILE LPAREN expr RPAREN stmt     { STMT_while { while_expr = $3; 
						     while_body = $5;
						     while_pos = $1} }
  | IF LPAREN expr RPAREN 
      block_stmt 
      ELSE 
      block_stmt                      { STMT_if { if_test = $3;
						  if_then = $5;
						  if_else = Some $7;
						  if_pos = $1     } }

  | IF LPAREN expr RPAREN 
      stmt                            { STMT_if { if_test = $3;
						  if_then = $5;
						  if_else = None;
						  if_pos = $1     } }

  | YIELD expr SEMI                   { STMT_yield ((Some $2), $1)  }
  | YIELD SEMI                        { STMT_yield (None, $1)       }
  | RETURN expr SEMI                  { STMT_return ($2, $1)        }
  | ASSERT pred SEMI                  { STMT_assert ($2, $1)        }
  | block_stmt                        { $1                          }
  | lval LARROW lval SEMI             { STMT_move ($1, $3)          }
  | lval EQ expr SEMI                 { STMT_copy ($1, $3)          }
  | call SEMI                         { STMT_call $1                }
  | decl_slot                         { STMT_decl $1                }


stmt_list: 
    stmt stmt_list               { $1 :: $2 }
  | stmt                         { [$1]     }

block_stmt:
    LBRACE stmt_list RBRACE           { STMT_block (Array.of_list $2, $1) }
  | LBRACE RBRACE                     { STMT_block (Array.of_list [], $1) }

subr_ty:
      subr_qual
      tuple_ty
      RARROW
      anonymous_tuple_type_maybe_state
{
  $1 { sig_param_tup = $2;
       sig_result_tup = $4; }
}

sig_bind:
  binding_tuple_type_maybe_state
  RARROW 
  anonymous_tuple_type_maybe_state
{
 match $1 with (itypes, inames) -> 
   ({ sig_param_tup = itypes;
      sig_result_tup = $3 }, 
    Array.of_list inames)
}
      
subr_bind:
      sig_bind
  {
    match $1 with (sigt, inames) -> 
      fun qualfn ->
	{ 
	  bind_subr = qualfn sigt;
	  bind_names = inames;
	}
 }


/* members of "decl" are constants, OK in nearly every context */

decl:
    TYPE IDENT EQ simple_ty_expr SEMI
      {  { decl_name = (fst $2);
	   decl_type = TY_type;
	   decl_value = VAL_dyn (TY_tup ty_NIL, VAL_tup val_NIL);
	   decl_state = Array.of_list [];
	   decl_pos = (snd $2)                       } }
      
  | subr_qual IDENT subr_bind block_stmt 
      {  let bs = $3 $1 in
      { decl_name = (fst $2); 
	decl_type = TY_subr bs.bind_subr;
	decl_value = VAL_dyn (TY_tup ty_NIL, VAL_tup val_NIL);
	decl_state = Array.of_list [];
	decl_pos = (snd $2)                       } }

decl_slot:
    decl  { $1 }

  | simple_ty_expr IDENT SEMI
      { { decl_name = (fst $2);
	  decl_type = $1;
	  decl_value = VAL_dyn (TY_tup ty_NIL, VAL_tup val_NIL);
	  decl_state = Array.of_list [];
	  decl_pos = (snd $2); } }

  | simple_ty_expr IDENT state SEMI
      { { decl_name = (fst $2);
	  decl_type = $1;
	  decl_value = VAL_dyn (TY_tup ty_NIL, VAL_tup val_NIL);
	  decl_state = $3;
	  decl_pos = (snd $2); } }


prog_item:
    decl_slot                   { (fun (p, b) -> (p, $1 :: b))        }
  | INIT sig_bind block_stmt    
      { (fun (p, b) -> ({p with prog_init = Some ($2, $3)}, b))       }
  | MAIN block_stmt    
      { (fun (p, b) -> ({p with prog_main = Some $2}, b))             }
  | FINI block_stmt    
      { (fun (p, b) -> ({p with prog_fini = Some $2}, b))             }

prog_items:
    prog_items prog_item        { fun pb -> $2 ($1 pb)                }
  | prog_item                   { $1                                  }

prog_body:
    LBRACE prog_items RBRACE 
      { let p = { prog_auto = false;
		  prog_init = None;
		  prog_main = None;
		  prog_fini = None;
		  prog_decls = Array.of_list [] } in      
        let (pp,b) = ($2 (p,[])) in
	{ pp with prog_decls = Array.of_list b }
      }

  | LBRACE RBRACE                   
      { let p = { prog_auto = false;
		  prog_init = None;
		  prog_main = None;
		  prog_fini = None;
		  prog_decls = Array.of_list [] }
      in p }

prog_head:
    AUTO PROG                   { fun p -> { p with prog_auto = true; } }
  | PROG                        { fun p -> p }

decl_top:
    decl 
      { $1 }

  | prog_head IDENT prog_body    
      {  { decl_name = (fst $2);
	   decl_type = TY_prog;
	   decl_value = VAL_dyn (TY_prog, VAL_prog ($1 $3));
	   decl_state = Array.of_list [];
	   decl_pos = (snd $2); } }
  

qual_decl_top:
    PUBLIC decl_top       { (VIS_public, $2)   }
  | PRIVATE decl_top      { (VIS_private, $2)  }
  | decl_top              { (VIS_standard, $1) }

qual_decl_top_list:
    qual_decl_top qual_decl_top_list              { $1 :: $2 }
  | qual_decl_top                                 { [$1]     }  

sourcefile:
   qual_decl_top_list EOF      
      { 
	let bindings = Hashtbl.create 100 in
	List.iter 
	  (fun (vis,decl) -> 
	    Hashtbl.add bindings decl.decl_name (vis,decl))
	  $1;
	bindings
      }

%%

(* Trailer *)
