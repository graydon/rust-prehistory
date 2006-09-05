%{

	(* Header *)

open Ast;;

let numty n =
  match n with 
    Num.Ratio _ -> TY_rat
  | _           -> TY_int
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
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET 

/* Keywords for the crate and module system. */
%token <Ast.rs_pos> CRATE MOD USE PUB

/* Metaprogramming keywords. */
%token <Ast.rs_pos> SYNTAX META TILDE

/* Control-flow keywords. */
%token <Ast.rs_pos> IF ELSE WHILE FOR
%token <Ast.rs_pos> TRY FAIL FINI
%token <Ast.rs_pos> YIELD RETURN

/* Type and type-state keywords. */
%token <Ast.rs_pos> TYPE PRED ASSERT

/* Parameter qualifiers. */
%token <Ast.rs_pos> IN OUT INOUT

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

%type <string list>      name_list
%type <rs_name>          name

%type <string list>      ident_list
%type <string array>     idents

%type <rs_type>          simple_ty_expr

%type <rs_param_mode>    param_mode
%type <rs_param>         param
%type <rs_param list>    param_list
%type <rs_param array>   params
%type <rs_param array>   paren_params

%type <rs_name option>          pred_arg
%type <(rs_name option) list>   pred_arg_list
%type <(rs_name option) array>  pred_args
%type <rs_pred>                 pred
%type <rs_pred list>            pred_list
%type <rs_pred array>           preds

%type <((rs_param array) * (rs_pred array))>   paren_params_and_maybe_state
%type <(rs_type * (rs_pred array))>            simple_ty_expr_and_maybe_state

%type <ty_sig>           sig_decl

%type <rs_decl>              decl
%type <rs_decl_top>          decl_top
%type <rs_decl_top list>     decl_top_list
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

lidx_list: 
    IDENT                             {  [LIDX_ident $1] }
  | lidx_list DOT IDENT               {  (LIDX_ident $3) :: $1 }
  | lidx_list DOT LPAREN expr RPAREN  {  (LIDX_index $4) :: $1 }

lval:
    lidx_list            { Array.of_list (List.rev $1) }


name_list: 
    IDENT DOT name_list  { (fst $1) :: $3 }
  | IDENT                { [fst $1]       }  

name: 
  name_list              { Array.of_list $1 }


ident_list: 
    IDENT COMMA ident_list { (fst $1) :: $3 }
  | IDENT                  { [fst $1]       }  

idents: 
  ident_list             { Array.of_list $1 }

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
    NIL                  { TY_nil                          }

  | mach_ty_expr         { $1                              }
  | arith_ty_expr        { $1                              }

  | STR                  { TY_str                          }
  | CHAR                 { TY_char                         }

  | PROC                 { TY_proc                         }
  | VEC                  { TY_vec                          }
  | name                 { TY_named $1                     }
  | prim_ty_expr 
            LBRACKET
            simple_ty_exprs
            RBRACKET     { TY_apply ($1, $3)               }

  | LPAREN 
      complex_ty_expr 
      RPAREN             { $2                              }


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

subr:
    FUNC  { fun r -> TY_func r }
  | ITER  { fun r -> TY_iter r }

qualified_subr:

    INLINE PURE subr    { fun s -> $3 { subr_inline = true; 
					subr_pure = true; 
					subr_sig = s; } }

  | INLINE subr         { fun s -> $2 { subr_inline = true; 
					subr_pure = false; 
					subr_sig = s; } }

  | PURE subr           { fun s -> $2 { subr_inline = false; 
					subr_pure = true; 
					subr_sig = s; } }

  | subr                { fun s -> $1 { subr_inline = false; 
					subr_pure = false; 
					subr_sig = s; } }

rec_slot:
    simple_ty_expr IDENT SEMI   { { rec_slot_name = (fst $2);
				    rec_slot_type = $1;
				    rec_slot_state = Array.of_list []} }

  | simple_ty_expr IDENT state  { { rec_slot_name = (fst $2);
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
    simple_ty_expr                { $1        }
  | qualified_subr sig_decl       { $1 $2     }
  | REC LBRACE rec_body RBRACE    { TY_rec $3 }
  | ALT LBRACE alt_cases RBRACE   { TY_alt $3 }


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
  | call SEMI                         { STMT_call $1                }

  | block_stmt                        { $1                          }

  | lval LARROW lval SEMI             { STMT_move ($1, $3) }
  | lval EQ expr SEMI                 { STMT_copy ($1, $3) }

stmt_list: 
    stmt stmt_list               { $1 :: $2 }
  | stmt                         { [$1]     }

block_stmt:
    LBRACE stmt_list RBRACE           { STMT_block (Array.of_list $2) }
  | LBRACE RBRACE                     { STMT_block (Array.of_list []) }


param_mode:
    IN                           { PARAM_move_in    }
  | OUT                          { PARAM_move_out   }
  | INOUT                        { PARAM_move_inout }

param:
    simple_ty_expr param_mode IDENT    { { param_type = $1; 
					   param_mode = $2; 
					   param_name = (fst $3) } }
  | simple_ty_expr IDENT               { { param_type = $1; 
					   param_mode = PARAM_copy; 
					   param_name = (fst $2) } }

param_list:
    param COMMA param_list       { $1 :: $3 }
  | param                        { [$1]     }  

params: 
  param_list                     { Array.of_list $1 }


pred_arg:
    name                         { Some $1 }
  | STAR                         { None    }

pred_arg_list:
    pred_arg COMMA pred_arg_list { $1 :: $3 }
  | pred_arg                     { [$1]     }  

pred_args:
    pred_arg_list                { Array.of_list $1 }

pred:
    name LPAREN pred_args RPAREN { { pred_name = $1; pred_args = $3 } }

pred_list:
    pred COMMA pred_list         { $1 :: $3 }
  | pred                         { [$1]     }  
    
preds:
    pred_list                    { Array.of_list $1 }

state:
    COLON preds                  { $2 }

paren_params:
    LPAREN params RPAREN         { $2               }
  | LPAREN RPAREN                { Array.of_list [] }

paren_params_and_maybe_state:
    paren_params state           { ($1, $2)               }
  | paren_params                 { ($1, Array.of_list []) }

simple_ty_expr_and_maybe_state:
    simple_ty_expr state         { ($1, $2)               }
  | simple_ty_expr               { ($1, Array.of_list []) }

sig_decl:
  paren_params_and_maybe_state 
  RARROW 
  simple_ty_expr_and_maybe_state
  {
    match ($1,$3) with ((args,istate),(res,ostate)) -> 
    { 
      sig_params = args;
      sig_result = res;
      sig_istate = istate;
      sig_ostate = ostate;
    }
  }


decl:
    TYPE IDENT EQ complex_ty_expr SEMI
      {  { decl_name = (fst $2);
	   decl_type = TY_type;
	   decl_value = VAL_dyn (TY_nil, VAL_nil);
	   decl_state = Array.of_list [];
	   decl_pos = (snd $2)                       } }
      
  | qualified_subr IDENT sig_decl block_stmt 
      {  { decl_name = (fst $2); 
	   decl_type = $1 $3;
	   decl_value = VAL_dyn (TY_nil, VAL_nil);
	   decl_state = Array.of_list [];
	   decl_pos = (snd $2)                       } }

decl_top:
    PUBLIC decl           { (VIS_public, $2)   }
  | PRIVATE decl          { (VIS_private, $2)  }
  | decl                  { (VIS_standard, $1) }

decl_top_list:
    decl_top decl_top_list              { $1 :: $2 }
  | decl_top                            { [$1]     }  

sourcefile:
   decl_top_list EOF      
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
