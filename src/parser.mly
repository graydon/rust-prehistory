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

/* Expression nodes that reduce to overridable 2 or 3-operand calls. */
%token PLUS MINUS STAR SLASH PERCENT
%token EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
%token LT LE EQEQ NE GE GT 
%token NOT AND OR LSL LSR ASR

/* No user-overriding beyond this line. */
%token DOT CARET

/* Structural symbols. */
%token COMMA SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET RARROW LARROW

/* Keywords for the crate and module system. */
%token CRATE MOD USE PUB

/* Metaprogramming keywords. */
%token SYNTAX META TILDE

/* Control-flow keywords. */
%token IF ELSE WHILE FOR
%token TRY FAIL FINI
%token YIELD RET

/* Type and type-state keywords. */
%token TYPE PRED ASSERT

/* Parameter qualifiers. */
%token IN OUT INOUT

/* Type qualifiers. */
%token LIM PURE

/* Declarator qualifiers. */
%token PUBLIC PRIVATE AUTO INLINE

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

/* Magic runtime services. */
%token SPAWN LOG REFLECT EVAL

/* Literals. */
%token <Num.num> LIT_NUM
%token <string> LIT_STR 
%token <char> LIT_CHAR 

/* Identifiers. */
%token <string> IDENT

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

%type <rs_val>           literal
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
%type <Ast.rs_decl_top array>    sourcefile

%%

/* Rules */

program: FUNC  { 1 }

expr: 
    expr OR expr        { EXPR_binary (BINOP_or, $1, $3)  }
  | expr AND expr       { EXPR_binary (BINOP_and, $1, $3) }

  | expr LT expr        { EXPR_binary (BINOP_lt, $1, $3)  }
  | expr LE expr        { EXPR_binary (BINOP_le, $1, $3)  }
  | expr GE expr        { EXPR_binary (BINOP_ge, $1, $3)  }
  | expr GT expr        { EXPR_binary (BINOP_gt, $1, $3)  }

  | expr LSL expr       { EXPR_binary (BINOP_lsl, $1, $3) }
  | expr LSR expr       { EXPR_binary (BINOP_lsr, $1, $3) }
  | expr ASR expr       { EXPR_binary (BINOP_asr, $1, $3) }

  | expr PLUS expr      { EXPR_binary (BINOP_add, $1, $3) }
  | expr MINUS expr     { EXPR_binary (BINOP_sub, $1, $3) }

  | expr STAR expr      { EXPR_binary (BINOP_mul, $1, $3) }
  | expr SLASH expr     { EXPR_binary (BINOP_div, $1, $3) }
  | expr PERCENT expr   { EXPR_binary (BINOP_mod, $1, $3) }

  | NOT expr            { EXPR_unary  (UNOP_not, $2)      }

  | literal             { EXPR_literal $1                 }
  | name                { EXPR_name $1                    }


literal:
    LIT_STR             { VAL_dyn (TY_str, VAL_str $1)    }

  | LIT_CHAR            { VAL_dyn (TY_char, VAL_char $1)  }

  | LIT_NUM             { VAL_dyn (TY_arith (numty $1),
				   VAL_arith $1)          }

name_list: 
    IDENT DOT name_list  { $1 :: $3 }
  | IDENT                { [$1]     }  

name: 
  name_list              { Array.of_list $1 }


ident_list: 
    IDENT DOT ident_list { $1 :: $3 }
  | IDENT                { [$1]     }  

idents: 
  ident_list             { Array.of_list $1 }


simple_ty_expr:
    NIL                  { TY_nil                          }

  | UNSIGNED             { TY_prim (TY_unsigned, $1)       }
  | SIGNED               { TY_prim (TY_signed, $1)         }
  | BFP                  { TY_prim (TY_ieee_bfp, $1)       }
  | DFP                  { TY_prim (TY_ieee_dfp, $1)       }

  | INT                  { TY_arith (TY_int)               }
  | NAT                  { TY_arith (TY_nat)               }
  | RAT                  { TY_arith (TY_rat)               }

  | STR                  { TY_str                          }
  | CHAR                 { TY_char                         }

  | PROC                 { TY_proc                         }
  | VEC                  { TY_vec                          }

  | CARET simple_ty_expr { TY_ref $2                       }
  | simple_ty_expr EQ    { TY_const $1                     }
  | name                 { TY_named $1                     }
  | simple_ty_expr 
            LBRACKET
            idents
            RBRACKET     { TY_param ($1, $3)               }

param_mode:
    IN                           { PARAM_move_in    }
  | OUT                          { PARAM_move_out   }
  | INOUT                        { PARAM_move_inout }

param:
    simple_ty_expr param_mode    { { param_type = $1; param_mode = $2;       } }
  | simple_ty_expr               { { param_type = $1; param_mode = PARAM_copy; } }

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
    paren_params COLON state     { ($1, $3)               }
  | paren_params                 { ($1, Array.of_list []) }

simple_ty_expr_and_maybe_state:
    simple_ty_expr COLON state   { ($1, $3)               }
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
   FUNC IDENT sig_decl SEMI   {  { decl_name = $2; 
				   decl_type = TY_func { subr_inline = false; 
							 subr_pure = false;
							 subr_sig = $3; };
				   decl_value = VAL_dyn (TY_nil, VAL_nil);
				   decl_state = Array.of_list []; } }
decl_top:
    PUBLIC decl           { (VIS_public, $2)   }
  | PRIVATE decl          { (VIS_private, $2)  }
  | decl                  { (VIS_standard, $1) }

decl_top_list:
    decl_top SEMI decl_top_list              { $1 :: $3 }
  | decl_top SEMI                            { [$1]     }  

sourcefile:
   decl_top_list        { Array.of_list $1 }

%%

(* Trailer *)
