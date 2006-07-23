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
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN PERCENT_ASSIGN
%token LT LE EQ NE GE GT 
%token NOT AND OR LSL LSR ASR

/* No user-overriding beyond this line. */
%token DOT 

/* Structural symbols. */
%token COMMA SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET ARROW

/* Keywords for the crate and module system. */
%token CRATE MODULE USE PUB

/* Metaprogramming keywords. */
%token SYNTAX META TILDE

/* Control-flow keywords. */
%token IF ELSE WHILE FOR
%token TRY FAIL FINI
%token YIELD RET

/* Type and type-state keywords. */
%token TYPE PRED

/* Type qualifiers. */
%token CONST PURE

/* Declarator qualifiers. */
%token PUBLIC AUTO INLINE

/* Basic types. */
%token NIL PTR
%token INT RAT
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
%start program
%type <int> program
%type <rs_expr> expr
%type <rs_name> name
%type <string list> name_list
%type <rs_val> literal

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

name: 
  name_list             { Array.of_list $1 }

name_list: 
    IDENT DOT name_list { $1 :: $3 }
  | IDENT               { [$1]     }


top_defn: 

    PUBLIC subr_defn           {

%%

(* Trailer *)
