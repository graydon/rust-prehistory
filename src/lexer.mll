

{
  open Parser;;
  let bump_line p = { p with 
		      Lexing.pos_lnum = p.Lexing.pos_lnum + 1; 
		      Lexing.pos_bol = p.Lexing.pos_cnum }
  ;;

  let lexpos lbuf = let p = lbuf.Lexing.lex_start_p in
                     (p.Lexing.pos_fname,
		      p.Lexing.pos_lnum ,
		      (p.Lexing.pos_cnum) - (p.Lexing.pos_bol))
  ;;


  let keyword_table = Hashtbl.create 100
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ ("crate", fun p -> CRATE p);
                ("mod", fun p -> MOD p);
                ("use", fun p -> USE p);

                ("pub", fun p -> PUBLIC p);
                ("priv", fun p -> PRIVATE p);

                ("meta", fun p -> META p);
                ("syntax", fun p -> SYNTAX p);

                ("if", fun p -> IF p);
                ("else", fun p -> ELSE p);
                ("while", fun p -> WHILE p);
                ("for", fun p -> FOR p);

                ("try", fun p -> TRY p);
                ("fail", fun p -> FAIL p);
                ("init", fun p -> INIT p);
                ("main", fun p -> MAIN p);
                ("fini", fun p -> FINI p);

                ("yield", fun p -> YIELD p);
                ("return", fun p -> RETURN p);

                ("type", fun p -> TYPE p);
                ("pred", fun p -> PRED p);
                ("assert", fun p -> ASSERT p);

                ("lim", fun p -> LIM p);
                ("pure", fun p -> PURE p);

                ("auto", fun p -> AUTO p);
                ("inline", fun p -> INLINE p);

                ("spawn", fun p -> SPAWN p);
                ("log", fun p -> LOG p);
                ("reflect", fun p -> REFLECT p);
                ("eval", fun p -> EVAL p);

		(* 
		 * Type constructors are copied around so much that
		 * we have decided not to give them positions. It's 
		 * just as likely to be wrong as right. 
		 *)

                ("int", fun _ -> INT);
                ("nat", fun _ -> NAT);
                ("rat", fun _ -> RAT);

                ("char", fun _ -> CHAR);
                ("str", fun _ -> STR);

                ("alt", fun _ -> ALT);
                ("vec", fun _ -> VEC);
                ("dyn", fun _ -> DYN);

                ("func", fun _ -> FUNC);
                ("iter", fun _ -> ITER);
                ("chan", fun _ -> CHAN);

                ("proc", fun _ -> PROC);
                ("prog", fun _ -> PROG);
                ("port", fun _ -> PORT);

              ]
;;
}

let bin = "0b" ['0' '1']['0' '1' '_']*
let oct = "0o" ['0'-'7']['0'-'7' '_']*
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*
let dec = ['-' '+']?['0'-'9']* ['.']? ['0'-'9']+ (['e''E']['-''+']?['0'-'9']+)? 
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  [ ' ' '\t' '\r' ]            { token lexbuf }
| '\n'                         { lexbuf.Lexing.lex_curr_p 
				 <- (bump_line lexbuf.Lexing.lex_curr_p);
				 token lexbuf }

| '+'                          { PLUS       (lexpos lexbuf) }
| '-'                          { MINUS      (lexpos lexbuf) }
| '*'                          { STAR       (lexpos lexbuf) }
| '/'                          { SLASH      (lexpos lexbuf) }
| '%'                          { PERCENT    (lexpos lexbuf) }
| '='                          { EQ         (lexpos lexbuf) }
| "+="                         { PLUS_EQ    (lexpos lexbuf) }
| "-="                         { MINUS_EQ   (lexpos lexbuf) }
| "*="                         { STAR_EQ    (lexpos lexbuf) }
| "/="                         { SLASH_EQ   (lexpos lexbuf) }
| "%="                         { PERCENT_EQ (lexpos lexbuf) }
| '<'                          { LT         (lexpos lexbuf) }
| "<="                         { LE         (lexpos lexbuf) }
| "=="                         { EQEQ       (lexpos lexbuf) }
| ">="                         { GE         (lexpos lexbuf) }
| '>'                          { GT         (lexpos lexbuf) }
| '!'                          { NOT        (lexpos lexbuf) }
| '&'                          { AND        (lexpos lexbuf) }
| '|'                          { OR         (lexpos lexbuf) }
| "<<"                         { LSL        (lexpos lexbuf) }
| ">>"                         { LSR        (lexpos lexbuf) }
| ">>>"                        { ASR        (lexpos lexbuf) }
| '~'                          { TILDE      (lexpos lexbuf) }
| '{'                          { LBRACE     (lexpos lexbuf) }
| '}'                          { RBRACE     (lexpos lexbuf) }

| "^"                          { CARET    }
| '.'                          { DOT      }
| ','                          { COMMA    }
| ';'                          { SEMI     }
| ':'                          { COLON    }
| "<-"                         { LARROW   }
| "->"                         { RARROW   }
| "()"                         { NIL      }
| '('                          { LPAREN   }
| ')'                          { RPAREN   }
| '['                          { LBRACKET }
| ']'                          { RBRACKET }

| id as i                 
                                { try
		       		    let tf = Hashtbl.find keyword_table i in
				      tf (lexpos lexbuf) 
		                  with
			 	    Not_found -> IDENT (i, lexpos lexbuf)        }

| (bin|oct|hex) as n            { LIT_NUM (Num.num_of_int (int_of_string n), 
					   lexpos lexbuf)                        }
| dec as d                      { LIT_NUM (Num.num_of_string d, lexpos lexbuf)   }

| ['"']    (([^'"']|"\\\"")* as s)   ['"']      { LIT_STR  (s, lexpos lexbuf)    }
| ['\'']   ( [^'\''] as c)           ['\'']     { LIT_CHAR (c, lexpos lexbuf)    }
| "'\\''"                                       { LIT_CHAR ('\'', lexpos lexbuf) }

| eof                           { EOF }
