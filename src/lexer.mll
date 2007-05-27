

{
  open Ll1parser;;
  let bump_line p = { p with 
		      Lexing.pos_lnum = p.Lexing.pos_lnum + 1; 
		      Lexing.pos_bol = p.Lexing.pos_cnum }
  ;;

  let keyword_table = Hashtbl.create 100
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ ("crate", CRATE);
                ("mod", MOD);
                ("use", USE);

                ("pub", PUBLIC);
                ("priv", PRIVATE);

                ("meta", META);
                ("syntax", SYNTAX);

                ("if", IF);
                ("else", ELSE);
                ("while", WHILE);
                ("for", FOR);

                ("try", TRY);
                ("fail", FAIL);
                ("init", INIT);
                ("main", MAIN);
                ("fini", FINI);

                ("yield", YIELD);
                ("return", RETURN);

                ("type", TYPE);
                ("pred", PRED);
                ("assert", ASSERT);

                ("lim", LIM);
                ("pure", PURE);

                ("auto", AUTO);
                ("inline", INLINE);
                ("native", NATIVE);

                ("new", NEW);
                ("log", LOG);
                ("reflect", REFLECT);
                ("eval", EVAL);

		("bool", BOOL);
		
                ("int", INT);
                ("nat", NAT);
                ("rat", RAT);

                ("char", CHAR);
                ("str", STR);

                ("alt", ALT);
                ("vec", VEC);
                ("dyn", DYN);

                ("proc", PROC);
                ("prog", PROG);
		
              ]
;;
}

let bin = "0b" ['0' '1']['0' '1' '_']*
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*
let dec = ['-' '+']?['0'-'9']* ['.']? ['0'-'9']+ (['e''E']['-''+']?['0'-'9']+)? 
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  [ ' ' '\t' '\r' ]            { token lexbuf }
| '\n'                         { lexbuf.Lexing.lex_curr_p 
				 <- (bump_line lexbuf.Lexing.lex_curr_p);
				 token lexbuf }

| '+'                          { PLUS       }
| '-'                          { MINUS      }
| '*'                          { STAR       }
| '/'                          { SLASH      }
| '%'                          { PERCENT    }
| '='                          { EQ         }
| "+="                         { PLUS_EQ    }
| "-="                         { MINUS_EQ   }
| "*="                         { STAR_EQ    }
| "/="                         { SLASH_EQ   }
| "%="                         { PERCENT_EQ }
| '<'                          { LT         }
| "<="                         { LE         }
| "=="                         { EQEQ       }
| "!="                         { NE         }
| ">="                         { GE         }
| '>'                          { GT         }
| '!'                          { NOT        }
| '&'                          { AND        }
| '|'                          { OR         }
| "<<"                         { LSL        }
| ">>"                         { LSR        }
| ">>>"                        { ASR        }
| '~'                          { TILDE      }
| '{'                          { LBRACE     }
| '}'                          { RBRACE     }

| "^"                          { CARET      }
| '.'                          { DOT        }
| ','                          { COMMA      }
| ';'                          { SEMI       }
| ':'                          { COLON      }
| "<-"                         { LARROW     }
| "->"                         { RARROW     }
| "()"                         { NIL        }
| '('                          { LPAREN     }
| ')'                          { RPAREN     }
| '['                          { LBRACKET   }
| ']'                          { RBRACKET   }

| "func"                       { FUNC      }
| "func?"                      { FUNC_Q    }
| "func*"                      { FUNC_STAR }
| "func+"                      { FUNC_PLUS }

| "chan"                       { CHAN      }
| "chan?"                      { CHAN_Q    }
| "chan*"                      { CHAN_STAR }
| "chan+"                      { CHAN_PLUS }

| "port"                       { PORT      }
| "port?"                      { PORT_Q    }
| "port*"                      { PORT_STAR }
| "port+"                      { PORT_PLUS }

| id as i                 
                                { 
				  try
		       		    Hashtbl.find keyword_table i
		                  with
			 	    Not_found -> IDENT (i)  
				}

| ('#' ['0' - '9']+) as s       { TUPIDX (int_of_string s)                       }
| bin as n                      { LIT_BIN (Num.num_of_int (int_of_string n))     }
| hex as n                      { LIT_HEX (Num.num_of_int (int_of_string n))     }
| dec as d                      { LIT_DEC (Num.num_of_string d)                  }
| (['"'] ([^'"']|"\\\"")* ['"'])  as s    { LIT_STR  (Scanf.sscanf s "%S" (fun x -> x))    }
| (['\''] [^'\'']         ['\'']) as c    { LIT_CHAR (Scanf.sscanf c "%C" (fun x -> x))    }
| "'\\''"                                 { LIT_CHAR ('\'')                                }

| eof                           { EOF        }
