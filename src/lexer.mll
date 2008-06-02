

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

                ("pub", PUB);

                ("meta", META);
                ("syntax", SYNTAX);

                ("if", IF);
                ("else", ELSE);
                ("while", WHILE);

                ("try", TRY);
                ("fail", FAIL);
                ("init", INIT);
                ("main", MAIN);
                ("fini", FINI);

                ("put", PUT);
                ("ret", RET);
                ("be", BE);

                ("type", TYPE);
                ("pred", PRED);
                ("check", CHECK);
                ("prove", PROVE);

                ("let", LET);

                ("pure", PURE);

                ("auto", AUTO);
                ("inline", INLINE);
                ("native", NATIVE);

                ("log", LOG);
                ("reflect", REFLECT);
                ("eval", EVAL);

                ("nil", NIL);
                ("bool", BOOL);
        
                ("int", INT);

                ("char", CHAR);
                ("str", STR);

                ("alt", ALT);
                ("vec", VEC);
                ("any", ANY);

                ("prog", PROG);
                ("port", PORT);
                ("chan", CHAN);

                ("true", LIT_BOOL true);
                ("false", LIT_BOOL false);
        
              ]
;;
}

let bin = "0b" ['0' '1']['0' '1' '_']*
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*

let dec = ['0'-'9']* ['.']? ['0'-'9']+ (['e''E']['-''+']?['0'-'9']+)? 

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  [ ' ' '\t' '\r' ]            { token lexbuf }
| '\n'                         { lexbuf.Lexing.lex_curr_p 
                                     <- (bump_line lexbuf.Lexing.lex_curr_p);
                                 token lexbuf }

(* FIXME: we actually want to preserve comments some day. *)
| "//" [^'\n']*                { token lexbuf }

| '+'                          { PLUS       }
| '-'                          { MINUS      }
| '*'                          { STAR       }
| '/'                          { SLASH      }
| '%'                          { PERCENT    }
| '='                          { EQ         }
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

| "@"                          { AT         }
| "^"                          { CARET      }
| '.'                          { DOT        }
| ','                          { COMMA      }
| ';'                          { SEMI       }
| ':' '='                      { COLONEQ    }
| ':'                          { COLON      }
| "<-"                         { LARROW     }
| "->"                         { RARROW     }
| '('                          { LPAREN     }
| ')'                          { RPAREN     }
| '['                          { LBRACKET   }
| ']'                          { RBRACKET   }

| "func"                       { FUNC      }
| "func?"                      { FUNC_QUES }
| "func!"                      { FUNC_BANG }
| "func*"                      { FUNC_STAR }
| "func+"                      { FUNC_PLUS }

| "for"                        { FOR       }
| "for?"                       { FOR_QUES  }
| "for!"                       { FOR_BANG  }
| "for*"                       { FOR_STAR  }
| "for+"                       { FOR_PLUS  }

| id as i                 
                               { try
                                     Hashtbl.find keyword_table i
                                 with
                                     Not_found -> IDENT (i)  
                                           }

| ('#' ['0' - '9']+) as s       { IDX (int_of_string s)                          }
| bin as n                      { LIT_INT (Big_int.big_int_of_int (int_of_string n), n)    }
| hex as n                      { LIT_INT (Big_int.big_int_of_int (int_of_string n), n)    }
| dec as n                      { LIT_INT (Big_int.big_int_of_string n, n)                 }
| (['"'] ([^'"']|"\\\"")* ['"'])  as s    { LIT_STR  (Scanf.sscanf s "%S" (fun x -> x))    }
| (['\''] [^'\'']         ['\'']) as c    { LIT_CHAR (Scanf.sscanf c "%C" (fun x -> x))    }
| "'\\''"                                 { LIT_CHAR ('\'')                                }

| eof                           { EOF        }
