

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

                ("syntax", SYNTAX);
                ("meta", META);

                ("if", IF);
                ("else", ELSE);
                ("while", WHILE);
                ("do", DO);
                ("alt", ALT);

                ("try", TRY);
                ("fail", FAIL);
                ("init", INIT);
                ("main", MAIN);
                ("fini", FINI);

                ("type", TYPE);
                ("pred", PRED);
                ("check", CHECK);
                ("prove", PROVE);

                ("pure", PURE);
                ("auto", AUTO);

                ("pub", PUB);

                ("val", VAL);
                ("dyn", DYN);

                ("log", LOG);

                ("bool", BOOL);
        
                ("int", INT);

                ("char", CHAR);
                ("str", STR);

                ("rec", REC);
                ("tag", TAG);
                ("vec", VEC);
                ("any", ANY);
                ("lim", LIM);


                ("port", PORT);
                ("chan", CHAN);

                ("prog", PROG);
                ("proc", PROC);

                ("true", LIT_BOOL true);
                ("false", LIT_BOOL false);
        
              ]
;;
}

let bin = "0b" ['0' '1']['0' '1' '_']*
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*
let dec = ['0'-'9']+
let exp = ['e''E']['-''+']? dec
let flo = (dec '.' dec (exp?)) | (dec exp)

let ws = [ ' ' '\t' '\r' ]

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  ws+                          { token lexbuf }
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
| '{' (dec as n) '}'           { IDX (int_of_string n) }
| '}'                          { RBRACE     }

| '#'                          { POUND      }
| "@"                          { AT         }
| "^"                          { CARET      }
| '.'                          { DOT        }
| '.' '.'                      { DOTDOT     }
| ','                          { COMMA      }
| ';'                          { SEMI       }
| ':'                          { COLON      }
| "<-"                         { LARROW     }
| "<|"                         { SEND       }
| "->"                         { RARROW     }
| '(' ws* ')'                  { NIL        }
| '('                          { LPAREN     }
| ')'                          { RPAREN     }
| '['                          { LBRACKET   }
| ']'                          { RBRACKET   }

| "fn"                         { FN None                }
| "fn?"                        { FN (Some Ast.PROTO_ques)   }
| "fn!"                        { FN (Some Ast.PROTO_bang)   }
| "fn*"                        { FN (Some Ast.PROTO_star)   }
| "fn+"                        { FN (Some Ast.PROTO_plus)   }

| "for"                        { FOR None               }
| "for?"                       { FOR (Some Ast.PROTO_ques)  }
| "for!"                       { FOR (Some Ast.PROTO_bang)  }
| "for*"                       { FOR (Some Ast.PROTO_star)  }
| "for+"                       { FOR (Some Ast.PROTO_plus)  }

| "ret"                        { RET None               }
| "ret?"                       { RET (Some Ast.PROTO_ques)  }
| "ret!"                       { RET (Some Ast.PROTO_bang)  }
| "ret*"                       { RET (Some Ast.PROTO_star)  }
| "ret+"                       { RET (Some Ast.PROTO_plus)  }

| "put"                        { PUT None               }
| "put?"                       { PUT (Some Ast.PROTO_ques)  }
| "put!"                       { PUT (Some Ast.PROTO_bang)  }
| "put*"                       { PUT (Some Ast.PROTO_star)  }
| "put+"                       { PUT (Some Ast.PROTO_plus)  }

| "be"                         { BE None               }
| "be?"                        { BE (Some Ast.PROTO_ques)  }
| "be!"                        { BE (Some Ast.PROTO_bang)  }
| "be*"                        { BE (Some Ast.PROTO_star)  }
| "be+"                        { BE (Some Ast.PROTO_plus)  }

| id as i                 
                               { try
                                     Hashtbl.find keyword_table i
                                 with
                                     Not_found -> IDENT (i)  
                                           }

| bin as n                      { LIT_INT (Big_int.big_int_of_int (int_of_string n), n)    }
| hex as n                      { LIT_INT (Big_int.big_int_of_int (int_of_string n), n)    }
| dec as n                      { LIT_INT (Big_int.big_int_of_int (int_of_string n), n)    }
| flo as n                      { LIT_FLO n                                                }
| (['"'] ([^'"']|"\\\"")* ['"'])  as s    { LIT_STR  (Scanf.sscanf s "%S" (fun x -> x))    }
| (['\''] [^'\'']         ['\'']) as c    { LIT_CHAR (Scanf.sscanf c "%C" (fun x -> x))    }
| "'\\''"                                 { LIT_CHAR ('\'')                                }

| eof                           { EOF        }
