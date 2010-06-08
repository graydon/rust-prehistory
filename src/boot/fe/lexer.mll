

{
  open Token;;
  open Common;;
  let bump_line p = { p with
              Lexing.pos_lnum = p.Lexing.pos_lnum + 1;
              Lexing.pos_bol = p.Lexing.pos_cnum }
  ;;

  let keyword_table = Hashtbl.create 100
  let _ =
    List.iter (fun (kwd, tok) -> Common.htab_put keyword_table kwd tok)
              [ ("crate", CRATE);
                ("mod", MOD);
                ("use", USE);

                ("native", NATIVE);
                ("syntax", SYNTAX);
                ("meta", META);

                ("if", IF);
                ("else", ELSE);
                ("while", WHILE);
                ("do", DO);
                ("alt", ALT);
                ("case", CASE);

                ("for", FOR);
                ("each", EACH);
                ("put", PUT);
                ("ret", RET);
                ("be", BE);

                ("fail", FAIL);
                ("drop", DROP);

                ("type", TYPE);
                ("pred", PRED);
                ("check", CHECK);
                ("claim", CLAIM);
                ("prove", PROVE);

                ("pure", PURE);
                ("mutable", MUTABLE);
                ("auto", AUTO);

                ("fn", FN);
                ("iter", ITER);

                ("import", IMPORT);
                ("export", EXPORT);

                ("let", LET);

                ("log", LOG);
                ("spawn", SPAWN);
                ("thread", THREAD);
                ("yield", YIELD);
                ("join", JOIN);

                ("bool", BOOL);

                ("int", INT);
                ("uint", UINT);

                ("char", CHAR);
                ("str", STR);

                ("rec", REC);
                ("tag", TAG);
                ("vec", VEC);
                ("any", ANY);

                ("obj", OBJ);

                ("port", PORT);
                ("chan", CHAN);

                ("task", TASK);

                ("true", LIT_BOOL true);
                ("false", LIT_BOOL false);

                ("in", IN);

                ("with", WITH);

                ("bind", BIND);

                ("u8", MACH TY_u8);
                ("u16", MACH TY_u16);
                ("u32", MACH TY_u32);
                ("u64", MACH TY_u64);
                ("i8", MACH TY_i8);
                ("i16", MACH TY_i16);
                ("i32", MACH TY_i32);
                ("i64", MACH TY_i64);
                ("f32", MACH TY_f32);
                ("f64", MACH TY_f64)
              ]
;;
}

let bin = "0b" ['0' '1']['0' '1' '_']*
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*
let dec = ['0'-'9']+
let exp = ['e''E']['-''+']? dec
let flo = (dec '.' dec (exp?)) | (dec exp)

let ws = [ ' ' '\t' '\r' ]

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  ws+                          { token lexbuf }
| '\n'                         { lexbuf.Lexing.lex_curr_p
                                     <- (bump_line lexbuf.Lexing.lex_curr_p);
                                 token lexbuf }
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
| '_' (dec as n)               { IDX (int_of_string n) }
| '_'                          { UNDERSCORE }
| '}'                          { RBRACE     }

| "+="                         { OPEQ (PLUS) }
| "-="                         { OPEQ (MINUS) }
| "*="                         { OPEQ (STAR) }
| "/="                         { OPEQ (SLASH) }
| "%="                         { OPEQ (PERCENT) }
| "&="                         { OPEQ (AND) }
| "|="                         { OPEQ (OR)  }
| "<<="                        { OPEQ (LSL) }
| ">>="                        { OPEQ (LSR) }
| ">>>="                       { OPEQ (ASR) }

| '#'                          { POUND      }
| '@'                          { AT         }
| '^'                          { CARET      }
| '.'                          { DOT        }
| ','                          { COMMA      }
| ';'                          { SEMI       }
| ':'                          { COLON      }
| "<-"                         { LARROW     }
| "<|"                         { SEND       }
| "->"                         { RARROW     }
| '('                          { LPAREN     }
| ')'                          { RPAREN     }
| '['                          { LBRACKET   }
| ']'                          { RBRACKET   }

| id as i
                               { try
                                     Hashtbl.find keyword_table i
                                 with
                                     Not_found -> IDENT (i)
                                           }

| bin as n                      { LIT_INT (Int64.of_string n, n)    }
| hex as n                      { LIT_INT (Int64.of_string n, n)    }
| dec as n                      { LIT_INT (Int64.of_string n, n)    }
| flo as n                      { LIT_FLO n                                                }
| (['"'] ([^'"']|"\\\"")* ['"'])  as s    { LIT_STR  (Scanf.sscanf s "%S" (fun x -> x))    }
| (['\''] [^'\'']         ['\'']) as c    { LIT_CHAR (Scanf.sscanf c "%C" (fun x -> x))    }
| "'\\''"                                 { LIT_CHAR ('\'')                                }

| eof                           { EOF        }

and bracequote buf depth = parse

  '\\' '{'                      { Buffer.add_char buf '{';
                                  bracequote buf depth lexbuf          }

| '{'                           { Buffer.add_char buf '{';
                                  bracequote buf (depth+1) lexbuf      }

| '\\' '}'                      { Buffer.add_char buf '}';
                                  bracequote buf depth lexbuf          }

| '}'                           { if depth = 1
                                  then BRACEQUOTE (Buffer.contents buf)
                                  else
                                    begin
                                      Buffer.add_char buf '}';
                                      bracequote buf (depth-1) lexbuf
                                    end                                }

| '\\' [^'{' '}']               { let s = Lexing.lexeme lexbuf in
                                    Buffer.add_string buf s;
                                    bracequote buf depth lexbuf        }


| [^'\\' '{' '}']+              { let s = Lexing.lexeme lexbuf in
                                    Buffer.add_string buf s;
                                    bracequote buf depth lexbuf        }
