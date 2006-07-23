

{
  open Parser;;
  let keyword_table = Hashtbl.create 100
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ ("crate", CRATE);
                ("module", MODULE);
                ("use", USE);
                ("pub", PUB);

                ("meta", META);
                ("syntax", SYNTAX);

                ("if", IF);
                ("else", ELSE);
                ("while", WHILE);
                ("for", FOR);

                ("try", TRY);
                ("fail", FAIL);
                ("fini", FINI);

                ("yield", YIELD);
                ("ret", RET);

                ("type", TYPE);
                ("pred", PRED);

                ("const", CONST);
                ("pure", PURE);

                ("auto", AUTO);
                ("inline", INLINE);

                ("nil", NIL);
                ("ptr", PTR);

                ("int", INT);
                ("rat", RAT);

                ("char", CHAR);
                ("str", STR);

                ("alt", ALT);
                ("vec", VEC);
                ("dyn", DYN);

                ("func", FUNC);
                ("iter", ITER);
                ("chan", CHAN);

                ("proc", PROC);
                ("prog", PROG);
                ("port", PORT);

                ("spawn", SPAWN);
                ("log", LOG);
                ("reflect", REFLECT);
                ("eval", EVAL);
              ]
;;
}

let bin = "0b" ['0' '1']['0' '1' '_']*
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F' '_']*
let oct = "0o" ['0'-'7']['0'-'7' '_']*
let dec = ['-' '+']?['0'-'9']* ['.']? ['0'-'9']+ (['e''E']['-''+']?['0'-'9']+)? 
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  [ ' ' '\t' '\n' ]            { token lexbuf }
| [ '+' ]                      { PLUS }
| [ '-' ]                      { MINUS }
| [ '*' ]                      { STAR }
| [ '/' ]                      { SLASH }
| [ '%' ]                      { PERCENT }
| [ '=' ]                      { ASSIGN }
| "+="                         { PLUS_ASSIGN }
| "-="                         { MINUS_ASSIGN }
| "*="                         { STAR_ASSIGN }
| "/="                         { SLASH_ASSIGN }
| "%="                         { PERCENT_ASSIGN }
| [ '<' ]                      { LT }
| "<="                         { LE }
| "=="                         { EQ }
| ">="                         { GE }
| [ '>' ]                      { GT }
| [ '!' ]                      { NOT }
| [ '&' ]                      { AND }
| [ '|' ]                      { OR }
| "<<"                         { LSL }
| ">>"                         { LSR }
| ">>>"                        { ASR }
| [ '.' ]                      { DOT }
| [ '~' ]                      { TILDE }
| id as i                 
                               { try
		       		    Hashtbl.find keyword_table i
		                 with
			 	    Not_found -> IDENT i }

| (bin|oct|hex) as n            { LIT_NUM (Num.num_of_int (int_of_string n)) }
| dec as d                      { LIT_NUM (Num.num_of_string d) }

| ['"']    (([^'"']|"\\\"")* as s)   ['"']      { LIT_STR s }
| ['\'']   ( [^'\''] as c)           ['\'']     { LIT_CHAR c } 
| "'\\''"                                       { LIT_CHAR '\'' }
