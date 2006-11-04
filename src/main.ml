let _ =
  if Array.length Sys.argv != 3 
  then
    (Printf.printf "usage: %s FILE.rs ENTRY\n"  Sys.argv.(0);
     exit 1)
  else
    let fname = Sys.argv.(1) in
    let lexbuf = Lexing.from_channel (open_in fname) in
    let spos = { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = fname } in
    let cpos = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname } in
    try
      lexbuf.Lexing.lex_start_p <- spos;
      lexbuf.Lexing.lex_curr_p <- cpos;
      let sf = Ll1parser.sourcefile Lexer.token lexbuf in
      Hashtbl.iter (fun name (vis,decl) -> 
	Printf.printf "parsed decl: %s\n" name)
	sf;
      flush stdout;
      Interp.interpret sf Sys.argv.(2);
      flush stdout
    with 
      Parsing.Parse_error ->
	let pos = lexbuf.Lexing.lex_start_p in
	Printf.eprintf "%s:%d:%d: syntax error\n" 
	  pos.Lexing.pos_fname pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
	exit 1
