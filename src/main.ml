let _ =
  if Array.length Sys.argv != 3 
  then
    (Printf.printf "usage: %s FILE.rs ENTRY\n"  Sys.argv.(0);
     exit 1)
  else
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    try
      let sf = Parser.sourcefile Lexer.token lexbuf in
      Hashtbl.iter (fun name (vis,decl) -> 
	Printf.printf "parsed decl: %s\n" name)
	sf;
      flush stdout;
      Interp.interpret sf Sys.argv.(2);
      flush stdout
    with 
      Parsing.Parse_error ->
	let pos = Lexing.lexeme_start_p lexbuf in
	Printf.eprintf "%d:%d: syntax error.@." 
	  pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
	exit 1
