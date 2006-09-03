open Format;;

let _ =
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  try
    let result = Parser.sourcefile Lexer.token lexbuf in
    for i=0 to Array.length result - 1 do
      match result.(i) with
	(vis,decl) -> 
	  Format.printf "parsed decl: %s\n" decl.Ast.decl_name
    done;
    flush stdout
  with 
    Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Format.eprintf "%d:%d: syntax error.@." 
	pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
