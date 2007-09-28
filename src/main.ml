let _ =
  if Array.length Sys.argv != 3 
  then
    (Printf.printf "usage: %s FILE.rs ENTRY\n"  Sys.argv.(0);
     exit 1);
  let fname = Sys.argv.(1) in
  let root = Ll1parser.parse_crate Lexer.token fname in
  Interp.interpret root Sys.argv.(2);
;;
