let _ =
  if Array.length Sys.argv != 3 
  then
    (Printf.printf "usage: %s FILE.rs ENTRY\n"  Sys.argv.(0);
     exit 1)
  else
    Printf.printf "starting up\n";
  let fname = Sys.argv.(1) in
  let root = Ll1parser.parse_crate Token.token fname in
  Hashtbl.iter 
    (fun name _ -> Printf.printf "parsed module: %s\n" name)
    root;
  flush stdout;
  Interp.interpret root Sys.argv.(2);
  flush stdout
;;
