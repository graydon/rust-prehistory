(*
 * LLVM translator.
 *)

let trans_crate
    (llcontext:Llvm.llcontext)
    (sess:Session.sess)
    (crate:Ast.crate)
    : Llvm.llmodule =
  let filename = Session.filename_of sess.Session.sess_in in
  let llmod = Llvm.create_module llcontext filename in
  try
    ignore crate;
    llmod
  with e -> Llvm.dispose_module llmod; raise e
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)

