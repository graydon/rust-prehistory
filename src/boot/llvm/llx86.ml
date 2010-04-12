(*
 * x86-specific assembler routines.
 *)

open Common;;

class runtime
  (llctx:Llvm.llcontext)
  (llmod:Llvm.llmodule)
  (abi:Llabi.abi)
  (sess:Session.sess) =
  let (prefix,align) =
    match sess.Session.sess_targ with
        Linux_x86_elf
      | Win32_x86_pe -> ("",4)
      | MacOS_x86_macho -> ("_", 16)
  in
  let glue =
    [
      ("rust_activate_glue",
       String.concat "\n\t"
         ["pushl %ebp";
          "pushl %edi";
          "pushl %esi";
          "pushl %ebx";
          "movl  20(%esp),%eax   # = rust_proc";
          "movl  %esp,8(%eax)    # runtime_sp = sp";
          "movl  12(%eax),%esp   # sp = rust_sp";
          "popl  %ebx";
          "popl  %esi";
          "popl  %edi";
          "popl  %ebp";
          "ret"])
    ]
  in
  let _ =
    Llvm.set_module_inline_asm llmod
      begin
        String.concat "\n"
          begin
            List.map
              begin
                fun (sym,asm) ->
                  Printf.sprintf
                    "\t.globl %s%s\n\t.balign %d\n%s%s:\n\t%s"
                    prefix sym align prefix sym asm
              end
              glue
          end
      end
  in
object
  val activate_glue : Llvm.llvalue =
    let proc_ptr_ty = Llvm.pointer_type abi.Llabi.proc_ty in
    let ty = Llvm.function_type (Llvm.void_type llctx) [| proc_ptr_ty |] in
      Llvm.declare_function "rust_activate_glue" ty llmod

  method get_activate_glue : Llvm.llvalue = activate_glue
end;;

let define_runtime = new runtime;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
