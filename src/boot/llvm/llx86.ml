(*
 * x86-specific assembler routines.
 *)

class runtime (llctx:Llvm.llcontext) (llmod:Llvm.llmodule) (abi:Llabi.abi) =
  let _ =
    Llvm.set_module_inline_asm llmod
      ".globl _rust_activate_glue
      .align 4,0x90
      _rust_activate_glue:
          pushl %ebp
          pushl %edi
          pushl %esi
          pushl %ebx
          movl  20(%esp),%eax   # = rust_proc
          movl  %esp,8(%eax)    # runtime_sp = sp
          movl  12(%eax),%esp   # sp = rust_sp
          popl  %ebx
          popl  %esi
          popl  %edi
          popl  %ebp
          ret"
  in
  object
    val activate_glue : Llvm.llvalue =
      let proc_ptr_ty = Llvm.pointer_type abi.Llabi.proc_ty in
      let ty = Llvm.function_type (Llvm.void_type llctx) [| proc_ptr_ty |] in
      Llvm.declare_function "rust_activate_glue" ty llmod

    method get_activate_glue : Llvm.llvalue = activate_glue
  end;;

let define_runtime = new runtime;;

