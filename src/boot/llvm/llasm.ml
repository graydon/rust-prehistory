(*
 * machine-specific assembler routines.
 *)

open Common;;

type asm_glue =
    {
      asm_activate_glue : Llvm.llvalue;
      asm_yield_glue : Llvm.llvalue;
    }
;;

(* x86-specific asm. *)

let x86_glue
  (llctx:Llvm.llcontext)
  (llmod:Llvm.llmodule)
  (abi:Llabi.abi)
  (sess:Session.sess)
  : asm_glue =
  let (prefix,align) =
    match sess.Session.sess_targ with
        Linux_x86_elf
      | Win32_x86_pe -> ("",4)
      | MacOS_x86_macho -> ("_", 16)
  in
  let save_callee_saves =
    ["pushl %ebp";
     "pushl %edi";
     "pushl %esi";
     "pushl %ebx";]
  in
  let restore_callee_saves =
    ["popl  %ebx";
     "popl  %esi";
     "popl  %edi";
     "popl  %ebp";]
  in
  let load_esp_from_rust_sp    = ["movl  12(%edx), %esp"] in
  let load_esp_from_runtime_sp = ["movl   8(%edx), %esp"] in
  let store_esp_to_rust_sp     = ["movl  %esp, 12(%edx)"] in
  let store_esp_to_runtime_sp  = ["movl  %esp,  8(%edx)"] in
  let glue =
    [
      ("rust_activate_glue",
       String.concat "\n\t"
         (["movl  4(%esp),%edx    # edx = rust_task"]
          @ save_callee_saves
          @ store_esp_to_runtime_sp
          @ load_esp_from_rust_sp
            (* 
             * This 'add' instruction is a bit surprising.
             * See lengthy comment in boot/be/x86.ml activate_glue.
             *)
          @ ["addl  $20,12(%edx)"]
          @ restore_callee_saves
          @ ["ret"]));

      ("rust_yield_glue",
       String.concat "\n\t"

         (["movl  0(%esp),%edx    # edx = rust_task"]
          @ load_esp_from_rust_sp
          @ save_callee_saves
          @ store_esp_to_rust_sp
          @ load_esp_from_runtime_sp
          @ restore_callee_saves
          @ ["ret"]))
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
  let decl_glue s =
    let task_ptr_ty = Llvm.pointer_type abi.Llabi.task_ty in
    let ty = Llvm.function_type (Llvm.void_type llctx) [| task_ptr_ty |] in
      Llvm.declare_function s ty llmod;
  in
    {
      asm_activate_glue = decl_glue "rust_activate_glue";
      asm_yield_glue = decl_glue "rust_yield_glue"
    }
;;

(* x64-specific asm. *)
(* arm-specific asm. *)
(* ... *)


let get_glue
  (llctx:Llvm.llcontext)
  (llmod:Llvm.llmodule)
  (abi:Llabi.abi)
  (sess:Session.sess)
  : asm_glue =
  match sess.Session.sess_targ with
      Linux_x86_elf
    | Win32_x86_pe
    | MacOS_x86_macho ->
        x86_glue llctx llmod abi sess
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
