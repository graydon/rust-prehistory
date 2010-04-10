(*
 * LLVM integration with the Rust runtime.
 *)

type abi = {
  crate_ty:   Llvm.lltype;
  proc_ty:    Llvm.lltype;
  rust_start: Llvm.llvalue
};;

let declare_abi (llctx:Llvm.llcontext) (llmod:Llvm.llmodule) : abi =
  let i32 = Llvm.i32_type llctx in

  let crate_ty =
    (* TODO: other architectures besides x86 *)
    let crate_opaque_ty = Llvm.opaque_type llctx in
    let crate_tyhandle = Llvm.handle_to_type (Llvm.struct_type llctx [|
        i32;                                (* ptrdiff_t image_base_off *)
        Llvm.pointer_type crate_opaque_ty;  (* uintptr_t self_addr *)
        i32;                                (* ptrdiff_t debug_abbrev_off *)
        i32;                                (* size_t debug_abbrev_sz *)
        i32;                                (* ptrdiff_t debug_info_off *)
        i32;                                (* size_t debug_info_sz *)
        i32;                                (* size_t activate_glue_off *)
        i32;                                (* size_t main_exit_proc_glue_off *)
        i32;                                (* size_t unwind_glue_off *)
        i32;                                (* size_t yield_glue_off *)
        i32;                                (* int n_rust_syms *)
        i32;                                (* int n_c_syms *)
        i32                                 (* int n_libs *)
      |])
    in
    Llvm.refine_type crate_opaque_ty (Llvm.type_of_handle crate_tyhandle);
    Llvm.type_of_handle crate_tyhandle
  in
  ignore (Llvm.define_type_name "rust_crate" crate_ty llmod);

  let proc_ty =
    (* TODO: other architectures besides x86 *)
    Llvm.struct_type llctx [|
      i32;                    (* size_t refcnt *)
      Llvm.pointer_type i32;  (* stk_seg *stk *)
      Llvm.pointer_type i32;  (* uintptr_t runtime_sp *)
      Llvm.pointer_type i32;  (* uintptr_t rust_sp *)
      Llvm.pointer_type i32;  (* rust_rt *rt *)
      Llvm.pointer_type i32   (* rust_crate_cache *cache *)
    |]
  in
  ignore (Llvm.define_type_name "rust_proc" proc_ty llmod);

  let rust_start_ty =
    let proc_ptr_ty = Llvm.pointer_type proc_ty in
    let main_ty = Llvm.function_type (Llvm.void_type llctx) [| proc_ptr_ty |] in
    let args_ty = Array.map Llvm.pointer_type [| main_ty; crate_ty |] in
    Llvm.function_type i32 args_ty
  in
  {
    crate_ty = crate_ty;
    proc_ty = proc_ty;
    rust_start = Llvm.declare_function "rust_start" rust_start_ty llmod
  }
;;

let postprocess_module
    (llctx:Llvm.llcontext)
    (llmod:Llvm.llmodule)
    (abi:abi)
    : unit =
  let i32 = Llvm.i32_type llctx in
  let crate_ptr = Llvm.declare_global abi.crate_ty "rust_crate" llmod in

  (* Count the number of Rust functions and the number of C functions by simply
   * (and crudely) testing whether each function in the module begins with
   * "_rust_". *)
  let (rust_fn_count, c_fn_count) =
    let count (rust_fn_count, c_fn_count) fn =
      let begins_with prefix str =
        let (str_len, prefix_len) = (String.length str, String.length prefix) in
        prefix_len <= str_len && (String.sub str 0 prefix_len) = prefix
      in
      if begins_with "_rust_" (Llvm.value_name fn) then
        (rust_fn_count + 1, c_fn_count)
      else
        (rust_fn_count, c_fn_count + 1)
    in
    Llvm.fold_left_functions count (0, 0) llmod
  in

  let crate_val =
    Llvm.const_struct llctx [|
      Llvm.const_int i32 0;             (* ptrdiff_t image_base_off *)
      crate_ptr;                        (* uintptr_t self_addr *)
      Llvm.const_int i32 0;             (* ptrdiff_t debug_abbrev_off *)
      Llvm.const_int i32 0;             (* size_t debug_abbrev_sz *)
      Llvm.const_int i32 0;             (* ptrdiff_t debug_info_off *)
      Llvm.const_int i32 0;             (* size_t debug_info_sz *)
      Llvm.const_int i32 0;             (* size_t activate_glue_off *)
      Llvm.const_int i32 0;             (* size_t main_exit_proc_glue_off *)
      Llvm.const_int i32 0;             (* size_t unwind_glue_off *)
      Llvm.const_int i32 0;             (* size_t yield_glue_off *)
      Llvm.const_int i32 rust_fn_count; (* int n_rust_syms *)
      Llvm.const_int i32 c_fn_count;    (* int n_c_syms *)
      Llvm.const_int i32 0              (* int n_libs *)
    |]
  in

  Llvm.set_initializer crate_val crate_ptr;

  (* Define the main function for crt0 to call. *)
  let main_fn =
    let main_ty = Llvm.function_type i32 [| i32; i32 |] in
    Llvm.define_function "main" main_ty llmod
  in
  let main_builder = Llvm.builder_at_end llctx (Llvm.entry_block main_fn) in
  let rust_main_fn =
    match Llvm.lookup_function "_rust_main" llmod with
        None -> raise (Failure "no main function found")
      | Some fn -> fn
  in
  let rust_start = abi.rust_start in
  let rust_start_args = [| rust_main_fn; crate_ptr |] in
  ignore (Llvm.build_call rust_start rust_start_args "start_rust" main_builder);
  ignore (Llvm.build_ret (Llvm.const_int i32 0) main_builder)
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)

