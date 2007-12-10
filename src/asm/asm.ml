
(* 
 * Frames look like this, as in C (stack grows down):
 * 
 *    [arg0     ]
 *    ...        
 *    [argN     ]
 *    [env ptr  ]
 *    [desc ptr ]
 *    [yield sp ] 
 *    [yield pc ]
 *    [return sp]
 *    [return pc]  <-- sp for this frame.
 *    [local 0  ] 
 *    ...
 *    [local N  ]
 *    [spill 0  ]
 *    ...
 *    [spill N  ]
 * 
 * All you have to work with is sp. At sp there is a return
 * pc, at sp+4 there is a saved sp of the frame under us,
 * which we reload before jumping back to pc=*sp. Note that 
 * the values of sps do not need to be anything remotely
 * like linear. Stack segments may go all over the heap.
 * 
 * At sp+8 there is a descriptor that tells you what 
 * sort of frame you're in. You should not look at anything
 * aside from sp, sp+4 and sp+8 "generically"; you have
 * to use the descriptor to do anything else.
 * 
 * If the descriptor says you're in a function that can yield,
 * you will then have a yield pc and yield sp above it. If the
 * descriptor says you're in a closure, you will have an 
 * environment pointer above that. Above these optional parts
 * you'll have the args.
 * 
 * The caller must know at least the following when it makes
 * a call:
 * 
 *   - if it's calling into a yielding function
 *   - if it's calling into a closure
 *   - if it's tail-calling
 *   - if it's tail-yielding
 * 
 * It needs to know these things for the following reasons:
 * 
 *   - When entering a yielding function, two extra words need
 *     to be reserved. Nothing needs to be put in them unless
 *     it's a tail-yield; the prologue of the callee will set 
 *     it up normally.
 * 
 *   - When entering a closure, the environment needs to be
 *     set.
 * 
 *   - When tail-calling, the current frame is taken apart
 *     and a new frame built in its place before jumping to
 *     the target.
 * 
 *   - When tail-yielding, the current frame remains but the
 *     caller copies its yield sp and pc to the callee. It does
 *     this by calling to an address a few words inside the callee,
 *     past the callee prologue that would *normally* set up the
 *     default yield sp and yield pc from the incoming return sp
 *     and return pc.
 * 
 * 
 *)


(* Basic test *)

let test_asm _ = 
  let f = mk_basic_x86_elf_file "test.elf32" in
  let buf = f.file_buf in

  let text_shdr = f.file_shdr_text in
  let rodata_shdr = f.file_shdr_rodata in

  let text_off = (Int32.to_int text_shdr.sh_offset) in
  let text_lim = text_off + (Int32.to_int text_shdr.sh_size) in
  let text_pos = ref text_off in
  let text_size _ = (!text_pos - text_off) in
  let text_vma _ = (Int32.to_int text_shdr.sh_addr) + (text_size()) in

  let rodata_off = (Int32.to_int rodata_shdr.sh_offset) in
  let rodata_lim = rodata_off + (Int32.to_int rodata_shdr.sh_size) in
  let rodata_pos = ref rodata_off in
  let rodata_size _ = (!rodata_pos - rodata_off) in
  let rodata_vma _ = (Int32.to_int rodata_shdr.sh_addr) + (rodata_size()) in

  let append_ro_rawstring str = 
    rodata_pos := write_rawstring buf !rodata_pos rodata_lim str 
  in

  let append_rodata_sym f data sym = 
    let size = String.length data in
      f.add_rodata_sym sym (rodata_vma()) size;
      append_ro_rawstring data
  in

  let append_insns iss = (text_pos := write_bytes buf !text_pos text_lim iss) in
  let append_push_sizeof sym = 
    append_insns (push_imm32 0);
    f.add_size_fixup (!text_pos - 4) sym
  in
  let append_push_vmaof sym = 
    append_insns (push_imm32 0);
    f.add_vma_fixup (!text_pos - 4) sym
  in
    
  let literal_data = "Hello, world\n" in
  let literal_symbol = "lit1" in
  let main_vma = text_vma() in

    f.file_ehdr.e_entry <- f.file_shdr_text.sh_addr;
    
    append_push_sizeof literal_symbol;
    append_push_vmaof literal_symbol;
    append_insns (push_imm32 1); (* fd 1 *)
    append_insns op_SYS_WRITE;
    append_insns op_SYS_EXIT;

    f.add_func_sym "main" main_vma (text_size());
    append_rodata_sym f literal_data literal_symbol;

    write_basic_x86_elf_file f;
    Unix.close f.file_buf.buf_fd
;;

test_asm ();;

