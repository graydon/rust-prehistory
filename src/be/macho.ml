open Asm;;
open Common;;


type cpu_type = 
    (* Maybe support more later. *)
    CPU_TYPE_X86 
  | CPU_TYPE_X86_64
;;


let macho_header (cpu:cpu_type) = 
  SEQ 
    [|
      WORD (TY_u32, IMM (0xfeedfaceL));
      WORD (TY_u32, (IMM (match cpu with
                              CPU_TYPE_X86 -> 7L
                            | CPU_TYPE_X86_64 -> 0x01000007L)));
    |]

let emit_file
    (sess:Session.sess)
    (code:Asm.item)
    (data:Asm.item)
    (dwarf:Dwarf.dwarf_records)
    (entry_prog_fixup:fixup)
    (c_to_proc_fixup:fixup)
    : unit =
  let
      all_items = SEQ [| macho_header CPU_TYPE_X86 |]
  in
  let buf = Buffer.create 16 in
  let out = open_out_bin sess.Session.sess_out in
    resolve_item sess all_items;
    lower_item ~lsb0: true ~buf ~it: all_items;
    Buffer.output_buffer out buf;
    flush out;
    close_out out
;;


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
