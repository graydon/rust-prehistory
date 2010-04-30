open Common;;
open Semant;;

type mem_ctrl =
    MEM_rc_opaque of int
  | MEM_rc_struct
  | MEM_gc
  | MEM_interior
;;

type clone_ctrl =
    CLONE_none
  | CLONE_chan of Il.cell
  | CLONE_all of Il.cell
;;

type call_ctrl =
    CALL_direct
  | CALL_vtbl
  | CALL_indirect
;;

type foreach_ctrl =
    {
      foreach_fixup: fixup;
      foreach_depth: int;
    }
;;

let word_sz (abi:Abi.abi) : int64 =
  abi.Abi.abi_word_sz
;;

let word_n (abi:Abi.abi) (n:int) : int64 =
  Int64.mul (word_sz abi) (Int64.of_int n)
;;

let word_bits (abi:Abi.abi) : Il.bits =
  abi.Abi.abi_word_bits
;;

let word_ty_mach (abi:Abi.abi) : ty_mach =
  match word_bits abi with
      Il.Bits8 -> TY_u8
    | Il.Bits16 -> TY_u16
    | Il.Bits32 -> TY_u32
    | Il.Bits64 -> TY_u64
;;

let word_ty_signed_mach (abi:Abi.abi) : ty_mach =
  match word_bits abi with
      Il.Bits8 -> TY_i8
    | Il.Bits16 -> TY_i16
    | Il.Bits32 -> TY_i32
    | Il.Bits64 -> TY_i64
;;

let exterior_gc_allocation_size (abi:Abi.abi) (slot:Ast.slot) : int64 =
  (Int64.add
     (ty_sz abi (slot_ty slot))
     (word_n abi Abi.exterior_gc_header_size))
;;

let exterior_rc_allocation_size (abi:Abi.abi) (slot:Ast.slot) : int64 =
  (Int64.add
     (ty_sz abi (slot_ty slot))
     (word_n abi Abi.exterior_rc_header_size))
;;

let type_is_structured (t:Ast.ty) : bool =
  let fold = ty_fold_bool_or false in
  let fold = { fold with
                 ty_fold_tup = (fun _ -> true);
                 ty_fold_vec = (fun _ -> true);
                 ty_fold_rec = (fun _ -> true);
                 ty_fold_tag = (fun _ -> true);
                 ty_fold_iso = (fun _ -> true);
                 ty_fold_idx = (fun _ -> true);
                 ty_fold_fn = (fun _ -> true);
                 ty_fold_pred = (fun _ -> true);
                 ty_fold_obj = (fun _ -> true) }
  in
    fold_ty fold t
;;

let slot_mem_ctrl (slot:Ast.slot) : mem_ctrl =
  let ty = slot_ty slot in
    if type_is_mutable ty
    then
      match slot.Ast.slot_mode with
          Ast.MODE_exterior _ -> MEM_gc
        | _ -> MEM_interior
    else
      match ty with
          Ast.TY_port _ -> MEM_rc_opaque Abi.port_field_refcnt
        | Ast.TY_chan _ -> MEM_rc_opaque Abi.chan_field_refcnt
        | Ast.TY_task -> MEM_rc_opaque Abi.task_field_refcnt
            (* Vecs and strs are pseudo-exterior. *)
        | Ast.TY_vec _ -> MEM_rc_struct
        | Ast.TY_str -> MEM_rc_opaque Abi.exterior_rc_slot_field_refcnt
        | _ ->
            match slot.Ast.slot_mode with
                Ast.MODE_exterior _ when type_is_structured (slot_ty slot) ->
                  MEM_rc_struct
              | Ast.MODE_exterior _ ->
                  MEM_rc_opaque Abi.exterior_rc_slot_field_refcnt
              | _ ->
                  MEM_interior
;;


let iter_block_slots
    (cx:Semant.ctxt)
    (block_id:node_id)
    (fn:Ast.slot_key -> node_id -> Ast.slot -> unit)
    : unit =
  let block_slots = Hashtbl.find cx.ctxt_block_slots block_id in
    Hashtbl.iter
      begin
        fun key slot_id ->
          let slot = referent_to_slot cx slot_id in
            fn key slot_id slot
      end
      block_slots
;;

let iter_frame_slots
    (cx:Semant.ctxt)
    (frame_id:node_id)
    (fn:Ast.slot_key -> node_id -> Ast.slot -> unit)
    : unit =
  let blocks = Hashtbl.find cx.ctxt_frame_blocks frame_id in
    List.iter (fun block -> iter_block_slots cx block fn) blocks
;;

let iter_frame_and_arg_slots
    (cx:Semant.ctxt)
    (frame_id:node_id)
    (fn:Ast.slot_key -> node_id -> Ast.slot -> unit)
    : unit =
  iter_frame_slots cx frame_id fn;
  match htab_search cx.ctxt_frame_args frame_id with
      None -> ()
    | Some ls ->
        List.iter
          begin
            fun slot_id ->
              let key = Hashtbl.find cx.ctxt_slot_keys slot_id in
              let slot = referent_to_slot cx slot_id in
                fn key slot_id slot
          end
          ls
;;

let next_power_of_two (x:int64) : int64 =
  let xr = ref (Int64.sub x 1L) in
    xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 1);
    xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 2);
    xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 4);
    xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 8);
    xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 16);
    xr := Int64.logor (!xr) (Int64.shift_right_logical (!xr) 32);
    Int64.add 1L (!xr)
;;

let iter_tup_slots
    (get_element_ptr:'a -> int -> 'a)
    (dst_ptr:'a)
    (src_ptr:'a)
    (slots:Ast.ty_tup)
    (f:'a -> 'a -> Ast.slot -> (Ast.ty_iso option) -> unit)
    (curr_iso:Ast.ty_iso option)
    : unit =
  Array.iteri
    begin
      fun i slot ->
        f (get_element_ptr dst_ptr i)
          (get_element_ptr src_ptr i)
          slot curr_iso
    end
    slots
;;

let iter_rec_slots
    (get_element_ptr:'a -> int -> 'a)
    (dst_ptr:'a)
    (src_ptr:'a)
    (entries:Ast.ty_rec)
    (f:'a -> 'a -> Ast.slot -> (Ast.ty_iso option) -> unit)
    (curr_iso:Ast.ty_iso option)
    : unit =
  iter_tup_slots get_element_ptr dst_ptr src_ptr (Array.map snd entries) f curr_iso
;;




(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
