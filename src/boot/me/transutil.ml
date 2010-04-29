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


(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
