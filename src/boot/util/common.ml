(*
 * This module goes near the *bottom* of the dependency DAG, and holds basic
 * types shared across all phases of the compiler.
 *)

type filename = string
type pos = (filename * int * int)
type span = {lo: pos; hi: pos}

type node_id = Node of int
type temp_id = Temp of int
type opaque_id = Opaque of int
type constr_id = Constr of int

let int_of_node (Node i) = i
let int_of_temp (Temp i) = i
let int_of_opaque (Opaque i) = i
let int_of_constr (Constr i) = i

type 'a identified = { node: 'a; id: node_id }

type target =
    Linux_x86_elf
  | Win32_x86_pe
  | MacOS_x86_macho
;;

type ty_mach =
    TY_u8
  | TY_u16
  | TY_u32
  | TY_u64
  | TY_s8
  | TY_s16
  | TY_s32
  | TY_s64
  | TY_b64
;;

let string_of_ty_mach (mach:ty_mach) : string =
  match mach with
    TY_u8 -> "u8"
  | TY_u16 -> "u16"
  | TY_u32 -> "u32"
  | TY_u64 -> "u64"
  | TY_s8 -> "s8"
  | TY_s16 -> "s16"
  | TY_s32 -> "s32"
  | TY_s64 -> "s64"
  | TY_b64 -> "b64"
;;

let bytes_of_ty_mach (mach:ty_mach) : int =
  match mach with
    TY_u8 -> 1
  | TY_u16 -> 2
  | TY_u32 -> 4
  | TY_u64 -> 8
  | TY_s8 -> 1
  | TY_s16 -> 2
  | TY_s32 -> 4
  | TY_s64 -> 8
  | TY_b64 -> 8
;;

let ty_mach_signed (mach:ty_mach) : bool =
  match mach with
      TY_s8 | TY_s16 | TY_s32 | TY_s64 -> true
    | _ -> false
;;

type fixup =
    { fixup_name: string;
      mutable fixup_file_pos: int option;
      mutable fixup_file_sz: int option;
      mutable fixup_mem_pos: int64 option;
      mutable fixup_mem_sz: int64 option }
;;


let new_fixup (s:string)
    : fixup =
  { fixup_name = s;
    fixup_file_pos = None;
    fixup_file_sz = None;
    fixup_mem_pos = None;
    fixup_mem_sz = None }
;;


type layout =
    {
      mutable layout_size: int64;
      mutable layout_offset: int64;
      mutable layout_align: int64;
    }
;;

(*
 * Auxiliary hashtable functions.
 *)

let htab_keys (htab:('a,'b) Hashtbl.t) : ('a list) =
  Hashtbl.fold (fun k _ accum -> k :: accum) htab []
;;

let htab_vals (htab:('a,'b) Hashtbl.t) : ('b list)  =
  Hashtbl.fold (fun _ v accum -> v :: accum) htab []
;;

let htab_pairs (htab:('a,'b) Hashtbl.t) : (('a * 'b) list) =
  Hashtbl.fold (fun k v accum -> (k,v) :: accum) htab []
;;

let htab_search (htab:('a,'b) Hashtbl.t) (k:'a) : ('b option) =
  if Hashtbl.mem htab k
  then Some (Hashtbl.find htab k)
  else None
;;

let htab_put (htab:('a,'b) Hashtbl.t) (a:'a) (b:'b) : unit =
  assert (not (Hashtbl.mem htab a));
  Hashtbl.add htab a b
;;

let htab_map (htab:('a,'b) Hashtbl.t) (f:'a -> 'b -> ('c * 'd)) : (('c,'d) Hashtbl.t) =
  let ntab = Hashtbl.create (Hashtbl.length htab) in
  let g a b =
    let (c,d) = f a b in
      htab_put ntab c d
  in
    Hashtbl.iter g htab;
    ntab
;;

(* 
 * Auxiliary association-array and association-list operations.
 *)
let atab_search (atab:('a * 'b) array) (a:'a) : ('b option) =
  let lim = Array.length atab in
  let rec step i =
    if i = lim
    then None
    else
      let (k,v) = atab.(i) in
        if k = a
        then Some v
        else step (i+1)
  in
    step 0

let atab_find (atab:('a * 'b) array) (a:'a) : 'b =
  match atab_search atab a with
      None -> failwith "Not found"
    | Some b -> b

let rec ltab_search (ltab:('a * 'b) list) (a:'a) : ('b option) =
  match ltab with
      [] -> None
    | (k,v)::_ when k = a -> Some v
    | _::lz -> ltab_search lz a

let ltab_put (ltab:('a * 'b) list) (a:'a) (b:'b) : (('a * 'b) list) =
  assert ((ltab_search ltab a) = None);
  (a,b)::ltab

(*
 * Auxiliary stack functions.
 *)

let stk_fold (s:'a Stack.t) (f:'a -> 'b -> 'b) (x:'b) : 'b =
  let r = ref x in
    Stack.iter (fun e -> r := f e (!r)) s;
    !r

let stk_elts_from_bot (s:'a Stack.t) : ('a list) =
  stk_fold s (fun x y -> x::y) []

let stk_elts_from_top (s:'a Stack.t) : ('a list) =
  List.rev (stk_elts_from_bot s)

let stk_search (s:'a Stack.t) (f:'a -> 'b option) : 'b option =
  stk_fold s (fun e accum -> match accum with None -> (f e) | x -> x) None


(*
 * Auxiliary array functions.
 *)

let arr_search (a:'a array) (f:int -> 'a -> 'b option) : 'b option =
  let max = Array.length a in
  let rec iter i =
    if i < max
    then
      let v = a.(i) in
      let r = f i v in
        match r with
            Some _ -> r
          | None -> iter (i+1)
    else
      None
  in
    iter 0
;;

let arr_map_partial (a:'a array) (f:'a -> 'b option) : 'b array =
  let accum a ls =
    match f a with
        None -> ls
      | Some b -> b :: ls
  in
    Array.of_list (Array.fold_right accum a [])
;;

(*
 * Auxiliary int64 functions
 *)

let i64_lt (a:int64) (b:int64) : bool = (Int64.compare a b) < 0
let i64_le (a:int64) (b:int64) : bool = (Int64.compare a b) <= 0
let i64_ge (a:int64) (b:int64) : bool = (Int64.compare a b) >= 0
let i64_gt (a:int64) (b:int64) : bool = (Int64.compare a b) > 0
let i64_max (a:int64) (b:int64) : int64 = (if (Int64.compare a b) > 0 then a else b)
let i64_min (a:int64) (b:int64) : int64 = (if (Int64.compare a b) < 0 then a else b)

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
