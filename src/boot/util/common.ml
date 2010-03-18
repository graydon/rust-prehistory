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
;;

let bug _ =
  let k s = failwith s
  in Printf.ksprintf k
;;

(* Some ubiquitous low-level types. *)

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
  | TY_f32
  | TY_f64
;;

let mach_is_integral (mach:ty_mach) : bool =
  match mach with
      TY_s8 | TY_s16 | TY_s32 | TY_s64
    | TY_u8 | TY_u16 | TY_u32 | TY_u64 -> true
    | TY_f32 | TY_f64 -> false
;;


let mach_is_signed (mach:ty_mach) : bool =
  match mach with
      TY_s8 | TY_s16 | TY_s32 | TY_s64 -> true
    | TY_u8 | TY_u16 | TY_u32 | TY_u64
    | TY_f32 | TY_f64 -> false
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
  | TY_f32 -> "f32"
  | TY_f64 -> "f64"
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
  | TY_f32 -> 4
  | TY_f64 -> 8
;;

type ty_param_idx = int
;;

type nabi_conv =
    CONV_rust
  | CONV_cdecl
;;

type nabi = { nabi_indirect: bool;
              nabi_convention: nabi_conv }
;;

let string_to_conv (a:string) : nabi_conv option =
  match a with
      "cdecl" -> Some CONV_cdecl
    | "rust" -> Some CONV_rust
    | _ -> None

(* FIXME: remove this when native items go away. *)
let string_to_nabi (s:string) (indirect:bool) : nabi option =
  match string_to_conv s with
      None -> None
    | Some c ->
        Some { nabi_indirect = indirect;
               nabi_convention = c }
;;

type import_lib_spec =
    {
      import_libname: string;
      import_prefix: int;
    }
;;

type import_lib =
    IMPORT_LIB_rustrt
  | IMPORT_LIB_crt
  | IMPORT_LIB_rust of import_lib_spec
  | IMPORT_LIB_c of import_lib_spec
;;

type segment =
    SEG_text
  | SEG_data
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


(*
 * Auxiliary hashtable functions.
 *)

let htab_keys (htab:('a,'b) Hashtbl.t) : ('a list) =
  Hashtbl.fold (fun k _ accum -> k :: accum) htab []
;;

let sorted_htab_keys (tab:('a, 'b) Hashtbl.t) : 'a array =
  let keys = Array.of_list (htab_keys tab) in
    Array.sort compare keys;
    keys
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

let htab_search_or_default (htab:('a,'b) Hashtbl.t) (k:'a) (def:unit -> 'b) : 'b =
  match htab_search htab k with
      Some v -> v
    | None -> def()
;;

let htab_search_or_add (htab:('a,'b) Hashtbl.t) (k:'a) (mk:unit -> 'b) : 'b =
  let def () =
    let v = mk() in
      Hashtbl.add htab k v;
      v
  in
    htab_search_or_default htab k def
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


let htab_fold (fn:'a -> 'b -> 'c -> 'c)  (init:'c) (h:('a, 'b) Hashtbl.t) : 'c =
  let accum = ref init in
  let f a b = accum := (fn a b (!accum)) in
    Hashtbl.iter f h;
    !accum
;;


let reduce_hash_to_list (fn:'a -> 'b -> 'c)  (h:('a, 'b) Hashtbl.t) : ('c list) =
  htab_fold (fun a b ls -> (fn a b) :: ls) [] h
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
      None -> bug () "atab_find: element not found"
    | Some b -> b

let atab_mem (atab:('a * 'b) array) (a:'a) : bool =
  match atab_search atab a with
      None -> false
    | Some _ -> true

let rec ltab_search (ltab:('a * 'b) list) (a:'a) : ('b option) =
  match ltab with
      [] -> None
    | (k,v)::_ when k = a -> Some v
    | _::lz -> ltab_search lz a

let ltab_put (ltab:('a * 'b) list) (a:'a) (b:'b) : (('a * 'b) list) =
  assert ((ltab_search ltab a) = None);
  (a,b)::ltab

(*
 * Auxiliary list functions.
 *)

let rec list_search (list:'a list) (f:'a -> 'b option) : ('b option) =
  match list with
      [] -> None
    | a::az ->
        match f a with
            Some b -> Some b
          | None -> list_search az f

let rec list_search_ctxt
    (list:'a list)
    (f:'a -> 'b option)
    : ((('a list) * 'b) option) =
  match list with
      [] -> None
    | a::az ->
        match f a with
            Some b -> Some (list, b)
          | None -> list_search_ctxt az f

let rec list_drop n ls =
  if n = 0
  then ls
  else list_drop (n-1) (List.tl ls)
;;


(*
 * Auxiliary option functions.
 *)

let bool_of_option x =
  match x with
      Some _ -> true
    | None -> false


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

let arr_idx (arr:'a array) (a:'a) : int =
  let find i v = if v = a then Some i else None in
    match arr_search arr find with
        None -> bug () "arr_idx: element not found"
      | Some i -> i
;;

let arr_map_partial (a:'a array) (f:'a -> 'b option) : 'b array =
  let accum a ls =
    match f a with
        None -> ls
      | Some b -> b :: ls
  in
    Array.of_list (Array.fold_right accum a [])
;;

let arr_filter_some (a:'a option array) : 'a array =
  arr_map_partial a (fun x -> x)
;;

let arr_find_dups (a:'a array) : ('a * 'a) option =
  let copy = Array.copy a in
    Array.sort compare copy;
    let lasti = (Array.length copy) - 1 in
    let rec find_dups i =
      if i < lasti then
        let this = copy.(i) in
        let next = copy.(i+1) in
          (if (this = next) then
             Some (this, next)
           else
             find_dups (i+1))
      else
        None
    in
      find_dups 0
;;

let arr_check_dups (a:'a array) (f:'a -> 'a -> unit) : unit =
  match arr_find_dups a with
      Some (x, y) -> f x y
    | None -> ()
;;

(* FIXME: use Array.build or whatever it's called for efficiency *)
let arr_map2 (f:'a -> 'b -> 'c) (a:'a array) (b:'b array) : 'c array =
  assert ((Array.length a) = (Array.length b));
  Array.init (Array.length a) (fun i -> f a.(i) b.(i))
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
let i64_align (align:int64) (v:int64) : int64 =
  (assert (align <> 0L));
  let padding = Int64.rem (Int64.sub align (Int64.rem v align)) align in
    Int64.add v padding
;;


let rec i64_for (lo:int64) (hi:int64) (thunk:int64 -> unit) : unit =
  if i64_lt lo hi then
    begin
      thunk lo;
      i64_for (Int64.add lo 1L) hi thunk;
    end
;;

let rec i64_for_rev (hi:int64) (lo:int64) (thunk:int64 -> unit) : unit =
  if i64_ge hi lo then
    begin
      thunk hi;
      i64_for_rev (Int64.sub hi 1L) lo thunk;
    end
;;


(*
 * Size-expressions.
 *)


type size =
    SIZE_fixed of int64
  | SIZE_param_size of ty_param_idx
  | SIZE_param_align of ty_param_idx
  | SIZE_rt_add of size * size
  | SIZE_rt_max of size * size
  | SIZE_rt_align of size * size
;;

let add_sz (a:size) (b:size) : size =
  match (a, b) with
      (SIZE_fixed a, SIZE_fixed b) -> SIZE_fixed (Int64.add a b)
    | (a, SIZE_fixed b) -> SIZE_rt_add (SIZE_fixed b, a)
    | (a, b) -> SIZE_rt_add (a, b)
;;

let max_sz (a:size) (b:size) : size =
  match (a, b) with
      (SIZE_fixed a, SIZE_fixed b) -> SIZE_fixed (i64_max a b)
    | (a, SIZE_fixed b) -> SIZE_rt_max (SIZE_fixed b, a)
    | (a, b) -> SIZE_rt_max (a, b)
;;

let align_sz (a:size) (b:size) : size =
  match (a, b) with
      (SIZE_fixed a, SIZE_fixed b) -> SIZE_fixed (i64_align a b)
    | (a, SIZE_fixed b) -> SIZE_rt_align (SIZE_fixed b, a)
    | (a, b) -> SIZE_rt_align (a, b)
;;

let force_sz (a:size) : int64 =
  match a with
      SIZE_fixed i -> i
    | _ -> bug () "force_sz: forced non-fixed size expression"
;;

(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
