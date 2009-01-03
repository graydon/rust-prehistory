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

let int_of_node (Node i) = i 
let int_of_temp (Temp i) = i 
let int_of_opaque (Opaque i) = i 

type 'a identified = { node: 'a; id: node_id }

type target = 
	Linux_x86_elf
  | Win32_x86_pe
;;


type abi_pseudo_reg = 
	FP (* frame pointer *)
  | PP (* process pointer *)
  | CP (* crate pointer *)
  | RP (* runtime pointer *)
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


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
