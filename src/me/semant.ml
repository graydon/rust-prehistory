
(* 
 * This module performs most of the semantic lowering:
 *
 *   - building frames full of typed slots
 *   - calculating the size of every type
 *   - resolving every name to a slot
 *   - checking type compatibility of every slot
 *   - running the type-state dataflow algorithm
 *   - checking type-states
 *   - inferring points of allocation and deallocation
 *)

open Common;;

let ty_nonce = ref 0 
;;

let next_ty_nonce _ = (ty_nonce := (!ty_nonce) + 1; !ty_nonce)
;;

exception Semant_err of ((node_id option) * string)
;;

let err (idopt:node_id option) = 
  let k s = 
    raise (Semant_err (idopt, s))
  in
    Printf.ksprintf k
;;


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
