type t = {
  storage: int64 array;
  nbits: int;
}
;;

let create nbits flag =
  { storage = Array.make (nbits / 64 + 1) (if flag then Int64.lognot 0L else 0L);
    nbits = nbits }
;;

(* 
 * mutate v0 in place: v0.(i) <- v0.(i) op v1.(i), returning bool indicating
 * whether any bits in v0 changed in the process. 
 *)
let process (op:int64 -> int64 -> int64) (v0:t) (v1:t) : bool =
  let changed = ref false in
    assert (v0.nbits = v1.nbits);
    assert ((Array.length v0.storage) = (Array.length v1.storage));
    Array.iteri
      begin
        fun i w1 ->
          let w0 = v0.storage.(i) in
          let w0' = op w0 w1 in
            if not (w0' = w0)
            then changed := true;
            v0.storage.(i) <- w0';
      end
      v1.storage;
    !changed
;;

let union = process Int64.logor ;;
let intersect = process Int64.logand ;;
let copy = process (fun _ w1 -> w1) ;;

let get (v:t) (i:int) : bool =
  assert (i >= 0);
  assert (i < v.nbits);
  let w = i / 64 in
  let b = i mod 64 in
  let x = Int64.logand 1L (Int64.shift_right_logical v.storage.(w) b) in
    x = 1L
;;

let equal (v1:t) (v0:t) : bool =
  v0 = v1
;;

let clear (v:t) : unit =
  for i = 0 to (Array.length v.storage) - 1
  do
    v.storage.(i) <- 0L
  done
;;

let invert (v:t) : unit =
  for i = 0 to (Array.length v.storage) - 1
  do
    v.storage.(i) <- Int64.lognot v.storage.(i)
  done
;;

let set (v:t) (i:int) (x:bool) : unit =
  assert (i >= 0);
  assert (i < v.nbits);
  let w = i / 64 in
  let b = i mod 64 in
  let w0 = v.storage.(w) in
  let flag = Int64.shift_left 1L b in
    v.storage.(w) <-
      if x
      then Int64.logor w0 flag
      else Int64.logand w0 (Int64.lognot flag)
;;

let to_list (v:t) : int list =
  if v.nbits = 0
  then []
  else
    let accum = ref [] in
    let word = ref v.storage.(0) in
      for i = 0 to v.nbits do
        if i mod 64 = 0
        then word := v.storage.(i / 64);
        if (Int64.logand 1L (!word)) = 1L
        then accum := i :: (!accum);
        word := Int64.shift_right_logical (!word) 1;
      done;
      !accum
;;

