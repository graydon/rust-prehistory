(*

   Our assembler is an all-at-once, buffer-in-memory job, very simple
   minded. I have 1gb of memory on my laptop: I don't expect to ever
   emit a program that large with this code.

   It is based on the 'frag' type, which has a variant for every major
   type of machine-blob we know how to write (bytes, zstrings, BSS
   blocks, words of various sorts).

   A frag can contain symbolic references between the sub-parts of
   it. These are accomplished through ref cells we call fixups, and a
   2-pass (resolution and writing) process defined recursively over
   the frag structure.

   Fixups are defined by wrapping a frag in a DEF pseudo-frag with
   a fixup attached. This will record information about the wrapped
   frag -- positions and sizes -- in the fixup during resolution.

   We say "positions" and "sizes" there, in plural, because both a
   file number and a memory number is recorded for each concept.

   File numbers refer to positions and sizes in the file we're
   generating, and are based on the native int type for the host
   platform -- usually 31 or 62 bits -- whereas the expressions that
   *use* position fixups tend to promote them up to 32 or 64 bits
   somehow. On a 32 bit platform, you can't generate output buffers
   with 64-bit positions (ocaml limitation!)

   Memory numbers are 64 bit, always, and refer to sizes and positions
   of frags when they are loaded into memory in the target. When
   you're generating code for a 32-bit target, or using a memory
   number in a context that's less than 64 bits, the value is
   range-checked and truncated. But in all other respects, we imagine
   a 32-bit address space is just the prefix of the continuing 64-bit
   address space. If you need to pin an object at a particular place
   from the point 2^32-1, say, you will need to do arithmetic and use
   the MEMPOS pseudo-frag, that sets the current memory position as
   it's being processed.

   Fixups can be *used* anywhere else in the frag tree, as many times
   as you like. If you try to write an unresolved fixup, the emitter
   faults. When you specify the use of a fixup, you need to specify
   whether you want to use its file size, file position, memory size,
   or memory position.

   Positions, addresses, sizes and such, of course, are in bytes.

   Expressions are evaluated to an int64 (signed), even if the
   expression is an int32 or less. Depending on how you use the result
   of the expression, a range check error may fire (for example, if
   the expression evaluates to -2^24 and you're emitting a word16).

   Word endianness is per-file. At the moment this seems acceptable.

   Because we want to be *very specific* about the time and place
   arithmetic promotions occur, we define two separate expression-tree
   types (with the same polymorphic constructors) and two separate
   evaluation functions, with an explicit operator for marking the
   promotion-points.

*)

open Common;;


let log (sess:Session.sess) =
  Session.log "asm"
    sess.Session.sess_log_asm
    sess.Session.sess_log_out
;;

let iflog (sess:Session.sess) (thunk:(unit -> unit)) : unit =
  if sess.Session.sess_log_asm
  then thunk ()
  else ()
;;

exception Bad_fit of string;;
exception Undef_sym of string;;

type ('a, 'b) expr =
    IMM of 'a
  | ADD of (('a, 'b) expr) * (('a, 'b) expr)
  | SUB of (('a, 'b) expr) * (('a, 'b) expr)
  | MUL of (('a, 'b) expr) * (('a, 'b) expr)
  | DIV of (('a, 'b) expr) * (('a, 'b) expr)
  | REM of (('a, 'b) expr) * (('a, 'b) expr)
  | SLL of (('a, 'b) expr) * int
  | SLR of (('a, 'b) expr) * int
  | SAR of (('a, 'b) expr) * int
  | AND of (('a, 'b) expr) * (('a, 'b) expr)
  | XOR of (('a, 'b) expr) * (('a, 'b) expr)
  | OR of (('a, 'b) expr) * (('a, 'b) expr)
  | NOT of (('a, 'b) expr)
  | F_POS of fixup
  | F_SZ of fixup
  | M_POS of fixup
  | M_SZ of fixup
  | EXT of 'b

type expr32 = (int32, int) expr
;;

type expr64 = (int64, expr32) expr
;;


let rec eval32 (e:expr32)
    : int32  =
  let chop64 kind name v =
    let x = Int64.to_int32 v in
      if (Int64.compare v (Int64.of_int32 x)) = 0 then
        x
      else raise (Bad_fit (kind
                           ^ " fixup "
                           ^ name
                           ^ " overflowed 32 bits in eval32: "
                           ^ Int64.to_string v))
  in
  let expandInt _ _ v = Int32.of_int v in
  let checkdef kind name v inj =
    match v with
        None -> raise (Undef_sym (kind ^ " fixup " ^ name ^ " undefined in eval32"))
      | Some x -> inj kind name x
  in
  match e with
      IMM i -> i
    | ADD (a, b) -> Int32.add (eval32 a) (eval32 b)
    | SUB (a, b) -> Int32.sub (eval32 a) (eval32 b)
    | MUL (a, b) -> Int32.mul (eval32 a) (eval32 b)
    | DIV (a, b) -> Int32.div (eval32 a) (eval32 b)
    | REM (a, b) -> Int32.rem (eval32 a) (eval32 b)
    | SLL (a, b) -> Int32.shift_left (eval32 a) b
    | SLR (a, b) -> Int32.shift_right_logical (eval32 a) b
    | SAR (a, b) -> Int32.shift_right (eval32 a) b
    | AND (a, b) -> Int32.logand (eval32 a) (eval32 b)
    | XOR (a, b) -> Int32.logxor (eval32 a) (eval32 b)
    | OR (a, b) -> Int32.logor (eval32 a) (eval32 b)
    | NOT a -> Int32.lognot (eval32 a)
    | F_POS f -> checkdef "file position" f.fixup_name f.fixup_file_pos expandInt
    | F_SZ f -> checkdef "file size" f.fixup_name f.fixup_file_sz expandInt
    | M_POS f -> checkdef "mem position" f.fixup_name f.fixup_mem_pos chop64
    | M_SZ f -> checkdef "mem size" f.fixup_name f.fixup_mem_sz chop64
    | EXT i -> Int32.of_int i
;;

let rec eval64 (e:expr64)
    : int64  =
  let checkdef kind name v inj =
    match v with
        None -> raise (Undef_sym (kind ^ " fixup '" ^ name ^ "' undefined in eval64"))
      | Some x -> inj x
  in
  match e with
      IMM i -> i
    | ADD (a, b) -> Int64.add (eval64 a) (eval64 b)
    | SUB (a, b) -> Int64.sub (eval64 a) (eval64 b)
    | MUL (a, b) -> Int64.mul (eval64 a) (eval64 b)
    | DIV (a, b) -> Int64.div (eval64 a) (eval64 b)
    | REM (a, b) -> Int64.rem (eval64 a) (eval64 b)
    | SLL (a, b) -> Int64.shift_left (eval64 a) b
    | SLR (a, b) -> Int64.shift_right_logical (eval64 a) b
    | SAR (a, b) -> Int64.shift_right (eval64 a) b
    | AND (a, b) -> Int64.logand (eval64 a) (eval64 b)
    | XOR (a, b) -> Int64.logxor (eval64 a) (eval64 b)
    | OR (a, b) -> Int64.logor (eval64 a) (eval64 b)
    | NOT a -> Int64.lognot (eval64 a)
    | F_POS f -> checkdef "file position" f.fixup_name f.fixup_file_pos Int64.of_int
    | F_SZ f -> checkdef "file size" f.fixup_name f.fixup_file_sz Int64.of_int
    | M_POS f -> checkdef "mem position" f.fixup_name f.fixup_mem_pos (fun x -> x)
    | M_SZ f -> checkdef "mem size" f.fixup_name f.fixup_mem_sz (fun x -> x)
    | EXT e -> Int64.of_int32 (eval32 e)
;;

type frag =
    MARK  (* MARK == 'PAD (IMM 0L)' *)
  | SEQ of frag array
  | PAD of int
  | BSS of int64
  | MEMPOS of int64
  | BYTE of int
  | BYTES of int array
  | CHAR of char
  | STRING of string
  | ZSTRING of string
  | ULEB128 of expr64
  | SLEB128 of expr64
  | WORD of (ty_mach * expr64)
  | ALIGN_FILE of (int * frag)
  | ALIGN_MEM of (int * frag)
  | DEF of (fixup * frag)
  | RELAX of relaxation

and relaxation =
    { relax_options: frag array;
      relax_choice: int ref; }
;;

exception Relax_more of relaxation;;

let new_relaxation (frags:frag array) =
  RELAX { relax_options = frags;
          relax_choice = ref 0; }
;;


let log sess = Session.log "asm"
  sess.Session.sess_log_asm
  sess.Session.sess_log_out
;;


let rec resolve_frag (sess:Session.sess) (frag:frag) : unit =
  let relaxations = ref [] in
  let reset_relaxation rel =
    rel.relax_choice := ((Array.length rel.relax_options) - 1);
  in
  let real_collector rel =
    reset_relaxation rel;
    relaxations := rel :: (!relaxations)
  in
  let dummy_collector _ = () in
  let _ = resolve_frag_full real_collector frag in
  let dummy_buffer = Buffer.create 0xffff in
  let relax r =
    r.relax_choice := (!(r.relax_choice)) - 1
  in
  let still_relaxing frag =
    try
      Buffer.reset dummy_buffer;
      resolve_frag_full dummy_collector frag;
      lower_frag sess true dummy_buffer frag;
      false
    with
        Relax_more r ->
          begin
            relax r;
            true
          end
  in
    while still_relaxing frag do
      log sess "relaxing";
    done;
    resolve_frag_full dummy_collector frag

and resolve_frag_full (relaxation_collector:relaxation -> unit) (frag:frag)
    : unit =
  let file_pos = ref 0 in
  let mem_pos = ref 0L in
  let bump i =
    mem_pos := Int64.add (!mem_pos) (Int64.of_int i);
    file_pos := (!file_pos) + i
  in

  let uleb (e:expr64) : unit =
    let rec loop value =
      let value = Int64.shift_right_logical value 7 in
        if value = 0L
        then bump 1
        else
          begin
            bump 1;
            loop value
          end
    in
      loop (eval64 e)
  in

  let sleb (e:expr64) : unit =
    let rec loop value =
      let byte = Int64.logand value 0xf7L in
      let value = Int64.shift_right value 7 in
      let signbit = Int64.logand byte 0x40L in
        if (((value = 0L) && (signbit = 0L)) ||
              ((value = -1L) && (signbit = 0x40L)))
        then bump 1
        else
          begin
            bump 1;
            loop value
          end
    in
      loop (eval64 e)
  in
  let rec resolve_frag it =
    match it with
      | MARK -> ()
      | SEQ frags -> Array.iter resolve_frag frags
      | PAD i -> bump i
      | BSS i -> mem_pos := Int64.add (!mem_pos) i
      | MEMPOS i -> mem_pos := i
      | BYTE _ -> bump 1
      | BYTES ia -> bump (Array.length ia)
      | CHAR _ -> bump 1
      | STRING s -> bump (String.length s)
      | ZSTRING s -> bump ((String.length s) + 1)
      | ULEB128 e -> uleb e
      | SLEB128 e -> sleb e
      | WORD (mach,_) -> bump (bytes_of_ty_mach mach)
      | ALIGN_FILE (n, frag) ->
          let spill = (!file_pos) mod n in
          let pad = (n - spill) mod n in
            file_pos := (!file_pos) + pad;
            (*
             * NB: aligning the file *causes* likewise alignment of
             * memory, since we implement "file alignment" by
             * padding!
             *)
            mem_pos := Int64.add (!mem_pos) (Int64.of_int pad);
            resolve_frag frag

      | ALIGN_MEM (n, frag) ->
          let n64 = Int64.of_int n in
          let spill = Int64.rem (!mem_pos) n64 in
          let pad = Int64.rem (Int64.sub n64 spill) n64 in
            mem_pos := Int64.add (!mem_pos) pad;
            resolve_frag frag

      | DEF (f, i) ->
          let fpos1 = !file_pos in
          let mpos1 = !mem_pos in
            resolve_frag i;
            f.fixup_file_pos <- Some fpos1;
            f.fixup_mem_pos <- Some mpos1;
            f.fixup_file_sz <- Some ((!file_pos) - fpos1);
            f.fixup_mem_sz <- Some (Int64.sub (!mem_pos) mpos1)

      | RELAX rel ->
          (relaxation_collector rel;
           resolve_frag rel.relax_options.(!(rel.relax_choice)))
  in
    resolve_frag frag

and lower_frag
    ~(sess:Session.sess)
    ~(lsb0:bool)
    ~(buf:Buffer.t)
    ~(it:frag)
    : unit =
  let byte (i:int) =
    if i < 0
    then raise (Bad_fit "byte underflow")
    else
      if i > 255
      then raise (Bad_fit "byte overflow")
      else Buffer.add_char buf (Char.chr i)
  in

  let uleb (e:expr64) : unit =
    let emit1 k = Buffer.add_char buf (Char.chr (Int64.to_int k)) in
    let rec loop value =
      let byte = Int64.logand value 0x7fL in
      let value = Int64.shift_right_logical value 7 in
        if value = 0L
        then emit1 byte
        else
          begin
            emit1 (Int64.logor byte 0x80L);
            loop value
          end
    in
      loop (eval64 e)
  in

  let sleb (e:expr64) : unit =
    let emit1 k = Buffer.add_char buf (Char.chr (Int64.to_int k)) in
    let rec loop value =
      let byte = Int64.logand value 0x7fL in
      let value = Int64.shift_right value 7 in
      let signbit = Int64.logand byte 0x40L in
        if (((value = 0L) && (signbit = 0L)) ||
              ((value = -1L) && (signbit = 0x40L)))
        then emit1 byte
        else
          begin
            emit1 (Int64.logor byte 0x80L);
            loop value
          end
    in
      loop (eval64 e)
  in

  let word (nbytes:int) (signed:bool) (e:expr64) =
    let i = eval64 e in

    (*
       FIXME:

       We should really base the entire assembler and memory-position
       system on Big_int.big_int, but in ocaml the big_int type lacks,
       oh, just about every useful function (no format string spec, no
       bitwise ops, blah blah) so it's useless; we're stuck on int64
       for bootstrapping.

       For the time being we're just going to require you to represent
       those few unsigned 64 bit terms you have in mind via their
       signed bit pattern. Suboptimal but it's the best we can do.
    *)

    let (top,bot) =
      if nbytes >= 8
      then
        if signed
        then (Int64.max_int,Int64.min_int)
        else (Int64.max_int,0L)
      else
        if signed
        then
          let bound = (Int64.shift_left 1L ((8 * nbytes) - 1)) in
            (Int64.sub bound 1L, Int64.neg bound)
        else
          let bound = (Int64.shift_left 1L (8 * nbytes)) in
            (Int64.sub bound 1L, 0L)
    in

    let mask1 = Int64.logand 0xffL in
    let shift = Int64.shift_right_logical in
    let emit1 k = Buffer.add_char buf (Char.chr (Int64.to_int k)) in
      if Int64.compare i bot = (-1)
      then raise (Bad_fit ("word underflow: "
                           ^ (Int64.to_string i)
                           ^ " into "
                           ^ (string_of_int nbytes)
                           ^ (if signed then " signed" else " unsigned")
                           ^ " bytes"))
      else
        if Int64.compare i top = 1
        then raise (Bad_fit ("word overflow: "
                             ^ (Int64.to_string i)
                             ^ " into "
                             ^ (string_of_int nbytes)
                             ^ (if signed then " signed" else " unsigned")
                             ^ " bytes"))
        else
          if lsb0
          then
            for n = 0 to (nbytes - 1) do
              emit1 (mask1 (shift i (8*n)))
            done
          else
            for n = (nbytes - 1) downto 0 do
              emit1 (mask1 (shift i (8*n)))
            done
  in
    match it with
        MARK -> ()

      | SEQ frags ->
          Array.iter (lower_frag_2 sess lsb0 buf) frags

      | PAD c ->
          for i = 1 to c do
            Buffer.add_char buf '\x00'
          done

      | BSS _ -> ()

      | MEMPOS _ -> ()

      | BYTE i -> byte i

      | BYTES bs ->
          log sess "lowering %d bytes" (Array.length bs);
          Array.iter byte bs

      | CHAR c ->
          log sess "lowering char: %c" c;
          Buffer.add_char buf c

      | STRING s ->
          log sess "lowering string: %s" s;
          Buffer.add_string buf s

      | ZSTRING s ->
          log sess "lowering zstring: %s" s;
          Buffer.add_string buf s;
          byte 0

      | ULEB128 e -> uleb e
      | SLEB128 e -> sleb e

      | WORD (m,e) ->
          log sess "lowering word %s"
            (string_of_ty_mach m);
          word (bytes_of_ty_mach m) (mach_is_signed m) e

      | ALIGN_FILE (n, frag) ->
          let spill = (Buffer.length buf) mod n in
          let pad = (n - spill) mod n in
            for i = 1 to pad do
              Buffer.add_char buf '\x00'
            done;
            lower_frag_2 sess lsb0 buf frag

      | ALIGN_MEM (_, i) -> lower_frag_2 sess lsb0 buf i
      | DEF (f, i) ->
          log sess "lowering fixup: %s" f.fixup_name;
          lower_frag_2 sess lsb0 buf i;

      | RELAX rel ->
          try
            lower_frag_2 sess lsb0 buf rel.relax_options.(!(rel.relax_choice))
          with
              Bad_fit _ -> raise (Relax_more rel)

(*
(*
  * We need to ensure that if the DEF advanced the file_pos
  * associated with the fixup -- indicative of an aligned MARK
  * or similar -- that we pad out to the recorded size.
*)
          lower_frag_2 lsb0 buf i;
          let len = Buffer.length buf in
          match (f.fixup_file_pos, f.fixup_file_sz) with
              (Some fp, Some fs) ->
                let target_len = fp + fs in
                  for i = 1 to (target_len - len)
                  do
                    Buffer.add_char buf '\x00'
                  done
            | _ -> ()
*)
and
    (* Some odd recursion bug? Unifier gets sad without this indirection. *)
    lower_frag_2 sess lsb0 buf i = lower_frag sess lsb0 buf i
;;

let fold_flags (f:'a -> int64) (flags:'a list) : int64 =
  List.fold_left (Int64.logor) 0x0L (List.map f flags)
;;


(* Asm-reader stuff for loading info back from mapped files. *)
(*
 * Unfortunately the ocaml Bigarray interface takes 'int' indices, so
 * f.e. can't do 64-bit offsets / files when running on a 32bit platform.
 * Despite the fact that we can possibly produce them. Sigh. Yet another
 * "bootstrap compiler limitation".
 *)
type asm_reader =
    {
      asm_seek: int -> unit;
      asm_get_u32: unit -> int;
      asm_get_u16: unit -> int;
      asm_get_u8: unit -> int;
      asm_get_uleb: unit -> int;
      asm_get_zstr: unit -> string;
      asm_get_zstr_padded: int -> string;
      asm_get_off: unit -> int;
      asm_adv: int -> unit;
      asm_adv_u32: unit -> unit;
      asm_adv_u16: unit -> unit;
      asm_adv_u8: unit -> unit;
      asm_adv_zstr: unit -> unit;
    }
;;

type mmap_arr = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;;

let new_asm_reader (sess:Session.sess) (s:filename) : asm_reader =
  log sess "opening file %s" s;
  let fd = Unix.openfile s [ Unix.O_RDONLY ] 0 in
  let arr = (Bigarray.Array1.map_file
               fd ~pos:0L
               Bigarray.int8_unsigned
               Bigarray.c_layout
               false (-1))
  in
  let tmp = ref Nativeint.zero in
  let buf = Buffer.create 16 in
  let off = ref 0 in
  let get_word_as_int (nbytes:int) : int =
  let lsb0 = true in
    tmp := Nativeint.zero;
    if lsb0
    then
      for j = nbytes-1 downto 0 do
        tmp := Nativeint.shift_left (!tmp) 8;
        tmp := Nativeint.logor (!tmp) (Nativeint.of_int arr.{(!off) + j})
      done
    else
      for j = 0 to nbytes-1 do
        tmp := Nativeint.shift_left (!tmp) 8;
        tmp := Nativeint.logor (!tmp) (Nativeint.of_int arr.{(!off) + j})
      done;
    off := (!off) + nbytes;
    Nativeint.to_int (!tmp)
  in
  let get_zstr_padded pad_opt =
    let i = ref (!off) in
      Buffer.clear buf;
      let buflen_ok _ =
        match pad_opt with
            None -> true
          | Some pad -> (Buffer.length buf) < pad
      in
      while arr.{!i} != 0 && (buflen_ok()) do
        Buffer.add_char buf (Char.chr arr.{!i});
        incr i
      done;
      begin
        match pad_opt with
            None -> off := (!off) + (Buffer.length buf) + 1
          | Some pad ->
              begin
                assert ((Buffer.length buf) <= pad);
                off := (!off) + pad
              end
      end;
      Buffer.contents buf
  in
  let bump i = off := (!off) + i in
    {
      asm_seek = (fun i -> off := i);
      asm_get_u32 = (fun _ -> get_word_as_int 4);
      asm_get_u16 = (fun _ -> get_word_as_int 2);
      asm_get_u8 = (fun _ -> get_word_as_int 1);
      asm_get_uleb =
        begin
          fun _ ->
            let hi_mask = 0x80 in
            let lo_mask = 0x7f in
            let rec accum i =
              tmp := Nativeint.logor (!tmp) (Nativeint.of_int (i land lo_mask));
              if (i land hi_mask) = hi_mask
              then
                begin
                  tmp := Nativeint.shift_left (!tmp) 7;
                  incr off;
                  accum arr.{!off}
                end
              else
                Nativeint.to_int (!tmp)
            in
            let first = arr.{!off} in
              incr off;
              if first = 0
              then 0
              else
                begin
                  tmp := Nativeint.zero;
                  accum first
                end
        end;
      asm_get_zstr = (fun _ -> get_zstr_padded None);
      asm_get_zstr_padded = (fun pad -> get_zstr_padded (Some pad));
      asm_get_off = (fun _ -> !off);
      asm_adv = bump;
      asm_adv_u32 = (fun _ -> bump 4);
      asm_adv_u16 = (fun _ -> bump 2);
      asm_adv_u8 = (fun _ -> bump 1);
      asm_adv_zstr = (fun _ -> while arr.{!off} != 0
                      do incr off done)
    }
;;




(*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C ../.. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 *)
