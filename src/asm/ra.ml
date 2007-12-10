open Il;;

(* Poletto and Sarkar's linear-scan algorithm. *)

type live_interval = { live_vreg: int;
		       live_slot: slot;
		       live_startpoint: int;
		       live_endpoint: int; }

let fmt_live_interval out li = 
  Printf.fprintf out "vreg %d = %a : [%d,%d]" 
    li.live_vreg 
    fmt_slot li.live_slot
    li.live_startpoint
    li.live_endpoint
;;


module StartOrderedIntervals = 
struct
  type t = live_interval
  let compare x y = 
    if x.live_startpoint = y.live_startpoint
    then compare x.live_vreg y.live_vreg
    else compare x.live_startpoint y.live_startpoint
end
;;

module EndOrderedIntervals = 
struct
  type t = live_interval
  let compare x y = 
    if x.live_endpoint = y.live_endpoint
    then compare x.live_vreg y.live_vreg
    else compare x.live_endpoint y.live_endpoint
end
;;

module OrderedInts = 
struct
  type t = int
  let compare x y = compare x y
end
;;
  
(* Live Intervals *)
module LI = Set.Make(StartOrderedIntervals);;

(* Active Intervals *)
module AI = Set.Make(EndOrderedIntervals);;

(* Hard Registers *)
module HR = Set.Make(OrderedInts);;

let print_live_intervals is = 
  LI.iter (Printf.printf "%a\n" fmt_live_interval) is
;;

let convert_regs_using_intervals intervals e =
  let vreg_slots = Array.create e.emit_next_vreg Nil in
  let convert_slot s = 
    match s with 
	Vreg i -> vreg_slots.(i)
      | x -> x
  in
  let convert_quad q = 
    { q with 
	quad_dst = convert_slot q.quad_dst;
	quad_lhs = convert_slot q.quad_lhs;
	quad_rhs = convert_slot q.quad_rhs }
  in
    LI.iter (fun i -> vreg_slots.(i.live_vreg) <- i.live_slot) intervals;
    e.emit_quads <- Array.map convert_quad e.emit_quads
;;

let remove_redundant_moves e =
  let useful q = not (q.quad_op = MOV && q.quad_dst = q.quad_lhs) in
    e.emit_quads <- 
      Array.of_list 
      (List.filter useful 
	 (Array.to_list e.emit_quads))
;;


(* 
 * On some ISAs we have hard register constraints. An example of this is 
 * on x86, where we have MUL placing its result in (EDX,EAX) no matter
 * what. 
 * 
 * In this case, when we have a quad of the form 
 * 
 *   i: (MUL,vreg dst, vreg a, vreg b) 
 * 
 * we have an ISA-specific "constraint" rule that replaces it with the following, 
 * pushing our quad numbers down by 1:
 * 
 *   i  : (MUL, [vreg x, vreg y], vreg a, vreg b)
 *   i+1: (MOV, [vreg dst], vreg x)
 *   i+2: (MOV, [vreg dst], vreg y)
 * 
 * and we insert a 2 fresh live intervals for fresh vregs x and y
 * 
 *    { live_vreg: x; 
 *      live_slot: Fixed (HWreg EDX); 
 *      live_startpoint: i;
 *      live_endpoint: i+1; }
 * 
 *    { live_vreg: y; 
 *      live_slot: Fixed (HWreg EAX); 
 *      live_startpoint: i;
 *      live_endpoint: i+2; }
 * 
 * Constraint rules must be careful not to insert unsatisfiable quads. This 
 * example will cause any live interval allocated to EAX or EDX to be 
 * reassigned to a spill slot; if any of those spilled intervals happened to 
 * be Fixed() to some other HWregs, the spill be unsatisfiable.
 * 
 * 
 *)

let build_incoming_edges quads = 
  (* 
   * Here we build a vector of "incoming edge sets", one for each quad. The
   * incoming edge set for a quad j is a bit vector with a i=true for every
   * other quad i that jumps to j.
   * 
   * We use this during iterative dataflow calculation, later.
   *)
  let n = Array.length quads in
  let new_bitv _ = Bitv.create n false in
  let (incoming_edges:Bitv.t array) = Array.init n new_bitv in    
  let note_edge i j = Bitv.set incoming_edges.(j) i true in
  let note_slot i s = 
    match s with 
	Label j -> note_edge i j
      | _ -> ()
  in
  for i = 0 to n - 1 do
    let q = quads.(i) in
      note_slot i q.quad_dst;
      note_slot i q.quad_lhs;
      note_slot i q.quad_rhs
  done;
    incoming_edges
;;


let calculate_live_intervals e = 
  (*
   * NB: this algorithm is not terribly accurate. You will get
   * tighter bounds with an iterative dataflow analysis. 
   *)
  let n_vregs = e.emit_next_vreg in
  let vreg_lo = Array.create n_vregs (-1) in
  let vreg_hi = Array.create n_vregs (-1) in
  let note_vreg v i = 
    if vreg_lo.(v) = -1
    then vreg_lo.(v) <- i
    else vreg_lo.(v) <- min vreg_lo.(v) i;
    if vreg_hi.(v) = -1
    then vreg_hi.(v) <- i
    else vreg_hi.(v) <- max vreg_lo.(v) i
  in
  let note_slot s i = match s with 
      (Vreg n) -> note_vreg n i 
    | _ -> ()
  in
  let intervals = ref LI.empty in
    for i = 0 to (Array.length e.emit_quads) - 1 do
      let quad = e.emit_quads.(i) in
	note_slot quad.quad_dst i;
	note_slot quad.quad_lhs i;
	note_slot quad.quad_rhs i
    done;
    for v = 0 to n_vregs - 1 do
      let interval = { live_vreg = v;
		       live_slot = Vreg v;
		       live_startpoint = vreg_lo.(v);
		       live_endpoint = vreg_hi.(v) }
      in
	intervals := LI.add interval (!intervals)
    done;
    (!intervals)
;;


(* Core 3 functions of Poletto and Sarkar's algorithm. *)

let expire_old_intervals active curr : (AI.t * HR.t) =
  let should_expire interval = 
    interval.live_endpoint < curr.live_startpoint
  in
  let (expired, kept) = AI.partition should_expire active in
  let free interval hardregs = 
    match interval.live_slot with
	HWreg i -> HR.add i hardregs
      | _ -> failwith "expiring non-hardreg interval"
  in
  let freed = AI.fold free expired HR.empty in
    (kept, freed)
;;


let spill_at_interval spill_num active curr : (live_interval * AI.t) =
  let spill = AI.max_elt active in
    if spill.live_endpoint > curr.live_endpoint 
    then
      let active = AI.remove spill active in
      let curr = { curr with live_slot = spill.live_slot } in
      let active = AI.add curr active in
      let spill = { spill with live_slot = Spill spill_num } in
	(spill, active)
    else
      let curr = { curr with live_slot = Spill spill_num } in
	(curr, active)
;;


let linear_scan_register_allocation e =
  let process interval (intervals, active, free) 
      : (LI.t * AI.t * HR.t) = 
    let (active, freed) = expire_old_intervals active interval in
    let free = HR.union free freed in
    let n_active = AI.cardinal active in
      if n_active > e.emit_n_hardregs
      then failwith "more active hardregs than available"
      else 
	if n_active = e.emit_n_hardregs
	then 
	  let spill_num = next_spill e in
	  let (spilled,active) = spill_at_interval spill_num active interval in
	  let intervals = LI.add spilled intervals in
	    (intervals, active, free)
	else
	  let hr = HR.min_elt free in
	  let free = HR.remove hr free in
	  let interval = { interval with live_slot = HWreg hr } in
	  let intervals = LI.add interval intervals in
	  let active = AI.add interval active in
	    (intervals, active, free)
  in
  let initial_intervals = LI.empty in
  let initial_active = AI.empty in
  let initial_free = (let x = ref HR.empty in 
			for i = 1 to e.emit_n_hardregs do
			  x := HR.add i (!x)
			done;
			!x)
  in
  let initials = (initial_intervals, initial_active, initial_free) in
  let unallocated_intervals:LI.t = calculate_live_intervals e in

    (*
      let _ = Printf.printf "unallocated live intervals:\n" in
      let _ = print_live_intervals unallocated_intervals in
    *)

  let (allocated_intervals, _, _) = 
    LI.fold process unallocated_intervals initials 
  in

    (* 
       let _ = Printf.printf "allocated live intervals:\n" in
       let _ = print_live_intervals allocated_intervals in
    *)

    convert_regs_using_intervals allocated_intervals e;
    remove_redundant_moves e
;;
  
let test _ = 
  let emit = new_emitter 6 in

  let a = next_vreg emit in
  let b = next_vreg emit in
  let c = next_vreg emit in
  let d = next_vreg emit in
  let e = next_vreg emit in
  let f = next_vreg emit in
  let g = next_vreg emit in
  let h = next_vreg emit in
  let i = next_vreg emit in

    emit_quad emit None ADD a b c;
    emit_quad emit None ADD d e f;
    emit_quad emit None ADD g h i;
    emit_quad emit None MOV a a Nil;
    emit_quad emit None SUB a b c;
    emit_quad emit None MOV a b Nil;
    emit_quad emit None MOV b b Nil;
    emit_quad emit None MOV d d Nil;
    emit_quad emit None SUB d e f;
    emit_quad emit None SUB g h i;

    Printf.printf "initial quads:\n";
    print_quads emit.emit_quads;
    linear_scan_register_allocation emit;
    Printf.printf "final quads:\n";
    print_quads emit.emit_quads
;;

test();
