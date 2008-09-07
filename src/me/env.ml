type 'a env = ((string, 'a) Hashtbl.t) list
;;

let add e k v = 
  match e with 
	  (t::ts) -> Hashtbl.add t k v
	| [] -> raise (Invalid_argument "Env.add")
;;

let extend e = (Hashtbl.create 4) :: e
;;

let has e k = List.exists (fun t -> Hashtbl.mem t k) e
;;

let rec get e k = 
  match e with 
	  (t::ts) ->
		if Hashtbl.mem t k 
		then Hashtbl.find t k 
		else get ts k
	| _ -> raise Not_found
;;
