
(* 
 * The 'abi' structure is pretty much just a grab-bag of machine
 * dependencies.  Make some attempt to factor it as time goes by.  
 *)

type abi =
  {
    abi_ptrsz: int;
    abi_ptr_mem: Il.mem;

    abi_is_2addr_machine: bool;
    abi_n_hardregs: int;
    abi_str_of_hardreg: (int -> string);
    
    abi_fp_operand: Il.operand;
    abi_pp_operand: Il.operand;
    abi_cp_operand: Il.operand;
    abi_rp_operand: Il.operand;  
  }


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
