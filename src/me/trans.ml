(* Translation *)

open Semant;;
open Common;;

let log cx = Session.log "trans" 
  cx.ctxt_sess.Session.sess_log_trans
  cx.ctxt_sess.Session.sess_log_out
;;


let marker = Il.Imm (Asm.IMM 0xdeadbeefL);;
let imm_true = Il.Imm (Asm.IMM 1L);;
let imm_false = Il.Imm (Asm.IMM 0L);;
let badlab = Il.Label (-1);;


(* 
type ctxt = 
    {
      mutable ctxt_emit: Il.emitter;
      ctxt_sess: Session.sess;
      ctxt_abi: Abi.abi;
      ctxt_entry_prog: fixup;
      mutable ctxt_path: string list;
      mutable ctxt_data_items: Asm.item list;
      mutable ctxt_epilogue_jumps: int list;
      mutable ctxt_frames: Ast.frame list;
      ctxt_text_items: (string, (Il.quads * int)) Hashtbl.t;
    }


let new_ctxt (sess:Session.sess) (abi:Abi.abi) : ctxt = 
  { 
    ctxt_emit = Il.new_emitter abi.Abi.abi_prealloc_quad abi.Abi.abi_is_2addr_machine;
    ctxt_sess = sess;
    ctxt_abi = abi;
    ctxt_path = [];
    ctxt_entry_prog = new_fixup "entry prog fixup";
    ctxt_data_items = [];
    ctxt_epilogue_jumps = [];
    ctxt_frames = [];
    ctxt_text_items = Hashtbl.create 0
  }

let find_heavy_frame cx = 
  let rec search list = 
    match list with 
        [] -> raise (Semant_err (None, "Trans.find_heavy_frame: no heavy frame"))
      | (Ast.FRAME_heavy hf)::_ -> hf
      | x::xs -> search xs
  in
    search cx.ctxt_frames
;;
    


let capture_emitted_quads (cx:ctxt) : unit = 
  let n_vregs = cx.ctxt_emit.Il.emit_next_vreg in 
  let quads = cx.ctxt_emit.Il.emit_quads in 
  let name = String.concat "." (List.rev cx.ctxt_path) in
    begin
      log cx "emitted quads for %s:" name;
      for i = 0 to (Array.length quads) - 1
      do 
        log cx "[%6d]\t%s" i (Il.string_of_quad cx.ctxt_abi.Abi.abi_str_of_hardreg quads.(i));
      done;
      htab_put cx.ctxt_text_items name (quads, n_vregs);
      reset_emitter cx
    end
;;


let rec trans_resolved_path 
    (cx:ctxt) 
    (resp:Ast.resolved_path) = 
  let emit = Il.emit cx.ctxt_emit in  
    match resp with 
        Ast.RES_pr FP -> cx.ctxt_abi.Abi.abi_fp_operand
      | Ast.RES_pr PP -> cx.ctxt_abi.Abi.abi_pp_operand
      | Ast.RES_pr CP -> Il.Nil
      | Ast.RES_pr RP -> Il.Nil
      | Ast.RES_idx (a, b) -> 
          let av = trans_resolved_path cx a in
          let bv = trans_resolved_path cx b in          
		  let tmp = Il.Reg (Il.next_vreg cx.ctxt_emit) in 
            begin
		      emit Il.ADD tmp av bv;
              tmp
            end
      | Ast.RES_member (layout, lv) -> 
          begin
            match trans_resolved_path cx lv with
                Il.Mem (m, v, Asm.IMM off') -> 
                  Il.Mem (m, v, Asm.IMM (Int64.add layout.layout_offset off'))
              | v -> 
                  let tmp = Il.Reg (Il.next_vreg cx.ctxt_emit) in
                    emit Il.ADD tmp v (Il.Imm (Asm.IMM layout.layout_offset));
                    tmp
          end
      | Ast.RES_deref lv -> 
          begin
            match trans_resolved_path cx lv with 
                Il.Reg r -> 
                  Il.Mem (cx.ctxt_abi.Abi.abi_ptr_mem, Some r, Asm.IMM 0L)
              | v -> 
		          let tmp = (Il.next_vreg cx.ctxt_emit) in 
		            emit Il.MOV (Il.Reg tmp) v Il.Nil;
                    Il.Mem (cx.ctxt_abi.Abi.abi_ptr_mem, Some tmp, Asm.IMM 0L)
          end
      | Ast.RES_vreg cell -> 
          begin
            match !cell with 
                Some v -> Il.Reg (Il.Vreg v)
              | None -> 
                  let vr = (Il.next_vreg cx.ctxt_emit) in
                    begin
                      match vr with 
                          Il.Vreg v -> cell := Some v
                        | _ -> failwith "non-vreg in Trans.trans_resolved_path"
                    end;
                    Il.Reg vr
          end

;;

let rec string_of_resolved_path p = 
  match p with 
      Ast.RES_pr FP -> "FP"
    | Ast.RES_pr PP -> "PP"
    | Ast.RES_pr CP -> "CP"
    | Ast.RES_pr RP -> "RP"
    | Ast.RES_idx (a, b) -> 
        Printf.sprintf "RES_idx(%s,%s)" 
          (string_of_resolved_path a) 
          (string_of_resolved_path b)
    | Ast.RES_member (layout, lv) -> 
        Printf.sprintf "RES_member(%Ld,%s)" 
          layout.layout_offset 
          (string_of_resolved_path lv)
    | Ast.RES_deref lv -> 
        Printf.sprintf "RES_deref(%s)" 
          (string_of_resolved_path lv)
    | Ast.RES_vreg _ -> "vreg"
;;

let string_of_name_base nb = 
  match nb with 
	  (Ast.BASE_ident id) -> id
	| (Ast.BASE_temp n) -> "<temp#" ^ (string_of_int n) ^ ">"
	| (Ast.BASE_app (id, tys)) -> "[...]"
;;

let string_of_lval lv = 
  match lv.Ast.lval_src.node with 
      Ast.LVAL_base nbase -> string_of_name_base nbase
    | _ -> "??"
;;


let trans_lval_full
    (cx:ctxt) 
    (lv:Ast.lval)
    (pcrel_ok:bool)
    (imm_ok:bool)
    : Il.operand = 
  let res = lv.Ast.lval_res in
    match (!(res.Ast.res_path), !(res.Ast.res_target)) with         
        _, None | None, _ -> raise (Semant_err (Some lv.Ast.lval_src.id, 
                                                "unresolved lval in trans_lval"))
      | (Some path, Some target)  ->
          begin
            match target with 
                Ast.RES_item ri -> 
                  begin
                    match ri.node with 
                        (Ast.MOD_ITEM_fn fd) -> 
                          let fix = fd.Ast.decl_item.Ast.fn_fixup in
                            if pcrel_ok
                            then Il.Pcrel fix
                            else 
                              let imm = (Il.Imm (Asm.M_POS fix)) in 
                                if imm_ok 
                                then imm
                                else 
                                  let tmp = (Il.next_vreg cx.ctxt_emit) in 
		                            Il.emit cx.ctxt_emit Il.MOV (Il.Reg tmp) imm Il.Nil;
                                    (Il.Reg tmp)
                      | _ -> raise (Semant_err (Some lv.Ast.lval_src.id, 
                                                "unhandled form of mod item in trans_lval"))
                  end
              | _ -> 
                  log cx "translating lval path for %s: %s" 
                    (string_of_lval lv) 
                    (string_of_resolved_path path); 
                  trans_resolved_path cx path
          end
;;
        

(* FIXME: this is awful. Synthesize a path in resolve? *)
let trans_out_local (cx:ctxt) (local:Ast.local) (callee:bool) : Il.operand = 
  if callee
  then 
    trans_resolved_path cx 
      (Ast.RES_member 
         (local.Ast.local_layout, 
          (Ast.RES_deref (Ast.RES_pr FP))))
  else 
    (* FIXME: hack upon hack *) 
    let sp_vn = Il.next_vreg_num cx.ctxt_emit in 
    let sp = Il.Reg (Il.Vreg sp_vn) in
    let emit = Il.emit cx.ctxt_emit in
      emit Il.MOV sp cx.ctxt_abi.Abi.abi_sp_operand Il.Nil;
      emit Il.SUB sp sp (Il.Imm (Asm.IMM (cx.ctxt_abi.Abi.abi_frame_base_sz)));
        trans_resolved_path cx
        (Ast.RES_member (local.Ast.local_layout, (Ast.RES_deref (Ast.RES_vreg (ref (Some sp_vn))))))
;;

let trans_out_slot (cx:ctxt) (heavy:Ast.heavy_frame) (callee:bool) : Il.operand = 
  match !(heavy.Ast.heavy_frame_out_slot) with 
      None -> raise (Semant_err (None, "translating output slot in heavy frame without one"))
    | Some local -> trans_out_local cx local callee
;;
        
let trans_lval
    (cx:ctxt) 
    (lv:Ast.lval)
    : Il.operand = 
  trans_lval_full cx lv false false
;;


let trans_atom 
    (cx:ctxt) 
    (atom:Ast.atom)
    : Il.operand = 
  match atom with 
      Ast.ATOM_lval lv -> 
        trans_lval cx lv
          
	| Ast.ATOM_literal lit -> 
        begin 
          match lit.node with 
              Ast.LIT_nil -> 
		        Il.Nil
                  
	        | Ast.LIT_bool false -> 
		        Il.Imm (Asm.IMM 0L)
          
	        | Ast.LIT_bool true -> 
		        Il.Imm (Asm.IMM 1L)
          
	        | Ast.LIT_char c -> 
		        Il.Imm (Asm.IMM (Int64.of_int (Char.code c)))
          
	        | Ast.LIT_int (bi, s) -> 
		        Il.Imm (Asm.IMM (Int64.of_int (Big_int.int_of_big_int bi)))

            | Ast.LIT_str s -> 
                let strfix = new_fixup "string fixup" in
                let str = Asm.DEF (strfix, Asm.ZSTRING s) in
                  cx.ctxt_data_items <- str :: cx.ctxt_data_items;
                  (Il.Imm (Asm.M_POS strfix))

	        | _ -> marker (* raise  (Invalid_argument "Trans.trans_atom: unimplemented translation") *)
        end
        

let trans_expr 
    (cx:ctxt) 
    (expr:Ast.expr)
    : Il.operand = 
  let emit = Il.emit cx.ctxt_emit in
    match expr with 
        
	    Ast.EXPR_binary (binop, a, b) -> 
	      let lhs = trans_atom cx a in
		  let rhs = trans_atom cx b in
		  let dst = Il.Reg (Il.next_vreg cx.ctxt_emit) in 
          let arith op = 
			emit op dst lhs rhs;
			dst
          in
          let rela cjmp = 
            emit Il.CMP Il.Nil lhs rhs;
            emit Il.MOV dst imm_true Il.Nil;
            let j = mark cx in
              emit cjmp Il.Nil badlab Il.Nil;
              emit Il.MOV dst imm_false Il.Nil;
              patch cx j;
              dst
          in
            begin 
		      match binop with
                  Ast.BINOP_or -> arith Il.OR
                | Ast.BINOP_and -> arith Il.AND
                    
                | Ast.BINOP_lsl -> arith Il.LSL
                | Ast.BINOP_lsr -> arith Il.LSR
                | Ast.BINOP_asr -> arith Il.ASR
                    
                | Ast.BINOP_add -> arith Il.ADD
                | Ast.BINOP_sub -> arith Il.SUB
                    
                (* FIXME: switch on type of operands, IMUL/IDIV/IMOD etc. *)
                | Ast.BINOP_mul -> arith Il.UMUL
                | Ast.BINOP_div -> arith Il.UDIV
                | Ast.BINOP_mod -> arith Il.UMOD
                    
                | Ast.BINOP_eq -> rela Il.JE                
                | Ast.BINOP_ne -> rela Il.JNE                
                | Ast.BINOP_lt -> rela Il.JL
                | Ast.BINOP_le -> rela Il.JLE
                | Ast.BINOP_ge -> rela Il.JGE
                | Ast.BINOP_gt -> rela Il.JG
                    
			    | _ -> raise (Invalid_argument "Trans.trans_expr: unimplemented binop")
            end

	  | Ast.EXPR_unary (unop, a) -> 
		  let src = trans_atom cx a in
		  let dst = Il.Reg (Il.next_vreg cx.ctxt_emit) in 
		  let op = match unop with
			  Ast.UNOP_not -> Il.NOT
			| Ast.UNOP_neg -> Il.NEG
		  in
			emit op dst src Il.Nil;
			dst

      | Ast.EXPR_atom a -> 
          trans_atom cx a
              
	  | _ -> raise (Invalid_argument "Trans.trans_expr: unimplemented translation")
;;

let atom_type at = 
  match at with 
      Ast.ATOM_literal {node=(Ast.LIT_str _); id=_} -> Some Ast.TY_str
    | Ast.ATOM_literal {node=(Ast.LIT_int _); id=_} -> Some Ast.TY_int
    | Ast.ATOM_lval lv -> 
        begin
          match !(lv.Ast.lval_res.Ast.res_target) with 
              Some (Ast.RES_slot local) -> 
                let slotr = local.Ast.local_slot in 
                let slot = !(slotr.node) in
                  begin
                    match slot with 
                        Ast.SLOT_exterior t -> Some t
                      | Ast.SLOT_interior t -> Some t
                      | Ast.SLOT_read_alias t -> Some t
                      | Ast.SLOT_write_alias t -> Some t
                      | Ast.SLOT_auto -> 
                          raise (Semant_err 
                                   (Some lv.Ast.lval_src.id, 
                                    "Unresolved auto slot in Trans.atom_type"))
                  end
            | Some _ -> None
            | None -> raise (Semant_err 
                               (Some lv.Ast.lval_src.id, 
                                "Unresolved lval in Trans.atom_type"))
        end
    | _ -> None
;;

let rec trans_stmt 
    (cx:ctxt) 
    (stmt:Ast.stmt)
    : unit =
  let emit = Il.emit cx.ctxt_emit in
    match stmt.node with 

        Ast.STMT_log a ->
          begin
            match atom_type a with 
                Some Ast.TY_str -> trans_log_str cx a
              | Some Ast.TY_int -> trans_log_int cx a
              | Some _ -> raise (Invalid_argument "Trans.trans_stmt: unimplemented known logging type")
              | _ -> raise (Invalid_argument "Trans.trans_stmt: unimplemented unknown logging type")
          end
          
	  | Ast.STMT_copy (lv_dst, e_src) -> 
		  let dst = trans_lval cx lv_dst in
		  let src = trans_expr cx e_src in
		    emit Il.MOV dst src Il.Nil
              
	  | Ast.STMT_block block -> 
          trans_block cx block		  
          
      | Ast.STMT_while sw -> 
          let back_jmp_target = mark cx in 
          let (head_stmts, head_atom) = sw.Ast.while_lval in
		    Array.iter (trans_stmt cx) head_stmts;
            let v = trans_atom cx head_atom in
              emit Il.CMP Il.Nil v imm_false;
              let fwd_jmp_quad = mark cx in
                emit Il.JE Il.Nil badlab Il.Nil;
                trans_block cx sw.Ast.while_body;
                emit Il.JMP Il.Nil (Il.Label back_jmp_target) Il.Nil;
                patch cx fwd_jmp_quad
                  
      | Ast.STMT_if si -> 
          let v = trans_atom cx si.Ast.if_test in 
            emit Il.CMP Il.Nil v imm_true;
            let skip_thn_clause_jmp = mark cx in 
              emit Il.JNE Il.Nil badlab Il.Nil;
              trans_block cx si.Ast.if_then;
              begin 
                match si.Ast.if_else with 
                    None -> patch cx skip_thn_clause_jmp
                  | Some els -> 
                      let skip_els_clause_jmp = mark cx in
                        emit Il.JMP Il.Nil badlab Il.Nil;
                        patch cx skip_thn_clause_jmp;
                        trans_block cx els;
                        patch cx skip_els_clause_jmp                        
              end

      | Ast.STMT_call (dst, flv, args) -> 
          let abi = cx.ctxt_abi in
          let fn = 
            match !(flv.Ast.lval_res.Ast.res_target) with 
                Some (Ast.RES_item ({node=(Ast.MOD_ITEM_fn fd); id=_})) -> fd.Ast.decl_item
              | _ -> raise (Invalid_argument "Trans.trans_stmt: call to unexpected lval")
          in
            (* FIXME: factor out call protocol into ABI bits. *)
            for i = 0 to (Array.length args) - 1 do              
              emit (Il.CPUSH TY_u32) Il.Nil (trans_atom cx args.(i)) Il.Nil
            done;
            (* Emit arg1: the process pointer. *)
            emit (Il.CPUSH TY_u32) Il.Nil (abi.Abi.abi_pp_operand) Il.Nil;
            (* Emit arg0: the output slot. *)
            emit (Il.CPUSH TY_u32) Il.Nil (Il.Imm (Asm.IMM 0L)) Il.Nil;
            let vr = Il.Reg (Il.next_vreg cx.ctxt_emit) in 
            let dst = trans_lval cx dst in
            let fv = (trans_lval_full cx flv abi.Abi.abi_has_pcrel_jumps abi.Abi.abi_has_imm_jumps) in
              emit Il.CCALL vr fv Il.Nil;
              emit Il.MOV dst vr Il.Nil;
              emit Il.MOV dst (trans_out_slot cx fn.Ast.fn_frame false) Il.Nil;
              emit Il.ADD abi.Abi.abi_sp_operand abi.Abi.abi_sp_operand 
                (Il.Imm (Asm.IMM (Int64.of_int (4 * (2 + (Array.length args))))));
              

      | Ast.STMT_ret (proto_opt, atom_opt) -> 
          begin
          match proto_opt with 
              None -> 
                begin 
                  begin 
                    match atom_opt with 
                        None -> ()
                      | Some at -> 
                          let heavy = find_heavy_frame cx in 
                          let ret = trans_out_slot cx heavy true in 
                            emit Il.MOV ret (trans_atom cx at) Il.Nil
                  end;
                  cx.ctxt_epilogue_jumps <- (mark cx) :: cx.ctxt_epilogue_jumps;
                end;                          
                emit Il.JMP Il.Nil badlab Il.Nil
            | Some _ -> ()
          end
            
      | _ -> ()
                
(* 

    | Ast.STMT_do_while sw ->
  | STMT_foreach of stmt_foreach
  | STMT_for of stmt_for
  | STMT_try of stmt_try
  | STMT_put of (proto option * lval option)
  | STMT_be of (proto option * lval * (lval array))
  | STMT_alt_tag of stmt_alt_tag
  | STMT_alt_type of stmt_alt_type
  | STMT_alt_port of stmt_alt_port
  | STMT_prove of (constrs)
  | STMT_check of (constrs)
  | STMT_checkif of (constrs * stmt)
  | STMT_send of (lval * lval)
  | STMT_recv of (lval * lval)
  | STMT_decl of stmt_decl 
  | STMT_use of (ty * ident * lval)
	| _ -> raise (Invalid_argument "Semant.trans_stmt: unimplemented translation")
*)


and trans_log_int (cx:ctxt) (a:Ast.atom) : unit = 
  let v = trans_atom cx a in 
  let f = cx.ctxt_abi.Abi.abi_load_kern_fn cx.ctxt_emit 0 in
  let dst = Il.Reg (Il.next_vreg cx.ctxt_emit) in 
    Il.emit cx.ctxt_emit (Il.CPUSH TY_u32) Il.Nil v Il.Nil;
    Il.emit cx.ctxt_emit Il.CCALL dst (Il.Reg f) Il.Nil;
    Il.emit cx.ctxt_emit Il.ADD cx.ctxt_abi.Abi.abi_sp_operand cx.ctxt_abi.Abi.abi_sp_operand (Il.Imm (Asm.IMM 4L));

and trans_log_str (cx:ctxt) (a:Ast.atom) : unit = 
  let v = trans_atom cx a in
  let f = cx.ctxt_abi.Abi.abi_load_kern_fn cx.ctxt_emit 1 in
  let dst = Il.Reg (Il.next_vreg cx.ctxt_emit) in 
    Il.emit cx.ctxt_emit (Il.CPUSH TY_u32) Il.Nil v Il.Nil;
    Il.emit cx.ctxt_emit Il.CCALL dst (Il.Reg f) Il.Nil;
    Il.emit cx.ctxt_emit Il.ADD cx.ctxt_abi.Abi.abi_sp_operand cx.ctxt_abi.Abi.abi_sp_operand (Il.Imm (Asm.IMM 4L));


and trans_block (cx:ctxt) (block:Ast.block) : unit = 
  Array.iter (trans_stmt cx) block.node.Ast.block_stmts

and trans_fn (cx:ctxt) (fn:Ast.fn) : unit =  
  let saved_epilogue_jumps = cx.ctxt_epilogue_jumps in
  let saved_frames = cx.ctxt_frames in
    cx.ctxt_frames <- (Ast.FRAME_heavy fn.Ast.fn_frame) :: cx.ctxt_frames;
    cx.ctxt_epilogue_jumps <- [];
    reset_emitter cx;
    cx.ctxt_abi.Abi.abi_emit_fn_prologue cx.ctxt_emit fn;
    trans_block cx fn.Ast.fn_body;
    List.iter (patch cx) cx.ctxt_epilogue_jumps;
    cx.ctxt_abi.Abi.abi_emit_fn_epilogue cx.ctxt_emit fn;      
    cx.ctxt_epilogue_jumps <- saved_epilogue_jumps;
    cx.ctxt_frames <- saved_frames;
    capture_emitted_quads cx

and trans_prog_block (cx:ctxt) (b:Ast.block) (ncomp:string) : fixup = 
  let oldpath = cx.ctxt_path in 
    cx.ctxt_path <- ncomp :: cx.ctxt_path;
    let name = String.concat "." (List.rev cx.ctxt_path) in
    let fix = new_fixup name in
      reset_emitter cx;
      Il.emit_full cx.ctxt_emit (Some fix) Il.DEAD Il.Nil Il.Nil Il.Nil;
      cx.ctxt_abi.Abi.abi_emit_main_prologue cx.ctxt_emit b;      
      trans_block cx b;
      cx.ctxt_abi.Abi.abi_emit_main_epilogue cx.ctxt_emit b;
      capture_emitted_quads cx;
      cx.ctxt_path <- oldpath;
      fix

and trans_prog (cx:ctxt) (p:Ast.prog) : unit =   
  let name = String.concat "." (List.rev cx.ctxt_path) in
  let _ = log cx "translating program: %s" name in
  let init = 
    (* FIXME: translate the init part as well. *)
    Asm.IMM 0L
  in
  let main =     
    match p.Ast.prog_main with 
        None -> Asm.IMM 0L
      | Some main -> Asm.M_POS (trans_prog_block cx main "main")            
  in
  let fini = 
    match p.Ast.prog_fini with 
        None -> Asm.IMM 0L
      | Some main -> Asm.M_POS (trans_prog_block cx main "fini")
  in
  let prog = 
    (* FIXME: only DEF the entry prog if its name matches a crate param! *)
    (* FIXME: extract prog layout from ABI. *)    
    Asm.DEF (cx.ctxt_entry_prog, 
             Asm.SEQ [| Asm.WORD32 init; 
                        Asm.WORD32 main; 
                        Asm.WORD32 fini |]) 
  in
    cx.ctxt_data_items <- prog :: cx.ctxt_data_items;
    trans_mod_items cx p.Ast.prog_mod


and trans_mod_item 
    (cx:ctxt) 
    (name:Ast.ident) 
    (item:Ast.mod_item) 
    : unit = 
  let oldpath = cx.ctxt_path in 
    cx.ctxt_path <- name :: cx.ctxt_path;
    begin
      match item.node with 
	      Ast.MOD_ITEM_fn f -> trans_fn cx f.Ast.decl_item
	    | Ast.MOD_ITEM_mod m -> trans_mod_items cx m.Ast.decl_item
	    | Ast.MOD_ITEM_prog p -> trans_prog cx p.Ast.decl_item
	    | _ -> ()
    end;
    cx.ctxt_path <- oldpath
            

and trans_mod_items 
    (cx:ctxt) 
    (items:Ast.mod_items)
    : unit = 
  Hashtbl.iter (trans_mod_item cx) items
*)

let trans_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor = 
  let path = Stack.create () in     
  let block_layouts = Stack.create () in 

  let emitters = Stack.create () in 
  let push_new_emitter _ = 
    Stack.push 
      (Il.new_emitter 
         cx.ctxt_abi.Abi.abi_prealloc_quad 
         cx.ctxt_abi.Abi.abi_is_2addr_machine) 
      emitters
  in
  let pop_emitter _ = ignore (Stack.pop emitters) in
  let emitter _ = Stack.top emitters in
  let emit op dst lhs rhs = Il.emit (emitter()) op dst lhs rhs in
  let mark _ : int = (emitter()).Il.emit_pc in
  let patch (i:int) : unit = 
    (emitter()).Il.emit_quads.(i) 
    <- { (emitter()).Il.emit_quads.(i)
         with Il.quad_lhs = Il.Label (mark ()) };  
    (* Insert a dead quad to ensure there's an otherwise-unused patch target here. *)
    emit Il.DEAD Il.Nil Il.Nil Il.Nil
  in

  let epilogue_jumps = Stack.create() in 


  let lval_to_referent (id:node_id) : node_id = 
    if Hashtbl.mem cx.ctxt_lval_to_referent id
    then Hashtbl.find cx.ctxt_lval_to_referent id
    else err (Some id) "Unresolved lval"
  in

  let lval_to_slot (id:node_id) : Ast.slot = 
    let referent = lval_to_referent id in 
      if Hashtbl.mem cx.ctxt_all_slots referent
      then Hashtbl.find cx.ctxt_all_slots referent
      else err (Some referent) "Unknown slot"
  in

  let get_block_layout (id:node_id) : layout = 
    if Hashtbl.mem cx.ctxt_block_layouts id
    then Hashtbl.find cx.ctxt_block_layouts id 
    else err (Some id) "Unknown block layout"
  in

  let get_fn_fixup (id:node_id) : fixup = 
    if Hashtbl.mem cx.ctxt_fn_fixups id
    then Hashtbl.find cx.ctxt_fn_fixups id 
    else err (Some id) "Fn without fixup"
  in

  let get_framesz (id:node_id) : int64 = 
    if Hashtbl.mem cx.ctxt_frame_sizes id
    then Hashtbl.find cx.ctxt_frame_sizes id 
    else err (Some id) "Missing framesz"
  in

  let path_name (_:unit) : string = 
    String.concat "." (stk_elts_from_bot path) 
  in

  let trans_lval_full
      (lv:Ast.lval) 
      (pcrel_ok:bool)
      (imm_ok:bool)
      : Il.operand = 
    match lv with 
        Ast.LVAL_base nb -> 
          let referent = lval_to_referent nb.id in 
            begin
              match htab_search cx.ctxt_all_items referent with
                  Some item ->
                    begin
                      match item with 
                          Ast.MOD_ITEM_fn fd -> 
                            let fix = get_fn_fixup referent in
                              if pcrel_ok
                              then Il.Pcrel fix
                              else 
                                let imm = (Il.Imm (Asm.M_POS fix)) in 
                                  if imm_ok 
                                  then imm
                                  else 
                                    let tmp = (Il.next_vreg (emitter())) in 
		                              emit Il.MOV (Il.Reg tmp) imm Il.Nil;
                                      (Il.Reg tmp)
                        | _ -> err (Some nb.id) "unhandled item type in trans_lval_full, item #%d" (int_of_node referent)
                    end
                | None -> 
                    begin
                      match htab_search cx.ctxt_slot_vregs referent with
                          Some vr -> 
                            begin
                              let vreg = 
                                match !vr with 
                                    None -> 
                                      begin
                                        let v = (Il.next_vreg_num (emitter())) in 
                                          vr := Some v; 
                                          v
                                      end
                                  | Some v -> v
                              in
                                Il.Reg (Il.Vreg vreg)
                            end
                        | None -> 
                            begin
                              match htab_search cx.ctxt_slot_layouts referent with 
                                  None -> err (Some nb.id) "slot assigned to neither vreg nor layout"
                                | Some layout -> 
                                    Il.Mem (TY_u32, 
                                            (Some cx.ctxt_abi.Abi.abi_fp_reg),
                                            Asm.IMM layout.layout_offset)
                            end
                    end
            end
      | _ -> err None "unhandled form of lval in trans_lval_full"
  in
    
  let trans_lval (lv:Ast.lval) : Il.operand = 
    trans_lval_full lv false false
  in

  let trans_out_slot (callee:bool) : Il.operand = 
    if callee 
    then 
      Il.Mem (TY_u32, (Some cx.ctxt_abi.Abi.abi_fp_reg),
              (Asm.IMM (cx.ctxt_abi.Abi.abi_frame_base_sz)))
    else
      let sp_vn = Il.next_vreg_num (emitter()) in 
      let sp = Il.Reg (Il.Vreg sp_vn) in
        emit Il.MOV sp (Il.Reg cx.ctxt_abi.Abi.abi_sp_reg) Il.Nil;
        Il.Mem (TY_u32, (Some (Il.Vreg sp_vn)), (Asm.IMM 0L))
  in

  let atom_type (at:Ast.atom) : Ast.ty = 
    match at with 
        Ast.ATOM_literal {node=(Ast.LIT_str _); id=_} -> Ast.TY_str
      | Ast.ATOM_literal {node=(Ast.LIT_int _); id=_} -> Ast.TY_int
      | Ast.ATOM_lval (Ast.LVAL_base nb) -> 
          let slot = lval_to_slot nb.id in 
            begin
              match slot.Ast.slot_ty with 
                  None -> err (Some nb.id) "name refers to untyped slot, in atom_type"
                | Some t -> t
            end
      | _ -> err None "unhandled form of atom in atom_type"
  in
    
  let trans_atom (atom:Ast.atom) : Il.operand = 
    match atom with 
        Ast.ATOM_lval lv -> 
          trans_lval lv
            
	  | Ast.ATOM_literal lit -> 
          begin 
            match lit.node with 
                Ast.LIT_nil -> 
		          Il.Nil
                    
	          | Ast.LIT_bool false -> 
		          Il.Imm (Asm.IMM 0L)
                    
	          | Ast.LIT_bool true -> 
		          Il.Imm (Asm.IMM 1L)
                    
	          | Ast.LIT_char c -> 
		          Il.Imm (Asm.IMM (Int64.of_int (Char.code c)))
                    
	          | Ast.LIT_int (bi, s) -> 
		          Il.Imm (Asm.IMM (Int64.of_int (Big_int.int_of_big_int bi)))
                    
              | Ast.LIT_str s -> 
                  let strfix = new_fixup "string fixup" in
                  let str = Asm.DEF (strfix, Asm.ZSTRING s) in
                    cx.ctxt_data_items <- str :: cx.ctxt_data_items;
                    (Il.Imm (Asm.M_POS strfix))
                      
	          | _ -> marker
          end
  in
  
  let trans_expr (expr:Ast.expr) : Il.operand = 
    match expr with 
        
	    Ast.EXPR_binary (binop, a, b) -> 
	      let lhs = trans_atom a in
		  let rhs = trans_atom b in
		  let dst = Il.Reg (Il.next_vreg (emitter())) in 
          let arith op = 
			emit op dst lhs rhs;
			dst
          in
          let rela cjmp = 
            emit Il.CMP Il.Nil lhs rhs;
            emit Il.MOV dst imm_true Il.Nil;
            let j = mark () in
              emit cjmp Il.Nil badlab Il.Nil;
              emit Il.MOV dst imm_false Il.Nil;
              patch j;
              dst
          in
            begin 
		      match binop with
                  Ast.BINOP_or -> arith Il.OR
                | Ast.BINOP_and -> arith Il.AND
                    
                | Ast.BINOP_lsl -> arith Il.LSL
                | Ast.BINOP_lsr -> arith Il.LSR
                | Ast.BINOP_asr -> arith Il.ASR
                    
                | Ast.BINOP_add -> arith Il.ADD
                | Ast.BINOP_sub -> arith Il.SUB
                    
                (* FIXME: switch on type of operands, IMUL/IDIV/IMOD etc. *)
                | Ast.BINOP_mul -> arith Il.UMUL
                | Ast.BINOP_div -> arith Il.UDIV
                | Ast.BINOP_mod -> arith Il.UMOD
                    
                | Ast.BINOP_eq -> rela Il.JE                
                | Ast.BINOP_ne -> rela Il.JNE                
                | Ast.BINOP_lt -> rela Il.JL
                | Ast.BINOP_le -> rela Il.JLE
                | Ast.BINOP_ge -> rela Il.JGE
                | Ast.BINOP_gt -> rela Il.JG
                    
	            | _ -> err None "Unhandled binop of expr in trans_expr"
            end
              
	  | Ast.EXPR_unary (unop, a) -> 
		  let src = trans_atom a in
		  let dst = Il.Reg (Il.next_vreg (emitter())) in 
		  let op = match unop with
			  Ast.UNOP_not -> Il.NOT
			| Ast.UNOP_neg -> Il.NEG
		  in
			emit op dst src Il.Nil;
			dst

      | Ast.EXPR_atom a -> 
          trans_atom a
              
	  | _ -> err None "Unhandled form of expr in Trans.trans_expr"
  in
    
  let trans_log_int (a:Ast.atom) : unit = 
    let v = trans_atom a in 
    let f = cx.ctxt_abi.Abi.abi_load_kern_fn (emitter()) 0 in
    let dst = Il.Reg (Il.next_vreg (emitter())) in 
    let sp = (Il.Reg cx.ctxt_abi.Abi.abi_sp_reg) in
      emit (Il.CPUSH TY_u32) Il.Nil v Il.Nil;
      emit Il.CCALL dst (Il.Reg f) Il.Nil;
      emit Il.ADD sp sp (Il.Imm (Asm.IMM 4L));
  in
    
  let trans_log_str (a:Ast.atom) : unit = 
    let v = trans_atom a in
    let f = cx.ctxt_abi.Abi.abi_load_kern_fn (emitter()) 1 in
    let dst = Il.Reg (Il.next_vreg (emitter())) in 
    let sp = (Il.Reg cx.ctxt_abi.Abi.abi_sp_reg) in
      emit (Il.CPUSH TY_u32) Il.Nil v Il.Nil;
      emit Il.CCALL dst (Il.Reg f) Il.Nil;
      emit Il.ADD sp sp (Il.Imm (Asm.IMM 4L));
  in

  let rec trans_block (block:Ast.block) : unit = 
    Stack.push (get_block_layout block.id) block_layouts;
    Array.iter trans_stmt block.node;
    ignore (Stack.pop block_layouts)
      
  and trans_stmt (stmt:Ast.stmt) : unit =
    match stmt.node with 
        
        Ast.STMT_log a ->
          begin
            match atom_type a with 
                Ast.TY_str -> trans_log_str a
              | Ast.TY_int -> trans_log_int a
              | _ -> err (Some stmt.id) "unimplemented logging type"
          end
            
	  | Ast.STMT_copy (lv_dst, e_src) -> 
		  let dst = trans_lval lv_dst in
		  let src = trans_expr e_src in
		    emit Il.MOV dst src Il.Nil
              
	  | Ast.STMT_block block -> 
          trans_block block		  
            
      | Ast.STMT_while sw -> 
          let back_jmp_target = mark () in 
          let (head_stmts, head_atom) = sw.Ast.while_lval in
		    Array.iter trans_stmt head_stmts;
            let v = trans_atom head_atom in
              emit Il.CMP Il.Nil v imm_false;
              let fwd_jmp_quad = mark () in
                emit Il.JE Il.Nil badlab Il.Nil;
                trans_block sw.Ast.while_body;
                emit Il.JMP Il.Nil (Il.Label back_jmp_target) Il.Nil;
                patch fwd_jmp_quad
                  
      | Ast.STMT_if si -> 
          let v = trans_atom si.Ast.if_test in 
            emit Il.CMP Il.Nil v imm_true;
            let skip_thn_clause_jmp = mark () in 
              emit Il.JNE Il.Nil badlab Il.Nil;
              trans_block si.Ast.if_then;
              begin 
                match si.Ast.if_else with 
                    None -> patch skip_thn_clause_jmp
                  | Some els -> 
                      let skip_els_clause_jmp = mark () in
                        emit Il.JMP Il.Nil badlab Il.Nil;
                        patch skip_thn_clause_jmp;
                        trans_block els;
                        patch skip_els_clause_jmp                        
              end
                
      | Ast.STMT_call (dst, flv, args) -> 
          let abi = cx.ctxt_abi in
            (* FIXME: factor out call protocol into ABI bits. *)
            for i = (Array.length args) - 1 downto 0 do
              emit (Il.CPUSH TY_u32) Il.Nil (trans_atom args.(i)) Il.Nil
            done;
            (* Emit arg1: the process pointer. *)
            emit (Il.CPUSH TY_u32) Il.Nil (abi.Abi.abi_pp_operand) Il.Nil;
            (* Emit arg0: the output slot. *)
            emit (Il.CPUSH TY_u32) Il.Nil (Il.Imm (Asm.IMM 0L)) Il.Nil;
            let vr = Il.Reg (Il.next_vreg (emitter())) in 
            let sp = Il.Reg (abi.Abi.abi_sp_reg) in
            let dst = trans_lval dst in
            let fv = (trans_lval_full flv abi.Abi.abi_has_pcrel_jumps abi.Abi.abi_has_imm_jumps) in
              emit Il.CCALL vr fv Il.Nil;
              emit Il.MOV dst vr Il.Nil;
              emit Il.MOV dst (trans_out_slot false) Il.Nil;
              emit Il.ADD sp sp 
                (Il.Imm (Asm.IMM (Int64.of_int (4 * (2 + (Array.length args))))));
              

      | Ast.STMT_ret (proto_opt, atom_opt) -> 
          begin
          match proto_opt with 
              None -> 
                begin 
                  begin 
                    match atom_opt with 
                        None -> ()
                      | Some at -> 
                          let ret = trans_out_slot true in 
                            emit Il.MOV ret (trans_atom at) Il.Nil
                  end;
                  Stack.push (mark()) (Stack.top epilogue_jumps);
                end;                          
                emit Il.JMP Il.Nil badlab Il.Nil
            | Some _ -> ()
          end

      | Ast.STMT_decl _ -> ()

      | _ -> err (Some stmt.id) "unhandled form of statement in trans_stmt"
  in

  let capture_emitted_quads (node:node_id) : unit = 
    let e = emitter() in 
    let n_vregs = e.Il.emit_next_vreg in 
    let quads = e.Il.emit_quads in 
    let name = path_name () in
      begin
        log cx "emitted quads for %s:" name;
        for i = 0 to (Array.length quads) - 1
        do 
          log cx "[%6d]\t%s" i (Il.string_of_quad cx.ctxt_abi.Abi.abi_str_of_hardreg quads.(i));
        done;
        htab_put cx.ctxt_text_items name (node, quads, n_vregs);
      end
  in
    
  let trans_fn (fnid:node_id) (fn:Ast.fn) : unit =  
    let fixup = get_fn_fixup fnid in
    let framesz = get_framesz fnid in
    Stack.push (Stack.create()) epilogue_jumps;
      push_new_emitter ();
      cx.ctxt_abi.Abi.abi_emit_fn_prologue (emitter()) fixup framesz;
      trans_block fn.Ast.fn_body;
      Stack.iter patch (Stack.pop epilogue_jumps);
      cx.ctxt_abi.Abi.abi_emit_fn_epilogue (emitter());
      capture_emitted_quads fnid;
      pop_emitter ()
  in

  let trans_prog_block (progid:node_id) (b:Ast.block) (ncomp:string) : fixup = 
    let _ = Stack.push ncomp path in
    let fix = new_fixup (path_name ()) in
      push_new_emitter ();
      Il.emit_full (emitter()) (Some fix) Il.DEAD Il.Nil Il.Nil Il.Nil;
      cx.ctxt_abi.Abi.abi_emit_main_prologue (emitter()) b;
      trans_block b;
      cx.ctxt_abi.Abi.abi_emit_main_epilogue (emitter()) b;
      capture_emitted_quads progid;
      pop_emitter ();
      ignore (Stack.pop path);
      fix
  in
    
  let trans_prog (progid:node_id) (p:Ast.prog) : unit =   
    let _ = log cx "translating program: %s" (path_name()) in
    let init = 
      (* FIXME: translate the init part as well. *)
      Asm.IMM 0L
    in
    let main =     
      match p.Ast.prog_main with 
          None -> Asm.IMM 0L
        | Some main -> Asm.M_POS (trans_prog_block progid main "main")            
    in
    let fini = 
      match p.Ast.prog_fini with 
          None -> Asm.IMM 0L
        | Some fini -> Asm.M_POS (trans_prog_block progid fini "fini")
    in
    let prog = 
      (* FIXME: only DEF the entry prog if its name matches a crate param! *)
      (* FIXME: extract prog layout from ABI. *)    
      Asm.DEF (cx.ctxt_entry_prog, 
               Asm.SEQ [| Asm.WORD (TY_u32, init); 
                          Asm.WORD (TY_u32, main); 
                          Asm.WORD (TY_u32, fini) |]) 
    in
      cx.ctxt_data_items <- prog :: cx.ctxt_data_items
  in
    
  let rec trans_mod_item 
      (item:Ast.mod_item) 
      : unit = 
    begin
      match item.node with 
	      Ast.MOD_ITEM_fn f -> trans_fn item.id f.Ast.decl_item
	    | Ast.MOD_ITEM_prog p -> trans_prog item.id p.Ast.decl_item
	    | _ -> ()
    end
  in
    
  let visit_mod_item_pre n p i =
    Stack.push n path;
    trans_mod_item i;
    inner.Walk.visit_mod_item_pre n p i
  in
  let visit_mod_item_post n p i = 
    inner.Walk.visit_mod_item_post n p i;
    ignore (Stack.pop path)
  in
    { inner with 
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post
    }
;;


let fixup_assigning_visitor
    (cx:ctxt)
    (inner:Walk.visitor)
    : Walk.visitor = 

  let path = Stack.create () in     

  let path_name (_:unit) : string = 
    String.concat "." (stk_elts_from_bot path) 
  in

  let visit_mod_item_pre n p i = 
    Stack.push n path;
    begin
      match i.node with 
          Ast.MOD_ITEM_fn _ -> 
            htab_put cx.ctxt_fn_fixups i.id (new_fixup (path_name()))
        | _ -> ()
    end;
    inner.Walk.visit_mod_item_pre n p i
  in

  let visit_mod_item_post n p i = 
    inner.Walk.visit_mod_item_post n p i;
    ignore (Stack.pop path)
  in
  { inner with 
        Walk.visit_mod_item_pre = visit_mod_item_pre;
        Walk.visit_mod_item_post = visit_mod_item_post;
    }

    
let trans_crate 
    (cx:ctxt)
    (items:Ast.mod_items) 
    : ((string, (node_id * Il.quads * int)) Hashtbl.t * Asm.item list * fixup) = 
  let passes = 
	[|
      (fixup_assigning_visitor cx
         Walk.empty_visitor);
      (trans_visitor cx
         Walk.empty_visitor)
    |];
  in
    run_passes cx passes (log cx "%s") items;
    (cx.ctxt_text_items, cx.ctxt_data_items, cx.ctxt_entry_prog)
;;


(* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 *)
