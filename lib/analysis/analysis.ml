open Ll
open Datastructures



(* Because the cfg instance of the dataflow graph uses _basic blocks_ as nodes,
   we need a way to recover the dataflow facts at individual instructions 
   within the block.  Depending on the direction of the analysis, this amounts to
   propagating information either forward or backwards through the block.  

   The following helper functions construct maps from each instruction or terminator 
   to the  corresponding dataflow fact.  *)

(* Compute IN for each instruction in a block, given IN of the first instruction *)
let block_flow_forwards_map (fi:uid * insn -> 'd -> 'd) 
                            (ft:terminator -> 'd -> 'd) 
                            ({insns; term}:block) (d_in:'d) : uid -> 'd =
  let t_id, t = term in
  let m, d_tmn = List.fold_left (fun (m, d) (u,i) -> 
                                 let d' = fi (u,i) d in
                                 UidM.add u d m, d')
                                (UidM.empty, d_in) insns in
  let m' = UidM.add t_id d_tmn m in
  fun u -> UidM.find u m'

(* Compute OUT for each instruction in a block, given OUT of the terminator*)
let block_flow_backwards_map (fi:uid * insn -> 'd -> 'd) 
                             (ft:terminator -> 'd -> 'd) 
                             ({insns; term}:block) (d_out:'d) : uid -> 'd =
  let t_id, t = term in
  let d_ins = ft t d_out in
  let m, _ = List.fold_right (fun (u,i) (m, d) -> 
                                 let d' = fi (u,i) d in
                                 UidM.add u d m, d')
                                insns (UidM.empty, d_ins) in
  let m' = UidM.add t_id d_out m in
  fun u -> UidM.find u m'
