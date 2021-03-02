(** Dead Code Elimination  *)
open Ll_lib.Ll
open Datastructures


(* expose a top-level analysis operation ------------------------------------ *)
(* TASK: This function should optimize a block by removing dead instructions
   - lb: a function from uids to the live-OUT set at the 
     corresponding program point
   - ab: the alias map flowing IN to each program point in the block
   - b: the current ll block

   Note: 
     Call instructions are never considered to be dead (they might produce
     side effects)

     Store instructions are not dead if the pointer written to is live _or_
     the pointer written to may be aliased.

     Other instructions are dead if the value they compute is not live.

   Hint: Consider using List.filter
 *)
let dce_block (lb:uid -> Liveness.Fact.t) 
              (ab:uid -> Alias.fact)
              (b:block) : block =
  {
    b with insns = List.filter (
      fun (uid, insn : uid * insn) -> 
        match insn with
        | Call _ -> true
        | Store (_, _, op) ->
          begin match op with
          | Gid u2 -> true
          | Id u2 -> 
            UidS.mem u2 (lb uid) ||
            begin try (0 = compare (UidM.find u2 (ab uid)) MayAlias)
            with Not_found -> false end
          | _ -> false
          end
        | _ -> UidS.mem uid (lb uid)
    ) b.insns
  }

let run (lg:Liveness.Graph.t) (ag:Alias.Graph.t) (cfg:Cfg.t) : Cfg.t =

  LblS.fold (fun l cfg ->
    let b = Cfg.block cfg l in

    (* compute liveness at each program point for the block *)
    let lb = Liveness.Graph.uid_out lg l in

    (* compute aliases at each program point for the block *)
    let ab = Alias.Graph.uid_in ag l in 

    (* compute optimized block *)
    let b' = dce_block lb ab b in
    Cfg.add_block l b' cfg
  ) (Cfg.nodes cfg) cfg

