open Ll_lib.Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst = struct
  type t =
    | NonConst (* Uid may take on multiple values at runtime *)
    | Const of int64 (* Uid will always evaluate to const i64 or i1 *)
    | UndefConst
  (* Uid is not defined at the point *)

  let compare s t =
    match (s, t) with
    | Const i, Const j -> Int64.compare i j
    | NonConst, NonConst | UndefConst, UndefConst -> 0
    | NonConst, _ | _, UndefConst -> 1
    | UndefConst, _ | _, NonConst -> -1

  let to_string : t -> string = function
    | NonConst -> "NonConst"
    | Const i -> Printf.sprintf "Const (%LdL)" i
    | UndefConst -> "UndefConst"
end

(* The analysis computes, at each program point, which UIDs in scope will evaluate 
   to integer constants *)
type fact = SymConst.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
 *)

let extract_const (op : operand) (d : fact) : SymConst.t =
  match op with
  | Const i -> Const i
  | Id uid -> UidM.find_or SymConst.UndefConst d uid
  | Null -> NonConst
  | Gid gid -> NonConst

let perform_bop (bop : bop) (i1 : int64) (i2 : int64) : SymConst.t =
  let f =
    match bop with
    | Add -> Int64.add
    | Sub -> Int64.sub
    | Mul -> Int64.mul
    | Shl -> fun i1 i2 -> Int64.shift_left i1 (Int64.to_int i2)
    | Lshr -> fun i1 i2 -> Int64.shift_right_logical i1 (Int64.to_int i2)
    | Ashr -> fun i1 i2 -> Int64.shift_right i1 (Int64.to_int i2)
    | And -> Int64.logand
    | Or -> Int64.logor
    | Xor -> Int64.logxor
  in
  Const (f i1 i2)

let perform_icmp (cnd : cnd) (i1 : int64) (i2 : int64) : SymConst.t =
  let tr = 1L in
  let fl = 0L in
  let cmp = Int64.compare i1 i2 in
  let res =
    match cnd with
    | Eq -> cmp = 0
    | Ne -> cmp <> 0
    | Slt -> cmp < 0
    | Sle -> cmp <= 0
    | Sgt -> cmp > 0
    | Sge -> cmp >= 0
  in
  Const (if res then tr else fl)

let insn_flow ((u, i) : uid * insn) (d : fact) : fact =
  let new_out : SymConst.t =
    match i with
    | Binop (bop, _, op1, op2) -> (
        let c1 : SymConst.t = extract_const op1 d in
        let c2 : SymConst.t = extract_const op2 d in
        match (c1, c2) with
        | Const i1, Const i2 -> perform_bop bop i1 i2
        | NonConst, _ | _, NonConst -> NonConst
        | UndefConst, _ | _, UndefConst -> UndefConst)
    | Icmp (cnd, _, op1, op2) -> (
        let c1 : SymConst.t = extract_const op1 d in
        let c2 : SymConst.t = extract_const op2 d in
        match (c1, c2) with
        | Const i1, Const i2 -> perform_icmp cnd i1 i2
        | NonConst, _ | _, NonConst -> NonConst
        | UndefConst, _ | _, UndefConst -> UndefConst)
    | Store _ | Call (Void, _, _) -> UndefConst
    | _ -> NonConst
  in
  UidM.update_or new_out (fun _ -> new_out) u d

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t : terminator) (d : fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact = struct
  type t = fact

  let forwards = true

  let insn_flow = insn_flow

  let terminator_flow = terminator_flow

  let normalize : fact -> fact =
    UidM.filter (fun _ v -> v != SymConst.UndefConst)

  let compare (d : fact) (e : fact) : int =
    UidM.compare SymConst.compare (normalize d) (normalize e)

  let to_string : fact -> string =
    UidM.to_string (fun _ v -> SymConst.to_string v)

  (* The constprop analysis should take the meet over predecessors to compute the
     flow into a node. You may find the UidM.merge function useful *)
  let combine (ds : fact list) : fact =
    List.fold_left
      (UidM.merge (fun uid const_op1 const_op2 ->
           match (const_op1, const_op2) with
           | Some _, Some _ | Some _, None -> const_op1
           | None, Some _ -> const_op2
           | None, None -> None))
      UidM.empty ds
end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g : Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in =
    List.fold_right
      (fun (u, _) -> UidM.add u SymConst.NonConst)
      g.Cfg.args UidM.empty
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg

(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper 
   functions.                                                                 *)

let replace_op const_map op =
  match op with
  | Id uid -> (
      match UidM.find_or SymConst.UndefConst const_map uid with
      | Const i -> Const i
      | _ -> op)
  | _ -> op

let rewrite_insn const_map insn =
  match insn with
  | Binop (bop, ty, op1, op2) ->
      let new_op1 = replace_op const_map op1 in
      let new_op2 = replace_op const_map op2 in
      Binop (bop, ty, new_op1, new_op2)
  | Load (ty, op) ->
      let new_op = replace_op const_map op in
      Load (ty, new_op)
  | Store (ty, op1, op2) ->
      let new_op1 = replace_op const_map op1 in
      let new_op2 = replace_op const_map op2 in
      Store (ty, new_op1, new_op2)
  | Icmp (cnd, ty, op1, op2) ->
      let new_op1 = replace_op const_map op1 in
      let new_op2 = replace_op const_map op2 in
      Icmp (cnd, ty, new_op1, new_op2)
  | Call (ty, op1, args) ->
      let new_op1 = replace_op const_map op1 in
      let new_args =
        List.map
          (fun p ->
            let ty = fst p in
            let op2 = snd p in
            let new_op2 = replace_op const_map op2 in
            (ty, new_op2))
          args
      in
      Call (ty, new_op1, new_args)
  | Bitcast (ty1, op1, ty2) ->
      let new_op1 = replace_op const_map op1 in
      Bitcast (ty1, new_op1, ty2)
  | Gep (ty, op1, opl) ->
      let new_op1 = replace_op const_map op1 in
      let new_opl = List.map (replace_op const_map) opl in
      Gep (ty, new_op1, new_opl)
  | _ -> insn

let rewrite_term t cb =
  let id = fst t in
  let const_map = cb id in
  let term = snd t in
  let new_term =
    match term with
    | Ret (ty, op_op) ->
        let new_op_op =
          match op_op with
          | Some op1 -> Some (replace_op const_map op1)
          | None -> None
        in
        Ret (ty, new_op_op)
    | Cbr (op1, lbl1, lbl2) ->
        let new_op1 = replace_op const_map op1 in
        Cbr (new_op1, lbl1, lbl2)
    | _ -> term
  in
  (id, new_term)

let run (cg : Graph.t) (cfg : Cfg.t) : Cfg.t =
  let open SymConst in
  let cp_block (l : lbl) (cfg : Cfg.t) : Cfg.t =
    let b : block = Cfg.block cfg l in
    let cb : uid -> Fact.t = Graph.uid_out cg l in
    let new_insns : (uid * insn) list =
      List.map
        (fun p ->
          let id = fst p in
          let const_map = cb id in
          let insn = snd p in
          (id, rewrite_insn const_map insn))
        b.insns
    in
    let new_term = rewrite_term b.term cb in
    let new_block : block = { insns = new_insns; term = new_term } in

    Cfg.add_block l new_block cfg
  in

  LblS.fold cp_block (Cfg.nodes cfg) cfg
