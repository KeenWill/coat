open Ll_lib.Ll
open Ll_lib
open Datastructures

(* control flow graphs ------------------------------------------------------ *)

(* This representation of control-flow graphs is more suited for dataflow
   analysis than the abstract syntax defined in fdecl

   - a cfg has:
         blocks - a map of labels to  Ll basic block, and
         preds  - a set of labels containing the blocks predecessors           
         ret_ty - the Ll return type of the function
         args   - a list of function parameters with their types

   Representing cfgs as maps makes it simpler to look up information
   about the nodes in the graph.                                              *)

type cfg = {
  blocks : block LblM.t;
  preds : LblS.t LblM.t;
  ret_ty : ty;
  args : (uid * ty) list;
}

let entry_lbl = "_entry"

(* compute a block's successors --------------------------------------------- *)
let block_succs (b : block) : LblS.t =
  match b.term with
  | _, Ret _ -> LblS.empty
  | _, Br l -> LblS.singleton l
  | _, Cbr (_, k, l) -> LblS.of_list [ k; l ]

(* compute a map from block labels to predecessors -------------------------- *)
let cfg_preds (ast : (lbl * block) list) : LblS.t LblM.t =
  let set_add l = LblM.update_or LblS.empty (LblS.add l) in
  List.fold_left
    (fun m (l, b) ->
      m
      |> LblM.update_or LblS.empty (fun s -> s) l
      |> LblS.fold (set_add l) (block_succs b))
    LblM.empty ast

(* lookup operations -------------------------------------------------------- *)
let preds (g : cfg) (l : lbl) : LblS.t = LblM.find l g.preds

let succs (g : cfg) (l : lbl) : LblS.t = block_succs @@ LblM.find l g.blocks

let block (g : cfg) (l : lbl) : block = LblM.find l g.blocks

let nodes (g : cfg) : LblS.t =
  LblM.bindings g.blocks |> List.map fst |> LblS.of_list

let exits (g : cfg) : LblS.t =
  LblM.bindings g.blocks
  |> List.filter (fun (l, b) -> LblS.is_empty @@ block_succs b)
  |> List.map fst |> LblS.of_list

(* convert an fdecl to this representation ------------------------------- *)
let of_ast (fdecl : fdecl) : cfg =
  let e, bs = fdecl.f_cfg in
  let ast = (entry_lbl, e) :: bs in
  let preds = cfg_preds ast in
  let blocks =
    List.fold_left (fun g (l, block) -> LblM.add l block g) LblM.empty ast
  in
  let paramtys, ret_ty = fdecl.f_ty in
  let args = List.combine fdecl.f_param paramtys in
  { preds; blocks; ret_ty; args }

(* convert this representation back to cfg ------------------------------- *)
let to_ast (g : cfg) : fdecl =
  let e = block g entry_lbl in
  let f_cfg = (e, g.blocks |> LblM.bindings |> List.remove_assoc entry_lbl) in
  let f_param, paramtys = List.split g.args in
  { f_cfg; f_param; f_ty = (paramtys, g.ret_ty) }

let add_block (l : lbl) (block : block) (g : cfg) : cfg =
  { g with blocks = LblM.add l block g.blocks }

(* creating a flow graph from a control flow graph -------------------------- *)

(* Conceptually, this is a view of a cfg annotated with dataflow information
   that should be usable by the generic solver and subsequent optimizations.

   To create a flow graph module for a particular analysis, we need to supply
   several parameters:                                                        *)
module type AS_GRAPH_PARAMS = sig
  (* The type of dataflow facts and the combine operator. This just implements
     the FACT interface from cfg.ml *)
  type t

  val combine : t list -> t

  val to_string : t -> string

  (* We also need to specify the direction of the analysis *)
  val forwards : bool

  (* The flow functions defined on the CFG. *)
  (* Flow functions across an instruction or terminator
       for forward analysis: should map in[n] to out[n]
       for backward analysis: should map out[n] to in[n]
  *)
  val insn_flow : uid * insn -> t -> t

  val terminator_flow : terminator -> t -> t
end

(* This Cfg.AsGraph can be used to create a flow graph for the solver from a
   control flow graph with of_cfg  *)
module AsGraph (D : AS_GRAPH_PARAMS) : sig
  (* Implement the DFA_GRAPH signature where facts are defined by the functor
     argument type t and nodes are blocks of the cfg identified by labels *)
  include Solver.DFA_GRAPH with type fact := D.t and module NodeS = LblS

  (* To use the resulting flow graph in optimizations, we need expose a few
     more operations: *)

  (* Create a flow graph for this analysis an initial mapping of facts for
     block labels, a constant flow-in value for the entry or exit labels depending
     on the direction of the analysis, and a CFG *)
  val of_cfg : (node -> D.t) -> D.t -> cfg -> t

  (* We need to be able to look up the resulting solved dataflow analysis facts
     per block and per instruction.  We also need to be able to inspect the
     input and output information for each.
     The way we compute this information depends on the direction of the
     analysis.
        uid_in and uid_out work for both instructions and terminators
        the provided lbl is that of the containing block
  *)

  val block_in : t -> lbl -> D.t

  val block_out : t -> lbl -> D.t

  val uid_in : t -> lbl -> uid -> D.t

  val uid_out : t -> lbl -> uid -> D.t

  (* For testing purposes, we would like to be able to access the underlying
     map of dataflow facts *)
  val dfa : t -> D.t LblM.t
end = struct
  module NodeS = LblS

  type t = { cfg : cfg; dfa : D.t LblM.t }

  (* We use _blocks_ of the control flow graph as the nodes of the dataflow
     graph.  Each block is identified by its lbl.

     This choice means that we won't use the "exploded" control-flow graph, where
     each instruction is considered a node.  The reason for this decision is two-fold:
     One: the edges of the cfg are defined in terms of block labesl.
     Two: we can speed up the dataflow analysis by propagating information across and
     entire block.
     The cost of this decision is that we have to re-calculate the flow information
     for individual instructions when we need it.
  *)
  type node = lbl

  (* The label of the logical "boundary" node.  This boundary node represents
     a logical predecessor of the entry block (for forward analysis) or the
     logical successor of the exits blocks (for backward analysis).  It provides
     an "edge" on which to put the entry/exit flow information. *)
  let bound_lbl = "__bound"

  (* The only way to create a flow graph is to provide an initial labeling *)
  let of_cfg init flow_in cfg =
    let dfa =
      cfg.blocks |> LblM.mapi (fun l _ -> init l) |> LblM.add bound_lbl flow_in
    in
    { cfg; dfa }

  (* Access to underlying cfg and facts map  *)
  let block g = block g.cfg

  let nodes g = nodes g.cfg |> NodeS.add bound_lbl

  let dfa g = LblM.remove bound_lbl g.dfa

  (* Create the dfa successors and predecessors based on the direction of the
     data flow analysis. This also adds the boundary node to the succs/preds.

     This graph is build at the "block" level, so nodes are block labels.
  *)
  let extend k v f l = if LblS.mem l k then v else f l

  let ns = NodeS.singleton

  let dfa_preds =
    if D.forwards then fun g ->
      preds g.cfg
      |> extend (ns entry_lbl) (ns bound_lbl)
      |> extend (ns bound_lbl) NodeS.empty
    else fun g ->
      succs g.cfg
      |> extend (exits g.cfg) (ns bound_lbl)
      |> extend (ns bound_lbl) NodeS.empty

  let dfa_succs =
    if D.forwards then fun g ->
      extend (ns bound_lbl) (ns entry_lbl) (succs g.cfg)
    else fun g -> extend (ns bound_lbl) (exits g.cfg) (preds g.cfg)

  let preds = dfa_preds

  let succs = dfa_succs

  (* Block flow function *)
  (* dataflow analysis helpers ------------------------------------------------ *)

  (* Propagate dataflow information forward through a whole block.
       - fi is the flow function for instructions
       - ft is the flow function for terminators
       - d_in is the dataflow fact on the in edge
     Returns the resulting out / in fact. *)
  let block_flow_forwards ({ insns; term } : block) (d_in : 'd) : 'd =
    let d_tmn =
      List.fold_left (fun d (u, i) -> D.insn_flow (u, i) d) d_in insns
    in
    D.terminator_flow (snd term) d_tmn

  let block_flow_backwards ({ insns; term } : block) (d_out : 'd) : 'd =
    let d_ins = D.terminator_flow (snd term) d_out in
    List.fold_right (fun (u, i) d -> D.insn_flow (u, i) d) insns d_ins

  let flow_block g l d =
    (if D.forwards then block_flow_forwards else block_flow_backwards)
      (block g l) d

  (* The supplied flow function, plus the boundary value. Used by the solver. *)
  let flow g (l : lbl) =
    if Lbl.compare l bound_lbl == 0 then fun _ -> LblM.find l g.dfa
    else flow_block g l

  (* Look up and modify facts when viewing the CFG as a dataflow graph, used by the solver. *)
  let out g n = LblM.find n g.dfa

  let add_fact n d g = { g with dfa = LblM.add n d g.dfa }

  (* Because the cfg instance of the dataflow graph uses _basic blocks_ as nodes,
     we need a way to recover the dataflow facts at individual instructions
     within the block.  Depending on the direction of the analysis, this amounts to
     propagating information either forward or backwards through the block.

     The following helper functions construct maps from each instruction or terminator
     to the  corresponding dataflow fact. *)

  (* Compute IN[n] for each instruction in a block, given IN of the first instruction *)
  let in_forwards_map ({ insns; term } : block) (d_in : 'd) : uid -> 'd =
    let t_id, t = term in
    let m, d_tmn =
      List.fold_left
        (fun (m, d) (u, i) ->
          let d' = D.insn_flow (u, i) d in
          (UidM.add u d m, d'))
        (UidM.empty, d_in) insns
    in
    let m' = UidM.add t_id d_tmn m in
    fun u -> UidM.find u m'

  (* Compute OUT[n] for each instruction in a block, given IN of the first instruction *)
  let out_forwards_map ({ insns; term } : block) (d_in : 'd) : uid -> 'd =
    let t_id, t = term in
    let m, d_tmn =
      List.fold_left
        (fun (m, d_in) (u, i) ->
          let d_out = D.insn_flow (u, i) d_in in
          (UidM.add u d_out m, d_out))
        (UidM.empty, d_in) insns
    in
    let d_out = D.terminator_flow t d_tmn in
    let m' = UidM.add t_id d_out m in
    fun u -> UidM.find u m'

  (* Compute IN[n] for each instruction in a block, given OUT of the terminator*)
  let in_backwards_map ({ insns; term } : block) (d_out : 'd) : uid -> 'd =
    let t_id, t = term in
    let d_ins = D.terminator_flow t d_out in
    let m_init = UidM.add t_id d_ins UidM.empty in
    let m, _ =
      List.fold_right
        (fun (u, i) (m, d_out) ->
          let d_in = D.insn_flow (u, i) d_out in
          (UidM.add u d_in m, d_in))
        insns (m_init, d_ins)
    in
    fun u -> UidM.find u m

  (* Compute OUT[n] for each instruction in a block, given OUT of the terminator*)
  let out_backwards_map ({ insns; term } : block) (d_out : 'd) : uid -> 'd =
    let t_id, t = term in
    let m_init = UidM.add t_id d_out UidM.empty in
    let d_ins = D.terminator_flow t d_out in
    let m, _ =
      List.fold_right
        (fun (u, i) (m, d_out) ->
          let d_in = D.insn_flow (u, i) d_out in
          (UidM.add u d_out m, d_in))
        insns (m_init, d_ins)
    in
    fun u -> UidM.find u m

  (* Wrapper functions to account for directionality of the analysis *)
  let block_in g n =
    if D.forwards then
      let preds = NodeS.elements @@ preds g n in
      let d_outs = List.map (out g) preds in
      D.combine d_outs
    else out g n

  let block_out g n =
    if D.forwards then out g n
    else
      let preds = NodeS.elements @@ preds g n in
      let d_outs = List.map (out g) preds in
      D.combine d_outs

  let uid_in g (l : lbl) =
    if D.forwards then in_forwards_map (block g l) (block_in g l)
    else in_backwards_map (block g l) (block_out g l)

  let uid_out g (l : lbl) =
    if D.forwards then out_forwards_map (block g l) (block_in g l)
    else out_backwards_map (block g l) (block_out g l)

  (* Printing functions *)
  (* printing functions ------------------------------------------------------- *)

  let annot_insn g l ((u, i) : uid * insn) =
    Printf.sprintf "  IN : %s\n   %s\n  OUT: %s"
      (D.to_string (uid_in g l u))
      (Llutil.string_of_named_insn (u, i))
      (D.to_string (uid_out g l u))

  let annot_terminator g l ((u, t) : uid * terminator) =
    Printf.sprintf "  IN : %s\n   %s\n  OUT: %s"
      (D.to_string (uid_in g l u))
      (Llutil.string_of_terminator t)
      (D.to_string (uid_out g l u))

  let to_string_annot (annot : lbl -> string) (g : t) : string =
    LblM.to_string
      (fun l block ->
        Printf.sprintf "%s\n%s\n%s\n\n" (annot l)
          (Llutil.mapcat "\n" (annot_insn g l) block.insns)
          (annot_terminator g l block.term))
      g.cfg.blocks

  let printer_annot (annot : lbl -> string) (f : Format.formatter) (g : t) :
      unit =
    Format.pp_print_string f (to_string_annot annot g)

  let to_string g = to_string_annot (fun l -> D.to_string (out g l)) g

  let printer f g = printer_annot (fun l -> D.to_string (out g l)) f g
end

(* exported type *)
type t = cfg
