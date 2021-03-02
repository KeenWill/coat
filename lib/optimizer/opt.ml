(** Optimizer *)
open Ll_lib
open Util

(* dead code elimination ---------------------------------------------------- *)
let dce (g:Cfg.t) : Cfg.t =
  let ag = Alias.analyze g in
  let lg = Liveness.analyze g in
  Dce.run lg ag g

(* constant propagation ----------------------------------------------------- *)
let cp (g:Cfg.t) : Cfg.t =
  let cg = Constprop.analyze g in
  Constprop.run cg g

(* "full" optimization: n rounds of (dce followed by constant) propagation -- *)
let rec pass n (g:Cfg.t) =
  if n <= 0 
  then g 
  else pass (n - 1) (g |> dce |> cp)

(* optimize an fdecl -------------------------------------------------------- *)
(* runs (two) passes of dce followed by constant propagation on the supplied 
   LL IR fdecl.                                                               *)
let opt_fdecl (gid,fdecl:Ll.gid * Ll.fdecl) : Ll.gid * Ll.fdecl =
  let g = pass 2 (Cfg.of_ast fdecl) in
  gid, Cfg.to_ast g

(* flag for the main compiler driver *)
let do_opt = ref false

(* optimize each fdecl in the program *)
let optimize (p:Ll.prog) : Ll.prog = 
  if !do_opt 
  then begin 
    Platform.verb @@ Printf.sprintf "..optimizing";  
    { p with Ll.fdecls = List.map opt_fdecl p.Ll.fdecls }
  end
  else p

