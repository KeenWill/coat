open Ll
open Llutil

(* LLVMlite interpreter *)

type mid = int                  (* memory block id *)
type fid = int                  (* stack frame id *)
type idx = int                  (* index *)

(* Memory block identifier *)
type bid = GlobId of gid
         | HeapId of mid
         | StckId of fid
         | NullId

(* Pointers are tagged with a description of the block they reference
   offsets are represented as paths into memory values *)
type ptr = ty * bid * idx list

(* "Simple" or stack values  *)
type sval = 
  | VUndef
  | VInt of int64
  | VPtr of ptr

(* Memory values *)
type mtree = MWord of sval
           | MStr of string
           | MNode of mval
and mval = mtree list

(* Locals *)
type locals = uid -> sval

(* The memory state *)
type config =
  { globals : (gid * mval) list
  ; heap    : (mid * mval) list
  ; stack   : (fid * mval) list
  }

(* Create memory value for global declaration *)
let mval_of_gdecl (gd:gdecl) : mval =
  let rec mtree_of_gdecl : gdecl -> mtree = function
    | ty, GNull              -> MWord (VPtr (ty, NullId, [0]))
    | ty, GGid g             -> MWord (VPtr (ty, GlobId g, [0]))
    | _, GBitcast (t1, v,t2) -> mtree_of_gdecl (t1, v)
    | _, GInt i              -> MWord (VInt i)
    | _, GString s           -> MStr s
    | _, GArray gs
    | _, GStruct gs          -> MNode (List.map mtree_of_gdecl gs)
  in [mtree_of_gdecl gd]

(* Create fully undefined memory value for a type *)
let mval_of_ty (nt:tid -> ty) (t:ty) : mval =
  let rec mtree_of_ty : ty -> mtree = function
    | I1 | I8 | I64 | Ptr _ -> MWord VUndef
    | Array (n, I8) -> MStr (String.make n '\x00')
    | Array (n, t)  -> MNode Array.(make n (MWord VUndef) |> to_list)
    | Struct ts     -> MNode (List.map mtree_of_ty ts)
    | Fun _ | Void  -> failwith "mval_of_ty: mval for bad type"
    | Namedt id     -> mtree_of_ty (nt id)
  in [mtree_of_ty t]


(* Printing machine states *)
let mapcat s f l = String.concat s @@ List.map f l
let prefix p f a = p ^ f a
let ( ^. ) s t = if s = "" || t = "" then "" else s ^ t
let pp = Printf.sprintf

let string_of_bid = function
  | GlobId gid -> "@" ^ gid
  | HeapId mid -> "M" ^ (string_of_int mid)
  | StckId fid -> "S" ^ (string_of_int fid)
  | NullId -> "null"

let string_of_ptr (t, b, i) =
  pp "%s %s %s" (string_of_ty t) (string_of_bid b)
    (mapcat ", " string_of_int i)

let string_of_sval (sv:sval) : string =
  match sv with
  | VUndef -> "undef"
  | VInt x -> Int64.to_string x
  | VPtr p -> string_of_ptr p
      
let rec string_of_mval (mv:mval) : string =
  "[" ^ (mapcat " " string_of_mtree mv) ^ "]"

and string_of_mtree = function
  | MWord sv -> string_of_sval sv
  | MStr s -> "\"" ^ s ^ "\""
  | MNode m -> string_of_mval m


(* Varieties of undefined behavior. Can be raised by load/store *)
exception OOBIndexDeref         (* mem access not in bounds of an array *)
exception NullPtrDeref          (* deref Null *)
exception UndefPtrDeref         (* deref Undef pointer (from bad GEP) *)
exception IncompatTagDeref      (* deref pointer at wrong type (bad bitcast) *)
exception UndefMemDeref         (* read uninitialized memory *)
exception UninitMemLoad         (* uninitialized memory load *)
exception UseAfterFree          (* deref freed stack/heap memory *)


(* Arithmetic operations are all signed 64bit 2s compliment (mod In64.max_int)  *)
let interp_bop (b:bop) (v1:sval) (v2:sval) : sval =
  let i, j = match v1, v2 with
    | VInt i, VInt j -> i, j
    | _ -> invalid_arg "interp_bop"
  in
  let open Int64 in
  let f = match b with
    | Add -> add | Sub -> sub | Mul -> mul
    | And -> logand | Or -> logor | Xor -> logxor
    | Shl -> fun i j -> shift_left i @@ to_int j
    | Lshr -> fun i j -> shift_right_logical i @@ to_int j
    | Ashr -> fun i j -> shift_right i @@ to_int j
  in VInt (f i j)
                                               
let interp_cnd (c:cnd) (v1:sval) (v2:sval) =
  let f = match c with 
    | Eq -> (=) | Ne -> (<>) | Slt -> (<)
    | Sle -> (<=) | Sgt -> (>) | Sge -> (>=)
  in
  match v1, v2, c with
    | VPtr (_,b1,i1), VPtr (_,b2,i2), Eq
    | VPtr (_,b1,i1), VPtr (_,b2,i2), Ne ->
       VInt (if f (b1,i1) (b2,i2) then 1L else 0L)
    | VInt i, VInt j, _ ->
       VInt (if f i j then 1L else 0L)
    | _ -> invalid_arg "interp_cnd"

let interp_i1 : sval -> bool = function
  | VInt 0L -> false
  | VInt 1L -> true
  | _ -> invalid_arg "interp_i1"

let rec interp_operand (nt:tid -> ty) (locs:locals) (ty:ty) (o:operand) : sval =
  match ty, o with
  | I64, Const i  -> VInt i
  | Ptr ty, Null  -> VPtr (ty, NullId, [0])
  | Ptr ty, Gid g -> VPtr (ty, GlobId g, [0])
  | _, Id u       -> locs u
  | Namedt id, o  -> interp_operand nt locs (nt id) o
  | _             -> failwith @@ "interp_operand: malformed operand " ^ soo o ^ ":" ^ sot ty 


(* Some utility functions *)
let update f k v = fun k' -> if k = k' then v else f k'

let rec is_prefix (l:'a list) (m:'a list) : bool =
  match l, m with
  | [], _ -> true
  | _, [] -> false
  | a::l, b::m -> a = b && is_prefix l m

let replace_assoc (l:('a * 'b) list) (a:'a) (b:'b) : ('a * 'b) list =
  let rec loop acc = function
    | [] -> List.rev @@ (a,b)::acc
    | (a',_)::l' when a = a' -> List.rev_append acc @@ (a,b):: l'
    | e::l' -> loop (e::acc) l'
  in
  loop [] l

let replace_nth (l:'a list) (n:int) (a:'a) : 'a list =
  let rec loop acc n  = function
    | [] -> raise Not_found
    | a'::l' -> if n = 0 then List.rev_append acc (a::l')
                         else loop (a'::acc) (pred n) l'
  in
  loop [] n l


(* Memory access *)
let rec load_idxs (m:mval) (idxs:idx list) : mtree =
  match idxs with
  | [] -> MNode m
  | i::idxs' ->
     let len = List.length m in
     if len <= i || i < 0 then raise OOBIndexDeref else
     match idxs', List.nth m i with
     | [],  mt       -> mt
     | [0], MStr s   -> MStr s  (* [n x i8]* %p and gep [n x i8]* %p, 0, 0 alias *)
     | _,   MWord v  -> failwith "load_idxs: attempted to index into word"
     | _,   MStr _   -> failwith "load_idxs: attempted to index into string"
     | _,   MNode m' -> load_idxs m' idxs'

let rec store_idxs (m:mval) (idxs:idx list) (mt:mtree) : mval =
  match idxs with
  | [] -> [mt]
  | i::idxs' ->
     let len = List.length m in
     if len <= i || i < 0 then raise OOBIndexDeref else
     match idxs', List.nth m i with
     | [], _        -> replace_nth m i mt
     | _,  MWord v  -> failwith "store_idxs: attempted to index into word"
     | _,  MStr _   -> failwith "store_idxs: attempted to index into string"
     | _,  MNode m' -> replace_nth m i @@ MNode (store_idxs m' idxs' mt)

let load_bid (c:config) (bid:bid) : mval =
  match bid with
  | NullId -> raise NullPtrDeref
  | HeapId mid -> 
     (try List.assoc mid c.heap 
      with Not_found -> raise UseAfterFree)
  | GlobId gid ->
     (try List.assoc gid c.globals 
      with Not_found -> failwith "Load: bogus gid")
  | StckId fid ->
     (try List.assoc fid c.stack 
      with Not_found -> raise UseAfterFree)


let load_ptr (c:config) (_, bid, idxs:ptr) : mtree = 
  load_idxs (load_bid c bid) idxs

let store_ptr (c:config) (_, bid, idxs:ptr) (mt:mtree) : config =
  let mval = load_bid c bid in
  match bid with
  | NullId -> raise NullPtrDeref
  | HeapId mid -> 
     let mval' = store_idxs mval idxs mt in
     let heap = replace_assoc c.heap mid mval' in
     {c with heap}
  | GlobId gid ->
     let mval' = store_idxs mval idxs mt in
     let globals = replace_assoc c.globals gid mval' in
     {c with globals}
  | StckId fid ->
     let frame' = store_idxs mval idxs mt in
     let stack = replace_assoc c.stack fid frame' in
     {c with stack}


(* Tag and GEP implementation *)
let effective_tag (nt:tid -> ty) (tag, _, idxs :ptr) : ty =
  let rec loop tag idxs = 
    match tag, idxs with
    | t, []                  -> t
    | Struct ts,    i::idxs' -> if List.length ts <= i 
                                then failwith "effective_tag: index oob of struct"
                                else loop (List.nth ts i) idxs'
    | Array (n, t), i::idxs' -> loop t idxs' (* Don't check if OOB! *)
    | Namedt id,    _        -> loop (nt id) idxs
    | _,            _::_     -> failwith "effective_tag: index into non-aggregate"
  in
  loop tag @@ try List.tl idxs
              with Failure _ -> failwith "effective_tag: invalid pointer missing first index"


let rec gep_idxs (p:idx list) (idxs:idx list) : idx list =
  match p, idxs with
  | [], _ | _, [] -> failwith "gep_idxs: invalid indices"
  | [i], j::idxs' -> i+j::idxs'
  | i::p', _ -> i::gep_idxs p' idxs


let legal_gep (nt:tid -> ty) (sty:ty) (tag:ty) : bool =
  let rec ptrtoi8 : ty -> ty = function
    | Ptr _ -> Ptr I8
    | Struct ts -> Struct (List.map ptrtoi8 ts)
    | Array (n, t) -> Array (n, ptrtoi8 t)
    | Namedt id -> ptrtoi8 (nt id)
    | t -> t
  in
  let rec flatten_ty : ty -> ty list = function
    | Struct ts -> List.(concat @@ map flatten_ty ts)
    | t -> [t]
  in
  is_prefix (flatten_ty @@ ptrtoi8 sty) (flatten_ty @@ ptrtoi8 tag)
  
let gep_ptr (nt:tid -> ty) (ot:ty) (p:ptr) (idxs':idx list) : sval =
  if not (legal_gep nt ot @@ effective_tag nt p) then VUndef else
    match p with
    | t, NullId, idxs -> VUndef
    | t, bid, idxs ->
       VPtr (t, bid, gep_idxs idxs idxs')


(* LLVMlite reference interpreter *)
let interp_prog {tdecls; gdecls; fdecls} (args:string list) : sval =
  
  let globals = List.map (fun (g,gd) -> g,mval_of_gdecl gd) gdecls in

  let nt (id:tid) : ty =
    try List.assoc id tdecls
    with Not_found -> failwith @@ "interp_prog: undefined named type " ^ id
  in

  let interp_op = interp_operand nt in

  let next_id : unit -> fid = 
    let c = ref 0 in
    fun () -> c := succ !c; !c
  in

  (* Global identifiers that will be interpreted as external functions *)
  let runtime_fns = [ "ll_puts"; "ll_strcat"; "ll_ltoa" ] 
  in

  (* External function implementation *)
  let runtime_call (t:ty) (fn:gid) (args:sval list) (c:config) : config * sval = 
    let load_strptr p = match load_ptr c p with
      | MStr s -> s
      | _ -> failwith "runtime_call: non string-ptr arg"
    in
    match t, fn, args with
    | Void, "ll_puts", [VPtr p] -> 
       let s = load_strptr p in
       print_endline s; 
       c, VUndef
    | Ptr t, "ll_strcat", [VPtr ps1; VPtr ps2] -> 
       let s1 = load_strptr ps1 in
       let s2 = load_strptr ps2 in
       let mid = next_id () in
       let mv = [MStr (s1 ^ s2)] in
       let heap = (mid, mv)::c.heap in
       {c with heap}, VPtr (t, HeapId mid, [0])
    | I64, "ll_ltoa", [VInt i; VPtr dst] ->
       let mid = next_id () in
       let mv = [MStr (Int64.to_string i)] in
       let heap = (mid, mv)::c.heap in
       {c with heap}, VPtr (t, HeapId mid, [0])
    | _ -> failwith @@ "runtime_call: invalid call to " ^ fn
  in


  (* Interprety the body of a function *)
  let rec interp_cfg (k, blocks:cfg) (locs:locals) (c:config) : config * sval =
    match k.insns, k.term with

    | (u, Binop (b, t, o1, o2))::insns, _ ->
       let v1 = interp_op locs t o1 in
       let v2 = interp_op locs t o2 in
       let vr = interp_bop b v1 v2 in
       let locs' = update locs u vr in
       interp_cfg ({k with insns}, blocks) locs' c
       
    | (u, Icmp (cnd, t, o1, o2))::insns, _ ->
       let v1 = interp_op locs t o1 in
       let v2 = interp_op locs t o2 in
       let vr = interp_cnd cnd v1 v2 in
       let locs' = update locs u vr in
       interp_cfg ({k with insns}, blocks) locs' c

    | (u, Alloca ty)::insns, _ ->
       begin match c.stack with
       | [] -> failwith "stack empty"
       | (fid,frame)::stack' -> 
          let ptr = VPtr (ty, StckId fid, [List.length frame]) in
          let stack = (fid, frame @ [MWord VUndef])::stack' in
          let locs' = update locs u ptr in
          interp_cfg ({k with insns}, blocks) locs' {c with stack}
       end

    | (u, Load (Ptr t, o))::insns, _ ->
       let mt = match interp_op locs (Ptr t) o with
         | VPtr p ->
            if effective_tag nt p <> t 
            then raise IncompatTagDeref 
            else load_ptr c p
         | VUndef -> raise UndefPtrDeref
         | VInt _ -> failwith "non-ptr arg for load"
       in
       let v = match mt with
         | MWord VUndef -> raise UninitMemLoad
         | MWord v -> v
         | _ -> failwith "load: returned aggregate"
       in
       let locs' = update locs u v in
       interp_cfg ({k with insns}, blocks) locs' c

    | (_, Store (t, os, od))::insns, _ ->
       let vs = interp_op locs t os in
       let vd = interp_op locs (Ptr t) od in
       let c' = match vd with
         | VPtr p ->
            if effective_tag nt p <> t 
            then raise IncompatTagDeref 
            else store_ptr c p (MWord vs)
         | VUndef -> raise UndefPtrDeref
         | VInt _ -> failwith "non-vptr arg for load"
       in
       interp_cfg ({k with insns}, blocks) locs c'
       
    | (u, Call (t, ofn, ato))::insns, _ ->
       let ats, aos = List.split ato in
       let ft = Ptr (Fun (ats, t)) in
       let g = match interp_op locs ft ofn with
         | VPtr (_, GlobId g, [0]) -> g
         | _ -> failwith "bad call arg"
       in
       let args = List.map2 (interp_op locs) ats aos in
       let c', r = interp_call t g args c in
       let locs' = update locs u r in
       interp_cfg ({k with insns}, blocks) locs' c'

    | (u, Bitcast (t1, o, _))::insns, _ ->
       let v = interp_op locs t1 o in
       let locs' = update locs u v in
       interp_cfg ({k with insns}, blocks) locs' c

    | (u, Gep (Ptr t, o, os))::insns, _ ->
       let idx_of_sval : sval -> idx = function
         | VInt i -> Int64.to_int i
         | _ -> failwith "idx_of_sval: non-integer value"
       in
       let vs = List.map (interp_op locs I64) os in
       let idxs' = List.map idx_of_sval vs in
       let v' = match interp_op locs (Ptr t) o with
         | VPtr p -> gep_ptr nt t p idxs'
         | VUndef -> VUndef
         | VInt _ -> failwith "non-ptr arg for gep"
       in           
       let locs' = update locs u v' in
       interp_cfg ({k with insns}, blocks) locs' c

    | [], (_, Ret (_, None)) -> 
       {c with stack = List.tl c.stack}, VUndef

    | [], (_, Ret (t, Some o)) -> 
       {c with stack = List.tl c.stack}, interp_op locs t o

    | [], (_, Br l) -> 
       let k' = List.assoc l blocks in
       interp_cfg (k', blocks) locs c

    | [], (_, Cbr (o, l1, l2)) -> 
       let v = interp_op locs I1 o in
       let l' = if interp_i1 v then l1 else l2 in
       let k' = List.assoc l' blocks in
       interp_cfg (k', blocks) locs c

    | (u,i)::_, _ ->  failwith @@ "interp_cfg: invalid instruction \"" 
                                  ^ string_of_insn i ^ "\" at %" ^ u

  and interp_call (ty:ty) (fn:gid) (args:sval list) (c:config) : config * sval =
    if List.mem fn runtime_fns
    then runtime_call ty fn args c
    else
    let {f_param; f_cfg} = try List.assoc fn fdecls 
                       with Not_found -> failwith @@ "interp_call: undefined function " ^ fn
    in
    if List.(length f_param <> length args) then
      failwith @@ "interp_call: wrong no. arguments for " ^ fn;
    let init_locs l = failwith @@ "interp_call: undefined local " ^ l in
    let locs = List.fold_left2 update init_locs f_param args in
    let stack = (next_id(), [])::c.stack in
    interp_cfg f_cfg locs {c with stack}
  in

  let mkarg a (p,h) = 
    let id = next_id () in
    VPtr (I8, HeapId id, [0])::p, (id, [MStr a])::h
  in
  let ptrs, heap = List.fold_right mkarg ("LLINTERP"::args) ([],[]) in

  let narg = List.length args + 1 in
  let argc = VInt (Int64.of_int @@ narg) in
  let aid = next_id () in
  let argv = VPtr (Array (narg, Ptr I8), HeapId aid, [0; 0]) in
  let amval = List.map (fun p -> MWord p) ptrs in
  let heap = (aid, [MNode amval])::heap in

  let _, r = interp_call I64 "main" [argc; argv] {globals; heap; stack=[]} in
  r


