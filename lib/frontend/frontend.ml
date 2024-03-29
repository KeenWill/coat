include Ast_lib.Ast
include Ll_lib.Ll
open Ll_lib
open Ll_lib.Llutil
open Ast_lib

(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for 
     compiling local variable declarations
*)

type elt =
  | L of Ll.lbl (* block labels *)
  | I of Ll.uid * Ll.insn (* instruction *)
  | T of Ll.terminator (* block terminators *)
  | G of Ll.gid * Ll.gdecl (* hoisted globals (usually strings) *)
  | E of Ll.uid * Ll.insn
(* hoisted entry block instructions *)

type stream = elt list

let ( >@ ) x y = y @ x

let ( >:: ) x y = y :: x

let lift : (Ll.uid * Ll.insn) list -> stream =
  List.rev_map (fun (x, i) -> I (x, i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code : stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list =
  let gs, einsns, insns, term_opt, blks =
    List.fold_left
      (fun ( (gs : (Ll.gid * Ll.gdecl) list),
             (einsns : (Ll.uid * Ll.insn) list),
             (insns : (Ll.uid * Ll.insn) list),
             (term_opt : (Ll.uid * Ll.terminator) option),
             (blks : (Ll.lbl * Ll.block) list) ) e ->
        match e with
        | L l -> (
            match term_opt with
            | None ->
                if List.length insns = 0 then (gs, einsns, [], None, blks)
                else
                  failwith
                  @@ Printf.sprintf
                       "build_cfg: block labeled %s hasno terminator" l
            | Some term -> (gs, einsns, [], None, (l, { insns; term }) :: blks))
        | T t -> (gs, einsns, [], Some (Llutil.Parsing.gensym "tmn", t), blks)
        | I (uid, insn) -> (gs, einsns, (uid, insn) :: insns, term_opt, blks)
        | G (gid, gdecl) -> ((gid, gdecl) :: gs, einsns, insns, term_opt, blks)
        | E (uid, i) -> (gs, (uid, i) :: einsns, insns, term_opt, blks))
      ([], [], [], None, []) code
  in
  match term_opt with
  | None -> failwith "build_cfg: entry block has no terminator"
  | Some term ->
      let insns = einsns @ insns in
      (({ insns; term }, blks), gs)

(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct
  type t = (Ast.id * (Ll.ty * Ll.operand)) list

  let empty = []

  (* Add a binding to the context *)
  let add (c : t) (id : Ast.id) (bnd : Ll.ty * Ll.operand) : t = (id, bnd) :: c

  (* Lookup a binding in the context *)
  let lookup (id : Ast.id) (c : t) : Ll.ty * Ll.operand = List.assoc id c
end

(* Mapping of identifiers representing struct definitions to 
 * the corresponding name-to-name-to-type map.
   Note:  You will need to use these operations when compiling structures.
 *)
module TypeCtxt = struct
  type t = (Ast.id * Ast.field list) list

  let empty = []

  let add c id bnd = (id, bnd) :: c

  let lookup id c = List.assoc id c

  let lookup_field st_name f_name (c : t) =
    let rec lookup_field_aux f_name (l : Ast.field list) =
      match l with
      | [] -> failwith "TypeCtxt.lookup_field: Not_found"
      | h :: t ->
          if h.fieldName = f_name then h.ftyp else lookup_field_aux f_name t
    in
    lookup_field_aux f_name (List.assoc st_name c)

  let rec index_of f (l : Ast.field list) i =
    match l with
    | [] -> None
    | h :: t -> if h.fieldName = f then Some i else index_of f t (i + 1)

  (* Return the index of a field in the struct. *)
  let index_of_field_opt (st : Ast.id) (f : Ast.id) (c : t) : int option =
    index_of f (List.assoc st c) 0

  let index_of_field (st : Ast.id) (f : Ast.id) (c : t) : int =
    match index_of_field_opt st f c with
    | None -> failwith "index_of_field: Not found"
    | Some x -> x

  (* Return a pair of base type and index into struct *)
  let rec lookup_field_name (st : Ast.id) (f : Ast.id) (c : t) :
      Ast.ty * Int64.t =
    let fields = lookup st c in
    match index_of f fields 0 with
    | None -> failwith "no such field"
    | Some x -> (List.(nth fields x).ftyp, Int64.of_int x)
end

let wrap_fun_name (fun_name : string) = fun_name ^ "_wrap"
let wrap_str_name (fun_name : string) = fun_name ^ "_struct_args"

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the the corresponding integer types. OAT strings are 
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.
   NOTE: structure types are named, so they compile to their named form
*)

let rec cmp_ty (ct: TypeCtxt.t) : Ast.ty -> Ll.ty = function
  | Ast.TRegTy regty -> cmp_regty ct regty
  | Ast.TLinTy lty -> cmp_lty ct lty

and cmp_regty (ct : TypeCtxt.t) : Ast.regty -> Ll.ty = function
  | Ast.TBool -> I1
  | Ast.TInt -> I64
  | Ast.TRef r -> Ptr (cmp_rty ct r)
  | Ast.TNullRef r -> Ptr (cmp_rty ct r)
  | Ast.TThreadGroup -> I64

and cmp_lty (ct: TypeCtxt.t) : Ast.lty -> Ll.ty = function
  | TChan (t, _, _) ->  Ptr I64
  | TMoved -> failwith "should never compile TMoved"

and cmp_ret_ty ct : Ast.ret_ty -> Ll.ty = function
  | Ast.RetVoid -> Void
  | Ast.RetVal t -> cmp_ty ct t

and cmp_fty ct (ts, r) : Ll.fty = (List.map (cmp_ty ct) ts, cmp_ret_ty ct r)

and cmp_rty ct : Ast.rty -> Ll.ty = function
  | Ast.RString -> I8
  | Ast.RArray u -> Struct [ I64; Array (0, cmp_ty ct u) ]
  | Ast.RStruct r -> Namedt r
  | Ast.RFun (ts, t) ->
      let args, ret = cmp_fty ct (ts, t) in
      Fun (args, ret)

let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TRegTy TInt, TRegTy TInt, TRegTy TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TRegTy TInt, TRegTy TInt, TRegTy TBool)
  | And | Or -> (TRegTy TBool, TRegTy TBool, TRegTy TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TRegTy TInt, TRegTy TInt)
  | Lognot -> (TRegTy TBool, TRegTy TBool)

(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s : string) ->
    incr c;
    Printf.sprintf "_%s%d" s !c

(* Amount of space an Oat type takes when stored in the stack, in bytes.  
   Note that since structured values are manipulated by reference, all
   Oat values take 8 bytes on the stack.
*)
let size_oat_ty (t : Ast.ty) = 8L

(* Amount of size that needs to be allocated to store a structure *)
let rec size_oat_struct (l : Ast.field list) =
  match l with
  | [] -> 0L
  | f :: t -> Int64.(add (size_oat_struct t) (size_oat_ty f.ftyp))

(* Generate code to allocate a zero-initialized array of source type TRef (RArray t) of the
   given size. Note "size" is an operand whose value can be computed at
   runtime *)
let oat_alloc_array ct (t : Ast.ty) (size : Ll.operand) :
    Ll.ty * Ll.operand * stream =
  let ans_id, arr_id = (gensym "array", gensym "raw_array") in
  let ans_ty = cmp_ty ct @@ TRegTy (TRef (RArray t)) in
  let arr_ty : Ll.ty = Ptr I64 in
  ( ans_ty,
    Id ans_id,
    lift
      [
        (arr_id, Call (arr_ty, Gid "oat_alloc_array", [ (I64, size) ]));
        (ans_id, Bitcast (arr_ty, Id arr_id, ans_ty));
      ] )

let oat_alloc_struct ct (id : Ast.id) : Ll.ty * Ll.operand * stream =
  let ret_id, arr_id = (gensym "struct", gensym "raw_struct") in
  let ans_ty = cmp_ty ct @@ TRegTy (TRef (RStruct id)) in
  let arr_ty : Ll.ty = Ptr I64 in
  ( ans_ty,
    Id ret_id,
    lift
      [
        ( arr_id,
          Call
            ( arr_ty,
              Gid "oat_malloc",
              [ (I64, Const (size_oat_struct (TypeCtxt.lookup id ct))) ] ) );
        (ret_id, Bitcast (arr_ty, Id arr_id, ans_ty));
      ] )

let str_arr_ty (s : string) : Ll.ty = Array (1 + String.length s, I8)

let i1_op_of_bool (b : bool) : Ll.operand = Ll.Const (if b then 1L else 0L)

let i64_op_of_int (i : int) : Ll.operand = Ll.Const (Int64.of_int i)

let payload_ty_of_chan (c : Ctxt.t) (tc : TypeCtxt.t) ({ elt; _ } : Ast.exp node) : Ll.ty =
  match elt with
  | Id x ->
      let pt, _ = Ctxt.lookup x c in
      begin match pt with Ptr t -> t | _ -> failwith "Unexpected variable type"
      end
  | _ -> failwith "payload_ty_of_chan: passed in exp is not a channel"

  
  


let cmp_binop t (b : Ast.binop) : Ll.operand -> Ll.operand -> Ll.insn =
  let ib b op1 op2 = Ll.Binop (b, t, op1, op2) in
  let ic c op1 op2 = Ll.Icmp (c, t, op1, op2) in
  match b with
  | Ast.Add -> ib Ll.Add
  | Ast.Mul -> ib Ll.Mul
  | Ast.Sub -> ib Ll.Sub
  | Ast.And -> ib Ll.And
  | Ast.IAnd -> ib Ll.And
  | Ast.IOr -> ib Ll.Or
  | Ast.Or -> ib Ll.Or
  | Ast.Shl -> ib Ll.Shl
  | Ast.Shr -> ib Ll.Lshr
  | Ast.Sar -> ib Ll.Ashr
  | Ast.Eq -> ic Ll.Eq
  | Ast.Neq -> ic Ll.Ne
  | Ast.Lt -> ic Ll.Slt
  | Ast.Lte -> ic Ll.Sle
  | Ast.Gt -> ic Ll.Sgt
  | Ast.Gte -> ic Ll.Sge


(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.
*)
let rec cmp_gexp c (tc : TypeCtxt.t) (e : Ast.exp Ast.node) :
    Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  match e.elt with
  | CNull r -> ((cmp_ty tc @@ TRegTy (TNullRef r), GNull), [])
  | CBool b -> ((I1, if b then GInt 1L else GInt 0L), [])
  | CInt i -> ((I64, GInt i), [])
  | Id id -> ((fst @@ Ctxt.lookup id c, GGid id), [])
  | CStr s ->
      let gid = gensym "str" in
      let ll_ty = str_arr_ty s in
      let cast = Ll.GBitcast (Ptr ll_ty, GGid gid, Ptr I8) in
      ((Ptr I8, cast), [ (gid, (ll_ty, GString s)) ])
  | CArr (u, cs) ->
      let elts, gs =
        List.fold_right
          (fun cst (elts, gs) ->
            let gd, gs' = cmp_gexp c tc cst in
            (gd :: elts, gs' @ gs))
          cs ([], [])
      in
      let len = List.length cs in
      let ll_u = cmp_ty tc u in
      let gid = gensym "global_arr" in
      let arr_t : Ll.ty = Struct [ I64; Array (len, ll_u) ] in
      let arr_i : Ll.ginit =
        GStruct
          [ (I64, GInt (Int64.of_int len)); (Array (len, ll_u), GArray elts) ]
      in
      let final_t : Ll.ty = Struct [ I64; Array (0, ll_u) ] in
      let cast : Ll.ginit = GBitcast (Ptr arr_t, GGid gid, Ptr final_t) in
      ((Ptr final_t, cast), (gid, (arr_t, arr_i)) :: gs)
  | CStruct (id, cs) ->
      let fields : Ast.field list = TypeCtxt.lookup id tc in
      let elts, gs =
        List.fold_right
          (fun (fs : Ast.field) (elts, gs) ->
            let gd, gs' =
              cmp_gexp c tc
                (snd (List.find (fun (xid, xname) -> xid = fs.fieldName) cs))
            in
            (gd :: elts, gs' @ gs))
          fields ([], [])
      in
      let gid = gensym "global_struct" in
      ((Ptr (Namedt id), GGid gid), (gid, (Namedt id, GStruct elts)) :: gs)
  | _ -> failwith "bad global initializer"


(* Compiles an expression exp in context c, outputting the Ll operand that will
   receive the value of the expression, and the stream of instructions
   implementing the expression. 
*)
let rec cmp_exp (tc : TypeCtxt.t) (c : Ctxt.t) (exp : Ast.exp Ast.node) :
    Ll.ty * Ll.operand * stream =
  match exp.elt with
  | Ast.CInt i -> (I64, Const i, [])
  | Ast.CNull r -> (cmp_ty tc @@ TRegTy (TNullRef r), Null, [])
  | Ast.CBool b -> (I1, i1_op_of_bool b, [])
  | Ast.CStr s ->
      let gid = gensym "str_arr" in
      let str_typ = str_arr_ty s in
      let uid = gensym "str" in
      ( Ptr I8,
        Id uid,
        []
        >:: G (gid, (str_typ, GString s))
        >:: I (uid, Gep (Ptr str_typ, Gid gid, [ Const 0L; Const 0L ])) )
  | Ast.Bop ((Ast.Eq as bop), e1, e2) | Ast.Bop ((Ast.Neq as bop), e1, e2) ->
      (* Polymorphic equality operations *)
      (* Allow any type for the first operand, and cast
         the second operand to the type of the first. *)
      let _, _, ret_ty = typ_of_binop bop in
      let ll_t, op1, code1 = cmp_exp tc c e1 in
      let op2, code2 = cmp_exp_as tc c e2 ll_t in
      let ans_id = gensym "bop" in
      ( cmp_ty tc ret_ty,
        Id ans_id,
        code1 >@ code2 >:: I (ans_id, cmp_binop ll_t bop op1 op2) )
  | Ast.Bop (bop, e1, e2) ->
      let t, _, ret_ty = typ_of_binop bop in
      let ll_t = cmp_ty tc t in
      let op1, code1 = cmp_exp_as tc c e1 ll_t in
      let op2, code2 = cmp_exp_as tc c e2 ll_t in
      let ans_id = gensym "bop" in
      ( cmp_ty tc ret_ty,
        Id ans_id,
        code1 >@ code2 >:: I (ans_id, cmp_binop ll_t bop op1 op2) )
  | Ast.Uop (uop, e) ->
      let t, ret_ty = typ_of_unop uop in
      let op, code = cmp_exp_as tc c e (cmp_ty tc t) in
      let ans_id = gensym "unop" in
      let cmp_uop op = function
        | Ast.Neg -> Ll.Binop (Sub, I64, i64_op_of_int 0, op)
        | Ast.Lognot -> Ll.Icmp (Eq, I1, op, i1_op_of_bool false)
        | Ast.Bitnot -> Ll.Binop (Xor, I64, op, i64_op_of_int (-1))
      in
      (cmp_ty tc ret_ty, Id ans_id, code >:: I (ans_id, cmp_uop op uop))
  | Ast.Id id -> (
      let t, op = Ctxt.lookup id c in
      match t with
      | Ptr (Fun _) -> (t, op, [])
      | Ptr t ->
          let ans_id = gensym id in
          (t, Id ans_id, [ I (ans_id, Load (Ptr t, op)) ])
      | _ -> failwith "broken invariant: identifier not a pointer")
  | Ast.Length e ->
      let arr_ty, arr_op, arr_code = cmp_exp tc c e in
      let _ =
        match arr_ty with
        | Ptr (Struct [ _; Array (_, t) ]) -> t
        | _ -> failwith "Length: indexed into non pointer"
      in
      let ptr_id, tmp_id = (gensym "index_ptr", gensym "tmp") in
      let ans_id = gensym "len" in
      ( I64,
        Id ans_id,
        arr_code
        >@ lift
             [
               ( ptr_id,
                 Gep (arr_ty, arr_op, [ i64_op_of_int 0; i64_op_of_int 0 ]) );
               (ans_id, Load (Ptr I64, Id ptr_id));
             ] )
  | Ast.Index (e, i) ->
      let ans_ty, ptr_op, code = cmp_exp_lhs tc c exp in
      let ans_id = gensym "index" in
      (ans_ty, Id ans_id, code >:: I (ans_id, Load (Ptr ans_ty, ptr_op)))
  | Ast.Call (f, es) -> cmp_call tc c f es
  | Ast.CArr (elt_ty, cs) ->
      let size_op = Ll.Const (Int64.of_int @@ List.length cs) in
      let arr_ty, arr_op, alloc_code = oat_alloc_array tc elt_ty size_op in
      let ll_elt_ty = cmp_ty tc elt_ty in
      let add_elt s (i, elt) =
        let elt_op, elt_code = cmp_exp_as tc c elt ll_elt_ty in
        let ind = gensym "ind" in
        s >@ elt_code
        >@ lift
             [
               ( ind,
                 Gep (arr_ty, arr_op, [ Const 0L; Const 1L; i64_op_of_int i ])
               );
               (gensym "store", Store (ll_elt_ty, elt_op, Id ind));
             ]
      in
      let ind_code =
        List.(fold_left add_elt [] @@ mapi (fun i e -> (i, e)) cs)
      in
      (arr_ty, arr_op, alloc_code >@ ind_code)
  | Ast.NewArr (elt_ty, e) ->
      let _, size_op, size_code = cmp_exp tc c e in
      let arr_ty, arr_op, alloc_code = oat_alloc_array tc elt_ty size_op in
      (arr_ty, arr_op, size_code >@ alloc_code)
  | Ast.NewArrInit (elt_ty, e1, id, e2) ->
      let ptr_id = gensym "ptr_" in
      let bound_id = gensym "bnd_" in
      let _, size_op, size_code = cmp_exp tc c e1 in
      let arr_ty, arr_op, alloc_code = oat_alloc_array tc elt_ty size_op in
      let for_loop =
        Ast.(
          no_loc
          @@ For
               ( [ (id, no_loc (CInt 0L)) ],
                 Some
                   (no_loc @@ Bop (Lt, no_loc @@ Id id, no_loc @@ Id bound_id)),
                 Some
                   (no_loc
                   @@ Assn
                        ( no_loc @@ Id id,
                          no_loc @@ Bop (Add, no_loc @@ Id id, no_loc @@ CInt 1L)
                        )),
                 [
                   no_loc
                   @@ Assn
                        ( no_loc @@ Index (no_loc @@ Id ptr_id, no_loc @@ Id id),
                          e2 );
                 ] ))
      in
      let new_context = Ctxt.add c ptr_id (Ptr arr_ty, Id ptr_id) in
      let new_context = Ctxt.add new_context bound_id (Ptr I64, Id bound_id) in
      let _, assign_code = cmp_stmt tc new_context arr_ty for_loop in
      ( arr_ty,
        arr_op,
        size_code >@ alloc_code
        >@ [ I (bound_id, Alloca I64) ]
        >@ [ I (gensym "store", Store (I64, size_op, Id bound_id)) ]
        >@ [ I (ptr_id, Alloca arr_ty) ]
        >@ [ I (gensym "store", Store (arr_ty, arr_op, Id ptr_id)) ]
        >@ assign_code )
  | Ast.CStruct (id, l) ->
      let struct_ty, struct_op, alloc_code = oat_alloc_struct tc id in
      let add_elt s (fid, fexp) =
        let field_type = cmp_ty tc @@ TypeCtxt.lookup_field id fid tc in
        let index = TypeCtxt.index_of_field id fid tc in
        let elt_op, elt_code = cmp_exp_as tc c fexp field_type in
        let ind = gensym "ind" in
        s >@ elt_code
        >@ lift
             [
               ( ind,
                 Gep (struct_ty, struct_op, [ Const 0L; i64_op_of_int index ])
               );
               (gensym "store", Store (field_type, elt_op, Id ind));
             ]
      in
      let ind_code = List.fold_left add_elt [] l in
      (struct_ty, struct_op, alloc_code >@ ind_code)
  | Ast.Proj (e, id) ->
      let ans_ty, ptr_op, code = cmp_exp_lhs tc c exp in
      let ans_id = gensym "proj" in
      (ans_ty, Id ans_id, code >:: I (ans_id, Load (Ptr ans_ty, ptr_op)))
  | Ast.CMakeChan _ ->
      let ret_id, chan_id = (gensym "chan", gensym "raw_chan") in
      let chan_ty : Ll.ty = Ptr I64 in
      chan_ty,
      Id chan_id,
      lift [
        (chan_id, Call (chan_ty, Gid "chan_create", []));
        (* (ret_id, Bitcast (chan_ty, Id chan_id, ans_ty)); *)
      ]
  | Ast.CSendChan (echan, epayload) -> 
      let _, chan_op, chan_code = cmp_exp tc c echan in
      let payload_ty, payload_op, payload_code = cmp_exp tc c epayload in
      let void_id = gensym "void" in
      let bitcast_id = gensym "bitcast" in
      I64,
      Id void_id,
      (chan_code >@ payload_code >:: 
        I (bitcast_id, Bitcast (payload_ty, payload_op, I64)) >::
        I (void_id, Call (I64, Gid "chan_send", [(Ptr I64, chan_op); (I64, Id bitcast_id)])))
  | Ast.CRecvChan (oat_payload_ty, echan) ->
      let chan_ty, chan_op, chan_code = cmp_exp tc c echan in
      let payload_id = gensym "payload" in
      let ll_payload_ty = cmp_ty tc oat_payload_ty in
      let ret_id = gensym "ret" in
      ll_payload_ty,
      Id ret_id,
      (chan_code >@ lift [
        (payload_id, Call (I64, Gid "chan_recv", [Ptr I64, chan_op]));
        (ret_id, Bitcast (I64, Id payload_id, ll_payload_ty))
      ])
  
  | Ast.CSpawn (efuncpointers, eargs_list) ->
      let len : int64 = Int64.of_int @@ List.length efuncpointers in 
      
      let rarr_ty, rarr_op, rarr_code = cmp_exp tc c @@ no_loc @@ 
        NewArr (TRegTy TInt, no_loc (CInt len)) in

      let counter = ref (-1) in

      (* For each function and associated argument list, spawn a thread. *)
      let spawn_code : stream = List.fold_left2 (fun acc { elt; _ } uids ->
        match elt with
        | Ast.Id fun_name ->
          counter := !counter + 1;

          (* Loop over all of the args to pass to this function and compile them. *)
          let arg_tys_and_ops_and_code : ((ty * operand * stream) list) = 
            List.map (fun uid ->
              let ty, op, code = cmp_exp tc c @@ no_loc (Ast.Id uid) in
              (ty, op, code)
            ) uids in
          let arg_code = 
            List.fold_left 
              (fun stracc (_, _, str) -> str >@ stracc) [] arg_tys_and_ops_and_code in


          let struct_name = wrap_str_name fun_name in
          let fields : Ast.field list = TypeCtxt.lookup struct_name tc in
          let struct_ty, struct_op, alloc_code = oat_alloc_struct tc struct_name in
          let add_field (s : stream) ({ fieldName; ftyp } : field) 
              ((arg_ty, arg_op, _) : ty * operand * stream) =
            let index = TypeCtxt.index_of_field struct_name fieldName tc in
            let arg_ind = gensym "ind" in
            s >@ lift
                [
                  (* Get a pointer into the struct argument that we are going to
                     pass to the wrapped function. Then store the argument
                     padded to add_field into the struct. *)
                  (arg_ind, Gep (struct_ty, struct_op, [ Const 0L; i64_op_of_int index ]));
                  (gensym "store", Store (arg_ty, arg_op, Id arg_ind));
                ]
          in
          let ind_code = List.fold_left2 add_field [] fields arg_tys_and_ops_and_code in
          
          let thrd_id = gensym "thrd_id" in
          let thrd_ind = gensym "ind" in
          let fun_ind = gensym "fun_ind" in 
          let str_ind = gensym "str_ind" in
          let fun_ty = Ctxt.lookup (wrap_fun_name fun_name) c in 
          let wrapped_fun_ty = Ptr (Fun ([ Ptr I64 ], I64)) in
          arg_code >@ alloc_code >@ ind_code >@
          lift
            [
              (* Bitcast the struct and function to match the parameter types of
                 thread_spawn. Then call the function and save the output to
                 thrd_id. *)
              (str_ind, Bitcast (struct_ty, struct_op, Ptr I64));
              (fun_ind, Bitcast (fst fun_ty, Gid (wrap_fun_name fun_name), wrapped_fun_ty));
              (thrd_id, Ll.Call (I64, Gid "thread_spawn", [ (wrapped_fun_ty, Id (fun_ind)); (Ptr I64, Id str_ind) ]) );
              
              (* Get a pointer into the thread group array, then store the
                 thread id of the thread just created into it.  *)
              (thrd_ind, Gep (rarr_ty, rarr_op, [ Const 0L; Const 1L; i64_op_of_int !counter ]) );
              (gensym "store", Store (I64, Id thrd_id, Id thrd_ind));

            ] @ acc
        | _ -> failwith "CSpawn not a function passed in"
      ) [] efuncpointers eargs_list in

      rarr_ty, rarr_op, rarr_code >@ spawn_code

  | Ast.CJoin (e_arr_tid) ->

      let tid_ty, tid_op, tid_code = cmp_exp tc c e_arr_tid in
      match tid_ty with
      | Ptr (Struct [ I64; Array (_, I64) ])  -> 
          let res = gensym "join_res" in 
          let cast_op = gensym "bitcast" in

          I64,
          Id res,
          tid_code >@ lift [
            (cast_op, Bitcast (tid_ty, tid_op, Ptr I64));
            (res, Ll.Call (I64, Gid "join_all_threads", [ Ptr I64, Id cast_op ]) ); 
          ]
      | _ -> failwith "Please pass array to CJoin :(" 


      
      

and cmp_exp_lhs (tc : TypeCtxt.t) (c : Ctxt.t) (e : Ast.exp Ast.node) :
    Ll.ty * Ll.operand * stream =
  match e.elt with
  | Ast.Id x ->
      let pt, op = Ctxt.lookup x c in
      let t =
        match pt with Ptr t -> t | _ -> failwith "Unexpected variable type"
      in
      (t, op, [])
  | Ast.Proj (e, i) ->
      let src_ty, src_op, src_code = cmp_exp tc c e in
      let struct_id =
        match src_ty with
        | Ptr (Namedt id) -> id
        | _ -> failwith "Project on non-struct type"
      in
      let ret_ty, ret_index = TypeCtxt.lookup_field_name struct_id i tc in
      let gep_id = gensym "index" in
      let ret_op = Ll.Gep (src_ty, src_op, [ Const 0L; Const ret_index ]) in
      (cmp_ty tc ret_ty, Id gep_id, src_code >:: I (gep_id, ret_op))
  | Ast.Index (e, i) ->
      let arr_ty, arr_op, arr_code = cmp_exp tc c e in
      let _, ind_op, ind_code = cmp_exp tc c i in
      let ans_ty =
        match arr_ty with
        | Ptr (Struct [ _; Array (_, t) ]) -> t
        | _ -> failwith "Index: indexed into non pointer"
      in
      let ptr_id, tmp_id, call_id =
        (gensym "index_ptr", gensym "tmp", gensym "call")
      in
      ( ans_ty,
        Id ptr_id,
        arr_code >@ ind_code
        >@ lift
             [
               (tmp_id, Bitcast (arr_ty, arr_op, Ptr I64));
               ( call_id,
                 Call
                   ( Void,
                     Gid "oat_assert_array_length",
                     [ (Ptr I64, Id tmp_id); (I64, ind_op) ] ) );
               ( ptr_id,
                 Gep
                   (arr_ty, arr_op, [ i64_op_of_int 0; i64_op_of_int 1; ind_op ])
               );
             ] )
  | _ -> failwith "invalid lhs expression"

and cmp_call (tc : TypeCtxt.t) (c : Ctxt.t) (exp : Ast.exp Ast.node)
    (es : Ast.exp Ast.node list) : Ll.ty * Ll.operand * stream =
  let t, op, s = cmp_exp tc c exp in
  let ts, rt =
    match t with
    | Ptr (Fun (l, r)) -> (l, r)
    | _ -> failwith "nonfunction passed to cmp_call"
  in
  let args, args_code =
    List.fold_right2
      (fun e t (args, code) ->
        let arg_op, arg_code = cmp_exp_as tc c e t in
        ((t, arg_op) :: args, arg_code @ code))
      es ts ([], [])
  in
  let res_id = gensym "result" in
  (rt, Id res_id, s >@ args_code >:: I (res_id, Call (rt, op, args)))

and cmp_exp_as (tc : TypeCtxt.t) (c : Ctxt.t) (e : Ast.exp Ast.node) (t : Ll.ty)
    : Ll.operand * stream =
  let from_t, op, code = cmp_exp tc c e in
  if from_t = t then (op, code)
  else
    let res_id = gensym "cast" in
    (Id res_id, code >:: I (res_id, Bitcast (from_t, op, t)))

(* Compile a statement in context c with return typ rt. Return a new context, 
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.
   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.
 *)
and cmp_stmt (tc : TypeCtxt.t) (c : Ctxt.t) (rt : Ll.ty)
    (stmt : Ast.stmt Ast.node) : Ctxt.t * stream =
  match stmt.elt with
  | Ast.Decl (id, init) ->
      let ll_ty, init_op, init_code = cmp_exp tc c init in
      let res_id = gensym id in
      let c' = Ctxt.add c id (Ptr ll_ty, Id res_id) in
      ( c',
        init_code
        >:: E (res_id, Alloca ll_ty)
        >:: I (gensym "store", Store (ll_ty, init_op, Id res_id)) )
  | Ast.Assn (path, e) ->
      let ll_ty, pop, path_code = cmp_exp_lhs tc c path in
      let eop, exp_code = cmp_exp_as tc c e ll_ty in
      (c, path_code >@ exp_code >:: I (gensym "store", Store (ll_ty, eop, pop)))
  | Ast.If (guard, st1, st2) ->
      let guard_ty, guard_op, guard_code = cmp_exp tc c guard in
      let _, then_code = cmp_block tc c rt st1 in
      let _, else_code = cmp_block tc c rt st2 in
      let lt, le, lm = (gensym "then", gensym "else", gensym "merge") in
      ( c,
        guard_code
        >:: T (Cbr (guard_op, lt, le))
        >:: L lt >@ then_code >:: T (Br lm) >:: L le >@ else_code >:: T (Br lm)
        >:: L lm )
  | Ast.Cast (typ, id, exp, notnull, null) ->
      let translated_typ = cmp_ty tc @@ TRegTy (TRef typ) in
      let guard_op, guard_code = cmp_exp_as tc c exp translated_typ in
      let res_id = gensym id in
      let c' = Ctxt.add c id (Ptr translated_typ, Id res_id) in
      let _, null_code = cmp_block tc c rt null in
      let _, notnull_code = cmp_block tc c' rt notnull in
      let cast_id = gensym "cast" in
      let ln, lnn, lm = (gensym "null", gensym "notnull", gensym "merge") in
      ( c,
        guard_code
        >:: I (cast_id, Icmp (Eq, translated_typ, guard_op, Null))
        >:: T (Cbr (Id cast_id, ln, lnn))
        >:: L lnn
        >:: E (res_id, Alloca translated_typ)
        >:: I (gensym "store", Store (translated_typ, guard_op, Id res_id))
        >@ notnull_code >:: T (Br lm) >:: L ln >@ null_code >:: T (Br lm)
        >:: L lm )
  | Ast.While (guard, body) ->
      let guard_ty, guard_op, guard_code = cmp_exp tc c guard in
      let lcond, lbody, lpost = (gensym "cond", gensym "body", gensym "post") in
      let _, body_code = cmp_block tc c rt body in
      ( c,
        [] >:: T (Br lcond) >:: L lcond >@ guard_code
        >:: T (Cbr (guard_op, lbody, lpost))
        >:: L lbody >@ body_code >:: T (Br lcond) >:: L lpost )
  | Ast.For (inits, guard, after, body) ->
      let ds = List.map (fun d -> Ast.no_loc (Ast.Decl d)) inits in
      let ci, init = cmp_block tc c rt ds in
      let guard =
        match guard with Some e -> e | None -> Ast.no_loc (Ast.CBool true)
      in
      let guard_ty, guard_op, guard_code = cmp_exp tc ci guard in
      let after = match after with Some s -> [ s ] | None -> [] in
      let lcond, lbody, lpost = (gensym "cond", gensym "body", gensym "post") in
      let _, body_code = cmp_block tc ci rt body in
      let _, after_code = cmp_block tc ci rt after in
      ( c,
        init >:: T (Br lcond) >:: L lcond >@ guard_code
        >:: T (Cbr (guard_op, lbody, lpost))
        >:: L lbody >@ body_code >@ after_code >:: T (Br lcond) >:: L lpost )
  | Ast.Ret None -> (c, [ T (Ret (Void, None)) ])
  | Ast.Ret (Some e) ->
      let op, code = cmp_exp_as tc c e rt in
      (c, code >:: T (Ret (rt, Some op)))
  | Ast.SCall (f, es) ->
      let _, op, code = cmp_call tc c f es in
      (c, code)

(* Compile a series of statements *)
and cmp_block (tc : TypeCtxt.t) (c : Ctxt.t) (rt : Ll.ty) (stmts : Ast.block) :
    Ctxt.t * stream =
  List.fold_left
    (fun (c, code) s ->
      let c, stmt_code = cmp_stmt tc c rt s in
      (c, code >@ stmt_code))
    (c, []) stmts

(* Construct the structure context for compilation.  We could reuse
   the H component from the Typechecker rather than recomputing this
   information here, but we do it this way to make the two parts of
   the project less interdependent.  *)
let get_struct_defns (p : Ast.prog) : TypeCtxt.t =
  List.fold_right
    (fun d ts ->
      match d with
      | Ast.Gtdecl { elt = id, fs } -> TypeCtxt.add ts id fs
      | _ -> ts)
    p TypeCtxt.empty

(* Adds each function identifier to the context at an
   appropriately translated type.  
   NOTE: The Gid of a function is just its source name
*)
let cmp_function_ctxt (tc : TypeCtxt.t) (c : Ctxt.t) (p : Ast.prog) : Ctxt.t =
  List.fold_left
    (fun c -> function
      | Ast.Gfdecl { elt = { frtyp; fname; args } } ->
          let struct_name = wrap_str_name fname in
          let ft = Ast.TRef (RFun (List.map fst args, frtyp)) in
          let ft_wrap = Ast.TRef (RFun ([TRegTy (TRef (RStruct struct_name))], frtyp))  in
          let wrap_name = wrap_fun_name fname in
          let c = Ctxt.add c fname (cmp_ty tc (TRegTy ft), Gid fname) in
          Ctxt.add c wrap_name (cmp_ty tc (TRegTy ft_wrap), Gid wrap_name)
      | _ -> c)
    c p

(* Populate a context with bindings for global variables 
   mapping OAT identifiers to LLVMlite gids and their types.
   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs. (The constructors starting with C and Id's 
   for global function values). 
*)
let cmp_global_ctxt (tc : TypeCtxt.t) (c : Ctxt.t) (p : Ast.prog) : Ctxt.t =
  let gexp_ty (c : Ctxt.t) : Ast.exp -> Ll.ty = function
    | Id id -> fst (Ctxt.lookup id c)
    | CStruct (t, cs) -> Ptr (Namedt t)
    | CNull r -> cmp_ty tc @@ TRegTy (TNullRef r)
    | CBool b -> I1
    | CInt i -> I64
    | CStr s -> Ptr I8
    | CArr (u, cs) -> Ptr (Struct [ I64; Array (0, cmp_ty tc u) ])
    | x ->
        failwith
          ("bad global initializer: " ^ Astlib.string_of_exp (Ast.no_loc x))
  in
  List.fold_left
    (fun c -> function
      | Ast.Gvdecl { elt = { name; init } } ->
          Ctxt.add c name (Ptr (gexp_ty c init.elt), Gid name)
      | _ -> c)
    c p

(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.
 *)
let cmp_fdecl (tc : TypeCtxt.t) (c : Ctxt.t) (f : Ast.fdecl Ast.node) :
    Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let ({ frtyp; args; body } : Ast.fdecl) = f.elt in
  let add_arg (s_typ, s_id) (c, code, args) =
    let ll_id = gensym s_id in
    let ll_ty = cmp_ty tc s_typ in
    let alloca_id = gensym s_id in
    let c = Ctxt.add c s_id (Ptr ll_ty, Ll.Id alloca_id) in
    ( c,
      []
      >:: E (alloca_id, Alloca ll_ty)
      >:: I (gensym "store", Store (ll_ty, Id ll_id, Id alloca_id))
      >@ code,
      (ll_ty, ll_id) :: args )
  in
  let c, args_code, args = List.fold_right add_arg args (c, [], []) in
  let ll_rty = cmp_ret_ty tc frtyp in
  let _, block_code = cmp_block tc c ll_rty body in
  let argtys, f_param = List.split args in
  let f_ty = (argtys, ll_rty) in
  let return_code =
    let return_val =
      match frtyp with
      | RetVoid -> None
      | RetVal TRegTy TBool | RetVal TRegTy TInt -> Some (Ll.Const 0L)
      | RetVal (TRegTy TRef _ | TRegTy TNullRef _) -> Some Null
      | RetVal TLinTy _ -> Some (Ll.Const 0L)
      | RetVal (TRegTy TThreadGroup) -> Some (Ll.Const 0L)
    in
    [ T (Ret (ll_rty, return_val)) ]
  in
  let f_cfg, globals = cfg_of_stream (args_code >@ block_code >@ return_code) in
  ({ f_ty; f_param; f_cfg }, globals)

let wrap_fdecl (f : Ast.fdecl Ast.node) (struct_name: string) (struct_fields: Ast.field list) : Ast.fdecl Ast.node =
  Ast.(
    let eargs = List.map (fun ({ fieldName; ftyp } : field) -> 
      no_loc @@ Proj (no_loc (Id "wrap_args"), fieldName)
    ) struct_fields in 
    let fdecl : fdecl = 
      {
          frtyp = (f.elt.frtyp)
        ; fname = (wrap_fun_name f.elt.fname)
        ; args  = [ TRegTy (TRef (RStruct struct_name)), "wrap_args" ]
        ; body  = [ no_loc @@ SCall (no_loc (Id f.elt.fname), eargs) ]
      } in
    no_loc fdecl
  )
  
(* Oat internals function context ------------------------------------------- *)
let internals =
  [
    ("oat_malloc", Ll.Fun ([ I64 ], Ptr I64));
    ("oat_alloc_array", Ll.Fun ([ I64 ], Ptr I64));
    ("oat_assert_not_null", Ll.Fun ([ Ptr I8 ], Void));
    ("oat_assert_array_length", Ll.Fun ([ Ptr I64; I64 ], Void));
    
    (* TODO: Double check these types *)
    ("chan_create", Ll.Fun ([], Ptr I64));
    ("chan_send", Ll.Fun ([ Ptr I64; I64 ], I64));
    ("chan_recv", Ll.Fun ([ Ptr I64 ], I64));
    ("thread_spawn", Ll.Fun ([ Ptr (Ll.Fun ([ Ptr I64 ], I64)); Ptr I64 ], I64));
    ("thread_join", Ll.Fun ([ I64 ], I64));
    ("join_all_threads", Ll.Fun ([ Ptr I64 ], I64));
  ]

(* Oat builtin function context --------------------------------------------- *)
let builtins =
  List.map
    (fun (fname, ftyp) ->
      let args, ret = cmp_fty TypeCtxt.empty ftyp in
      (fname, Ll.Fun (args, ret)))
    Typechecker.builtins

let tctxt_to_tdecls (c : TypeCtxt.t) =
  List.map
    (fun (i, l) ->
      (i, Ll.Struct (List.map (fun (f : Ast.field) -> cmp_ty c f.ftyp) l)))
    c

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p : Ast.prog) : Ll.prog =
  let tc = get_struct_defns p in
  (* add built-in functions to context *)
  let init_ctxt =
    List.fold_left
      (fun c (i, t) -> Ctxt.add c i (Ll.Ptr t, Gid i))
      Ctxt.empty builtins
  in

  let tc = List.fold_left (fun tc d  ->
    match d with
    | Ast.Gfdecl fd ->
      let struct_name = wrap_str_name fd.elt.fname in
      let struct_ty : Ast.field list = 
        List.map (fun (ty, name) -> 
          {fieldName = name; ftyp = ty}
        ) fd.elt.args in
      TypeCtxt.add tc struct_name struct_ty
    | _ -> tc
  ) tc p in

  let fc = cmp_function_ctxt tc init_ctxt p in

  (* build global variable context *)
  let c = cmp_global_ctxt tc fc p in
  (* compile functions and global variables *)


  let fdecls, gdecls =
    List.fold_right
      (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt = gd } ->
            let ll_gd, gs' = cmp_gexp c tc gd.init in
            (fs, (gd.name, ll_gd) :: gs' @ gs)
        | Ast.Gfdecl fd ->
            let struct_name = wrap_str_name fd.elt.fname in
            let struct_ty : Ast.field list = 
              List.map (fun (ty, name) -> 
                {fieldName = name; ftyp = ty}
              ) fd.elt.args in
            let fdecl, gs' = cmp_fdecl tc c fd in
            let fdecl_wrap, gs_wrap = cmp_fdecl tc c @@ wrap_fdecl fd struct_name struct_ty in
            ((wrap_fun_name fd.elt.fname, fdecl_wrap) :: (fd.elt.fname, fdecl) :: fs, gs_wrap @ gs' @ gs)
        | Ast.Gtdecl _ -> (fs, gs))
      p ([], [])
  in
  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls = tctxt_to_tdecls tc; gdecls; fdecls; edecls }
