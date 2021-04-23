open Ast_lib
open Ast
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) err =
  let _, (s, e), _ = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [
    ( "array_of_string",
      ([ TRegTy (TRef RString) ], RetVal (TRegTy (TRef (RArray (TRegTy TInt)))))
    );
    ( "string_of_array",
      ([ TRegTy (TRef (RArray (TRegTy TInt))) ], RetVal (TRegTy (TRef RString)))
    );
    ("length_of_string", ([ TRegTy (TRef RString) ], RetVal (TRegTy TInt)));
    ("string_of_int", ([ TRegTy TInt ], RetVal (TRegTy (TRef RString))));
    ( "string_cat",
      ( [ TRegTy (TRef RString); TRegTy (TRef RString) ],
        RetVal (TRegTy (TRef RString)) ) );
    ("print_string", ([ TRegTy (TRef RString) ], RetVoid));
    ("print_int", ([ TRegTy TInt ], RetVoid));
    ("print_bool", ([ TRegTy TBool ], RetVoid));
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : binop -> ty * ty * ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr ->
      (TRegTy TInt, TRegTy TInt, TRegTy TInt)
  | Lt | Lte | Gt | Gte -> (TRegTy TInt, TRegTy TInt, TRegTy TBool)
  | And | Or -> (TRegTy TBool, TRegTy TBool, TRegTy TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : unop -> ty * ty = function
  | Neg | Bitnot -> (TRegTy TInt, TRegTy TInt)
  | Lognot -> (TRegTy TBool, TRegTy TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec subtype (c : Tctxt.t) (t1 : ty) (t2 : ty) : bool =
  match (t1, t2) with
  | TRegTy reg1, TRegTy reg2 -> subtype_regty c reg1 reg2
  | TLinTy lin1, TLinTy lin2 -> subtype_linty c lin1 lin2
  | _, _ -> false

and subtype_linty (c : Tctxt.t) (t1 : lty) (t2 : lty) : bool =
  match (t1, t2) with
  | TChan (ty1, r1, s1), TChan (ty2, r2, s2) ->
      subtype c ty1 ty2 && subtype_mults r1 r2 && subtype_mults s1 s2
  | _, _ -> false

and subtype_mults (m1 : mult) (m2 : mult) : bool =
  match (m1, m2) with
  | MArb, _ -> true
  | MNum n1, MNum n2 -> n1 = n2
  | _ -> false

and subtype_regty (c : Tctxt.t) (t1 : regty) (t2 : regty) : bool =
  match (t1, t2) with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TNullRef x, TNullRef y | TRef x, TNullRef y | TRef x, TRef y ->
      subtype_ref c x y
  | _, _ -> false

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : rty) (t2 : rty) : bool =
  match (t1, t2) with
  | RString, RString -> true
  | RArray at1, RArray at2 -> at1 = at2
  | RFun (ts1, rt1), RFun (ts2, rt2) ->
      subtype_list c ts2 ts1 && subtype_ret c rt1 rt2
  | RStruct id1, RStruct id2 -> id1 = id2 || subtype_fields c id1 id2
  | _, _ -> false

and subtype_ret (c : Tctxt.t) (t1 : ret_ty) (t2 : ret_ty) : bool =
  match (t1, t2) with
  | RetVoid, RetVoid -> true
  | RetVal v1, RetVal v2 -> subtype c v1 v2
  | _, _ -> false

and subtype_list c l1 l2 : bool =
  if List.length l1 != List.length l2 then false
  else List.fold_left2 (fun a x y -> a && subtype c x y) true l1 l2

(* fields n1 are a subtype of n2 if n2 is a prefix of n1 *)
and subtype_fields c n1 n2 : bool =
  let fields1 = Tctxt.lookup_struct n1 c in
  let fields2 = Tctxt.lookup_struct n2 c in
  let rec helper l1 l2 =
    match (l1, l2) with
    | _, [] -> true
    | [], _ -> false
    | f1 :: t1, f2 :: t2 ->
        f1.fieldName = f2.fieldName && f1.ftyp = f2.ftyp && helper t1 t2
  in
  helper fields1 fields2

(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules

    - the function should fail using the "type_error" helper function if the 
      type is not well formed

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

    - tc contains the structure definition context
 *)

let is_lin_ty (t : ty) = match t with TLinTy _ -> true | _ -> false

let rec typecheck_ty (l : 'a node) (tc : Tctxt.t) (t : ty) : unit =
  match t with
  | TLinTy TMoved -> type_error l "Moved type cannot be named"
  | TRegTy regty -> typecheck_regty l tc regty
  | TLinTy (TChan (ty, rm, sm)) ->
      typecheck_mult l rm;
      typecheck_mult l sm;
      typecheck_ty l tc ty

and typecheck_mult (l : 'a node) (m : mult) : unit =
  match m with
  | MNum i ->
      if i = 0 || i == 1 then ()
      else type_error l "Multiplicities can either be 0, 1, or *"
  | MArb -> ()

and typecheck_regty l tc (regty : regty) : unit =
  match regty with
  | TBool -> ()
  | TInt -> ()
  | TThreadGroup -> ()
  | TNullRef r | TRef r -> typecheck_ref l tc r

and typecheck_ref l tc (r : rty) : unit =
  match r with
  | RString -> ()
  | RStruct id -> (
      match Tctxt.lookup_struct_option id tc with
      | None -> type_error l "Unbound struct type"
      | Some _ -> () )
  | RArray t ->
      if is_lin_ty t then type_error l "Arrays cannot contain linear types"
      else typecheck_ty l tc t
  | RFun (tl, rt) ->
      typecheck_ret l tc rt;
      List.iter (typecheck_ty l tc) tl

and typecheck_ret l tc (rt : ret_ty) : unit =
  match (rt : ret_ty) with RetVoid -> () | RetVal t -> typecheck_ty l tc t

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oat.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)

(* Typechecks an expression and returns the type, the resulting context
 * NOTE: The resulting context is the same except for lin_locals, which
 * which will *always* be smaller, since expressions cannot introduce
 * idents to the context
 *)

module LinTyMap = Map.Make (String)

let comb_mult (m1 : mult) (m2 : mult) : mult =
  match (m1, m2) with MNum 0, m -> m | m, MNum 0 -> m | _, _ -> MArb

let rec coerce_chan_types (l : lty list) : (mult * mult) option =
  match l with
  | [ TChan (_, rm, wm) ] -> Some (rm, wm)
  | TChan (_, rm, wm) :: tl -> (
      match coerce_chan_types tl with
      | Some (crm, cwm) -> Some (comb_mult crm rm, comb_mult cwm wm)
      | _ -> None )
  | _ -> None

let rec typecheck_exp (c : Tctxt.t) (e : exp node) : ty * Tctxt.t =
  match e.elt with
  | CNull r -> (TRegTy (TNullRef r), c)
  | CBool _ -> (TRegTy TBool, c)
  | CInt _ -> (TRegTy TInt, c)
  | CStr _ -> (TRegTy (TRef RString), c)
  | Id i -> (
      match Tctxt.lookup_option i c with
      | Some x -> (TRegTy x, c)
      | None -> (
          match Tctxt.lookup_local_lin_option i c with
          | Some TMoved -> type_error e "Usage of moved value"
          | Some l ->
              let removed_ctx = List.remove_assoc i c.lin_locals in
              (TLinTy l, { c with lin_locals = (i, TMoved) :: removed_ctx })
          | None -> type_error e ("Unbound identifier " ^ i) ) )
  | CArr (t, l) ->
      typecheck_ty e c (TRegTy (TRef (RArray t)));
      let types_of, ctx = typecheck_exp_list c l in
      if List.for_all (fun u -> subtype c u t) types_of then
        (TRegTy (TRef (RArray t)), ctx)
      else type_error e "Mismatched array type"
  | NewArr (t, e1) ->
      ( match t with
      | TRegTy TBool | TRegTy TInt | TRegTy (TNullRef _) | TRegTy TThreadGroup
        ->
          ()
      | TLinTy _ -> type_error e "Arrays cannot contain linear types"
      | TRegTy (TRef _) ->
          type_error e
            "Non-null types cannot be used with default-initialized arrays" );
      let size_type, ctx = typecheck_exp c e1 in
      if size_type = TRegTy TInt then (TRegTy (TRef (RArray t)), ctx)
      else type_error e "Array size not an int"
  | NewArrInit (t, e1, id, e2) ->
      typecheck_ty e c (TRegTy (TRef (RArray t)));
      let size_type, ctx1 = typecheck_exp c e1 in
      if size_type = TRegTy TInt then
        let ctx2 =
          if
            List.exists (fun x -> fst x = id) c.reg_locals
            || List.exists (fun x -> fst x = id) c.lin_locals
          then type_error e1 "Cannot redeclare variable"
          else Tctxt.add_local_reg ctx1 id TInt
        in
        let t', ctx3 = typecheck_exp ctx2 e2 in
        if subtype c t' t then (TRegTy (TRef (RArray t)), ctx3)
        else type_error e2 "Initializer has incorrect type"
      else type_error e1 "Array size not an int"
  | Bop (b, l, r) -> (
      let ltyp, new_ctxt1 = typecheck_exp c l in
      let rtyp, new_ctxt2 = typecheck_exp new_ctxt1 r in
      match b with
      | Eq | Neq ->
          if subtype c ltyp rtyp && subtype c rtyp ltyp then
            (TRegTy TBool, new_ctxt2)
          else type_error e "== or != used with non type-compatible arguments"
      | _ ->
          let bl, br, bres = typ_of_binop b in
          if bl = ltyp then
            if br = rtyp then (bres, new_ctxt2)
            else type_error r "Incorrect type in binary expression"
          else type_error l "Incorrect type in binary expression" )
  | Uop (u, e) ->
      let t, new_ctxt = typecheck_exp c e in
      let us, ures = typ_of_unop u in
      if us = t then (ures, new_ctxt)
      else type_error e "Incorrect type for unary operator"
  | Index (e1, e2) ->
      let arr_t, new_ctxt1 = typecheck_exp c e1 in
      let ind_t, new_ctxt2 = typecheck_exp new_ctxt1 e2 in
      if ind_t = TRegTy TInt then
        match arr_t with
        | TRegTy (TRef (RArray t)) -> (t, new_ctxt2)
        | _ ->
            type_error e1
              ("Tried to compute index into type " ^ Astlib.string_of_ty arr_t)
      else type_error e2 "Index of array index operator not an int"
  | Proj (s, id) -> (
      let str_t, new_ctxt = typecheck_exp c s in
      match str_t with
      | TRegTy (TRef (RStruct sn)) -> (
          match Tctxt.lookup_field_option sn id c with
          | None -> type_error e (id ^ " not member of struct " ^ sn)
          | Some t -> (t, new_ctxt) )
      | _ -> type_error s "Cannot project from non-struct" )
  | CStruct (id, l) -> (
      match Tctxt.lookup_struct_option id c with
      | None -> type_error e (id ^ "not a struct type")
      | Some x ->
          let ids, exps = List.split l in
          let tys, ctx = typecheck_exp_list c exps in
          let field_types = List.combine ids tys in
          let struct_names =
            List.sort compare (List.map (fun x -> x.fieldName) x)
          in
          let local_names = List.sort compare (List.map fst field_types) in
          if struct_names <> local_names then
            type_error e
              "Mismatch of fields between struct definition and local \
               declaration";
          List.iter
            (fun (id, ft) ->
              let t = (List.find (fun i -> i.fieldName = id) x).ftyp in
              if not (subtype c ft t) then
                type_error e (id ^ " field of struct incorrect")
              else ())
            field_types;
          (TRegTy (TRef (RStruct id)), ctx) )
  | Length l -> (
      let t, new_ctxt = typecheck_exp c l in
      match t with
      | TRegTy (TRef (RArray _)) -> (TRegTy TInt, new_ctxt)
      | _ -> type_error l "Cannot take length of non-array" )
  | Call (f, args) -> (
      let argtyps, ctx1 = typecheck_exp_list c args in
      match typecheck_exp ctx1 f with
      | TRegTy (TRef (RFun (l, RetVal r))), ctx2 ->
          if List.length l <> List.length argtyps then
            type_error e "Incorrect number of arguments"
          else
            List.iter2
              (fun arg l ->
                if not (subtype c arg l) then
                  type_error e "Incorrect type of argument")
              argtyps l;
          (r, ctx2)
      | _ -> type_error e "Need function argument for function call" )
  | CMakeChan (ty, rm, wm) ->
      typecheck_ty e c (TLinTy (TChan (ty, rm, wm)));
      (TLinTy (TChan (ty, rm, wm)), c)
  | CSendChan (exp1, exp2) -> (
      let chan_ty, ctx1 = typecheck_exp c exp1 in
      let send_ty, ctx2 = typecheck_exp ctx1 exp2 in
      match chan_ty with
      | TLinTy (TChan (ty, rm, wm)) ->
          if wm = MNum 0 then
            type_error e "Cannot send on channel type without sends"
          else if rm = MNum 1 then
            type_error e "Cannot send on channel that has reads remaining"
          else if subtype c send_ty ty then (TRegTy TInt, ctx2)
          else type_error e "Cannot send incompatible type"
      | _ -> type_error e "Cannot send on non-channel type" )
  | CRecvChan (annot_ty, exp) -> (
      let t, ctx = typecheck_exp c exp in
      match t with
      | TLinTy (TChan (ty, rm, wm)) ->
          if annot_ty <> ty then type_error e "Type annotations incorrect"
          else if rm = MNum 0 then
            type_error e "Cannot receive on channel type without reads"
          else if wm = MNum 1 then
            type_error e "Cannot receive on channel that has writes remaining"
          else (ty, ctx)
      | _ -> type_error e "Cannot receive on non-channel type" )
  | CSpawn (exp1, args) ->
      (* Step 1: Check whether argument length matches, and arguments match types. Use the
       * same context for each fptr.
       * Step 2: Collect all identifiers for each process that are linear. Check that
       * combined use of identifiers fit types *)
      let rec typecheck_processes (ctx : Tctxt.t)
          (lin_map : lty list LinTyMap.t) (fptrs : exp node list)
          (args_list : id list list) : lty list LinTyMap.t =
        match (fptrs, args_list) with
        | fptr :: fptr_tl, args :: args_tl -> (
            let fptr_ty, ctx1 = typecheck_exp ctx fptr in
            let args_tys, _ =
              typecheck_exp_list ctx1
                (List.map (fun id -> { elt = Id id; loc = e.loc }) args)
            in
            match fptr_ty with
            | TRegTy (TRef (RFun (l, RetVoid))) ->
                let rec verify_fargs (args : ty list) (args_id : id list)
                    (l : ty list) (lin_map : lty list LinTyMap.t) =
                  match (args, args_id, l) with
                  | arg_ty :: args_tl, arg_id :: args_id_tl, l_ty :: l_tl ->
                      ( match (arg_ty, l_ty) with
                      | TLinTy (TChan (ty1, _, _)), TLinTy (TChan (ty2, _, _))
                        ->
                          if not (subtype c ty1 ty2) then
                            type_error fptr "spawn: argument type mismatch"
                      | _, _ ->
                          if not (subtype c arg_ty l_ty) then
                            type_error fptr "spawn: argument type mismatch" );
                      let new_map =
                        match l_ty with
                        | TLinTy lty ->
                            LinTyMap.update arg_id
                              (fun usage ->
                                match usage with
                                | None -> Some [ lty ]
                                | Some usage_l -> Some (lty :: usage_l))
                              lin_map
                        | _ -> lin_map
                      in
                      verify_fargs args_tl args_id_tl l_tl new_map
                  | [], [], [] -> lin_map
                  | _, _, _ -> type_error fptr "spawn: argument length mismatch"
                in
                let new_map = verify_fargs args_tys args l lin_map in
                typecheck_processes ctx new_map fptr_tl args_tl
            | _ ->
                type_error fptr
                  "spawn: cannot spawn process with non void function pointer" )
        | [], [] -> lin_map
        | _, _ ->
            type_error e "spawn: length of fptr list and args list do not match"
      in
      let lin_map = typecheck_processes c LinTyMap.empty exp1 args in
      LinTyMap.iter
        (fun id l ->
          match coerce_chan_types l with
          | Some (rm, wm) -> (
              match List.assoc id c.lin_locals with
              | TMoved -> type_error e "spawn: usage of moved linear value"
              | TChan (_, crm, cwm) ->
                  if rm = crm && wm = cwm then ()
                  else type_error e "spawn: channel usage incorrect" )
          | None -> type_error e "spawn: coercing chan types failed?")
        lin_map;

      let new_local_lin_ctx =
        LinTyMap.fold
          (fun k _ l -> (k, TMoved) :: List.remove_assoc k l)
          lin_map c.lin_locals
      in
      (TRegTy TThreadGroup, { c with lin_locals = new_local_lin_ctx })
  | CJoin exp -> (
      let t, ctx = typecheck_exp c exp in
      match t with
      | TRegTy TThreadGroup -> (TRegTy TInt, ctx)
      | _ -> type_error exp "Cannot join on non-int handle" )

and typecheck_exp_list (c : Tctxt.t) (el : exp node list) : ty list * Tctxt.t =
  match el with
  | [] -> ([], c)
  | exp :: tl ->
      let exp_ty, ctx1 = typecheck_exp c exp in
      let tl_ty, ctx2 = typecheck_exp_list ctx1 tl in
      (exp_ty :: tl_ty, ctx2)

(* statements --------------------------------------------------------------- *)

(* Typecheck a statement 
   This function should implement the statment typechecking rules from oat.pdf.  

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement)

     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns 

        in the branching statements, the return behavior of the branching 
        statement is the conjunction of the return behavior of the two 
        branches: both both branches must definitely return in order for 
        the whole statement to definitely return.

        Intuitively: if one of the two branches of a conditional does not 
        contain a return statement, then the entire conditional statement might 
        not return.
  
        looping constructs never definitely return 

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)

let verify_arb_consumption (og_tc : Tctxt.t) (res_tc : Tctxt.t) (nd : 'a node) :
    unit =
  List.iter
    (fun (id, ty) ->
      match List.assoc_opt id res_tc.lin_locals with
      | None | Some TMoved -> (
          match ty with
          | TMoved -> type_error nd "usage of moved linear value"
          | TChan (_, rm, wm) ->
              if rm = MNum 1 || wm = MNum 1 then
                type_error nd
                  "single-use channel possibly consumed multiple times" )
      | _ -> ())
    og_tc.lin_locals

let can_unconsumed (lty : lty) : bool =
  match lty with
  | TMoved
  | TChan (_, MArb, MNum 0)
  | TChan (_, MNum 0, MArb)
  | TChan (_, MArb, MArb) ->
      true
  | _ -> false

let diff_lin_tc (og_tc : Tctxt.t) (scoped_tc : Tctxt.t) : Tctxt.t =
  {
    og_tc with
    lin_locals =
      List.fold_left
        (fun acc (id, ty) ->
          match List.assoc_opt id scoped_tc.lin_locals with
          | None | Some TMoved -> acc
          | _ -> (id, ty) :: acc)
        [] og_tc.lin_locals;
  }

let rec typecheck_stmt (tc : Tctxt.t) (s : stmt node) (to_ret : ret_ty) :
    Tctxt.t * bool =
  match s.elt with
  | Assn (e1, e2) -> (
      let () =
        match e1.elt with
        | Id x -> (
            match lookup_local_lin_option x tc with
            | Some _ -> ()
            | None -> (
                match lookup_local_reg_option x tc with
                | Some _ -> ()
                | None -> (
                    match lookup_global_option x tc with
                    | Some (TRef (RFun _)) ->
                        type_error s ("cannot assign to global function " ^ x)
                    | _ -> () ) ) )
        | _ -> ()
      in
      let assn_to, ctx1 = typecheck_exp tc e1 in
      match assn_to with
      | TLinTy _ -> type_error s "cannot assign to linear type"
      | _ ->
          let assn_from, ctx2 = typecheck_exp ctx1 e2 in
          if subtype tc assn_from assn_to then (ctx2, false)
          else type_error s "Mismatched types in assignment" )
  | Decl (id, exp) -> (
      let exp_type, tc1 = typecheck_exp tc exp in
      if
        List.exists (fun x -> fst x = id) tc.reg_locals
        || List.exists (fun x -> fst x = id) tc.lin_locals
      then type_error s "Cannot redeclare variable"
      else
        match exp_type with
        | TLinTy lty -> (add_local_lin tc1 id lty, false)
        | TRegTy regty -> (add_local_reg tc1 id regty, false) )
  | Ret r -> (
      match (r, to_ret) with
      | None, RetVoid -> (tc, true)
      | Some r, RetVal to_ret ->
          let t, tc' = typecheck_exp tc r in
          if subtype tc t to_ret then (tc', true)
          else type_error s "Returned incorrect type"
      | None, RetVal _ -> type_error s "Returned void in non-void function"
      | Some _, RetVoid -> type_error s "Returned non-void in void function" )
  | SCall (f, args) -> (
      let argtyps, ctx1 = typecheck_exp_list tc args in
      let ftyp, ctx2 = typecheck_exp ctx1 f in
      match ftyp with
      | TRegTy (TNullRef (RFun (l, RetVoid)))
      | TRegTy (TRef (RFun (l, RetVoid))) ->
          if List.length l <> List.length argtyps then
            type_error s "Incorrect number of arguments"
          else
            List.iter2
              (fun arg l ->
                if not (subtype tc arg l) then
                  type_error s "Incorrect type of argument")
              argtyps l;
          (ctx2, false)
      | _ -> type_error s "Need function argument for function call" )
  | If (e, b1, b2) ->
      let guard_type, ctx1 = typecheck_exp tc e in
      if guard_type <> TRegTy TBool then type_error e "Incorrect type for guard"
      else
        let ctx_b1, lft_ret = typecheck_block ctx1 b1 to_ret in
        let ctx_b2, rgt_ret = typecheck_block ctx1 b2 to_ret in
        if ctx_b1.lin_locals <> ctx_b2.lin_locals then
          type_error s
            "Both branches in an if-conditional has to consume the same \
             resources"
        else (diff_lin_tc tc ctx_b1, lft_ret && rgt_ret)
  | Cast (r, id, exp, b1, b2) -> (
      let exp_type, ctx1 = typecheck_exp tc exp in
      match exp_type with
      | TRegTy (TNullRef r') ->
          if subtype_ref tc r' r then
            let ctx_b1, lft_ret =
              typecheck_block (add_local_reg ctx1 id (TRef r)) b1 to_ret
            in
            let ctx_b2, rgt_ret = typecheck_block ctx1 b2 to_ret in
            if ctx_b1.lin_locals <> ctx_b2.lin_locals then
              type_error s
                "Both branches in an if-conditional has to consume the same \
                 resources"
            else (diff_lin_tc tc ctx_b1, lft_ret && rgt_ret)
          else type_error exp "if? expression not a subtype of declared type"
      | _ -> type_error exp "if? expression has non-? type" )
  | While (b, bl) ->
      let guard_type, ctx1 = typecheck_exp tc b in
      verify_arb_consumption tc ctx1 b;
      if guard_type <> TRegTy TBool then type_error b "Incorrect type for guard"
      else
        let res_ctx, _ = typecheck_block ctx1 bl to_ret in
        (* Check what res_ctx has consumed, and make sure they are MArb *)
        verify_arb_consumption ctx1 res_ctx (List.hd bl);
        (diff_lin_tc tc res_ctx, false)
  | For (vs, guard, upd, b) ->
      let ctx1 =
        List.fold_left
          (fun c (id, e) ->
            let t, new_ctx = typecheck_exp c e in
            let regty =
              match t with
              | TLinTy _ ->
                  type_error s "for: introduced linear types in declaration"
              | TRegTy r -> r
            in
            Tctxt.add_local_reg new_ctx id regty)
          tc vs
      in
      let ctx2 =
        match guard with
        | None -> ctx1
        | Some b ->
            let ty, new_ctx = typecheck_exp ctx1 b in
            (* Check if expression consumed any linear types, and make sure they are MArb *)
            verify_arb_consumption ctx1 new_ctx s;
            if TRegTy TBool <> ty then type_error b "Incorrect type for guard"
            else new_ctx
      in
      let ctx3 =
        match upd with
        | None -> ctx2
        | Some upd ->
            let nc, rt = typecheck_stmt ctx2 upd to_ret in
            (* Check if statement consumed any linear types, and make sure they are MArb *)
            verify_arb_consumption ctx2 nc s;
            if rt then type_error s "Cannot return in for loop increment"
            else nc
      in
      let ctx4, _ = typecheck_block ctx3 b to_ret in
      verify_arb_consumption ctx3 ctx4 s;
      (diff_lin_tc tc ctx4, false)

and typecheck_block (tc : Tctxt.t) (b : block) (to_ret : ret_ty) :
    Tctxt.t * bool =
  let res_ctx, ret = typecheck_block_helper tc b to_ret in
  (* Check that all linear types introduced in block are consumed *)
  List.iter
    (fun (id, ty) ->
      if not (can_unconsumed ty) then
        match List.assoc_opt id tc.lin_locals with
        | None ->
            type_error (List.hd b) "Linear types not consumed at end of block"
        | Some og_ty ->
            if ty <> og_ty then
              type_error (List.hd b)
                "Linear types does not match - should not be possible"
            else ())
    res_ctx.lin_locals;
  (res_ctx, ret)

and typecheck_block_helper (tc : Tctxt.t) (b : block) (to_ret : ret_ty) :
    Tctxt.t * bool =
  match b with
  | [] -> (tc, false)
  | [ h ] -> typecheck_stmt tc h to_ret
  | h1 :: h2 :: t ->
      let new_context, r = typecheck_stmt tc h1 to_ret in
      if r then type_error h2 "Dead code"
      else typecheck_block_helper new_context (h2 :: t) to_ret

(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elswhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> List.exists (fun x -> x.fieldName = h.fieldName) t || check_dups t

let typecheck_tdecl (tc : Tctxt.t) id fs (l : 'a node) : unit =
  if check_dups fs then type_error l ("Repeated fields in " ^ id)
  else
    List.iter
      (fun f ->
        match f.ftyp with
        | TLinTy _ -> type_error l "Structs cannot contain linear types"
        | _ -> typecheck_ty l tc f.ftyp)
      fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let typecheck_fdecl (tc : Tctxt.t) (f : fdecl) (l : 'a node) : unit =
  let updated =
    List.fold_left
      (fun c (t, i) ->
        typecheck_ty l tc t;
        match t with
        | TLinTy lty -> add_local_lin c i lty
        | TRegTy regty -> add_local_reg c i regty)
      tc f.args
  in
  let ctx, returned = typecheck_block updated f.body f.frtyp in
  if not returned then type_error l "Need return statement"
  else
    List.iter
      (fun (_, ty) ->
        if not (can_unconsumed ty) then
          type_error l "Linear types not consumed at end of function")
      ctx.lin_locals

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'S'
   context (checking to see that there are no duplicate fields

     H |-s prog ==> H'


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'G' context (ensuring that there are no redeclared
   function identifiers)

     H ; G1 |-f prog ==> G2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G1 |-g prog ==> G2    


   NOTE: global initializers may mention function identifiers as
   constants, but can mention only other global values that were declared earlier
*)

let create_struct_ctxt (p : prog) : Tctxt.t =
  List.fold_left
    (fun c d ->
      match d with
      | Gtdecl ({ elt = id, fs; _ } as l) ->
          if List.exists (fun x -> id = fst x) c.structs then
            type_error l ("Redeclaration of struct " ^ id)
          else Tctxt.add_struct c id fs
      | _ -> c)
    Tctxt.empty p

let create_function_ctxt (tc : Tctxt.t) (p : prog) : Tctxt.t =
  let builtins_context =
    List.fold_left
      (fun c (id, (args, ret)) ->
        Tctxt.add_global c id (TRef (RFun (args, ret))))
      tc builtins
  in
  List.fold_left
    (fun c d ->
      match d with
      | Gfdecl ({ elt = f; _ } as l) ->
          if List.exists (fun x -> fst x = f.fname) c.globals then
            type_error l ("Redeclaration of " ^ f.fname)
          else
            Tctxt.add_global c f.fname
              (TRef (RFun (List.map fst f.args, f.frtyp)))
      | _ -> c)
    builtins_context p

let create_global_ctxt (tc : Tctxt.t) (p : prog) : Tctxt.t =
  List.fold_left
    (fun c d ->
      match d with
      | Gvdecl ({ elt = decl; _ } as l) -> (
          let e, _ = typecheck_exp c decl.init in
          match e with
          | TLinTy _ -> type_error l "Global variables cannot be linear"
          | TRegTy regty ->
              if List.exists (fun x -> fst x = decl.name) c.globals then
                type_error l ("Redeclaration of " ^ decl.name)
              else Tctxt.add_global c decl.name regty )
      | _ -> c)
    tc p

(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
*)
let typecheck_program (p : prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter
    (fun p ->
      match p with
      | Gfdecl ({ elt = f; _ } as l) -> typecheck_fdecl tc f l
      | Gtdecl ({ elt = id, fs; _ } as l) -> typecheck_tdecl tc id fs l
      | _ -> ())
    p
