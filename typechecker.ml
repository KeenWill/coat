open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))


(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
 *)

let filter_map (f : 'a -> ('b option)) (l: 'a list): 'b list =
  List.fold_left (fun (acc : 'b list) (a : 'a) -> 
    match f a with
    | Some b -> b :: acc
    | None -> acc
  ) [] l


let fst (p: 'a * 'b) : 'a =
  match p with
  | left, right -> left

let snd (p: 'a * 'b) : 'b =
  match p with
  | left, right -> right
                 
let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool =
  match t1, t2 with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TNullRef rty1, TNullRef rty2
    | TRef rty1, TRef rty2 
    | TRef rty1, TNullRef rty2 -> subtype_ref c rty1 rty2
  | _, _ -> false

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool =
  match t1, t2 with
  | RString, RString -> true
  | RArray ty1, RArray ty2 -> ty1 = ty2
  | RStruct stid1, RStruct stid2 -> 
     let st1_fields : Ast.field list = lookup_struct stid1 c in
     let st2_fields : Ast.field list = lookup_struct stid2 c in

     let num_fields_diff : int = (List.length st1_fields) - (List.length st2_fields) in
     if num_fields_diff < 0 then false else 
       let st2_fields : Ast.field option list =
         (List.map (fun field -> Some field) st2_fields) @ (List.init num_fields_diff (fun _ -> None)) in 

       let matched_fields = List.combine st1_fields st2_fields in
       List.fold_left
         (fun acc pair ->
           let field1 = fst pair in
           let field2 = snd pair in
           match field2 with
           | Some field2 -> acc && (field1.fieldName = field2.fieldName) && (field1.ftyp = field2.ftyp)
           | None -> acc)
         true matched_fields

  | RFun (arg_l1, ret_ty1), RFun (arg_l2, ret_ty2) ->
     if List.length arg_l1 <> List.length arg_l2 then false else
       let matched_args = List.combine arg_l1 arg_l2 in
       List.fold_left
         (fun acc pair ->
           let arg1 = fst pair in
           let arg2 = snd pair in
           acc && (subtype c arg2 arg1))
         (subtype_return c ret_ty1 ret_ty2) matched_args
  | _, _ -> false
and subtype_return(c : Tctxt.t) (t1 : Ast.ret_ty) (t2 : Ast.ret_ty) : bool =
  match t1, t2 with
  | RetVoid, RetVoid -> true
  | RetVal ty1, RetVal ty2 -> subtype c ty1 ty2
  | _, _ -> false

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
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with
  | TInt | TBool -> ()
  | TRef rty | TNullRef rty -> typecheck_ref l tc rty
and typecheck_ref (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit =
  match t with
  | RString -> ()
  | RArray ty -> typecheck_ty l tc ty
  | RStruct id ->
     begin match lookup_struct_option id tc with
     | Some _ -> ()
     | None -> type_error l ("typecheck_ref: could not find struct id = " ^ id)
     end
  | RFun (arg_l, ret_ty) -> 
     List.iter (typecheck_ty l tc) arg_l ;
     typecheck_return l tc ret_ty
and typecheck_return (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ret_ty) : unit =
  match t with
  | RetVoid -> ()
  | RetVal ty -> typecheck_ty l tc ty


(* A helper function to determine whether a type allows the null value *)
let is_nullable_ty (t : Ast.ty) : bool =
  match t with
  | TNullRef _ -> true
  | _ -> false

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
let local_declared (tc: Tctxt.t) (id: id) (l: 'a Ast.node) : unit =
  match lookup_local_option id tc with
  | Some _ -> type_error l ("redeclaring local variable: " ^ id)
  | None -> ()

let global_declared (tc: Tctxt.t) (id: id) (l: 'a Ast.node) : unit =
  match lookup_global_option id tc with
  | Some _ -> type_error l ("redeclaring global: " ^ id)
  | None -> ()

let struct_declared (tc: Tctxt.t) (id: id) (l: 'a Ast.node) : unit =
  match lookup_struct_option id tc with
  | Some _ -> type_error l ("redeclaring struct: " ^ id)
  | None -> ()

let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  match e.elt with
  | CNull rty ->
     typecheck_ref e c rty ;
     TNullRef rty            
  | CBool _ -> TBool
  | CInt _ -> TInt
  | CStr _ -> TRef RString
  | Id id ->
     begin match lookup_option id c with
     | Some ty -> ty
     | None -> type_error e ("typecheck_exp: failed to find id " ^  id)
     end
  | Bop (bop, e1, e2) -> 
     begin match bop with
     | Eq | Neq ->
        let t1 : Ast.ty = typecheck_exp c e1 in
        let t2 : Ast.ty = typecheck_exp c e2 in
        if not ((subtype c t1 t2) || (subtype c t2 t1)) then
          type_error e "typecheck_exp equality/inequality operator type mismatch" else () ;
        TBool
     | _ -> 
        begin match typecheck_bop c bop e1 e2 with
        | Some ty -> ty
        | None -> type_error e ("typecheck_exp: ill-formed bop")
        end
     end
  | Uop (uop, e1) -> 
     begin match typecheck_uop c uop e1 with
     | Some ty -> ty
     | None -> type_error e ("typecheck_exp: ill-formed uop")
     end

  | CArr (t, el1) ->
     typecheck_ty e c t ;
     List.iter
       (fun e_n ->
         let t' : Ast.ty = typecheck_exp c e_n in
         if not (subtype c t' t)
         then type_error e "typecheck_exp CArr: implicit elements not a subtype of array type" else ()
       ) el1 ;
     TRef (RArray t)
  | NewArr (t, e1) ->
     typecheck_ty e c t ;
     begin match typecheck_exp c e1 with
     | TInt -> ()
     | _ -> type_error e "typecheck_exp NewArr: length must be of type int"
     end ; 
     begin match t with
     | TRef _ -> type_error e "typecheck_exp NewArr: type of this array can be null"
     | _ -> ()
     end ;
     TRef (RArray t)
  | NewArrInit (t, e1, x, e2) ->
     typecheck_ty e c t ;
     begin match typecheck_exp c e1 with
     | TInt -> ()
     | _ -> type_error e "typecheck_exp NewArrInit: length must be of type int"
     end ; 
     local_declared c x e ;
     let new_tc : Tctxt.t = add_local c x TInt in
     let t' : Ast.ty = typecheck_exp new_tc e2 in
     if not (subtype new_tc t' t) then
       type_error e "typecheck_exp NewArrInit: initializer must be same type as array" else () ;
     TRef (RArray t)

  | Index (arr_e, index_e) ->
     let t : Ast.ty =
       begin match typecheck_exp c arr_e with
       | TRef rty ->
          begin match rty with
          | RArray t -> t
          | _ -> type_error e "typecheck_exp Index: Can only index into defnitely non-null arrays"
          end 
       | _ -> type_error e "typecheck_exp Index: Can only index into definitely non-null arrays"
       end in
     begin match typecheck_exp c index_e with
     | TInt -> ()
     | _ -> type_error e "typecheck_exp Index: index must be of type int"
     end ; 
    t 
  | Length arr_e ->
     begin match typecheck_exp c arr_e with
     | TRef rty ->
        begin match rty with
        | RArray t -> ()
        | _ -> type_error e "typecheck_exp Length: Can only call length on definitely non-null arrays"
        end 
     | _ -> type_error e "typecheck_exp Index: Can only call length on definitely non-null arrays"
     end ;
     TInt
     
  | CStruct (sid, fields) ->
     let sty : Ast.ty = TRef (RStruct sid) in
     typecheck_ty e c sty ; 
     let fields : Ast.field list =
       List.map (fun f -> { fieldName = fst f; ftyp = typecheck_exp c (snd f); }) fields in
     let fields : Ast.field list =
       List.fast_sort (fun f1 f2 -> compare f1.fieldName f2.fieldName) fields in
     let context_fields : Ast.field list = lookup_struct sid c in
     if (List.length fields) <> (List.length context_fields) then
       type_error e "typecheck_exp CStruct: Implicit fields do not match structs' field count" else () ;
     let matched_fields : (Ast.field * Ast.field) list = List.combine fields context_fields in
     List.iter
       (fun field_pair ->
         let field_name : id = (fst field_pair).fieldName in
         let ctxt_field_name : id = (snd field_pair).fieldName in
         if field_name <> ctxt_field_name then 
           type_error e "typecheck_exp CStruct: Implicit fields do not match structs' field names" else () ;
         let field_ty : Ast.ty = (fst field_pair).ftyp in
         let ctxt_field_ty : Ast.ty = (snd field_pair).ftyp in
         if not (subtype c field_ty ctxt_field_ty) then
           type_error e "typecheck_exp CStruct: Implicit fields do not match structs' field types" else ()
       )
       matched_fields ; 
     sty
  | Proj (s_e, fid) ->
     let sid : id =
       begin match typecheck_exp c s_e with
       | TRef rty ->
          begin match rty with
          | RStruct id -> id
          | _ -> type_error e "typecheck_exp Proj: Can only project out of definitely non-null structs"
          end
       | _ -> type_error e "typecheck_exp Proj: Can only project out of definitely non-null structs"
       end in
     begin match lookup_field_option sid fid c with
     | Some t -> t
     | None -> type_error e "typecheck_exp Proj: Struct id, field name pair not found"
     end

  | Call (func_e, args_el) ->
     let func_ty : Ast.ty = typecheck_exp c func_e in
     let (func_args_ty, func_ret_ty) =
       begin match func_ty with
       | TRef rty ->
          begin match rty with
          | RFun (tyl, ty) -> tyl, ty
          | _ -> type_error e "typecheck_exp Call: Can only call something of function type"
          end 
       | _ -> type_error e "typecheck_exp Call: Can only call something of function type"
       end in
     if (List.length args_el) <> (List.length func_args_ty) then
       type_error e "typecheck_exp Call: Function call argument count does not match up" else () ;
     let matched_args = List.combine args_el func_args_ty in
     List.iter
       (fun arg_pair ->
         let t' = typecheck_exp c (fst arg_pair) in
         let t = snd arg_pair in
         if not (subtype c t' t) then
           type_error e "typecheck_exp Call: Function call argument types do not match up" else ()
       ) matched_args ;
     begin match func_ret_ty with
     | RetVal t -> t
     | RetVoid -> type_error e "typecheck_exp Call: Function call as expression cannot return void"
     end

and typecheck_bop (c : Tctxt.t) (bop: Ast.binop) (e1: Ast.exp node) (e2: Ast.exp node) : Ast.ty option =
  match bop with
  | Add | Sub | Mul | IAnd | IOr | Shl | Shr | Sar ->
     begin match typecheck_exp c e1, typecheck_exp c e2 with
     | TInt, TInt -> Some TInt
     | _, _ -> None
     end
  | Lt | Lte | Gt | Gte ->
     begin match typecheck_exp c e1, typecheck_exp c e2 with
     | TInt, TInt -> Some TBool
     | _, _ -> None
     end
  | And | Or ->
     begin match typecheck_exp c e1, typecheck_exp c e2 with
     | TBool, TBool -> Some TBool
     | _, _ -> None
     end
  | _ -> failwith "do not use typecheck_bop for == and !="

and typecheck_uop (c : Tctxt.t) (uop: Ast.unop) (e1 : Ast.exp node) : Ast.ty option =
  match uop, typecheck_exp c e1 with
  | Lognot, TBool -> Some TBool
  | Bitnot, TInt -> Some TInt
  | Neg, TInt -> Some TInt
  | _, _ -> None


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
(* similar to stmt, typecheck_block returns whether this block will definitely return or not *)

let typecheck_vdecl (l: 'a Ast.node) (tc : Tctxt.t) (vd: vdecl) : Tctxt.t =
  let id : id = fst vd in
  let e : exp Ast.node = snd vd in
  local_declared tc id l ;
  let e_ty : Ast.ty = typecheck_exp tc e in

  add_local tc id e_ty

let typecheck_guard (tc : Tctxt.t) (guard_e: Ast.exp Ast.node) : unit =
  match typecheck_exp tc guard_e with
  | TBool -> ()
  | _ -> type_error guard_e "typecheck_stmt: guard must be a bool"
       
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  match s.elt with
  | Assn (e1, e2) ->
     begin match e1.elt with
     | Id id ->
        begin match lookup_global_option id tc with
        | Some ty ->
           begin match ty with
           | TRef rty ->
              begin match rty with
              | RFun _ -> type_error s "typecheck_stmt Assn: LHS cannot be a function id"
              | _ -> () end | _ -> () end | _ -> () end | _ -> () end ;

     let t : Ast.ty = typecheck_exp tc e1 in
     let t' : Ast.ty = typecheck_exp tc e2 in
     if not (subtype tc t' t) then type_error s "typecheck_stmt Assn: rhs not a subtype of lhs" else 
       tc, false

  | Decl vd ->
     typecheck_vdecl s tc vd, false

  | Ret e_opt ->
     let t' : Ast.ret_ty = match e_opt with
       | Some e -> RetVal (typecheck_exp tc e)
       | None -> RetVoid
     in
     if not (subtype_return tc t' to_ret) then type_error s "typecheck_stmt Ret: return type mismatch" else
       tc, true 

  | SCall (func_e1, arg_el) ->
     let (e1_args, e1_ret) : Ast.ty list * Ast.ret_ty =
       begin match typecheck_exp tc func_e1 with
       | TRef rty ->
          begin match rty with
          | RFun (args, ret) -> args, ret
          | _ -> type_error s "typecheck_stmt SCall: can only call function types"
          end
       | _ -> type_error s "typecheck_stmt SCall: can only call function types"
       end in
     begin match e1_ret with
     | RetVoid -> ()
     | _ -> type_error s "typecheck_stmt SCall: statement function calls must have void return type"
     end ;

     if List.length e1_args <> List.length arg_el then
       type_error s "typecheck_stmt SCall: number of args do not match function type" else () ;
     let matched_args : (Ast.ty * Ast.exp Ast.node) list = List.combine e1_args arg_el in
     
     List.iter
       (fun ty_pair ->
         let t = fst ty_pair in
         let t' = typecheck_exp tc (snd ty_pair) in
         if not (subtype tc t' t) then type_error (snd ty_pair) "typecheck_stmt SCall: function args type mismatch"
         else ()
       ) matched_args ;
     tc, false

  | If (guard_e, if_b, else_b) ->
     typecheck_guard tc guard_e ;

     let if_defret : bool = typecheck_block tc if_b to_ret s.loc in
     let else_defret : bool = typecheck_block tc else_b to_ret s.loc in
     tc, (if_defret && else_defret)

  | Cast (v_rty, v, guard_e, if_b, else_b) ->
     let guard_ty_null : Ast.ty = typecheck_exp tc guard_e in
     let guard_rty : Ast.rty =
       begin match guard_ty_null with
       | TNullRef rty -> rty
       | _ -> type_error s "typecheck_stmt Cast: guard cast must be possibly null type"
       end in
     if not (subtype_ref tc guard_rty v_rty) then type_error s "typecheck_stmt Cast: guard and declaration type mistmatch" else () ;

     let casted_tc : Tctxt.t = add_local tc v (TRef v_rty) in
     let if_defret : bool = typecheck_block casted_tc if_b to_ret s.loc in
     let else_defret : bool = typecheck_block tc else_b to_ret s.loc in
     tc, (if_defret && else_defret)

  | While (guard_e, loop_b) ->
     typecheck_guard tc guard_e ; 
     let _ = typecheck_block tc loop_b to_ret s.loc in

     tc, false
     
  | For (inits, guard_opt, post_opt, loop_b) ->
     let new_tc : Tctxt.t = List.fold_left (typecheck_vdecl s) tc inits in
     begin match guard_opt with
     | Some guard_e -> typecheck_guard new_tc guard_e
     | None -> ()
     end ;

     let (_, post_defret) =
       begin match post_opt with
       | Some post_stmt -> typecheck_stmt new_tc post_stmt to_ret
       | None -> Tctxt.empty, false
       end in
     if post_defret then type_error s "typecheck_stmt For: post statement in for loop must not return" else () ;

     let _ = typecheck_block new_tc loop_b to_ret s.loc in

     tc, false

and typecheck_block (tc : Tctxt.t) (body : block) (frtyp: ret_ty) (loc : Range.t): bool =
  (* This fold returns Tctxt.t * bool, so take the second value *)
  let l : int Ast.node = { elt = 5;
                           loc = loc } in
  snd
    (List.fold_left
       (fun acc st_n ->
         let acc_ctxt : Tctxt.t = fst acc in
         let defret : bool = snd acc in
         if defret then type_error l "Only final statement can definitely return" else () ;
         typecheck_stmt acc_ctxt st_n frtyp
       )
       (tc, false) body )

(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elsewhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) id fs  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id) 
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns

    - no duplicate function names



type fdecl = { frtyp : ret_ty; fname : id; args : (ty * id) list; body : block }

 *)
(* Helper function to look for duplicate arg names *)
let rec check_dups_args args =
  match args with
  | [] -> false
  | h :: t -> (List.exists (fun x -> snd x = snd h) t) || check_dups_args t


let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =
  if check_dups_args f.args then type_error l ("Function has duplicate args: " ^ f.fname) else

    let local_tc = List.fold_left (fun acc_tc arg -> add_local acc_tc (snd arg) (fst arg)) tc f.args in

    if typecheck_block local_tc f.body f.frtyp l.loc then ()
    else type_error l ("Function may not return: " ^ f.fname) 
    

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

let create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  let struct_decls : tdecl node list =
    filter_map (fun d -> match d with
                         | Gtdecl td -> Some td
                         | _ -> None) p in
  List.fold_left
    (fun acc_ctxt struct_n ->
      let id = fst (struct_n.elt) in

      struct_declared acc_ctxt id struct_n ;

      let fields : Ast.field list =
        List.fast_sort (fun f1 f2 -> compare f1.fieldName f2.fieldName) (snd (struct_n.elt)) in
      add_struct acc_ctxt id fields )
    Tctxt.empty struct_decls


let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let g0: Tctxt.t = List.fold_left
                      (fun acc_ctxt built ->
                        let name = fst built in
                        let types = snd built in
                        let ftyp : Ast.ty = TRef (RFun (fst types, snd types)) in
                        add_global acc_ctxt name ftyp) tc builtins in

  let func_decls : fdecl node list =
    filter_map (fun d -> match d with
                              | Gfdecl fd -> Some fd
                              | _ -> None) p in

  List.fold_left
    (fun acc_ctxt func_n ->
      let fname = func_n.elt.fname in
      let frtyp = func_n.elt.frtyp in
      let args = fst (List.split (func_n.elt.args)) in
      let ftyp : Ast.ty = TRef (RFun (args, frtyp)) in

      global_declared acc_ctxt fname func_n ; 

      add_global acc_ctxt fname ftyp )
    g0 func_decls

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let g_decls : gdecl node list =
    filter_map (fun d -> match d with
                              | Gvdecl vd -> Some vd
                              | _ -> None) p in
  List.fold_left
    (fun acc_ctxt var_n ->
      let vname = var_n.elt.name in
      let vexp_n = var_n.elt.init in
      let vtyp = typecheck_exp acc_ctxt vexp_n in

      global_declared acc_ctxt vname var_n ; 

      add_global acc_ctxt vname vtyp)
    tc g_decls
  


(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
 *)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
      match p with
      | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
      | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l 
      | _ -> ()) p
