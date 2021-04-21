open Ast_lib

type fty = Ast.ty list * Ast.ret_ty

(* typing contexts *)
type local_reg_ctxt = (Ast.id * Ast.regty) list

type local_lin_ctxt = (Ast.id * Ast.lty) list

type global_ctxt = (Ast.id * Ast.regty) list

type fun_ctxt = (Ast.id * fty) list

type struct_ctxt = (Ast.id * Ast.field list) list

(* bundled together *)
type t = {
  reg_locals : local_reg_ctxt;
  lin_locals : local_lin_ctxt;
  globals : global_ctxt;
  functions : fun_ctxt;
  structs : struct_ctxt;
}

let empty =
  {
    reg_locals = [];
    lin_locals = [];
    globals = [];
    functions = [];
    structs = [];
  }

(* local regular ------------------------------------------------------------ *)
let add_local_reg (c : t) (id : Ast.id) (bnd : Ast.regty) : t =
  { c with reg_locals = (id, bnd) :: c.reg_locals }

let lookup_local_reg (id : Ast.id) (c : t) : Ast.regty =
  List.assoc id c.reg_locals

let lookup_local_reg_option id c : Ast.regty option =
  try Some (List.assoc id c.reg_locals) with Not_found -> None

(* local linear ------------------------------------------------------------- *)
let add_local_lin (c : t) (id : Ast.id) (bnd : Ast.lty) : t =
  { c with lin_locals = (id, bnd) :: c.lin_locals }

let lookup_local_lin (id : Ast.id) (c : t) : Ast.lty =
  List.assoc id c.lin_locals

let lookup_local_lin_option id c : Ast.lty option =
  try Some (List.assoc id c.lin_locals) with Not_found -> None

(* globals ------------------------------------------------------------------ *)
let add_global (c : t) (id : Ast.id) (bnd : Ast.regty) : t =
  { c with globals = (id, bnd) :: c.globals }

let lookup_global (id : Ast.id) (c : t) : Ast.regty = List.assoc id c.globals

let lookup_global_option id c : Ast.regty option =
  try Some (List.assoc id c.globals) with Not_found -> None

(* functions ---------------------------------------------------------------- *)
let add_function (c : t) (id : Ast.id) (bnd : fty) : t =
  { c with functions = (id, bnd) :: c.functions }

let lookup_function (id : Ast.id) (c : t) : fty = List.assoc id c.functions

let lookup_function_option id c : fty option =
  try Some (List.assoc id c.functions) with Not_found -> None

(* general-purpose lookup: for local _or_ global: note function ids aren't considered  *)
let lookup_reg id c : Ast.regty =
  match lookup_local_reg_option id c with
  | None -> lookup_global id c
  | Some x -> x

let lookup_option id c : Ast.regty option =
  match lookup_local_reg_option id c with
  | None -> lookup_global_option id c
  | Some x -> Some x

(* structures --------------------------------------------------------------- *)
let add_struct c id bnd = { c with structs = (id, bnd) :: c.structs }

let lookup_struct id c = List.assoc id c.structs

let lookup_struct_option id c =
  try Some (lookup_struct id c) with Not_found -> None

let lookup_field_option st_name f_name c =
  let rec lookup_field_aux f_name (l : Ast.field list) =
    match l with
    | [] -> None
    | h :: t ->
        if h.fieldName = f_name then Some h.ftyp else lookup_field_aux f_name t
  in
  match lookup_struct_option st_name c with
  | None -> None
  | Some x -> lookup_field_aux f_name x

let lookup_field st_name f_name c =
  match lookup_field_option st_name f_name c with
  | None -> failwith "StructCtxt.lookup_field: Not found"
  | Some x -> x
