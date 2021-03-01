
open Ast

type fty = Ast.ty list * Ast.ret_ty

(* typing contexts *)
type local_ctxt = (Ast.id * Ast.ty) list
type global_ctxt = (Ast.id * Ast.ty) list
type struct_ctxt = (Ast.id * Ast.field list) list

(* bundled together *)
type t = {
  locals : local_ctxt;      (* Corresponds to the L of the inference rules *)
  globals : global_ctxt;    (* Corresponds to the G of the inference rules *)
  structs : struct_ctxt;    (* Corresponds to the H of the inference rules *)
}

let empty = { locals = []; globals = []; structs = [] }

(* locals ------------------------------------------------------------------- *)
let add_local (c:t) (id:id) (bnd : Ast.ty) : t = {c with locals = (id, bnd)::c.locals}
let lookup_local (id : Ast.id) (c : t) : Ast.ty = List.assoc id c.locals
let lookup_local_option id c : Ast.ty option =
  try Some (List.assoc id c.locals) with Not_found -> None

(* globals ------------------------------------------------------------------ *)
let add_global (c:t) (id:id) (bnd:Ast.ty) : t = {c with globals = (id, bnd)::c.globals}
let lookup_global (id : Ast.id) (c : t) : Ast.ty = List.assoc id c.globals
let lookup_global_option id c : Ast.ty option =
  try Some (List.assoc id c.globals) with Not_found -> None

(* general-purpose lookup: for local _or_ global *)
let lookup id c : Ast.ty =
  match lookup_local_option id c with
  | None -> lookup_global id c
  | Some x -> x

let lookup_option id c : Ast.ty option =
  match lookup_local_option id c with
  | None -> lookup_global_option id c
  | Some x -> Some x


(* structures --------------------------------------------------------------- *)
let add_struct c id bnd = {c with structs=(id, bnd)::c.structs}
let lookup_struct id c = List.assoc id c.structs

let lookup_struct_option id c =
  try Some (lookup_struct id c) with Not_found -> None

let lookup_field_option st_name f_name c =
  let rec lookup_field_aux f_name l =
    match l with
    | [] -> None
    | h :: t -> if h.fieldName = f_name then Some h.ftyp else lookup_field_aux f_name t in
  match lookup_struct_option st_name c with
  | None -> None
  | Some x -> lookup_field_aux f_name x

let lookup_field st_name f_name c =
  match lookup_field_option st_name f_name c with
  | None -> failwith "StructCtxt.lookup_field: Not found"
  | Some x -> x
