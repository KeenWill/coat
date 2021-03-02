
type 'a node = { elt : 'a; loc : Range.t }

(** val no_loc : 'a1 -> 'a1 node **)

let no_loc x =
  { elt = x; loc = Range.norange }

type id = string

type ty =
| TBool
| TInt
| TRef of rty
| TNullRef of rty
and rty =
| RString
| RStruct of id
| RArray of ty
| RFun of ty list * ret_ty
and ret_ty =
| RetVoid
| RetVal of ty

type unop =
| Neg
| Lognot
| Bitnot

type binop =
| Add
| Sub
| Mul
| Eq
| Neq
| Lt
| Lte
| Gt
| Gte
| And
| Or
| IAnd
| IOr
| Shl
| Shr
| Sar

type exp =
| CNull of rty
| CBool of bool
| CInt of int64
| CStr of string
| Id of id
| CArr of ty * exp node list
| NewArr of ty * exp node
| NewArrInit of ty * exp node * id * exp node
| Index of exp node * exp node
| Length of exp node
| CStruct of id * (id * exp node) list
| Proj of exp node * id
| Call of exp node * exp node list
| Bop of binop * exp node * exp node
| Uop of unop * exp node


type cfield = id * exp node

type vdecl = id * exp node

type stmt =
| Assn of exp node * exp node
| Decl of vdecl
| Ret of exp node option
| SCall of exp node * exp node list
| If of exp node * stmt node list * stmt node list
| Cast of rty * id * exp node * stmt node list * stmt node list
| For of vdecl list * exp node option * stmt node option * stmt node list
| While of exp node * stmt node list

type block = stmt node list

type gdecl = { name : id; init : exp node }

type fdecl = { frtyp : ret_ty; fname : id; args : (ty * id) list; body : block }

type field = { fieldName : id; ftyp : ty }

type tdecl = id * field list

type decl =
| Gvdecl of gdecl node
| Gfdecl of fdecl node
| Gtdecl of tdecl node

type prog = decl list
