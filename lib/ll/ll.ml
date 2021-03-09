(* LLVMlite: A simplified subset of LLVM IR *)

(* Local identifiers *)
type uid = string

(* Global identifiers *)
type gid = string

(* Named types *)
type tid = string

(* Labels *)
type lbl = string

(* LLVM types *)
type ty =
| Void
| I1
| I8
| I64
| Ptr of ty
| Struct of ty list
| Array of int * ty
| Fun of ty list * ty
| Namedt of tid

(* Function type: argument types and return type *)
type fty = ty list * ty

(* Syntactic Values *)
type operand =
| Null
| Const of int64
| Gid of gid
| Id of uid

(* Binary i64 Operations *)
type bop =
| Add
| Sub
| Mul
| Shl
| Lshr
| Ashr
| And
| Or
| Xor

(* Comparison Operators *)
type cnd =
| Eq
| Ne
| Slt
| Sle
| Sgt
| Sge

(* Instructions *)
type insn =
| Binop of bop * ty * operand * operand
| Alloca of ty
| Load of ty * operand
| Store of ty * operand * operand
| Icmp of cnd * ty * operand * operand
| Call of ty * operand * (ty * operand) list
| Bitcast of ty * operand * ty
| Gep of ty * operand * operand list

type terminator =
| Ret of ty * operand option
| Br of lbl
| Cbr of operand * lbl * lbl

(* Basic Blocks *)
type block = { insns : (uid * insn) list; term : (uid * terminator) }

(* Control Flow Graphs: entry and labeled blocks *)
type cfg = block * (lbl * block) list

(* Function Declarations *)
type fdecl = { f_ty : fty; f_param : uid list; f_cfg : cfg }

(* Global Data Initializers *)
type ginit =
| GNull
| GGid of gid
| GInt of int64
| GString of string
| GArray of (ty * ginit) list
| GStruct of (ty * ginit) list
| GBitcast of ty * ginit * ty

(* Global Declarations *)
type gdecl = ty * ginit

(* LLVMlite Programs *)
type prog = { tdecls : (tid * ty) list; gdecls : (gid * gdecl) list;
              fdecls : (gid * fdecl) list; edecls : (gid * ty) list }
