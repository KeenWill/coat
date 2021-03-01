(* ll ir compilation -------------------------------------------------------- *)

open Ll
open X86

(* Overview ----------------------------------------------------------------- *)

(* We suggest that you spend some time understinging this entire file and 
   how it fits with the compiler pipeline before making changes.  The suggested
   plan for implementing the compiler is provided on the project web page. 
*)


(* helpers ------------------------------------------------------------------ *)

(* Map LL comparison operations to X86 condition codes *)
let compile_cnd = function
  | Ll.Eq  -> X86.Eq
  | Ll.Ne  -> X86.Neq
  | Ll.Slt -> X86.Lt
  | Ll.Sle -> X86.Le
  | Ll.Sgt -> X86.Gt
  | Ll.Sge -> X86.Ge

let imm_of_int (n:int) = Imm (Lit (Int64.of_int n))

(* Compute an indirect address that is a fixed byte offset from %rbp *)
let rbp_offset (offset:int) : X86.operand =
  let amt = Int64.of_int offset in
  Ind3 (Lit amt, Rbp)

(* locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
   identifiers to X86 abstractions.  For the best performance, one
   would want to use an X86 register for each LLVM %uid.  However,
   since there are an unlimited number of %uids and only 16 registers,
   doing so effectively is quite difficult.  We will see later in the
   course how _register allocation_ algorithms can do a good job at
   this.

   A simpler, but less performant, implementation is to map each %uid
   in the LLVM source to a _stack slot_ (i.e. a region of memory in
   the stack).  Since LLVMlite, unlike real LLVM, permits %uid locals
   to store only 64-bit data, each stack slot is an 8-byte value.

   [ NOTE: For compiling LLVMlite, even i1 data values should be
   represented as a 8-byte quad. This greatly simplifies code
   generation. ]

   We call the datastructure that maps each %uid to its stack slot a
   'stack layout'.  A stack layout maps a uid to an X86 operand for
   accessing its contents.  For this compilation strategy, the operand
   is always an offset from %rbp (in bytes) that represents a storage slot in
   the stack.  
*)

type layout = (uid * X86.operand) list

(* A context contains the global type declarations (needed for getelementptr
   calculations) and a stack layout. *)
type ctxt = { tdecls : (tid * ty) list
            ; layout : layout
            }

(* useful for looking up items in tdecls or layouts *)
let lookup m x = List.assoc x m


(* compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

   LL local %uids live in stack slots, whereas global ids live at
   global addresses that must be computed from a label.  Constants are
   immediately available, and the operand Null is the 64-bit 0 value.

     NOTE: two important facts about global identifiers:

     (1) You should use (Platform.mangle gid) to obtain a string 
     suitable for naming a global label on your platform (OS X expects
     "_main" while linux expects "main").

     (2) 64-bit assembly labels are not allowed as immediate operands.
     That is, the X86 code: movq _gid %rax which looks like it should
     put the address denoted by _gid into %rax is not allowed.
     Instead, you need to compute an %rip-relative address using the
     leaq instruction:   leaq _gid(%rip).

   One strategy for compiling instruction operands is to use a
   designated register (or registers) for holding the values being
   manipulated by the LLVM IR instruction. You might find it useful to
   implement the following helper function, whose job is to generate
   the X86 instruction that moves an LLVM operand into a designated
   destination (usually a register).  
*)
let compile_operand ctxt dest : Ll.operand -> ins =
  function
  | Null     -> Asm.(Movq, [~$0; dest])
  | Const i  -> Asm.(Movq, [Imm (Lit i); dest])
  | Gid id   -> Asm.(Leaq, [Ind3 (Lbl (Platform.mangle id), Rip); dest])
  | Id id    -> Asm.(Movq, [lookup ctxt.layout id; dest])


(* compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that 
   generates code for the LLVM IR call instruction.

   The code you generate should follow the x64 System V AMD64 ABI
   calling conventions, which places the first six 64-bit (or smaller)
   values in registers and pushes the rest onto the stack.  Note that,
   since all LLVM IR operands are 64-bit values, the first six
   operands will always be placed in registers.  (See the notes about
   compiling fdecl below.)

   [ NOTE: It is the caller's responsibility to clean up arguments
   pushed onto the stack, so you must free the stack space after the
   call returns. ]

   [ NOTE: Don't forget to preserve caller-save registers (only if
   needed). ]
*)


let arg_reg : int -> (X86.operand) option = function
  | 0 -> Some (Reg Rdi)
  | 1 -> Some (Reg Rsi)
  | 2 -> Some (Reg Rdx)
  | 3 -> Some (Reg Rcx)
  | 4 -> Some (Reg R08)
  | 5 -> Some (Reg R09)
  | n -> None

let compile_call ctxt fop args =
  let op_to_rax = compile_operand ctxt (Reg Rax) in
  let call_code, op = match fop with
    | Gid g -> [], Imm (Lbl (Platform.mangle g))
    | Id _ -> [op_to_rax fop], (Reg Rax)
    | _ -> failwith "call function operand was not a local or global id"
  in

  let arg_code =
    let (_, register_arg_code, stack_arg_code) =
      List.fold_left (fun (i, r, s) (_,op) ->
        let r, s = match arg_reg i with
          | Some reg -> r @ (op_to_rax op :: Asm.([Movq, [~%Rax; reg]])), s
          | None     -> r, (op_to_rax op :: Asm.([Pushq, [~%Rax]])) @ s
        in (i+1, r, s)
      ) (0, [], []) args
    in register_arg_code @ stack_arg_code
  in

  arg_code @ call_code @
  (X86.Callq, [op])  ::
  (if (List.length args) > 6 then
     Asm.([Addq, [imm_of_int (8 * ((List.length args) - 6)); ~%Rsp]])
   else [])

(* compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
   a datastructure, following a path of offsets.  It computes the
   address based on the size of the data, which is dictated by the
   data's type.

   To compile getelmentptr, you must generate x86 code that performs
   the appropriate arithemetic calculations.
*)

(* [size_ty] maps an LLVMlite type to a size in bytes. 
    (needed for getelementptr)

   - the size of a struct is the sum of the sizes of each component
   - the size of an array of t's with n elements is n * the size of t
   - all pointers, I1, and I64 are 8 bytes
   - the size of a named type is the size of its definition

   - Void, i8, and functions have undefined sizes according to LLVMlite.
     Your function should simply return 0 in those cases
*)
let rec size_ty (tdecls:(tid * ty) list) (t:Ll.ty) : int =
  begin match t with
    | Void | I8 | Fun _ -> 0
    | I1 | I64 | Ptr _ -> 8 (* Target 64-bit only subset of X86 *)
    | Struct ts -> List.fold_left (fun acc t -> acc + (size_ty tdecls t)) 0 ts
    | Array (n, t) -> n * (size_ty tdecls t)
    | Namedt id -> size_ty tdecls (List.assoc id tdecls)
  end

(* Compute the size of the offset (in bytes) of the nth element of a region
   of memory whose types are given by the list. Also returns the nth type. *)
let index_into tdecls (ts:ty list) (n:int) : int * ty =
  let rec loop ts n acc =
    begin match (ts, n) with
      | (u::_, 0) -> (acc, u)
      | (u::us, n) -> loop us (n-1) (acc + (size_ty tdecls u))
      | _ -> failwith "index_into encountered bogus index"
    end
  in loop ts n 0


(* Generates code that computes a pointer value.  

   1. op must be of pointer type: t*

   2. the value of op is the base address of the calculation

   3. the first index in the path is treated as the index into an array
     of elements of type t located at the base address

   4. subsequent indices are interpreted according to the type t:

     - if t is a struct, the index must be a constant n and it 
       picks out the n'th element of the struct. [ NOTE: the offset
       within the struct of the n'th element is determined by the 
       sizes of the types of the previous elements ]

     - if t is an array, the index can be any operand, and its
       value determines the offset within the array.
 
     - if t is any other type, the path is invalid

   5. if the index is valid, the remainder of the path is computed as
      in (4), but relative to the type f the sub-element picked out
      by the path so far
*)
let compile_gep (ctxt:ctxt) (op : Ll.ty * Ll.operand) (path: Ll.operand list) : ins list =
  let op_to_rax = compile_operand ctxt (Reg Rax) in
  let rec loop ty path code =
    match (ty, path) with
    | (_, []) -> List.rev code

    | (Struct ts, Const n::rest) ->
       let (offset, u) = index_into ctxt.tdecls ts (Int64.to_int n) in
       loop u rest @@ Asm.(Addq, [~$offset; ~%Rax])::code

    | (Array(_, u), Const n::rest) ->
       (* Statically calculate the offset *)
       let offset = (size_ty ctxt.tdecls u) * (Int64.to_int n) in
       loop u rest @@ Asm.(Addq, [~$offset; ~%Rax])::code

    | (Array(_, u), offset_op::rest) ->
       loop u rest @@
       Asm.([ Addq, [~%Rcx; ~%Rax]
           ; Imulq, [imm_of_int @@ size_ty ctxt.tdecls u; ~%Rax] ])
       @ (op_to_rax offset_op) ::
         Asm.(Movq, [~%Rax; ~%Rcx])
         :: code

    | (Namedt t, p) -> loop (List.assoc t ctxt.tdecls) p code

    | _ -> failwith "compile_gep encountered unsupported getelementptr data" in

  match op with
  | (Ptr t, op) -> loop (Array(0, t)) path [op_to_rax op]
  | _ -> failwith "compile_gep got incorrect parameters"


(* compiling instructions  -------------------------------------------------- *)

(* The result of compiling a single LLVM instruction might be many x86
   instructions.  We have not determined the structure of this code
   for you. Some of the instructions require only a couple of assembly
   instructions, while others require more.  We have suggested that
   you need at least compile_operand, compile_call, and compile_gep
   helpers; you may introduce more as you see fit.

   Here are a few notes:

   - Icmp:  the Setb instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.

   - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier.

   - Alloca: needs to return a pointer into the stack

   - Bitcast: does nothing interesting at the assembly level
*)
let compile_insn (ctxt:ctxt) ((uid:uid), (i:Ll.insn)) : X86.ins list =
  let op_to = compile_operand ctxt in 
  let op_to_rax = op_to (Reg Rax) in (* Move the value of op into rax *)
  let op_to_rcx = op_to (Reg Rcx) in (* Move the value of op into rax *)
  let dst = lookup ctxt.layout uid in
  match i with
  | Binop (bop, t, op1, op2) ->
    let bin op = 
      (op_to_rax op1) :: 
      (op_to_rcx op2) :: 
      Asm.([ op, [~%Rcx; ~%Rax]
          ; Movq, [~%Rax; dst] ])
    in
    begin match bop with
     | Ll.Add ->  bin Addq
     | Ll.Sub ->  bin Subq
     | Ll.Mul ->  bin Imulq
     | Ll.Shl ->  bin Shlq
     | Ll.Lshr -> bin Shrq
     | Ll.Ashr -> bin Sarq
     | Ll.And ->  bin Andq
     | Ll.Or ->   bin Orq
     | Ll.Xor ->  bin Xorq
    end

  (* Alloca instructions allocate an fresh stack slot and 
     move the address of the newly allocated storage into the
     destination uid.   *)
  | Alloca (_t) -> Asm.([ Pushq, [~$0]
                       ; Movq, [~%Rsp; dst] ])

  (* Load dereferences the pointer value stored in a local.
     Global and constant pointers don't need indirection. *)
  | Load (t, op) -> (op_to_rax op) :: Asm.([ Movq, [Ind2 Rax; ~%Rcx]
                                          ; Movq, [~%Rcx; dst] ])

  (* Store also needs to dereference the destination pointer if it's a global *)
  | Store (_, src, (Id uid as dest)) ->
    (op_to_rcx src) ::
    (op_to_rax dest) :: Asm.([Movq, [~%Rcx; Ind2 Rax]])
  | Store (_, src, Gid gid) ->
    (op_to_rax src) :: Asm.([Movq, [~%Rax; Ind3 (Lbl (Platform.mangle gid), Rip)]])
  | Store (_, _, _) -> failwith "store destination was not a local or global id"

  (* Treat LL i1 values as words, so zero-out the rest of the bits *)
  | Icmp (cnd, _, op1, op2) -> (op_to_rax op1) ::
                               (op_to_rcx op2) ::
                               Asm.([ Cmpq, [~%Rcx; ~%Rax]
                                    ; (Set (compile_cnd cnd)), [dst]
                                    ; Andq, [imm_of_int 1; dst] ])

  | Call(ret_ty, fop, args) ->
    let code = compile_call ctxt fop args in
    code @
    (match ret_ty with
     | Void -> []
     | _ ->  Asm.([Movq, [~%Rax; dst]]))

  (* Bitcast is effectively just a Mov at the assembly level *)
  | Bitcast (_, op, _) -> (op_to_rax op) :: Asm.([Movq, [~%Rax; dst]])

  (* Defer to the helper function to compute the pointer value *)
  | Gep (t, op, path) ->
    let code = compile_gep ctxt (t, op) path in
    code @ Asm.([ Movq, [~%Rax; dst] ])


(* compiling terminators  --------------------------------------------------- *)

(* prefix the function name [fn] to a label to ensure that the X86 labels are 
   globally unique . *)
let mk_lbl (fn:string) (l:string) = fn ^ "." ^ l

(* Compile block terminators is not too difficult:

   - Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.

   - Br should jump

   - Cbr branch should treat its operand as a boolean conditional
*)
let compile_terminator (fn:string) (ctxt:ctxt) (t:Ll.terminator) : ins list =
  let epilogue = Asm.([ Movq, [~%Rbp; ~%Rsp]
                     ; Popq, [~%Rbp]
                     ; Retq, []])
  in match t with
    | Ll.Ret (_, None) -> epilogue
    | Ll.Ret (_, Some o) -> (compile_operand ctxt (Reg Rax) o) :: epilogue
    | Ll.Br l -> Asm.([ Jmp, [~$$(mk_lbl fn l)] ])
    | Ll.Cbr (o, l1, l2) -> (compile_operand ctxt (Reg Rax) o)
                         :: Asm.([ Cmpq, [~$0; ~%Rax]
                                ; J (X86.Neq) , [~$$(mk_lbl fn l1)]
                                ; Jmp, [~$$(mk_lbl fn l2)]
                                ])

(* compiling blocks --------------------------------------------------------- *)

let compile_block (fn:string) (ctxt:ctxt) (blk:Ll.block) : ins list =
  let insns = List.map (compile_insn ctxt) blk.insns |> List.flatten in
  let term = compile_terminator fn ctxt (snd blk.term) in
  insns @ term

let compile_lbl_block fn lbl ctxt blk : elem =
  Asm.text (mk_lbl fn lbl) (compile_block fn ctxt blk)



(* compile_fdecl ------------------------------------------------------------ *)
let rbp_offset n = Ind3 (Lit (Int64.of_int @@ 8 * n), Rbp)

(* This helper function computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions.  You might find it useful for
   compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)
let arg_loc (n : int) : operand =
  begin match arg_reg n with
    | Some op -> op
    | None -> rbp_offset (n-4)
  end

(* We suggest that you create a helper function that computes the 
   stack layout for a given function declaration.

   - each function argument should be copied into a stack slot
   - in this (inefficient) compilation strategy, each local id 
     is also stored as a stack slot.
   - see the discussion about locals 

*)
let stack_layout (args : uid list) ((block, lbled_blocks):cfg) : layout =
  let lbled_block_isns = List.map (fun (_, blk) -> blk.insns) lbled_blocks in
  let cfg_uids = List.map fst (block.insns @ (List.flatten lbled_block_isns)) in
  List.mapi (fun i uid -> (uid, rbp_offset (-i - 1))) (args @ cfg_uids)

(* The code for the entry-point of a function must do several things:

   - since our simple compiler maps local %uids to stack slots,
     compiling the control-flow-graph body of an fdecl requires us to
     compute the layout (see the discussion of locals and layout)

   - the function code should also comply with the calling
     conventions, typically by moving arguments out of the parameter
     registers (or stack slots) into local storage space.  For our
     simple compilation strategy, that local storage space should be
     in the stack. (So the function parameters can also be accounted
     for in the layout.)

   - the function entry code should allocate the stack storage needed
     to hold all of the local stack slots.
*)
let compile_fdecl (tdecls:(tid * ty) list) (name:string) ({ f_ty; f_param; f_cfg }:fdecl) : prog =
  let entry_name = (Platform.mangle name) in
  let layout = stack_layout f_param f_cfg in
  let init_arg_code =
    (List.mapi (fun i uid -> Asm.([ Movq, [arg_loc i; ~%Rax]
                                ; Movq, [~%Rax; lookup layout uid] ]) )
      f_param) |> List.flatten
  in
  let ctxt = { tdecls; layout } in
  let tmpsize = 8 * (List.length layout) in

  let prologue = Asm.([ Pushq, [~%Rbp]
                     ; Movq, [~%Rsp; ~%Rbp]
                     ; Subq, [~$tmpsize; ~%Rsp] ])
                   @ init_arg_code
  in
  let (entry, body) = f_cfg in
  let entry_insns = compile_block name ctxt entry in
  (Asm.gtext entry_name @@ prologue @ entry_insns) ::
  (List.map (fun (lbl, blk) -> compile_lbl_block name lbl ctxt blk) body)


(* compile_gdecl ------------------------------------------------------------ *)
(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label.
*)
let rec compile_ginit : ginit -> X86.data list = function
  | GNull     -> [Quad (Lit 0L)]
  | GGid gid  -> [Quad (Lbl (Platform.mangle gid))]
  | GInt c    -> [Quad (Lit c)]
  | GString s -> [Asciz s]
  | GArray gs | GStruct gs -> List.map compile_gdecl gs |> List.flatten
  | GBitcast (t1,g,t2) -> compile_ginit g

and compile_gdecl (_, g) = compile_ginit g


(* compile_prog ------------------------------------------------------------- *)
let compile_prog {tdecls; gdecls; fdecls} : X86.prog =
  let g = fun (lbl, gdecl) -> Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let f = fun (name, fdecl) -> compile_fdecl tdecls name fdecl in
  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)
