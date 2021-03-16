open Assert
open Ast
open Tctxt

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let fields_1 =
  [
    { fieldName = "f1"; ftyp = TInt };
    { fieldName = "f2"; ftyp = TInt };
    { fieldName = "f3"; ftyp = TBool };
  ]

let fields_2 =
  [ { fieldName = "f1"; ftyp = TInt }; { fieldName = "f2"; ftyp = TInt } ]

let fields_3 =
  [ { fieldName = "f1"; ftyp = TInt }; { fieldName = "f3"; ftyp = TInt } ]

let id_1 = "some_struct1"

let id_2 = "some_struct2"

let id_3 = "some_struct3"

let struct_context1 =
  add_struct (add_struct Tctxt.empty id_1 fields_1) id_2 fields_2

let struct_context2 =
  add_struct (add_struct Tctxt.empty id_1 fields_1) id_3 fields_3

let provided_tests : suite =
  [
    Test
      ( "Some Sick Unit Tests For Subtype Function",
        [
          ( "subtype1",
            fun () ->
              if Typechecker.subtype Tctxt.empty TInt TInt then ()
              else failwith "should not fail" );
          ( "subtype2",
            fun () ->
              if Typechecker.subtype Tctxt.empty TBool TBool then ()
              else failwith "should not fail" );
          ( "subtype3",
            fun () ->
              if
                Typechecker.subtype Tctxt.empty (TNullRef (RArray TInt))
                  (TNullRef (RArray TInt))
              then ()
              else failwith "should not fail" );
          ( "subtype4",
            fun () ->
              if
                Typechecker.subtype struct_context1 (TRef (RStruct id_1))
                  (TNullRef (RStruct id_2))
              then ()
              else failwith "should not fail" );
          ( "subtype5",
            fun () ->
              if
                Typechecker.subtype struct_context1
                  (TRef (RFun ([ TRef (RStruct id_2) ], RetVoid)))
                  (TRef (RFun ([ TRef (RStruct id_1) ], RetVoid)))
              then ()
              else failwith "should not fail" );
          ( "notsubtype1",
            fun () ->
              if Typechecker.subtype Tctxt.empty TBool TInt then
                failwith "should not succeed"
              else () );
          ( "notsubtype2",
            fun () ->
              if
                Typechecker.subtype Tctxt.empty (TNullRef (RArray TInt))
                  (TNullRef (RArray TBool))
              then failwith "should not succeed"
              else () );
          ( "notsubtype3",
            fun () ->
              if
                Typechecker.subtype struct_context2 (TRef (RStruct id_1))
                  (TNullRef (RStruct id_3))
              then failwith "should not succeed"
              else () );
          ( "notsubtype4",
            fun () ->
              if
                Typechecker.subtype struct_context1 (TRef (RStruct id_2))
                  (TNullRef (RStruct id_1))
              then failwith "should not succeed"
              else () );
          ( "notsubtype5",
            fun () ->
              if
                Typechecker.subtype Tctxt.empty
                  (TRef (RFun ([], RetVal TInt)))
                  (TRef (RFun ([], RetVal TBool)))
              then failwith "should not succeed"
              else () );
          ( "notsubtype6",
            fun () ->
              if
                Typechecker.subtype struct_context1
                  (TRef (RFun ([ TRef (RStruct id_1) ], RetVoid)))
                  (TRef (RFun ([ TRef (RStruct id_2) ], RetVoid)))
              then failwith "should not succeed"
              else () );
        ] );
    Test
      ( "Kento and Will Piazza Test Cases",
        [
          ( "Equality operator match",
            fun () ->
              let res : Ast.ty =
                try
                  Typechecker.typecheck_exp Tctxt.empty
                    (no_loc (Bop (Eq, no_loc (CInt 5L), no_loc (CInt 3L))))
                with Typechecker.TypeError _ -> failwith "should not fail"
              in
              match res with TBool -> () | _ -> failwith "expected TInt" );
          ( "Equality operator mismatch",
            fun () ->
              let res = ref 0 in
              let _ =
                try
                  Typechecker.typecheck_exp Tctxt.empty
                    (no_loc (Bop (Eq, no_loc (CInt 5L), no_loc (CBool true))))
                with Typechecker.TypeError _ ->
                  res := 1;
                  TInt
              in
              if !res = 1 then () else failwith "shoud not succeed" );
        ] );
  ]
