open Util

let%test _ = oat_tc_ok_test "test/coatprograms/easy.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/ping.oat"

let%test _ = oat_tc_err_test "test/coatprograms/easier.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/tc_ok_1.oat"

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_1.oat";
  [%expect{| [2, 2] Linear types not consumed at end of block |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_2.oat";
  [%expect{| [6, 26] Usage of moved value |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_3.oat";
  [%expect{| [8, 2] Cannot redeclare variable |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_4.oat";
  [%expect{| [1, 0] Structs cannot contain linear types |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_5.oat";
  [%expect{| [7, 2] Both branches in an if-conditional has to consume the same resources |}]
