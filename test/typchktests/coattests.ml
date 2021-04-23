open Util

let%test _ = oat_tc_ok_test "test/coatprograms/easy.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/ping.oat"

let%test _ = oat_tc_err_test "test/coatprograms/easier.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/tc_ok_1.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/tc_ok_2.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/tc_ok_3.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/tc_ok_4.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/tc_ok_5.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/tc_ok_6.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/tc_ok_7.oat"

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_1.oat";
  [%expect {| [2, 2] Linear types not consumed at end of block |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_2.oat";
  [%expect {| [6, 26] Usage of moved value |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_3.oat";
  [%expect {| [8, 2] Cannot redeclare variable |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_4.oat";
  [%expect {| [1, 0] Structs cannot contain linear types |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_5.oat";
  [%expect
    {| [7, 2] Both branches in an if-conditional has to consume the same resources |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_6.oat";
  [%expect {|[5, 14] Cannot receive on channel that has writes remaining|}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_7.oat";
  [%expect {|[1, 0] Multiplicities can either be 0, 1, or *|}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_8.oat";
  [%expect {|[4, 2] single-use channel possibly consumed multiple times|}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_9.oat";
  [%expect {|[9, 2] Incorrect type of argument|}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_10.oat";
  [%expect {| [10, 12] spawn: channel usage incorrect |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_11.oat";
  [%expect {| [11, 12] spawn: channel usage incorrect |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_12.oat";
  [%expect {| [11, 12] Usage of moved value |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_13.oat";
  [%expect {| [3, 12] Cannot receive on channel type without reads |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_14.oat";
  [%expect {| [3, 12] Cannot send on channel type without sends |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_15.oat";
  [%expect {| [5, 12] Arrays cannot contain linear types |}]

let%expect_test _ =
  expect_coat_tc_err_test "test/coatprograms/tc_err_16.oat";
  [%expect {| [5, 4] single-use channel possibly consumed multiple times |}]
