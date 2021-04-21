open Util

let%test _ = oat_tc_ok_test "test/coatprograms/easy.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/ping.oat"

let%test _ = oat_tc_err_test "test/coatprograms/easier.oat"

let%test _ = oat_tc_ok_test "test/coatprograms/tc_ok_1.oat"

let%test _ = oat_tc_err_test "test/coatprograms/tc_err_1.oat"

let%test _ = oat_tc_err_test "test/coatprograms/tc_err_2.oat"

let%test _ = oat_tc_err_test "test/coatprograms/tc_err_3.oat"
