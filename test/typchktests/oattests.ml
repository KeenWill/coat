open Util

let%test _ = oat_tc_ok_test "test/hw5programs/tc_subtyping1.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_subtyping2.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_subtyping3.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_subtyping4.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_subtyping5.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_subtyping6.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_subtyping7.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_subtyping8.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_subtyping9.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_subtyping_err1.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_subtyping_err2.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_subtyping_err3.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_subtyping_err4.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_subtyping_err5.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_subtyping_err6.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_subtyping_err7.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_subtyping_err8.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_early_return.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_early_return_void.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_return_wrong.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_while_nonbool.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_while.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_if_nonbool.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_if.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_for.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_void.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_assign_void.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_scall_nonvoid.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_while.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_for.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_if.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_void.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_binop1.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_binop2.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_binop3.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_call1.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_call2.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_unop1.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_array1.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_array2.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_array3.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_array4.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_null.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_struct_proj.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_struct1.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_struct2.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_struct3.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_struct4.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_struct_dup.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_struct.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_dupstruct.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_struct_unbound.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_global_dup.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_global.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_func_redeclaration.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_func_assign.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_overwrite.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_error_function_no_shadow.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_correct_null.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_array.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_array2.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_array3.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_call.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_fptr.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_global_fptr_scope.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_global.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_struct.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_struct_fptr.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_void.oat"

let%test _ =
  oat_tc_ok_test "test/hw5programs/tc_correct_local_redeclaration.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_correct_fptr_array.oat"

let%test _ = oat_tc_err_test "test/hw4programs/run3.oat"

let%test _ = oat_tc_err_test "test/hw4programs/run5.oat"

let%test _ = oat_tc_err_test "test/hw4programs/run35.oat"

let%test _ = oat_tc_err_test "test/hw4programs/run43.oat"

let%test _ = oat_tc_err_test "test/hw4programs/run44.oat"

let%test _ = oat_tc_err_test "test/hw4programs/run45.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_eq1.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_eq2.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_struct_ok.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_func_ret_ok.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_func_arg_ok.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_ifq1.oat"

let%test _ = oat_tc_ok_test "test/hw4programs/tc_ok1.oat"

let%test _ = oat_tc_ok_test "test/hw4programs/tc_ok2.oat"

let%test _ = oat_tc_ok_test "test/hw4programs/tc_ok4.oat"

let%test _ = oat_tc_ok_test "test/hw4programs/tc_ok5.oat"

let%test _ = oat_tc_ok_test "test/hw4programs/tc_ok6.oat"

let%test _ = oat_tc_ok_test "test/hw4programs/tc_ok7.oat"

let%test _ = oat_tc_ok_test "test/hw4programs/tc_ok8.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_arrow.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_arrow_null.oat"

let%test _ = oat_tc_ok_test "test/hw5programs/tc_arrow_null_rec.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_null_array_err.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_struct_err.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_func_ret_err.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_func_arg_err.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_array_err.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_struct_field_err.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_recursive_struct_err.oat"

let%test _ = oat_tc_err_test "test/hw5programs/tc_ifq_err1.oat"
