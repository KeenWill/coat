let adj_path = "../../../../"

let oat_tc_ok_test path =
  let oat_ast = Driver.parse_oat_file (adj_path ^ path) in
  let _ = Typechecker.typecheck_program oat_ast in
  true

let oat_tc_err_test path =
  let oat_ast = Driver.parse_oat_file (adj_path ^ path) in
  try
    let _ = Typechecker.typecheck_program oat_ast in
    false
  with Typechecker.TypeError _ -> true

let expect_coat_tc_err_test path =
  let oat_ast = Driver.parse_oat_file (adj_path ^ path) in
  try
    let _ = Typechecker.typecheck_program oat_ast in
    print_endline ""
  with Typechecker.TypeError s -> print_endline s
