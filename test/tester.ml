open X86_lib.X86
include X86_lib
open Driver
open Ll_lib.Ll
include Ll_lib
open Backend
open Datastructures
open Util
open Util.Assert
include Platform
include Optimizer

let adj_path = "../../../"

let runtime_files =
  List.map (fun s -> adj_path ^ s) [ "include/runtime.c"; "include/channels.c" ]

let _ = Platform.configure_os ()

let exec_ll_ast path ll_ast args extra_files =
  let () = Platform.verb @@ Printf.sprintf "** exec_ll_ast: %s\n" path in

  let output_path = adj_path ^ !Platform.output_path in

  (* First - optimize the ll ast *)
  let _ = Opt.do_opt := true in
  let ll_ast = Opt.optimize ll_ast in

  (* Write out the optimized ll file for debugging purposes *)
  let ll_str = Driver.string_of_ll_ast path ll_ast in
  let dot_ll_file = Platform.gen_name output_path "test" ".ll" in
  let () = write_file dot_ll_file ll_str in

  (* Run the ll backend *)
  let _ = Backend.set_liveness "dataflow" in
  let _ = Backend.set_regalloc "better" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in

  (* Write out the resulting .s file for debugging purposes *)
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let _ = Driver.write_file dot_s_file asm_str in

  (* Create the executable *)
  let exec_file = Platform.gen_name output_path "exec" "" in
  let _ = Platform.link (dot_s_file :: extra_files) exec_file in

  (* Run it, piping the output to a temporary file *)
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in
  let result = Driver.run_program args exec_file tmp_file in
  let () =
    Platform.sh
      (Printf.sprintf "rm -f %s %s %s" dot_ll_file exec_file tmp_file)
      Platform.ignore_error
  in
  let () =
    Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result
  in
  result

let exec_ll_file path args =
  let ast = Driver.parse_ll_file path in
  exec_ll_ast path ast args [ adj_path ^ "include/cinterop.c" ]

let oat_file_e2e_test path args =
  let () = Platform.verb @@ Printf.sprintf "** oat_file_e2e_test: %s\n" path in
  (* Run the Oat typechecker and frontend *)
  let oat_ast = parse_oat_file path in
  Typechecker.typecheck_program oat_ast;
  let ll_ast = Frontend.cmp_prog oat_ast in
  exec_ll_ast path ll_ast args runtime_files

let execute_ll_file_with_output args path ans =
  exec_ll_file (adj_path ^ path) args = ans

let execute_oat_file_with_output (path, args, ans) =
  oat_file_e2e_test (adj_path ^ path) args = ans

let compile_with_config live regalloc ll_ast =
  let open Registers in
  let open Backend in
  let _ = set_liveness live in
  let _ = set_regalloc regalloc in
  let asm_ast = compile_prog ll_ast in
  let histogram, size = histogram_of_prog asm_ast in
  (histogram, size, asm_ast)

(* let assert_quality fn ll_ast =
   if not !pass_all then failwith "Your register allocator failed a correctness test" else
   let _ = Opt.do_opt := true in
   let ll_ast = Opt.optimize ll_ast in
   let h_greedy, size_greedy, x86_greedy = compile_with_config "dataflow" "greedy" ll_ast in
   let h_better, size_better, x86_better = compile_with_config "dataflow" "better" ll_ast in
   let mem_greedy = Registers.memop_of_prog x86_greedy in
   let mem_better = Registers.memop_of_prog x86_better in
   let _ =
    if !Driver.print_regs_flag then begin
      Printf.printf "greedy sz: %4d mem: %4d\t\tbetter sz: %4d mem: %4d \t diff_sz: %4d diff_mem: %4d - %s\n"
      size_greedy mem_greedy size_better mem_better (size_greedy - size_better) (mem_greedy - mem_better) fn
    end
   in
   if
    mem_better < mem_greedy then ()
   else if
    size_better < size_greedy then ()
   else failwith @@ Printf.sprintf "greedy is better"

   let assert_quality_oat fn () =
   let oat_ast = parse_oat_file fn in
   let ll_ast = Frontend.cmp_prog oat_ast in
   assert_quality fn ll_ast

   let quality_oat tests =
   List.map (fun (fn, _, _) -> fn, assert_quality_oat fn) tests *)

let fdecl_of_path path =
  Platform.verb @@ Printf.sprintf "* processing file: %s\n" path;
  let ll_ast = parse_ll_file path in
  match ll_ast.fdecls with
  | [ (_, fdecl) ] -> fdecl
  | _ -> failwith "test expected one fdecl"

let ll_dfa_file_test path compare analyze expected =
  let fdecl = fdecl_of_path path in
  let dfa = analyze (Cfg.of_ast fdecl) in
  compare dfa expected

let throw_key_diff compare val_to_string a b =
  let keys = LblM.diff_keys compare a b in
  if List.length keys == 0 then true
  else
    let str_a = LblM.to_string val_to_string a in
    let str_b = LblM.to_string val_to_string b in
    print_string
    @@ Printf.sprintf "Output differs at labels: %s in maps\n%s\n%s\n"
         (String.concat ", " keys) str_a str_b;
    false

let ll_opt_file_test path optimize ans =
  let fdecl = fdecl_of_path path in
  let expected = (Cfg.of_ast @@ fdecl_of_path ans).Cfg.blocks in
  let opt = optimize (Cfg.of_ast fdecl) in
  let printer k b =
    Printf.sprintf "%s %s" (Lbl.to_string k) (Llutil.string_of_block b)
  in
  throw_key_diff Llutil.compare_block printer opt expected

let dfa_liveness_file (tests : (string * 'a Datastructures.LblM.t) list) =
  let open Liveness in
  let analyze f = Graph.dfa (analyze f) in
  let printer k s =
    Printf.sprintf "%s %s" (Lbl.to_string k) (UidS.to_string s)
  in
  List.map
    (fun (path, ans) ->
      ( "liveness: " ^ path,
        fun () ->
          ll_dfa_file_test path
            (throw_key_diff Fact.compare printer)
            analyze ans ))
    tests

let dfa_alias_file tests =
  let open Alias in
  let analyze f = Graph.dfa (analyze f) in
  let printer k f =
    Printf.sprintf "%s %s" (Lbl.to_string k) (Alias.Fact.to_string f)
  in
  List.map
    (fun (path, ans) ->
      ( "alias: " ^ path,
        fun () ->
          ll_dfa_file_test path
            (throw_key_diff Fact.compare printer)
            analyze ans ))
    tests

let dfa_constprop_file tests =
  let open Constprop in
  let analyze f = Graph.dfa (analyze f) in
  let printer k f =
    Printf.sprintf "%s %s" (Lbl.to_string k) (Constprop.Fact.to_string f)
  in
  List.map
    (fun (path, ans) ->
      ( "constprop: " ^ path,
        fun () ->
          ll_dfa_file_test path
            (throw_key_diff Fact.compare printer)
            analyze ans ))
    tests

let execute_opt_dce_file input output =
  let opt g =
    let ag = Alias.analyze g in
    let lg = Liveness.analyze g in
    let g = Dce.run lg ag g in
    g.Cfg.blocks
  in
  ll_opt_file_test (adj_path ^ input) opt (adj_path ^ output)

let execute_opt_constfold_file input output =
  let opt g =
    let cg = Constprop.analyze g in
    let g = Constprop.run cg g in
    g.Cfg.blocks
  in
  ll_opt_file_test (adj_path ^ input) opt (adj_path ^ output)

(* this test harness is used for part iv of the homework -------------------- *)
let executed_fullopt_file tests =
  let opt n g =
    let g = Opt.pass n g in
    g.Cfg.blocks
  in
  List.map
    (fun (n, path, ans) ->
      ( Printf.sprintf "fullopt %d iterations: %s" n path,
        fun () -> ll_opt_file_test path (opt n) ans ))
    tests

(*
 * LLVM TESTS
 *)

(* binop_tests *)
let%test _ = execute_ll_file_with_output "" "test/llprograms/add.ll" "14"

let%test _ = execute_ll_file_with_output "" "test/llprograms/sub.ll" "1"

let%test _ = execute_ll_file_with_output "" "test/llprograms/mul.ll" "45"

let%test _ = execute_ll_file_with_output "" "test/llprograms/and.ll" "0"

let%test _ = execute_ll_file_with_output "" "test/llprograms/or.ll" "1"

let%test _ = execute_ll_file_with_output "" "test/llprograms/xor.ll" "0"

let%test _ = execute_ll_file_with_output "" "test/llprograms/shl.ll" "168"

let%test _ = execute_ll_file_with_output "" "test/llprograms/lshr.ll" "10"

let%test _ = execute_ll_file_with_output "" "test/llprograms/ashr.ll" "5"

(* calling convention tests *)
let%test _ = execute_ll_file_with_output "" "test/llprograms/call.ll" "42"

let%test _ = execute_ll_file_with_output "" "test/llprograms/call1.ll" "17"

let%test _ = execute_ll_file_with_output "" "test/llprograms/call2.ll" "19"

let%test _ = execute_ll_file_with_output "" "test/llprograms/call3.ll" "34"

let%test _ = execute_ll_file_with_output "" "test/llprograms/call4.ll" "34"

let%test _ = execute_ll_file_with_output "" "test/llprograms/call5.ll" "24"

let%test _ = execute_ll_file_with_output "" "test/llprograms/call6.ll" "26"

let%test _ = execute_ll_file_with_output "" "test/llprograms/call7.ll" "7"

let%test _ = execute_ll_file_with_output "" "test/llprograms/call8.ll" "21"

(* memory_tests *)
let%test _ = execute_ll_file_with_output "" "test/llprograms/alloca1.ll" "17"

let%test _ = execute_ll_file_with_output "" "test/llprograms/alloca2.ll" "17"

let%test _ = execute_ll_file_with_output "" "test/llprograms/global1.ll" "12"

(* terminator_tests *)
let%test _ = execute_ll_file_with_output "" "test/llprograms/return.ll" "0"

let%test _ = execute_ll_file_with_output "" "test/llprograms/return42.ll" "42"

let%test _ = execute_ll_file_with_output "" "test/llprograms/br1.ll" "9"

let%test _ = execute_ll_file_with_output "" "test/llprograms/br2.ll" "17"

let%test _ = execute_ll_file_with_output "" "test/llprograms/cbr1.ll" "7"

let%test _ = execute_ll_file_with_output "" "test/llprograms/cbr2.ll" "9"

let%test _ = execute_ll_file_with_output "" "test/llprograms/cbr3.ll" "9"

(* bitcast_tests *)
let%test _ = execute_ll_file_with_output "" "test/llprograms/bitcast1.ll" "3"

(* gep_tests *)
let%test _ = execute_ll_file_with_output "" "test/llprograms/gep1.ll" "6"

let%test _ = execute_ll_file_with_output "" "test/llprograms/gep2.ll" "4"

let%test _ = execute_ll_file_with_output "" "test/llprograms/gep3.ll" "1"

let%test _ = execute_ll_file_with_output "" "test/llprograms/gep4.ll" "2"

let%test _ = execute_ll_file_with_output "" "test/llprograms/gep5.ll" "4"

let%test _ = execute_ll_file_with_output "" "test/llprograms/gep6.ll" "7"

let%test _ = execute_ll_file_with_output "" "test/llprograms/gep7.ll" "7"

let%test _ = execute_ll_file_with_output "" "test/llprograms/gep8.ll" "2"

let%test _ = execute_ll_file_with_output "" "test/llprograms/gep9.ll" "5"

let%test _ = execute_ll_file_with_output "" "test/llprograms/gep10.ll" "3"

(* arithmetic_tests *)
let%test _ = execute_ll_file_with_output "" "test/llprograms/add_twice.ll" "29"

let%test _ = execute_ll_file_with_output "" "test/llprograms/sub_neg.ll" "255"
(* Why, oh why, does the termianl only report the last byte? *)

let%test _ = execute_ll_file_with_output "" "test/llprograms/arith_combo.ll" "4"

let%test _ =
  execute_ll_file_with_output "" "test/llprograms/return_intermediate.ll" "18"

let%test _ = execute_ll_file_with_output "" "test/llprograms/sum_tree.ll" "116"

let%test _ =
  execute_ll_file_with_output "" "test/llprograms/gcd_euclidian.ll" "2"

let%test _ = execute_ll_file_with_output "" "test/llprograms/sieve.ll" "1"

let%test _ =
  execute_ll_file_with_output "" "test/llprograms/binarysearch.ll" "8"

let%test _ = execute_ll_file_with_output "" "test/llprograms/qtree.ll" "3"

let%test _ = execute_ll_file_with_output "" "test/llprograms/binary_gcd.ll" "3"

let%test _ =
  execute_ll_file_with_output "" "test/llprograms/linear_search.ll" "1"

let%test _ = execute_ll_file_with_output "" "test/llprograms/lfsr.ll" "108"

let%test _ =
  execute_ll_file_with_output "" "test/llprograms/naive_factor_prime.ll" "1"

let%test _ =
  execute_ll_file_with_output "" "test/llprograms/naive_factor_nonprime.ll" "0"

let%test _ = execute_ll_file_with_output "" "test/llprograms/euclid.ll" "2"

let%test _ = execute_ll_file_with_output "" "test/llprograms/matmul.ll" "0"

let%test _ = execute_ll_file_with_output "" "test/llprograms/list1.ll" "3"

let%test _ = execute_ll_file_with_output "" "test/llprograms/cbr.ll" "42"

let%test _ = execute_ll_file_with_output "" "test/llprograms/factorial.ll" "120"

let%test _ = execute_ll_file_with_output "" "test/llprograms/factrect.ll" "120"

(*
 * OAT TESTS
 *)

(* greedy_is_good_tests *)
let%test _ =
  execute_oat_file_with_output ("test/hw4programs/easyrun1.oat", "", "17")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/easyrun2.oat", "", "35")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/easyrun5.oat", "", "212")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/easyrun6.oat", "", "9")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/easyrun7.oat", "", "23")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/easyrun8.oat", "", "160")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/path1.oat", "", "17")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run26.oat", "", "0")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run27.oat", "", "99")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run29.oat", "", "1")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run30.oat", "", "9")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run31.oat", "", "9")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run13.oat", "", "1")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run38.oat", "", "31")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run40.oat", "", "8")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run60.oat", "", "85")

let%test _ = execute_oat_file_with_output ("test/hw4programs/heap.oat", "", "1")

let%test _ = execute_oat_file_with_output ("test/hw5programs/ifq2.oat", "", "5")

let%test _ =
  execute_oat_file_with_output ("test/hw5programs/length1.oat", "", "5")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/lcs.oat", "", "OAT0")

(* hw4_easiest_tests *)
let%test _ =
  execute_oat_file_with_output ("test/hw4programs/easyrun3.oat", "", "73")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/easyrun4.oat", "", "6")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/easyrun9.oat", "", "236")

(* Should not be used for quality tests *)

(* hw4_globals_tests *)
let%test _ =
  execute_oat_file_with_output ("test/hw4programs/globals1.oat", "", "42")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/globals2.oat", "", "17")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/globals3.oat", "", "17")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/globals4.oat", "", "5")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/globals5.oat", "", "17")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/globals6.oat", "", "15")

(* hw4_path_tests *)
let%test _ =
  execute_oat_file_with_output ("test/hw4programs/path2.oat", "", "35")

let%test _ = execute_oat_file_with_output ("test/hw4programs/path3.oat", "", "3")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/arrayargs1.oat", "", "17")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/arrayargs2.oat", "", "17")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/arrayargs4.oat", "", "0")

(* hw4_easy_tests *)
let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run28.oat", "", "18")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run32.oat", "", "33")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run21.oat", "", "99")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run33.oat", "", "1")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run34.oat", "", "66")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run39.oat", "a", "2")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run42.oat", "", "2")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run49.oat", "", "abc0")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run50.oat", "", "abcde0")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run61.oat", "", "3410")

(* hw4_medium_tests *)
let%test _ =
  execute_oat_file_with_output ("test/hw4programs/fact.oat", "", "1200")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run1.oat", "", "153")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run2.oat", "", "6")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run8.oat", "", "2")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run9.oat", "", "4")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run10.oat", "", "5")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run11.oat", "", "7")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run14.oat", "", "16")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run15.oat", "", "19")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run16.oat", "", "13")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run22.oat", "", "abc0")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run23.oat", "", "1230")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run25.oat", "", "nnn0")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run46.oat", "", "420")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run47.oat", "", "3")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run48.oat", "", "11")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/lib4.oat", "", "53220")

let%test _ = execute_oat_file_with_output ("test/hw4programs/lib5.oat", "", "20")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/lib6.oat", "", "56553")

let%test _ = execute_oat_file_with_output ("test/hw4programs/lib7.oat", "", "53")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/lib8.oat", "", "Hello world!0")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/lib9.oat", "a b c d", "abcd5")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/lib11.oat", "", "45")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/lib14.oat", "", "~}|{zyxwvu0")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/lib15.oat", "123456789", "456780")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/regalloctest.oat", "", "0")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/regalloctest2.oat", "", "137999986200000000")

(* hw4_hard_tests *)
let%test _ = execute_oat_file_with_output ("test/hw4programs/fac.oat", "", "120")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/bsort.oat", "", "y}xotnuw notuwxy}255")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/msort.oat", "", "~}|{zyxwvu uvwxyz{|}~ 0")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/msort2.oat", "", "~}|{zyxwvu uvwxyz{|}~ 0")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/selectionsort.oat", "", "01253065992000")

let%test _ =
  execute_oat_file_with_output
    ( "test/hw4programs/matrixmult.oat",
      "",
      "19 16 13 23 \t5 6 7 6 \t19 16 13 23 \t5 6 7 6 \t0" )

(* hw4_old_student_tests *)
let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/binary_search.oat", "", "Correct!0")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/xor_shift.oat", "", "838867572\n22817190600")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/sieve.oat", "", "25")

let%test _ = execute_oat_file_with_output ("test/hw4programs/fibo.oat", "", "0")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/lfsr.oat", "", "TFTF FFTT0")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/gnomesort.oat", "", "01253065992000")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/josh_joyce_test.oat", "", "0")

let%test _ = execute_oat_file_with_output ("test/hw4programs/gcd.oat", "", "16")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/insertion_sort.oat", "", "42")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/maxsubsequence.oat", "", "107")

(* struct_tests *)
let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_assign_struct.oat", "", "16")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_basic_struct.oat", "", "7")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_global_struct.oat", "", "254")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_nested_struct.oat", "", "10")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_return_struct.oat", "", "0")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_struct_array.oat", "", "15")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_struct_fptr.oat", "", "7")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_various_fields.oat", "", "hello253")

(* fptr_tests *)
let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_array_fptr.oat", "", "2")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_func_argument.oat", "", "4")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_global_fptr.oat", "", "7")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_global_fptr_unordered.oat", "", "2")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_scall_fptr.oat", "", "4")

let%test _ =
  execute_oat_file_with_output ("test/hw5programs/compile_var_fptr.oat", "", "1")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_local_fptr.oat", "", "5")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_function_shadow.oat", "", "12")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_global_struct_fptr.oat", "", "20")

let%test _ =
  execute_oat_file_with_output
    ("test/hw5programs/compile_builtin_argument.oat", "", "abab0")

(* regalloc_challenge_tests *)
let%test _ =
  execute_oat_file_with_output ("test/hw4programs/arrayargs3.oat", "", "34")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run41.oat", "", "3")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run51.oat", "", "341")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run52.oat", "", "15")

let%test _ =
  execute_oat_file_with_output ("test/hw4programs/run54.oat", "", "10")

let%test _ = execute_oat_file_with_output ("test/hw4programs/run55.oat", "", "6")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/qsort.oat", "", "kpyf{shomfhkmopsy{255")

let%test _ =
  execute_oat_file_with_output
    ("test/hw4programs/count_sort.oat", "", "AFHZAAEYC\nAAACEFHYZ0")

(* new_tests *)
let%test _ = execute_oat_file_with_output ("test/hw5programs/ifq1.oat", "", "4")

let%test _ =
  execute_oat_file_with_output ("test/hw5programs/length2.oat", "", "3")

let%test _ =
  execute_oat_file_with_output ("test/hw5programs/initarr1.oat", "", "1")

let%test _ =
  execute_oat_file_with_output ("test/hw5programs/initarr2.oat", "", "2")

(*
 * DCE TESTS
 *)

(* dce_opt_tests *)
let%test _ =
  execute_opt_dce_file "test/llprograms/analysis1_cf_opt.ll"
    "test/llprograms/analysis1_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis2_cf_opt.ll"
    "test/llprograms/analysis2_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis3_cf_opt.ll"
    "test/llprograms/analysis3_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis4_cf_opt.ll"
    "test/llprograms/analysis4_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis5_cf_opt.ll"
    "test/llprograms/analysis5_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis6_cf_opt.ll"
    "test/llprograms/analysis6_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis7_cf_opt.ll"
    "test/llprograms/analysis7_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis8_cf_opt.ll"
    "test/llprograms/analysis8_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis9_cf_opt.ll"
    "test/llprograms/analysis9_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis10_cf_opt.ll"
    "test/llprograms/analysis10_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis11_cf_opt.ll"
    "test/llprograms/analysis11_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis12_cf_opt.ll"
    "test/llprograms/analysis12_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis13_cf_opt.ll"
    "test/llprograms/analysis13_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis14_cf_opt.ll"
    "test/llprograms/analysis14_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis15_cf_opt.ll"
    "test/llprograms/analysis15_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis16_cf_opt.ll"
    "test/llprograms/analysis16_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis17_cf_opt.ll"
    "test/llprograms/analysis17_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis18_cf_opt.ll"
    "test/llprograms/analysis18_dce_opt.ll"

let%test _ =
  execute_opt_dce_file "test/llprograms/analysis19_cf_opt.ll"
    "test/llprograms/analysis19_dce_opt.ll"

(* constprop_opt_tests *)
let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis1.ll"
    "test/llprograms/analysis1_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis2.ll"
    "test/llprograms/analysis2_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis3.ll"
    "test/llprograms/analysis3_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis4.ll"
    "test/llprograms/analysis4_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis5.ll"
    "test/llprograms/analysis5_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis6.ll"
    "test/llprograms/analysis6_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis7.ll"
    "test/llprograms/analysis7_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis8.ll"
    "test/llprograms/analysis8_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis9.ll"
    "test/llprograms/analysis9_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis10.ll"
    "test/llprograms/analysis10_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis11.ll"
    "test/llprograms/analysis11_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis12.ll"
    "test/llprograms/analysis12_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis13.ll"
    "test/llprograms/analysis13_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis14.ll"
    "test/llprograms/analysis14_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis15.ll"
    "test/llprograms/analysis15_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis16.ll"
    "test/llprograms/analysis16_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis17.ll"
    "test/llprograms/analysis17_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis18.ll"
    "test/llprograms/analysis18_cf_opt.ll"

let%test _ =
  execute_opt_constfold_file "test/llprograms/analysis19.ll"
    "test/llprograms/analysis19_cf_opt.ll"
