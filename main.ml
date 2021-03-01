open Ll
open Arg
open Assert
open Driver

exception Ran_tests
let suite = ref (Studenttests.provided_tests @ Gradedtests.graded_tests)

let execute_tests () =
  Platform.configure_os ();
  let outcome = run_suite !suite in
  Printf.printf "%s\n" (outcome_to_string outcome);
  raise Ran_tests
    
let args =
  [ ("-linux", Set Platform.linux, "use linux-style name mangling [must preceed --test on linux]")
  ; ("--test", Unit execute_tests, "run the test suite, ignoring other files inputs")
  ; ("-op", Set_string Platform.output_path, "set the path to the output files directory  [default='output']")
  ; ("-o", Set_string executable_filename, "set the name of the resulting executable [default='a.out']")    
  ; ("-S", Clear assemble, "stop after generating .s files; do generate .o files")
  ; ("-c", Clear link, "stop after generating .o files; do not generate executables")
  ; ("--print-ll", Set print_ll_flag, "prints the program's LL code (after lowering to clang code if --clang-malloc is set)")
  ; ("--print-x86", Set print_x86_flag, "prints the program's assembly code")
  ; ("--print-regs", Set print_regs_flag, "prints the register usage statistics for x86 code")
  ; ("--clang", Set clang, "compiles to assembly using clang, not the 341 backend (implies --clang-malloc)")
  ; ("--execute-x86", Set execute_x86, "run the resulting executable file")
  ; ("-v", Set Platform.verbose, "enables more verbose compilation output")
  ] 

let files = ref []

let _ =
  Platform.configure_os ();
  Platform.create_output_dir ();
  try
    Arg.parse args (fun filename -> files := filename :: !files)
      "CIS 341 main test harness\n\
       USAGE: ./main.native [options] <files>\n\
       see README for details about using the compiler";
    Platform.configure_os ();
    process_files !files

  with Ran_tests ->
    ()
