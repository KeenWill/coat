# Oat++

Oat with type-safe channels.

## Building and running tests

Using main.native for testing:

* To run the automated test harness do:
  - on OS X:   `./main.native --test`            
  - on Linux:  `./main.native -linux --test`

* To compile ll files using the backend:
  `./main.native path/to/foo.ll`

  - creates `output/foo.s`   backend assembly code
  - creates `output/foo.o`   assembled object file
  - creates `a.out`          linked executable

 **NOTE:** by default the `.s` and `.o` files are created in 
 a directory called output, and the filenames are 
 chosen so that multiple runs of the compiler will
 not overwrite previous outputs. `foo.ll` will be 
 compiled first to `foo.s` then `foo_1.s`, `foo_2.s`, etc.


* To compile ll files using the clang backend:

```shell
  ./main.native --clang path/to/foo.ll
```

* Useful flags:

  --print-ll 
    echoes the ll program to the terminal

  --print-x86
    echoes the resulting .s file to the terminal

  --simulate-x86
    runs the resulting .s file through the reference 
    x86 simulator and outputs the result to the console

  --execute-x86
    runs the resulting a.out file natively
    (applies to either the backend or clang-compiled code)

  -v
    generates verbose output, showing which commands are used
    for linking, etc.

  -op <dirname>
    change the output path [DEFAULT=output]

  -o 
    change the generated executable's name [DEFAULT=a.out]

  -S
    stop after generating .s files 

  -c 
    stop after generating .o files 

  -h or --help
    display the list of options

* Example uses:

Run the test case `/programs/factrect.ll` using the backend:

```shell
./main.native --execute-x86 programs/factrect.ll 
--------------------------------------------------------------- Executing: a.out
* a.out returned 120
```

Run the test
