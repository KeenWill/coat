declare void @ll_puts(i8*)

@gstr = global [14 x i8] c"hello, world!\00"

define i64 @main(i64 %argc, i8** %argv) {
  %1 = getelementptr [14 x i8], [14 x i8]* @gstr, i32 0, i32 0
  call void @ll_puts(i8* %1)
  ret i64 0
}
