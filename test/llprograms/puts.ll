declare void @ll_puts(i8*)
declare i8* @ll_strcat(i8*, i8*)

define i64 @main(i64 %argc, i8** %argv) {
  %p1 = getelementptr i8*, i8** %argv, i32 1
  %a1 = load i8*, i8** %p1
  %p2 = getelementptr i8*, i8** %argv, i32 2
  %a2 = load i8*, i8** %p2
  %r = call i8* @strcat(i8* %a1, i8* %a2)
  ret i64 0
}

define i8* @strcat(i8* %s1, i8* %s2) {
  %p = call i8* @ll_strcat(i8* %s1, i8* %s2)
  call void @ll_puts(i8* %p)
  ret i8* %p
}
