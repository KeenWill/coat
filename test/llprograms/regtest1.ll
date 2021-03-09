define i64 @foo(i64 %x, i64 %y) {
  %z = add i64 %x, %y
  ret i64 %z
}

define i64 @main(i64 %argc, i8** %arcv) {
  %v = add i64 341, 42
  %ans = call i64 @foo(i64 %v, i64 %v)
  ret i64 %ans
}
