define i64 @program(i64 %argc, i8** %argv) {
  %1 = add i64 2, 0
  %2 = add i64 3, 0
  %3 = add i64 4, 0
  br i1 0, label %then, label %else
else:
  %4 = sub i64 4, 3
  br i1 1, label %then, label %merge
merge:
  ret i64 4
then:
  ret i64 2
}

