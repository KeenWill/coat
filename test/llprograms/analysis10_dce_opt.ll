define i64 @program(i64 %argc, i8** %argv) {
  %4 = alloca i64
  store i64 3, i64* %4
  %7 = alloca i64
  store i64 12, i64* %7
  br i1 1, label %then, label %else
else:
  %12 = load i64, i64* %7
  %13 = add i64 %12, 10
  store i64 %13, i64* %7
  br label %merge
merge:
  %14 = load i64, i64* %7
  %16 = mul i64 %14, 30
  ret i64 %16
then:
  %10 = load i64, i64* %7
  %11 = sub i64 %10, 10
  store i64 %11, i64* %7
  br label %merge
}

