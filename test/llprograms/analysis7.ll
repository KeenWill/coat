define i64 @program(i64 %argc, i8** %argv) {
  %1 = add i64 10, 0
  %2 = alloca i64
  store i64 %1, i64* %2
  %3 = add i64 1, 0
  %4 = alloca i64
  store i64 %3, i64* %4
  br label %guard
guard:
  %5 = load i64, i64* %4
  %6 = icmp slt i64 %5, %1
  br i1 %6, label %body, label %end
body:
  %7 = load i64, i64* %4
  %8 = mul i64 %7, 2
  store i64 %8, i64* %4
  br label %guard
end:
  ret i64 %1
}

