define i64 @program(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  %2 = bitcast i64* %1 to i8*
  %3 = mul i64 1, 2
  %4 = icmp slt i64 2, %3
  ret i64 42
}

